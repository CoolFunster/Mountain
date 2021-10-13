module CategoryData where

import Data.List (intercalate)
import Data.Maybe (mapMaybe, catMaybes, fromJust)
import Data.List (find)
import Data.Dynamic
import Data.Typeable

import Debug.Trace (trace)
import Foreign.Marshal.Alloc (free)

data Id = Name [Char] | Index Int | Unnamed deriving (Show, Eq)
getNameStr :: Id -> [Char]
getNameStr (Name n) = n
getNameStr (Index idx) = show idx
getNameStr Unnamed = ""

data CompositionType = 
    Product | -- tuple
    Sum | -- a or b, union
    Higher | -- the type operator
    Composition | -- a chain of functions (a->b, b->c = a->c)
    Sumposition  -- if else statement
    deriving (Show, Eq)
data SpecialCategoryType = Flexible | Reference | Universal | Any deriving (Show, Eq)
data ForeignAttached = 
    HaskellObject Dynamic |
    HaskellType TypeRep |
    HaskellFunction (Category -> Maybe Category)
instance Eq ForeignAttached where
    (==) _ _ = True
instance Show ForeignAttached where
    show any = ""

data Category = 
    -- categories
    Thing { name::Id } | -- a concrete object
    Composite {  -- a group of things
        name::Id, 
        composition_type::CompositionType, 
        inner::[Category]
    } |
    Morphism { 
        name::Id, 
        input::Category, 
        output::Category
    } |
    Placeholder {
        name::Id,
        ph_level::Maybe Int, -- if nothing then is interpreted as just has
        ph_category::Category
    } |
    RefinedCategory {
        name::Id,
        base_category::Category,
        predicate::Category -- specifically predicates are morphisms of Category -> Bool
    } |
    Special {
        name::Id,
        special_type::SpecialCategoryType
    } |
    ForeignCategory {
        name::Id,
        category_type::Category,
        attached::ForeignAttached
    } |
    -- language constructs
    RecursiveCategory {
        inner_expr::Category
    } |
    MorphismCall {
        base_morphism::Category,
        argument::Category
    } |
    Dereference {
        base_category::Category,
        category_id::Id
    } |
    Membership {
        big_category::Category,
        small_category::Category
    }
    deriving (Eq, Show)

-- instance Show Category where
--     show Thing{name=name} = "`"++ show name ++"`"
--     show Composite{name=name,composition_type=Product,inner=inner} = show name ++":("++intercalate "," (map show inner)++")"
--     show Composite{name=name ,composition_type=Composition,inner=inner} = show name ++":("++intercalate "," (map show inner)++")"
--     show Composite{name=name,composition_type=Sum,inner=inner} = show name ++":|"++intercalate "," (map show inner)++"|"
--     show Composite{name=name,composition_type=Sumposition,inner=inner} = show name ++":|"++intercalate "," (map show inner)++"|"
--     show Composite{name=name,composition_type=Higher,inner=inner} = show name ++":<"++intercalate "," (map show inner)++">"
--     show Morphism{name=name,input=input,output=output} = "Morphism{"++show name ++","++show input++"->"++show output++"}"
--     show Placeholder{name=name,ph_level=Just ph_level,ph_category=ph_category} = show name ++"_"++show ph_level++"@"++show ph_category
--     show Placeholder{name=name,ph_level=Nothing,ph_category=ph_category} = show name ++"@"++show ph_category++"}"
--     show MorphismCall{base_morphism=m, argument=a} = show m ++ "(" ++ show a ++ ")"
--     show RecursiveCategory{inner_expr=Morphism{input=input}} = "$$" ++ show (name input)
--     show RecursiveCategory{} = error "not supported"
--     show Special{name=name, special_type=Reference} = "$" ++ show name
--     show Special{name=name, special_type=Flexible} = "<~" ++ show name ++ "~>"
--     show Special{name=name, special_type=Universal} = "**" ++ show name ++ "**"
--     show Special{name=name, special_type=Any} = show name ++ ":Any"
--     show Dereference{base_category=bc,category_id=id} = show bc ++ "." ++ show id
--     show Membership{big_category=bc,small_category=sc} = "(" ++ show bc ++ " :: " ++ show sc ++ ")"
--     show ForeignCategory{name=name,category_type=ct} = "External{" ++ show name ++ "," ++ show ct ++ "}"
--     show RefinedCategory{name=_name, base_category=_base_category, predicate=_predicate} = "{" ++ show _base_category ++ " | " ++ show _predicate ++ "}"

-- checks for data constructor type
isThing :: Category -> Bool
isThing Thing{} = True
isThing MorphismCall{base_morphism=bm, argument=a} = isThing $ output bm
isThing _ = False

isMorphism :: Category -> Bool
isMorphism Morphism{} = True
isMorphism MorphismCall{base_morphism=bm} = isMorphism $ output bm
isMorphism _ = False

isComposite :: Category -> Bool
isComposite Composite{} = True
isComposite MorphismCall{base_morphism=bm} = isComposite $ output bm
isComposite _ = False

isCompositeType :: CompositionType -> Category -> Bool
isCompositeType input_comp_type Composite{composition_type=cmp_comp_type} = input_comp_type == cmp_comp_type
isCompositeType input_comp_type MorphismCall{base_morphism=bm} = isCompositeType input_comp_type $ output bm
isCompositeType _ _ = False

isPlaceholder :: Category -> Bool
isPlaceholder Placeholder{} = True
isPlaceholder _ = False

isMorphismCall :: Category -> Bool
isMorphismCall MorphismCall{} = True
isMorphismCall _ = False

isRecursiveCategory :: Category -> Bool
isRecursiveCategory RecursiveCategory{} = True
isRecursiveCategory _ = False

isSpecialCategory :: SpecialCategoryType -> Category -> Bool
isSpecialCategory sctype Special{special_type=c_type} = sctype == c_type
isSpecialCategory sctype other = False

isDereference :: Category -> Bool
isDereference Dereference{} = True
isDereference _ = False

isForeignCategory :: Category -> Bool
isForeignCategory ForeignCategory{} = True
isForeignCategory _ = False

isMembership :: Category -> Bool
isMembership Membership{} = True
isMembership _ = False

isNamedCategory :: Category -> Bool
isNamedCategory Thing{} = True
isNamedCategory Composite{} = True
isNamedCategory Morphism{} = True
isNamedCategory Placeholder{} = True
isNamedCategory RefinedCategory{} = True
isNamedCategory Special{} = True
isNamedCategory ForeignCategory{} = True
isNamedCategory other = False

isCategoricalObject :: Category -> Bool
isCategoricalObject object = 
    isThing object || 
    isMorphism object || 
    isComposite object || 
    isPlaceholder object ||
    (isRecursiveCategory object && isCategoricalObject (inner_expr object)) ||
    (isForeignCategory object && isCategoricalObject (category_type object))

isCategoricalConstruct :: Category -> Bool
isCategoricalConstruct = not . isCategoricalObject

isLanguageConstruct :: Category -> Bool
isLanguageConstruct thing =
    isRecursiveCategory thing ||
    isMorphismCall thing ||
    isDereference thing ||
    isMembership thing

isMorphic :: Category -> Bool
isMorphic Morphism{} = True
isMorphic Composite{composition_type=Sumposition} = True
isMorphic Composite{composition_type=Composition} = True
isMorphic MorphismCall{base_morphism=base_morphism} = True
isMorphic RecursiveCategory{inner_expr=Morphism{output=rec_category}} = isMorphic rec_category
isMorphic Dereference{} = True
isMorphic Special{} = True
isMorphic RefinedCategory{base_category=bc} = isMorphic bc
isMorphic ForeignCategory{category_type=ct} = isMorphic ct
isMorphic _ = False

-- end checks for data constructor type

-- higher constructors
makeTuple :: Id -> [Category] -> Category
makeTuple id = Composite id Product

makeSumple :: Id -> [Category] -> Category
makeSumple id = Composite id Sum
-- end higher constructors

-- some basic functionality on categories

freeVariables :: Category -> [Category]
freeVariables (Thing _) = []
freeVariables (Morphism _ input output) = freeVariables input ++ freeVariables output
freeVariables (Composite _ _ inner) = concatMap freeVariables inner
freeVariables ph@(Placeholder _ _ ph_category) = ph : freeVariables ph_category
freeVariables rc@RecursiveCategory{} = freeVariables $ unfold Flat rc
freeVariables MorphismCall{base_morphism=bm, argument=a} = freeVariables bm ++ freeVariables a
freeVariables ForeignCategory{category_type=ct} = freeVariables ct
freeVariables f@Special{} = [f]
freeVariables RefinedCategory{base_category=bc} = freeVariables bc
freeVariables Dereference{base_category=bc, category_id=_category_id} = error "not implemented yet"
freeVariables Membership{small_category=sc} = freeVariables sc

-- replace :: base_expr -> old -> new -> new_expr 
-- idea compare base expr to old. if same replace with new. otherwise return old. recurse
-- TODO: foreign categories
replace :: Category -> Category -> Category -> Category
replace base_expr old new 
    | base_expr == old = new
    | isSpecialCategory Reference base_expr && name base_expr == name old = new
    | otherwise = 
        case base_expr of
            Thing _ -> base_expr
            Composite id composite_type inner -> Composite id composite_type (sequence (sequence (map replace inner) old) new)
            Morphism id input output -> Morphism id (replace input old new) (replace output old new)
            Placeholder id ph_level ph_category -> Placeholder id ph_level (replace ph_category old new)
            MorphismCall bm a -> MorphismCall (replace bm old new) (replace a old new)
            Dereference base_category id -> Dereference (replace base_category old new) id
            rc@RecursiveCategory{} -> RecursiveCategory $ replace (inner_expr rc) old new
            Special{} -> base_expr
            RefinedCategory {} -> error "not implemented"
            ForeignCategory {} -> error "not implemented"
            Membership {} -> error "not implemented"

data UnfoldType = Flat | Recursive
unfold :: UnfoldType -> Category -> Category
unfold Flat RecursiveCategory{inner_expr=Morphism{output=rest_of_expr}} = rest_of_expr
unfold Recursive rc@RecursiveCategory{inner_expr=Morphism{input=rec_ph, output=rest_of_expr}} = replace rest_of_expr rec_ph rc
unfold _ _ = error "Bad input"

makeRecursiveCategory :: Id -> Category -> Category
makeRecursiveCategory rec_ph_name rec_cat 
    | Special{name=rec_ph_name, special_type=Reference} `notElem` freeVariables rec_cat = error $ "Can't make recursive expr. Does not have a recursive ph of name " ++ show rec_ph_name
    | otherwise = RecursiveCategory{inner_expr=Morphism{name=rec_ph_name,input=Special{name=rec_ph_name, special_type=Reference},output=rec_cat}}

dereference :: Id -> Category -> Maybe Category
dereference id category
        | isNamedCategory category && name category == id = Just category
        | otherwise =
        case category of
            t@Thing{name=name} -> Nothing
            c@Composite{name=name, inner=inner} -> 
                case id of
                    Index idx -> Just (inner!!idx)
                    Name str_name ->
                        case mapMaybe (dereference id) inner of
                            [] -> Nothing
                            x:stuff -> Just x
            m@Morphism{input=input, output=output} ->
                case id of
                    Name "input" -> Just input
                    Name "output" -> Just output
                    _ -> Nothing
            m@RecursiveCategory{} -> dereference id (unfold Recursive m)
            ForeignCategory{category_type=ct} -> dereference id ct
            _ -> Nothing

asMorphism :: Category -> Category
asMorphism input_category
    | not . isMorphic $ input_category = error $ "unable to represent as morphism: " ++ show input_category
    | otherwise =
        case input_category of
            m@Morphism{} -> m
            (Composite _ Composition comp_inner) -> Morphism (Name "") (input (head morphised_comp_inner)) (output (last morphised_comp_inner))
                where  
                    morphised_comp_inner = map asMorphism comp_inner
            (Composite _ Sumposition comp_inner) -> Morphism (Name "") (Composite (Name "") Sum (map (input . asMorphism) comp_inner)) (Composite (Name "") Sum (map (output . asMorphism) comp_inner))
            MorphismCall{base_morphism=bm} -> asMorphism $ output bm
            f@Special{name=f_name, special_type=Flexible} -> Morphism f_name f f
            rc@RecursiveCategory{} -> asMorphism $ unfold Recursive rc
            ForeignCategory{category_type=ct} -> asMorphism ct
            Dereference{base_category=bc, category_id=id} -> asMorphism $ fromJust $ dereference id bc
            something -> error $ "not covered: " ++ show something

level :: Category -> Maybe Int
level input_category =
    let
        getInnerLevel inner_categories = 
            case mapMaybe level inner_categories of
                [] -> Nothing
                levels -> Just $ maximum levels
    in
        case input_category of
            (Thing t) -> Just 0
            (Composite _ Higher inner) -> Just (+1) <*> getInnerLevel inner
            (Composite _ _ inner) -> getInnerLevel inner 
            Morphism{input=input,output=output} -> getInnerLevel [input, output]
            Placeholder{ph_level=ph_level} -> ph_level
            RecursiveCategory{inner_expr=inner_expr} -> Just (+1) <*> level inner_expr
            MorphismCall{base_morphism=bm} -> level $ output bm
            Special{} -> Nothing
            ForeignCategory{category_type=ct} -> level ct
            Dereference{base_category=bc,category_id=id} -> level bc
            RefinedCategory {name=_name, base_category=_base_category, predicate=_predicate} -> level _base_category
            Membership {} -> error "Not implemented"

-- useful categories

valid :: Category
valid = Composite (Name "valid") Product []

empty :: Category
empty = Composite (Name "empty") Sum []
