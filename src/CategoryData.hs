module CategoryData where

import Data.List ( intercalate, find )
import Data.Maybe (mapMaybe, catMaybes, fromJust)
import Data.Dynamic
import Data.Typeable

import Debug.Trace (trace)

data Id = Name [Char] | Index Int | Unnamed deriving (Show, Eq)
getNameStr :: Id -> [Char]
getNameStr (Name n) = n
getNameStr (Index idx) = show idx
getNameStr Unnamed = ""

data SpecialCategoryType = Flexible | Reference | Universal deriving (Show, Eq)
data CompositionType =
    Product | -- tuple
    Sum | -- a or b, union
    Higher | -- the type operator
    Composition | -- a chain of functions (a->b, b->c = a->c)
    Sumposition -- a case statement of functions (a->b, b->c = a->c)
    deriving (Show, Eq)

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
        ph_level::Maybe Int, -- if nothing then is interpreted as any level
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
    -- language constructs
    MorphismCall {
        name::Id,
        base_morphism::Category,
        argument::Category
    } |
    Dereference {
        name::Id,
        base_category::Category,
        category_id::Id
    } |
    Membership {
        name::Id,
        big_category::Category,
        small_category::Category
    } |   
    -- TODO: Add this to all the functions
    IntermediateMorphism {
        name::Id,
        chain::IntermediateMorphism
    }
    deriving (Eq, Show)

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
isRecursiveCategory cat
    | isCategoricalObject cat = name cat `elem` map name (freeVariables cat)
    | otherwise = False

isSpecialCategory :: SpecialCategoryType -> Category -> Bool
isSpecialCategory sctype Special{special_type=c_type} = sctype == c_type
isSpecialCategory sctype other = False

isSpecial :: Category -> Bool
isSpecial Special{} = True
isSpecial _ = False 

isDereference :: Category -> Bool
isDereference Dereference{} = True
isDereference _ = False

isMembership :: Category -> Bool
isMembership Membership{} = True
isMembership _ = False

isRefinedCategory :: Category -> Bool
isRefinedCategory RefinedCategory{} = True
isRefinedCategory _ = False

isCategoricalObject :: Category -> Bool
isCategoricalObject object =
    isThing object ||
    isMorphism object ||
    isComposite object ||
    isPlaceholder object ||
    isRefinedCategory object ||
    isSpecial  object

isCategoricalConstruct :: Category -> Bool
isCategoricalConstruct = not . isCategoricalObject

isLanguageConstruct :: Category -> Bool
isLanguageConstruct thing =
    isMorphismCall thing ||
    isDereference thing ||
    isMembership thing

isMorphic :: Category -> Bool
isMorphic Morphism{} = True
isMorphic Composite{composition_type=Sumposition} = True
isMorphic Composite{composition_type=Composition} = True
isMorphic MorphismCall{base_morphism=base_morphism} = True
isMorphic Dereference{} = True
isMorphic Special{} = True
isMorphic RefinedCategory{base_category=bc} = isMorphic bc
isMorphic _ = False

-- end checks for data constructor type

-- some basic functionality on categories

freeVariables :: Category -> [Category]
freeVariables (Thing _) = []
freeVariables (Morphism _ input output) = freeVariables input ++ freeVariables output
freeVariables (Composite _ _ inner) = concatMap freeVariables inner
freeVariables ph@(Placeholder _ _ ph_category) = ph : freeVariables ph_category
freeVariables MorphismCall{base_morphism=bm, argument=a} = freeVariables bm ++ freeVariables a
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
            MorphismCall id bm a -> MorphismCall id (replace bm old new) (replace a old new)
            Dereference d_id base_category id -> Dereference d_id (replace base_category old new) id
            Special{} -> base_expr
            RefinedCategory {} -> error "not implemented"
            Membership {} -> error "not implemented"

data UnfoldType = Flat | Recursive
unfold :: UnfoldType -> Category -> Category
unfold Flat category 
    | not $ isRecursiveCategory category = category
    | otherwise = replace category Special{name=name category, special_type=Reference} Special{name=name category, special_type=Flexible}
unfold Recursive category
    | not $ isRecursiveCategory category = category
    | otherwise = replace category Special{name=name category, special_type=Reference} category

dereference :: Id -> Category -> Maybe Category
dereference id category
        | name category == id = Just category
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
                    Unnamed -> if name == Unnamed then Just category else Nothing
            m@Morphism{input=input, output=output} ->
                case id of
                    Name "input" -> Just input
                    Name "output" -> Just output
                    _ -> Nothing
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
            Dereference{base_category=bc, category_id=id} -> asMorphism $ fromJust $ dereference id bc
            something -> error $ "not covered: " ++ show something

level :: Category -> Maybe Int
level input_category =
    let
        getInnerLevel inner_categories =
            case mapMaybe level inner_categories of
                [] -> Nothing
                levels -> Just $ maximum levels 
        basicLevel a_category = 
            case input_category of
                (Thing t) -> Just 0
                (Composite _ Higher inner) -> Just (+1) <*> getInnerLevel inner
                (Composite _ _ inner) -> getInnerLevel inner
                Morphism{input=input,output=output} -> getInnerLevel [input, output]
                Placeholder{ph_level=ph_level} -> ph_level
                MorphismCall{base_morphism=bm} -> level $ output bm
                Special{} -> Nothing
                Dereference{base_category=bc,category_id=id} -> level bc
                RefinedCategory {name=_name, base_category=_base_category, predicate=_predicate} -> level _base_category
                Membership {} -> error "Not implemented"
    in
        if isRecursiveCategory input_category then Just (+1) <*> basicLevel input_category else basicLevel input_category

-- useful categories

valid :: Category
valid = Composite (Name "valid") Product []

empty :: Category
empty = Composite (Name "empty") Sum []

-- Intermediate morphism representation for parsing

data MorphismTermType = Import | Given | Definition | Return deriving (Show, Eq)
data IntermediateMorphism =
    MorphismTerm {
        m_type::MorphismTermType,
        m_category::Category
    } |
    MorphismChain {
        m_input::IntermediateMorphism,
        m_output::IntermediateMorphism
    } deriving (Show, Eq)

-- NEXT TODO: Fix this to handle definitions and replacing things in the chain
imToMorphism :: IntermediateMorphism -> Category
-- imToMorphism MorphismChain{m_input=MorphismTerm{m_type=Definition, m_category=category}, m_output=m_out} = 
imToMorphism MorphismChain{m_input=m_in, m_output=m_out} = Morphism{name=Unnamed, input=m_category m_in, output=imToMorphism m_out}
imToMorphism MorphismTerm{m_category=category} = category

replaceIMTerm :: IntermediateMorphism -> Category -> Category -> IntermediateMorphism
replaceIMTerm mt@MorphismTerm{m_category=base_category} old_category new_category = mt{m_category=replace base_category old_category new_category}
replaceIMTerm mt@MorphismChain{m_input=m_in, m_output=m_out} old_category new_category = mt{m_input=replaceIMTerm m_in old_category new_category, m_output=replaceIMTerm m_out old_category new_category}

categoryToIm :: Category -> IntermediateMorphism
categoryToIm Morphism{name=name, input=m_input, output=m@Morphism{}} = 
    MorphismChain{
        m_input=MorphismTerm{
            m_type=Given, 
            m_category=m_input
        },
        m_output=categoryToIm m
    }
categoryToIm Morphism{name=name, input=m_input, output=anything_else} = 
    MorphismChain{
        m_input=MorphismTerm{
            m_type=Given, 
            m_category=m_input
        },
        m_output=MorphismTerm{
            m_type=Return, 
            m_category=anything_else
        }
    }
categoryToIm anything_else = error $ "categoryToIM only supports Morphisms. gave " ++ show anything_else

validateIM :: Bool -> IntermediateMorphism -> Bool
validateIM is_head_of_chain MorphismTerm{m_type=the_type} = not is_head_of_chain && (the_type == Return)
validateIM is_head_of_chain MorphismChain{m_input=m_input, m_output=m_output} = 
    if is_head_of_chain
        then m_type m_input `elem` [Import, Given, Definition] && validateIM False m_output
        else m_type m_input `elem` [Given, Definition] && validateIM False m_output
