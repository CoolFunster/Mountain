module CategoryData where

import Data.List ( intercalate, find )
import Data.Maybe (mapMaybe, catMaybes, fromJust)
import Data.Dynamic
import Data.Typeable

import Debug.Trace (trace)
import Control.Monad

data Id = Name [Char] | Index Int | Unnamed deriving (Show, Eq)
getNameStr :: Id -> [Char]
getNameStr (Name n) = n
getNameStr (Index idx) = show idx
getNameStr Unnamed = ""

isName :: Id -> Bool
isName Name{} = True
isName _ = False

data SpecialCategoryType = Flexible | Universal deriving (Show, Eq)
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
        composition_type::CompositionType,
        inner::[Category]
    } |
    Morphism {
        input::Category,
        output::Category
    } |
    Placeholder {
        name::Id,
        ph_level::Maybe Int, -- if nothing then is interpreted as any level
        ph_category::Category
    } |
    RefinedCategory {
        base_category::Category,
        predicate::Category -- specifically predicates are morphisms of Category -> Bool
    } |
    Special {
        special_type::SpecialCategoryType
    } |
    -- language constructs
    Label {
        name::Id,
        target::Category
    } |
    Reference {
        name::Id
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
    } |
    IntermediateMorphism {
        chain::[MorphismTerm]
    }
    deriving (Eq, Show)

-- checks for data constructor type
isThing :: Category -> Bool
isThing Thing{} = True
isThing _ = False

isMorphism :: Category -> Bool
isMorphism Morphism{} = True
isMorphism _ = False

isComposite :: Category -> Bool
isComposite Composite{} = True
isComposite _ = False

isCompositeType :: CompositionType -> Category -> Bool
isCompositeType input_comp_type Composite{composition_type=cmp_comp_type} = input_comp_type == cmp_comp_type
isCompositeType _ _ = False

isPlaceholder :: Category -> Bool
isPlaceholder Placeholder{} = True
isPlaceholder _ = False

isMorphismCall :: Category -> Bool
isMorphismCall MorphismCall{} = True
isMorphismCall _ = False

isReference :: Category -> Bool
isReference Reference{} = True
isReference _ = False

isLabel :: Category -> Bool
isLabel Label{} = True
isLabel _ = False

isIntermediateMorphism :: Category -> Bool
isIntermediateMorphism IntermediateMorphism{} = True
isIntermediateMorphism _ = False

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
    isIntermediateMorphism object ||
    isComposite object ||
    isPlaceholder object ||
    isRefinedCategory object ||
    isSpecial  object ||
    isMorphismCall object && isCategoricalObject (output (asMorphism (base_morphism object)))

isCategoricalConstruct :: Category -> Bool
isCategoricalConstruct = not . isCategoricalObject

isLanguageConstruct :: Category -> Bool
isLanguageConstruct thing =
    isMorphismCall thing ||
    isDereference thing ||
    isMembership thing

isMorphic :: Category -> Bool
isMorphic Morphism{} = True
isMorphic IntermediateMorphism{} = True
isMorphic Composite{composition_type=Sumposition} = True
isMorphic Composite{composition_type=Composition} = True
isMorphic Dereference{} = True
isMorphic Special{} = True
isMorphic RefinedCategory{base_category=bc} = isMorphic bc
isMorphic Label{target=target} = isMorphic target
isMorphic _ = False

isNamedCategory :: Category -> Bool
isNamedCategory Thing{} = True
isNamedCategory Placeholder{} = True
isNamedCategory Reference{} = True
isNamedCategory _ = False

getName :: Category -> Maybe Id
getName input_category
    | not $ isNamedCategory input_category = Nothing
    | otherwise = Just $ name input_category
-- end checks for data constructor type

-- some basic functionality on categories

isRecursiveCategory :: Category -> Bool
isRecursiveCategory Label{name=rec_name,target=rec_category} = rec_name `elem` map name (freeVariables rec_category)
isRecursiveCategory _ = False

freeVariables :: Category -> [Category]
freeVariables (Thing _) = []
freeVariables (Morphism input output) = freeVariables input ++ freeVariables output
freeVariables (Composite _ inner) = concatMap freeVariables inner
freeVariables ph@(Placeholder _ _ ph_category) = ph : freeVariables ph_category
freeVariables MorphismCall{base_morphism=bm, argument=a} = freeVariables bm ++ freeVariables a
freeVariables f@Special{} = []
freeVariables r@Reference{} = [r]
freeVariables RefinedCategory{base_category=bc} = freeVariables bc
freeVariables Dereference{base_category=bc, category_id=_category_id} = 
    case dereference _category_id bc of
        Just something -> freeVariables something
        _ -> []
freeVariables Membership{small_category=sc} = freeVariables sc
freeVariables Label{name=name,target=category} = filter (/= Reference{name=name}) $ freeVariables category
freeVariables i@IntermediateMorphism{} = freeVariables (asMorphism i)

-- replace :: base_expr -> old -> new -> new_expr 
-- idea compare base expr to old. if same replace with new. otherwise return old. recurse
replace :: Category -> Category -> Category -> Category
replace base_expr old new
    | base_expr == old = new
    | otherwise =
        case base_expr of
            Thing _ -> base_expr
            Composite composite_type inner -> Composite composite_type (map replace inner <*> [old] <*> [new])
            Morphism input output -> Morphism (replace input old new) (replace output old new)
            Placeholder id ph_level ph_category -> Placeholder id ph_level (replace ph_category old new)
            MorphismCall bm a -> MorphismCall (replace bm old new) (replace a old new)
            Dereference base_category id -> Dereference (replace base_category old new) id
            Special{} -> base_expr
            Reference{} -> base_expr
            l@Label{name=name, target=target} -> l{target=replace target old new}
            Membership{big_category=bc, small_category=sc} -> Membership{big_category=replace bc old new, small_category=replace sc old new}
            RefinedCategory {base_category=bc, predicate=p} -> RefinedCategory{base_category=replace bc old new, predicate=replace p old new}
            IntermediateMorphism{chain=im_chain} -> IntermediateMorphism{chain=map replaceMorphismTerm im_chain <*> [old] <*> [new]} 

replaceReferences :: Category -> Category -> Category
replaceReferences base_expr l@Label{name=label_name, target=c_target} = replace base_expr Reference{name=label_name} (unfold Recursive l)
replaceReferences base_expr p@Placeholder{name=ph_name} = replace base_expr Reference{name=ph_name} p
replaceReferences be l = error $ "bad replace references: " ++ show be ++ " replacing " ++ show l

data UnfoldType = Flat | Recursive
unfold :: UnfoldType -> Category -> Category
unfold Flat l@Label{name=rec_name, target=rec_cat} = replace rec_cat Reference{name=rec_name} Special{special_type=Flexible}
unfold Recursive l@Label{name=rec_name, target=rec_cat} = replace rec_cat Reference{name=rec_name} l
unfold _ something_else = error $ "Cannot unfold a non labeled category: " ++ show something_else

dereference :: Id -> Category -> Maybe Category
dereference id Label{name=l_name, target=l_target}
    | l_name == id = Just l_target
    | otherwise = dereference id l_target
dereference id t@Thing{name=t_name}
    | id == t_name = Just t
    | otherwise = Nothing
dereference id p@Placeholder{name=p_name,ph_category=ph_category}
    | id == p_name = Just p
    | otherwise = 
        case dereference id ph_category of
            Nothing -> Nothing
            Just something -> Just p{name=Unnamed, ph_category=something}
dereference id r@Reference{name=r_name}
    | id == r_name = Just r
    | otherwise = Just Special{special_type=Flexible}
dereference id c@Composite{inner=inner} =
    case id of
        Index idx -> Just (inner!!idx)
        Name str_name ->
            case mapMaybe (dereference id) inner of
                [] -> Nothing
                x:stuff -> Just x
        Unnamed -> error "Cannot dereference with an Unnamed"
dereference id m@Morphism{input=input, output=output} =
    case id of
        Name "input" -> Just input
        Name "output" -> Just output
        _ -> Nothing
dereference id m@IntermediateMorphism{chain=[]} = Nothing 
dereference id m@IntermediateMorphism{chain=[head,tail]} = 
    case id of
        Name "input" -> Just (m_category head)
        Name "output" -> Just (m_category tail)
        _ -> Nothing
dereference id m@IntermediateMorphism{chain=(head:tail)} = 
    case id of
        Name "input" -> Just (m_category head)
        Name "output" -> Just (IntermediateMorphism{chain=tail})
        _ -> Nothing
dereference id other = Nothing

asMorphism :: Category -> Category
asMorphism input_category
    | not . isMorphic $ input_category = error $ "unable to represent as morphism: " ++ show input_category
    | otherwise =
        case input_category of
            m@Morphism{} -> m
            (Composite Composition comp_inner) -> Morphism (input (head morphised_comp_inner)) (output (last morphised_comp_inner))
                where
                    morphised_comp_inner = map asMorphism comp_inner
            (Composite Sumposition comp_inner) -> Morphism (Composite Sum (map (input . asMorphism) comp_inner)) (Composite Sum (map (output . asMorphism) comp_inner))
            f@Special{special_type=Flexible} -> Morphism f f
            Dereference{base_category=bc, category_id=id} -> asMorphism $ fromJust $ dereference id bc
            Label{target=target} -> asMorphism target
            im@IntermediateMorphism{} -> imToMorphism im
            something -> error $ "Missing definition in isMorphic: " ++ show something

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
                (Composite Higher inner) -> Just (+1) <*> getInnerLevel inner
                (Composite _ inner) -> getInnerLevel inner
                Morphism{input=input,output=output} -> getInnerLevel [input, output]
                Placeholder{ph_level=ph_level} -> ph_level
                m@MorphismCall{base_morphism=bm, argument=a} -> if isMorphic bm then level $ output (asMorphism bm) else Nothing
                Special{} -> Nothing
                Reference{} -> Nothing
                Label{target=target} -> level target
                Dereference{base_category=bc,category_id=id} -> level bc
                RefinedCategory {base_category=_base_category, predicate=_predicate} -> level _base_category
                Membership {small_category=sc} -> level sc 
                im@IntermediateMorphism{} -> level (asMorphism im)
    in
        if isRecursiveCategory input_category then Just (+1) <*> basicLevel input_category else basicLevel input_category

levelIsContained :: Maybe Int -> Maybe Int -> Bool 
-- candidate category, contained category
levelIsContained Nothing Nothing = True
levelIsContained Nothing (Just _) = True
levelIsContained (Just _) Nothing = False
levelIsContained (Just a) (Just b) = a < b
-- useful categories

valid :: Category
valid = Composite Product []

empty :: Category
empty = Composite Sum []

universal :: Category
universal = Special{special_type=Universal}

flex :: Category
flex = Special{special_type=Flexible}

-- Intermediate morphism representation for parsing

data MorphismTermType = Import | Given | Definition | Return deriving (Show, Eq)
data MorphismTerm = 
    MorphismTerm {
        m_type::MorphismTermType,
        m_category::Category
    } |
    MorphismTermChain {
        c_input::MorphismTerm,
        c_output::MorphismTerm
    } deriving (Show, Eq)

-- NEXT TODO: Fix this to handle definitions and replacing things in the chain
imToMorphism :: Category -> Category
imToMorphism IntermediateMorphism{chain=[]} = error "empty morphism chain list?"
imToMorphism IntermediateMorphism{chain=[something]} = error "single morphism chain list?"
imToMorphism IntermediateMorphism{chain=[head,tail]} = Morphism{input=m_category head, output=m_category tail}
imToMorphism IntermediateMorphism{chain=(head:tail)} = Morphism{input=m_category head, output=imToMorphism IntermediateMorphism{chain=tail}}
imToMorphism somthing_weird = error $ "Only supports Intermediate Morphisms. Passed in " ++ show somthing_weird

morphismToTermList :: Category -> [MorphismTerm]
morphismToTermList Morphism{input=m_input, output=m@Morphism{}} = MorphismTerm{m_type=Given, m_category=m_input}:morphismToTermList m
morphismToTermList Morphism{input=m_input, output=anything_else} = [MorphismTerm{m_type=Given, m_category=m_input}, MorphismTerm{m_type=Return,m_category=anything_else}]
morphismToTermList anything_else = error $ "categoryToIM only supports Morphisms. gave " ++ show anything_else

morphismToIm :: Category -> Category
morphismToIm m@Morphism{} = IntermediateMorphism{chain=morphismToTermList m}
morphismToIm something_else = error $ "morphismToIm must take in a Morphism specifically. Got " ++ show something_else

replaceMorphismTerm :: MorphismTerm -> Category -> Category -> MorphismTerm
replaceMorphismTerm mt@MorphismTerm{m_category=base_category} old_category new_category = mt{m_category=replace base_category old_category new_category}
replaceMorphismTerm MorphismTermChain{c_input=input, c_output=output} old_category new_category = MorphismTermChain{c_input=replaceMorphismTerm input old_category new_category, c_output=replaceMorphismTerm output old_category new_category} 

isMorphismTermOfTypes :: [MorphismTermType] -> MorphismTerm -> Bool
isMorphismTermOfTypes allowable_term_types morphism_term = m_type morphism_term `elem` allowable_term_types

uncurryMorphismTermChain :: MorphismTerm -> [MorphismTerm]
uncurryMorphismTermChain MorphismTermChain{c_input=input, c_output=mc@MorphismTermChain{}} = input:uncurryMorphismTermChain mc
uncurryMorphismTermChain MorphismTermChain{c_input=input, c_output=anything_else} = [input, anything_else]
uncurryMorphismTermChain m@MorphismTerm{} = error $ "Bad morphism term chain with only one term. " ++ show (m_category m)

validMorphismTermSequence :: [MorphismTerm] -> Bool
validMorphismTermSequence [] = False
validMorphismTermSequence populated_term_list = and ([
        isMorphismTermOfTypes [Import, Given, Definition, Return] (head populated_term_list),
        isMorphismTermOfTypes [Return] (last populated_term_list)
    ] ++ map (isMorphismTermOfTypes [Given,Definition,Return]) (tail populated_term_list))
