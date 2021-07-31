module CategoryData where

import Data.List (intercalate)

type Id = [Char]
data CompositionType = Product | Sum | Higher | Composition | Sumposition deriving (Show, Eq)

data Category = 
    -- basic categories
    Thing { name::Id } |
    Composite { 
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
        ph_level::Maybe Int,
        ph_category::Category
    } |
    FlexibleCategory {
        name::Id
    } |
    -- language constructs
    MorphismCall {
        base_morphism::Category,
        argument::Category
    } |
    Dereference {
        base_category::Category,
        category_id::Id
    } |
    RecursiveCategory {
        inner_expr::Category -- this has to be a morphism of (ph -> rec_inner)
    }
    deriving (Eq)

instance Show Category where
    show Thing{name=name} = "`"++name++"`"
    show Composite{name=name,composition_type=Product,inner=inner} = name++":("++intercalate "," (map show inner)++")"
    show Composite{name=name,composition_type=Composition,inner=inner} = name++":("++intercalate "," (map show inner)++")"
    show Composite{name=name,composition_type=Sum,inner=inner} = name++":|"++intercalate "," (map show inner)++"|"
    show Composite{name=name,composition_type=Sumposition,inner=inner} = name++":|"++intercalate "," (map show inner)++"|"
    show Composite{name=name,composition_type=Higher,inner=inner} = name++":<"++intercalate "," (map show inner)++">"
    show Morphism{name=name,input=input,output=output} = "Morphism{"++name++","++show input++"->"++show output++"}"
    show Placeholder{name=name,ph_level=ph_level,ph_category=ph_category} = "PH_"++show ph_level++"{"++name++"@"++show ph_category++"}"
    show MorphismCall{base_morphism=m, argument=a} = show m ++ "(" ++ show a ++ ")"
    show RecursiveCategory{inner_expr=Morphism{input=input}} = "<" ++ show (name input) ++ ">"
    show FlexibleCategory{name=name} = "$" ++ name
    show Dereference{base_category=bc,category_id=id} = show bc ++ "[" ++ id ++ "]"

-- checks for data constructor type
isThing :: Category -> Bool
isThing Thing{} = True
isThing MorphismCall{base_morphism=bm} = isThing $ output bm
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

isFlexibleCategory :: Category -> Bool
isFlexibleCategory FlexibleCategory{} = True
isFlexibleCategory _ = False

isDereference :: Category -> Bool
isDereference Dereference{} = True
isDereference _ = False

-- end checks for data constructor type

valid :: Category
valid = Composite "valid" Product []

empty :: Category
empty = Composite "empty" Sum []
