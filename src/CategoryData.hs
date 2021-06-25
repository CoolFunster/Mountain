module CategoryData where

type Id = [Char]
data CompositionType = Product | Sum | Higher | Composition | Sumposition deriving (Show, Eq)

data Category = 
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
        ph_level::Int,
        ph_category::Category
    }
    deriving (Show, Eq)

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

compositionAsMorphism :: Category -> Category
compositionAsMorphism (Composite _ Composition comp_inner) = Morphism "" (input (head comp_inner)) (output (last comp_inner))

isPlaceholder :: Category -> Bool
isPlaceholder Placeholder{} = True
isPlaceholder _ = False
