module Categories.Numbers where

import CategoryData

{- Natural numbers -}
natural :: Category
natural = makeRecursiveCategory "self" Composite{
    name="nat",
    composition_type=Sum,
    inner=[
        Thing "zero",
        Composite{
            name="succ",
            composition_type=Product,
            inner=[Special{name="self", special_type=Reference}]
        }
    ]
}

increment :: Category
increment = Morphism{
    name="increment",
    input=Placeholder{name="x", ph_level=Just 0, ph_category=natural},
    output=Composite{
        name="succ",
        composition_type=Product,
        inner=[Special{name="x", special_type=Reference}]
    }
}

decrement :: Category
decrement = Morphism{
    name="increment",
    input=Placeholder{name="x", ph_level=Just 0, ph_category=positiveNatural},
    output=Dereference{base_category=Special{name="x", special_type=Reference}, category_id="x"}
}

zero :: Category
zero = Dereference{base_category=natural, category_id="zero"}

positiveNatural :: Category
positiveNatural = Dereference{base_category=natural, category_id="succ"}
{- END: Natural numbers -}

{- Integer numbers -}
integer :: Category
integer = Composite{
    name="integer",
    composition_type=Sum,
    inner=[
        Composite{
            name="+",
            composition_type=Product,
            inner=[positiveNatural]
        },
        zero,
        Composite{
            name="-",
            composition_type=Product,
            inner=[positiveNatural]
        }
    ]
}
