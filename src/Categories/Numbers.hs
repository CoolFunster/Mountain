module Categories.Numbers where

import CategoryData
import Data.Maybe ( fromJust )

{- Natural numbers -}
natural :: Category
natural = makeRecursiveCategory (Name "self") Composite{
    name=Name "nat",
    composition_type=Sum,
    inner=[
        Thing (Name "zero"),
        Composite{
            name=Name "succ",
            composition_type=Product,
            inner=[Special{name=Name "self", special_type=Reference}]
        }
    ]
}

zero :: Category
zero = fromJust $ dereference (Name "zero") (unfold Recursive natural)

positiveNatural :: Category
positiveNatural = fromJust $ dereference (Name "succ") (unfold Recursive natural)

increment :: Category
increment = Morphism{
    name=Name "increment",
    input=Placeholder{name=Name "x", ph_level=Just 0, ph_category=natural},
    output=Composite{
        name=Name "succ",
        composition_type=Product,
        inner=[Special{name=Name "x", special_type=Reference}]
    }
}

decrement :: Category
decrement = Morphism{
    name=Name "decrement",
    input=Placeholder{name=Name "x", ph_level=Just 0, ph_category=positiveNatural},
    output=Dereference{base_category=Special{name=Name "x", special_type=Reference}, category_id=Index 0}
}
{- END: Natural numbers -}

{- Integer numbers -}
integer :: Category
integer = Composite{
    name=Name "integer",
    composition_type=Sum,
    inner=[
        Composite{
            name=Name "+",
            composition_type=Product,
            inner=[positiveNatural]
        },
        zero,
        Composite{
            name=Name "-",
            composition_type=Product,
            inner=[positiveNatural]
        }
    ]
}
