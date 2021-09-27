module Categories.Base where

import Data.Maybe (fromJust)

import CategoryData
import CategoryCore

boolean :: Category
boolean = Composite{
    name=Name "Boolean",
    composition_type=Higher,
    inner=[
        Thing (Name "true"),
        Thing (Name "false")
    ]
}

{- NATURAL NUMBERS -}
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

add :: Category
add = Morphism{
    name= Name "+",
    input=Composite{
        name=Name "+_args",
        composition_type=Product,
        inner=[
            Placeholder{name=Name "left",ph_level=Just 0,ph_category=natural},
            Placeholder{name=Name "right",ph_level=Just 0,ph_category=natural}
        ]
    },
    output=Composite{
        name=Name "output",
        composition_type=Sumposition,
        inner=[
            Morphism{
                name=Name "identity",
                input=Composite{
                    name=Name "zero,something", 
                    composition_type=Product,
                    inner=[zero, Placeholder{name=Name "something",ph_level=Just 0,ph_category=natural}]
                },
                output=Dereference{
                    base_category=Special{name=Name "zero,something",special_type=Reference},
                    category_id=Index 1
                }
            }  
        ]
    }
}

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
{- END NATURAL NUMBERS-}

boolToBoolean :: Bool -> Category
boolToBoolean True = fromJust $ dereference (Name "true") boolean 
boolToBoolean False = fromJust $ dereference (Name "false") boolean

base :: Category
base = Composite{
    name=Name "Base",
    composition_type=Higher,
    inner=[
        ForeignCategory{
            name=Name "has",
            category_type=Morphism{
                name=Name "has",
                input=Composite{
                    name=Name "has_input",
                    composition_type=Product,
                    inner=[
                        Special{name=Name "BigCategory",special_type=Any},
                        Special{name=Name "SmallCategory",special_type=Any}
                    ]
                },
                output=Placeholder{
                    name=Name "has_output",
                    ph_level=Just 0,
                    ph_category=boolean
                }
            },
            attached=HaskellFunction (\input_category ->
                let
                    inner_cats = inner input_category
                    big_category = head inner_cats
                    small_category = inner_cats!!1
                in
                    Just $ boolToBoolean $ has big_category small_category)
        },
        ForeignCategory{
            name=Name "isValidArgumentTo",
            category_type=Morphism{
                name=Name "isValidArgumentTo",
                input=Composite{
                    name=Name "isValidArgumentTo_input",
                    composition_type=Product,
                    inner=[
                        Special{name=Name "Argument",special_type=Any},
                        Morphism{
                            name=Name "some_morphism",
                            input=Special{name=Name "morphism_input",special_type=Any},
                            output=Special{name=Name "morphism_output",special_type=Any}
                        }
                    ]
                },
                output=Placeholder{
                    name=Name "has_output",
                    ph_level=Just 0,
                    ph_category=boolean
                }
            },
            attached=HaskellFunction (\input_category ->
                let
                    inner_cats = inner input_category
                    argument_cat = head inner_cats
                    morphism_cat = inner_cats!!1
                in
                    Just $ boolToBoolean $ isValidArgumentTo argument_cat morphism_cat)
        }
    ]
}