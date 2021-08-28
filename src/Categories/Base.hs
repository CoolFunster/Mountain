module Categories.Base where

import Data.Maybe (fromJust)

import CategoryData
import CategoryCore

boolean :: Category
boolean = Composite{
    name="Boolean",
    composition_type=Higher,
    inner=[
        Thing "true",
        Thing "false"
    ]
}

boolToBoolean :: Bool -> Category
boolToBoolean True = fromJust $ dereference "true" boolean 
boolToBoolean False = fromJust $ dereference "false" boolean

base :: Category
base = Composite{
    name="Base",
    composition_type=Higher,
    inner=[
        ForeignCategory{
            name="has",
            category_type=Morphism{
                name="has",
                input=Composite{
                    name="has_input",
                    composition_type=Product,
                    inner=[
                        Special{name="BigCategory",special_type=Any},
                        Special{name="SmallCategory",special_type=Any}
                    ]
                },
                output=Placeholder{
                    name="has_output",
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
            name="isValidArgumentTo",
            category_type=Morphism{
                name="isValidArgumentTo",
                input=Composite{
                    name="isValidArgumentTo_input",
                    composition_type=Product,
                    inner=[
                        Special{name="Argument",special_type=Any},
                        Morphism{
                            name="some_morphism",
                            input=Special{name="morphism_input",special_type=Any},
                            output=Special{name="morphism_output",special_type=Any}
                        }
                    ]
                },
                output=Placeholder{
                    name="has_output",
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