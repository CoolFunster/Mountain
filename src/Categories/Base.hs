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