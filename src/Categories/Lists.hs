module Categories.Lists where

import CategoryData


list :: Category
list = 
    Morphism{
        name=Name "List",
        input=Special{name=Name "list_data_type", special_type=Any},
        output= makeRecursiveCategory (Name "list_inner_def") Composite{
            name=Name "inner_list",
            composition_type=Sum,
            inner=[
                Thing (Name "empty"),
                Composite {
                    name=Name "recursive",
                    composition_type=Product,
                    inner=[
                        Placeholder{
                            name=Name "data", 
                            ph_level=Nothing,
                            -- ph_level=MorphismCall{
                            --     base_morphism=Special{
                            --         name=Name "level data type - 1", 
                            --         special_type=Reference
                            --     }, 
                            --     argument=Special{
                            --         name=Name "list_data_type", 
                            --         special_type=Reference
                            --     }
                            -- },
                            ph_category=Special{
                                name=Name "list_inner_def", 
                                special_type=Reference
                            }
                        }
                    ]
                }
            ]
        }        
    }
