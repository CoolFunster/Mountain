module Categories.Lists where

import CategoryData


list :: Category
list = 
    Morphism{
        name="List",
        input=Special{name="list_data_type", special_type=Any},
        output= makeRecursiveCategory "list_inner_def" Composite{
            name="inner_list",
            composition_type=Sum,
            inner=[
                Thing "empty",
                Composite {
                    name="recursive",
                    composition_type=Product,
                    inner=[
                        Placeholder{
                            name="data", 
                            ph_level=Nothing,
                            -- ph_level=MorphismCall{
                            --     base_morphism=Special{
                            --         name="level data type - 1", 
                            --         special_type=Reference
                            --     }, 
                            --     argument=Special{
                            --         name="list_data_type", 
                            --         special_type=Reference
                            --     }
                            -- },
                            ph_category=Special{
                                name="list_inner_def", 
                                special_type=Reference
                            }
                        }
                    ]
                }
            ]
        }        
    }