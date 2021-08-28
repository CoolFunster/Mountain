module Categories.Numbers where

import CategoryData


nat :: Category
nat = Morphism{
        name="Int",
        input=Special{name="Int_data_type", special_type=Universal},
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
                            {- ph_level= MorphismCall{
                                base_morphism=Special{
                                    name="level data type - 1", 
                                    special_type=Reference
                                }, 
                                argument=Special{
                                    name="list_data_type", 
                                    special_type=Reference
                                }
                            } -}
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
