module Categories.Foreign.Haskell.Numbers where

import CategoryData
import Categories.Numbers

import Data.Typeable
import Data.Dynamic
import Data.Maybe (fromJust)

haskellValid :: Category
haskellValid = ForeignCategory{
    name=Name "haskellValid",
    category_type=valid,
    attached=HaskellObject (toDyn ())
}

haskellInt :: Category
haskellInt = ForeignCategory{
    name=Name "haskellInt",
    category_type=integer,
    attached=HaskellType (typeRep (Proxy :: Proxy Int))
}
