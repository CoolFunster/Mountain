module PythonCoreSpec (spec) where

import Test.Hspec
import PythonCore
import CategoryCore

spec :: Spec
spec = do
    describe "renderToPython" $ do
        it "should create the appropriate directories" $ do
            renderToPython ()
        

            