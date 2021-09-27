module PythonCore where

import System.Directory (doesDirectoryExist, getDirectoryContents)

import CategoryData




renderToPython :: () -> IO ()
renderToPython input_category = do 
    print "hello"