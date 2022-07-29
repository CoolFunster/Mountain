

module FrontEnds.AST.V1.CategoryParser where
import CategoryData
import System.Directory (doesFileExist)

import Control.Monad.Trans (lift)
import Control.Monad.Except

basePath :: FilePath
basePath = "/home/mpriam/git/mtpl_language/src/FrontEnds/AST/V1/Categories/"

fileExt :: String
fileExt = ".ast.mtpl"

pAST :: FilePath -> CategoryContextT IO Category
pAST file_path = do
  fileExist <- lift $ doesFileExist file_path
  if fileExist
      then lift $ fmap read (readFile file_path)
      else throwError [Error{error_type=BadImport, error_stack=[Reference (Name file_path)]}]

loadAST :: FilePath -> CategoryContextT IO Category
loadAST = loadModule basePath fileExt pAST