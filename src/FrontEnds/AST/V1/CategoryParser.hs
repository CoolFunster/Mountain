

module FrontEnds.AST.V1.CategoryParser where
import CategoryData
import System.Directory (doesFileExist)

basePath :: FilePath
basePath = "/home/mpriam/git/mtpl_language/src/FrontEnds/AST/V1/Categories/"

fileExt :: String
fileExt = ".ast.mtpl"

pAST :: FilePath -> ErrorableT IO Category
pAST file_path = ErrorableT $ do
        fileExist <- doesFileExist file_path
        if not fileExist
            then return (ErrorList [Error{error_type=BadImport, error_stack=[Reference (Name file_path)]}])
            else fmap (Valid . read) (readFile file_path)

loadAST :: FilePath -> ErrorableT IO Category
loadAST = loadModule basePath fileExt pAST