module Mountain.Hash where

import Crypto.Hash ( hashWith, SHA256(..), Digest )
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding             as T
import Data.ByteArray.Encoding ( convertToBase, Base(Base16) )

-- Hashing
newtype Hash = Hash B.ByteString deriving (Eq, Ord)

instance Show Hash where
  show (Hash b_string) = B.foldr showHex "" b_string

sumHash :: [Hash] -> Hash
sumHash [] = error "bad sum hash"
sumHash [x] = x
sumHash ((Hash x):xs) = do
  let (Hash res_xs) = sumHash xs
  let ux = B.unpack x
  let uxs = B.unpack res_xs
  Hash $ B.pack $ zipWith (+) ux uxs

seqHash :: [Hash] -> Hash
seqHash [] = error "bad seq hash"
seqHash [x] = x
seqHash ((Hash x):xs) = do
  let (Hash res_xs) = seqHash xs
  Hash $ sha256 $ show (x <> res_xs)

class Hashable a where
  hash :: a -> Hash

sha256 :: String -> B.ByteString
sha256 input = do
  let bytes = (T.encodeUtf8 . T.pack) input :: B.ByteString
  let digest = hashWith SHA256 bytes     :: Digest SHA256
  convertToBase Base16 digest  :: B.ByteString

hashStr :: String -> Hash
hashStr = Hash . sha256
