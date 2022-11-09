module CopiedMountain.Hash where

import Crypto.Hash ( hashWith, SHA256(..), Digest )
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding             as T
import Data.ByteArray.Encoding ( convertToBase, Base(Base16) )
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

-- Hashing
data Hash =
    Hash B.ByteString
  | Nil
  | Unset
  deriving (Eq, Ord)

instance Show Hash where
  show (Hash b_string) = "<...>"
  show Nil = "Nil"
  show Unset = "_"

sumHash :: [Hash] -> Hash
sumHash [] = Nil
sumHash ((Hash x):xs) = do
  let res = sumHash xs
  case res of
    Nil -> Hash x
    Unset -> error "should not be unset"
    Hash res_xs -> do
      let (Hash res_xs) = sumHash xs
      let ux = B.unpack x
      let uxs = B.unpack res_xs
      Hash $ B.pack $ zipWith (+) ux uxs
sumHash (Nil:xs) = sumHash xs
sumHash (Unset:xs) = error "unset in sum hash"

seqHash :: [Hash] -> Hash
seqHash [] = Nil
seqHash [x] = x
seqHash ((Hash x):xs) = do
  let res = seqHash xs
  case res of
    Nil -> Hash x
    Unset -> error "should not be unset"
    Hash res_xs -> Hash $ sha256 $ show (x <> res_xs)
seqHash (Nil:xs) = seqHash xs
seqHash (Unset:xs) = error "unset in seq hash"

randHash :: IO Hash
randHash = hashStr . toString <$> nextRandom

class Hashable a where
  hash :: a -> Hash

sha256 :: String -> B.ByteString
sha256 input = do
  let bytes = (T.encodeUtf8 . T.pack) input :: B.ByteString
  let digest = hashWith SHA256 bytes     :: Digest SHA256
  convertToBase Base16 digest  :: B.ByteString

hashStr :: String -> Hash
hashStr = Hash . sha256
