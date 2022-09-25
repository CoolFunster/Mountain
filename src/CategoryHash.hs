{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module CategoryHash where

import Category
import Data.UUID
import Data.UUID.V5

import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

data CategoryHashType =
  HAny |
  HId |
  HByLabel |
  HSubtractive |
  HFlexible |
  HThing |
  HSet |
  HUnique |
  HTuple |
  HFunction |
  HEither |
  HComposition |
  HMatch |
  HRefined |
  HBasicType |
  HCall |
  HAccess |
  HPlaceholder |
  HReference |
  HScope |
  HBinding
  deriving (Eq, Show)

newtype CategoryHash = Hash UUID
  deriving (Eq, Ord)

instance Show CategoryHash where
  show (Hash id) = toString id

_hashStr :: CategoryHash -> [Char] -> CategoryHash
_hashStr (Hash id) = Hash . generateNamed id . B.unpack . T.encodeUtf8 . T.pack

_hashCombine :: CategoryHash -> CategoryHash -> CategoryHash
_hashCombine base (Hash nid) = _hashStr base (show nid)

_addUUIDs :: [CategoryHash] -> CategoryHash
_addUUIDs [] = Hash nil
_addUUIDs (Hash x:rest)= Hash $ do
  let (Hash id) = _addUUIDs rest
  let a = toWords64 id
  let b = toWords64 x
  fromWords64 (fst a + fst b) (snd a + snd b)

_sequenceUUIDs :: [CategoryHash] -> CategoryHash
_sequenceUUIDs [] = baseHash HTuple
_sequenceUUIDs other = foldl1 _hashCombine other

baseHash :: CategoryHashType -> CategoryHash
baseHash x = _hashStr (Hash nil) (show x)

hashId :: Id -> CategoryHash
hashId Unnamed = _hashStr (baseHash HId) (show Unnamed)
hashId (Name n) = _hashStr (baseHash HId) n

hashPlaceholderType :: PlaceholderType -> CategoryHash
hashPlaceholderType Label = _hashStr (Hash nil) (show Label)
hashPlaceholderType Variable = _hashStr (Hash nil) (show Variable)
hashPlaceholderType Resolved = error "should not be here"

hashAccessType :: AccessType -> CategoryHash
hashAccessType (ByLabelGroup labels) = baseHash HByLabel `_hashCombine` _addUUIDs (map hashId labels)
hashAccessType (Subtractive labels) = baseHash HSubtractive `_hashCombine` _addUUIDs (map hashId labels)


hash :: Category -> CategoryContext CategoryHash
hash c = normalize c >>= innerHash

innerHash :: Category -> CategoryContext CategoryHash
innerHash Thing{name=t_id} = return $ baseHash HThing `_hashCombine` hashId t_id
innerHash Set{elements=elems} = do
  x <- mapM hash elems
  return $ _hashCombine (baseHash HSet) $ _addUUIDs x
innerHash Unique{unique_id=uid, inner_category=c} = do
  let u_hash = baseHash HUnique
  let u_id_hash = hashId uid
  c_hash <- hash c
  return $ u_hash `_hashCombine` u_id_hash `_hashCombine` c_hash
innerHash Composite{composite_type=c_type, inner_categories=inner} = do
  let c_type_hash =
        case c_type of
          Tuple -> baseHash HTuple
          Either -> baseHash HEither
          Function -> baseHash HFunction
          Composition -> baseHash HComposition
          Match -> baseHash HMatch
  inner_hashes <- mapM hash inner
  let seq_inner_hash = _sequenceUUIDs inner_hashes
  let add_inner_hash = _addUUIDs inner_hashes
  return $ c_type_hash `_hashCombine`
        case c_type of
          Either -> add_inner_hash
          Match -> add_inner_hash
          _ -> seq_inner_hash
innerHash Refined{base=c, predicate=p} = _hashCombine <$> hash c <*> hash p
innerHash Special{special_type=Any} = return $ baseHash HAny
innerHash Special{special_type=Flexible} = return $ baseHash HFlexible
innerHash Placeholder{name=n, placeholder_kind=p_kind, placeholder_category=ph_c} = do
  let id_hash = hashId n
  let ph_hash = hashPlaceholderType p_kind
  phc_hash <- hash ph_c
  return $ baseHash HPlaceholder `_hashCombine` id_hash `_hashCombine` ph_hash `_hashCombine` phc_hash
innerHash Reference{name=n} = return $ baseHash HReference `_hashCombine` hashId n
innerHash Call{base=b, argument=a} = do
  b_hash <- hash b
  a_hash <- hash a
  let call_hash = baseHash HCall
  return $ call_hash `_hashCombine` b_hash `_hashCombine` a_hash
innerHash Access{base=b, access_type=a} = do
  b_hash <- hash b
  let a_hash = hashAccessType a
  let access_hash = baseHash HAccess
  return $ access_hash `_hashCombine` b_hash `_hashCombine` a_hash
innerHash TypeAnnotation{small_category=a} = hash a
innerHash Import{import_category=ic} = error "dunno what to do here"
innerHash Scope{statements=s} = do
  let s_hash = baseHash HScope
  inner_hash <- _sequenceUUIDs <$> mapM hash s
  return $ s_hash `_hashCombine` inner_hash
innerHash Binding{placeholder=p, category_to_bind=c} = do
  let b_hash = baseHash HBinding
  p_hash <- innerHash p
  c_hash <- innerHash c
  return $ b_hash `_hashCombine` p_hash `_hashCombine` c_hash