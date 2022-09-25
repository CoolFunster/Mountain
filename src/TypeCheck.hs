{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module TypeCheck where

import CategoryHash
import Category

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except

data CategoryData = Data {
  category :: Category,
  category_type :: Category
} deriving (Eq, Show)

type TypeEnv = Map.Map CategoryHash CategoryData

findType :: TypeEnv -> Category -> CategoryContext (Maybe Category)
findType type_env cat = do
  c_hash <- hash cat
  case Map.lookup c_hash type_env of
    Nothing -> return Nothing
    Just (Data _ c_type) -> return $ Just c_type

-- add :: TypeEnv -> Category -> Category -> CategoryContext TypeEnv
-- add tenv c c2 = do
--   new_entry <- Map.singleton <$> hash c <*> return $ Data c2 (typeOf c2)
--   return $ tenv <> new_entry

-- typeOf :: TypeEnv -> Category -> Category
-- typeOf t@Thing{} = Set [t]
-- typeOf _ s@Set{elements=elems} = Set [s]
-- typeOf _ u@Unique = Set [u]
-- typeOf _ Composite{composite_type=Either, inner_categories=inners} = Set inners
-- typeOf tenv Composite{composite_type=c_type, inner_categories=inners} = 
--   Composite c_type (map (typeOf tenv) inners)
-- typeOf Composite{composite_type=Function, inner_categories=inners} = Composite Function (map typeOf inners)
-- -- typeOf 

typeCheck :: TypeEnv -> Category -> CategoryContext TypeEnv
typeCheck tenv c = normalize c >>= typeCheck' tenv

typeCheck' :: TypeEnv -> Category -> CategoryContext TypeEnv
typeCheck' tenv t@Thing{} = return tenv
typeCheck' tenv s@Set{elements=elems} = Map.unions . (:) tenv <$> mapM (typeCheck tenv) elems
typeCheck' tenv u@Unique{inner_category=ic} = Map.union tenv <$> typeCheck tenv ic
typeCheck' tenv c@Composite{composite_type=Either, inner_categories=inners} = Map.unions . (:) tenv <$> mapM (typeCheck tenv) inners
typeCheck' tenv c@Composite{inner_categories=[]} = return tenv
-- typeCheck' tenv c@Composite{inner_categories=x:xs} = return tenv
typeCheck' tenv r@Refined{base=b, predicate=p} = do
  r1 <- typeCheck tenv b
  r2 <- typeCheck tenv $ TypeAnnotation bool p
  return $ Map.unions [tenv, r1, r2]
typeCheck' tenv Special{} = return tenv
-- typeCheck' tenv p@Placeholder{name=n, placeholder_kind=Label, placeholder_category=ph_c} = do
--   rec_tenv <- add tenv (Reference n) (Data p
--   r1 <- typeCheck tenv ph_c
typeCheck' tenv r@Reference{name=n} = do
  r1 <- findType tenv r
  case r1 of
    Nothing -> throwError [Error UnresolvedReference [r]]
    Just _ -> return tenv
typeCheck' _ t@TypeAnnotation{big_category=bc, small_category=sc} = do
  result <- bc `has` sc
  if result
    then Map.singleton <$> hash sc <*> return (Data sc bc)
    else throwError [Error BadTypeAnnotation [t]]
typeCheck' _ _ = error "404"