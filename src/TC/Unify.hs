{-# LANGUAGE LambdaCase #-}
module TC.Unify where
import Control.Monad
import qualified Data.Map as M
import Data.List (intersect)
import Source
import TC.Util

type Subst = M.Map Name

class HasNames t where
  named :: Name -> t
  occurs :: Name -> t -> Bool
  apply :: Subst t -> t -> t
instance HasNames Kind where
  named = KVar
  occurs n = \case
    KVar m -> n == m
    Star -> False
    KFun l r -> occurs n l || occurs n r
  apply s = go
    where go (KFun l r) = go l `KFun` go r
          go (KVar v) = maybe (KVar v) id $ M.lookup v s
          go Star = Star


instance HasNames Type where
  named = TVar Nothing
  occurs n = \case
    TFun l r -> occurs n l || occurs n r
    TTuple l r -> occurs n l || occurs n r
    TVar _ m -> n == m
    TApp l r -> occurs n l || occurs n r
    _ -> False
  apply s = go
    where go (TFun l r) = go l `TFun` go r
          go (TTuple l r) = go l `TTuple` go r
          go (TVar k v) = maybe (TVar k v) id $ M.lookup v s
          go (TApp l r) = go l `TApp` go r
          go t = t

add :: (HasNames t, Eq t) =>
       Name -> t -> Subst t -> Either TypeError (Subst t)
add n t | named n == t = Right . id
        | occurs n t = const . Left $ OccursFail n
        | otherwise = Right . M.insert n t

mergeApply :: HasNames t => Subst t -> Subst t -> Subst t
mergeApply s1 s2 = M.map (apply s1) s2 `M.union` s1

mergeAgree :: (Eq t, HasNames t) =>
              Subst t -> Subst t -> Either TypeError (Subst t)
mergeAgree s1 s2 = do
  checkAgrees (M.keys s1 `intersect` M.keys s2)
  return (s1 `M.union` s2)
  where checkAgrees nms = forM_ nms $ \n -> do
          let t = apply s1 (named n)
              t' = apply s2 (named n)
          when (t /= t') . Left $ CannotMerge -- Todo, this should show t
