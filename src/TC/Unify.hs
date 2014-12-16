{-# LANGUAGE LambdaCase #-}
module TC.Unify where
import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as M
import           Data.List           (intersect)
import           Source
import           TC.Util

type Subst = M.Map Name

nullSubst :: Subst t
nullSubst = M.empty

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
    TVar _ m -> n == m
    TApp l r -> occurs n l || occurs n r
    _ -> False
  apply s = go
    where go (TVar k v) = maybe (TVar k v) id $ M.lookup v s
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

kunify :: Kind -> Kind -> Either TypeError (Subst Kind)
kunify k1 k2 = case (k1, k2) of
  (KVar n, t) -> add n t nullSubst
  (t, KVar n) -> add n t nullSubst
  (KFun l r, KFun l' r') -> mergeApply <$> kunify l r <*> kunify l' r'
  (Star, Star) -> Right nullSubst
  (_, _) -> Left CannotUnify

kindOf :: Type -> Kind
kindOf = \case
  TVar (Just k) _ -> k
  TVar Nothing _ -> undefined
  TCon (Just k) _ -> k
  TCon Nothing _ -> undefined
  TApp f _ -> case kindOf f of KFun _ t -> t
  TFun -> KFun Star (KFun Star Star)
  TTuple -> KFun Star (KFun Star Star)
  TList -> KFun Star Star
  TInt -> Star
  TDouble -> Star
  TUnit -> Star
  TBool -> Star
  TIO -> KFun Star Star
  TChar -> Star
