{-# LANGUAGE LambdaCase #-}
module TC.Unify where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
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

add :: (HasNames t, Eq t) => Name -> t -> Subst t -> TCM (Subst t)
add n t | named n == t = return . id
        | occurs n t = const . throwError $ OccursFail n
        | otherwise = return . M.insert n t

mergeApply :: HasNames t => Subst t -> Subst t -> Subst t
mergeApply s1 s2 = M.map (apply s1) s2 `M.union` s1

mergeAgree :: (Eq t, HasNames t) => Subst t -> Subst t -> TCM (Subst t)
mergeAgree s1 s2 = do
  checkAgrees (M.keys s1 `intersect` M.keys s2)
  return (s1 `M.union` s2)
  where checkAgrees nms = forM_ nms $ \n -> do
          let t = apply s1 (named n)
              t' = apply s2 (named n)
          when (t /= t') . throwError $ CannotMerge -- Todo, this should show t

kunify :: Kind -> Kind -> TCM (Subst Kind)
kunify k1 k2 = case (k1, k2) of
  (KVar n, t) -> add n t nullSubst
  (t, KVar n) -> add n t nullSubst
  (KFun l r, KFun l' r') -> mergeApply <$> kunify l r <*> kunify l' r'
  (Star, Star) -> return nullSubst
  (_, _) -> throwError CannotUnify

tunify :: Type -> Type -> TCM (Subst Type, Subst Kind)
tunify t1 t2 = do
  case (t1, t2) of
   (TVar _ n, t) -> (,) <$> add n t nullSubst <*> kindCheck t1 t2
   (t, TVar _ n) -> (,) <$> add n t nullSubst <*> kindCheck t1 t2
   (TApp l r, TApp l' r') -> mergeBoth <$> tunify l r <*> tunify l' r'
   (TCon _ n, TCon _ n') ->
     if n == n' -- Special case so we don't check kind equality with ==
     then (,) nullSubst <$> kindCheck t1 t2
     else throwError CannotUnify
   (_, _) ->
     if t1 == t2
     then (,) nullSubst <$> kindCheck t1 t2
     else throwError CannotUnify
  where kindCheck t1 t2 = kunify (kindOf t1) (kindOf t2)
        mergeBoth (l, r) (l', r') = (mergeApply l l', mergeApply r r')
