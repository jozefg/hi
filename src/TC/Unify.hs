{-# LANGUAGE LambdaCase #-}
module TC.Unify where
import qualified Data.Map as M
import Source
import TC.Util

type Subst = M.Map Name

class HasNames t where
  named :: Name -> t
  occurs :: Name -> t -> Bool
instance HasNames Kind where
  named = KVar
  occurs n = \case
    KVar m -> n == m
    Star -> False
    KFun l r -> occurs n l || occurs n r
instance HasNames Type where
  named = TVar Nothing
  occurs n = \case
    TFun l r -> occurs n l || occurs n r
    TTuple l r -> occurs n l || occurs n r
    TVar _ m -> n == m
    TApp l r -> occurs n l || occurs n r
    _ -> False

add :: (HasNames t, Eq t) =>
       Name -> t -> Subst t -> Either TypeError (Subst t)
add n t | named n == t = Right . id
        | occurs n t = const . Left $ OccursFail n
        | otherwise = Right . M.insert n t
