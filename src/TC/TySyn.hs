{-# LANGUAGE LambdaCase #-}
module TC.TySyn where
import qualified Data.Map   as M
import qualified Data.Set   as S
import           Source

tyNamesIn :: Type -> S.Set Name
tyNamesIn = \case
  TCon n -> S.singleton n
  TFun l r -> tyNamesIn l `S.union` tyNamesIn r
  TApp l r -> tyNamesIn l `S.union` tyNamesIn r
  TList t -> tyNamesIn t
  _ -> S.empty

synonymInfo :: [Decl a] -> M.Map Name Type
synonymInfo = foldr go M.empty
  where go (DType name _ ty) = M.insert name ty
        go _ = id

dependencies :: [Decl a] -> M.Map Name (S.Set Name)
dependencies decls = fmap (S.intersection names . tyNamesIn) info
  where info = synonymInfo decls
        names = S.fromList (M.keys info)
