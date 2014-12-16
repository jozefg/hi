{-# LANGUAGE LambdaCase #-}
module TC.TySyn (checkTySyns) where
import           Control.Monad.Except
import           Data.Graph
import qualified Data.Map   as M
import qualified Data.Set   as S
import           Source
import           TC.Util


tyNamesIn :: Type -> S.Set Name
tyNamesIn = \case
  TCon _ n -> S.singleton n
  TApp l r -> tyNamesIn l `S.union` tyNamesIn r
  _ -> S.empty

synonymInfo :: [Decl a] -> M.Map Name Type
synonymInfo = foldr go M.empty
  where go (DType name _ ty) = M.insert name ty
        go _ = id

dependencies :: [Decl a] -> M.Map Name (S.Set Name)
dependencies decls = fmap (S.intersection names . tyNamesIn) info
  where info = synonymInfo decls
        names = S.fromList (M.keys info)

dep2sccs :: M.Map Name (S.Set Name) -> [SCC Name]
dep2sccs deps = map (fmap (\(x, _, _) -> x)) sccs
  where sccs = stronglyConnCompR
               . map (\(x, y) -> (x, x, S.toList y))
               $ M.toList deps

checkSCC :: SCC Name -> TCM ()
checkSCC = \case
  AcyclicSCC _ -> return ()
  CyclicSCC (h : _) -> throwError (TySynCycle h)

checkTySyns :: [Decl a] -> TCM ()
checkTySyns = mapM_ checkSCC . dep2sccs . dependencies
