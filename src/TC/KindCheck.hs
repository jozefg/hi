{-# LANGUAGE LambdaCase #-}
module KindCheck where
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.Gen
import Control.Monad.Writer
import Source
import TC.Unify
import TC.Util

data KConstr = Kind :~: Kind

solveConstrs :: [KConstr] -> TCM (Subst Kind)
solveConstrs = foldr merge (return nullSubst) . map solve
  where solve (l :~: r) = kunify l r
        merge l r = join $ mergeAgree <$> l <*> r

hasKind :: Type -> Kind -> WriterT [KConstr] TCM ()
hasKind t k = case t of
  TApp f a -> do
    [l, r] <- mapM (fmap KVar) [gen, gen]
    a `hasKind` l
    f `hasKind` KFun l r
    tell [ kindOf f :~: KFun l r
         , kindOf a :~: l
         , r :~: k]
  _ -> tell [kindOf t :~: k]

constrainDecl :: Decl a -> WriterT [KConstr] TCM ()
constrainDecl = \case
  DType n args ty -> do
    res <- KVar <$> gen
    constrainRhs n args res
    ty `hasKind` res
  DData n args cons -> do
    res <- KVar <$> gen
    constrainRhs n args res
    forM_ cons $ \(ConD _ tys) ->
      forM_ tys $ \t -> t `hasKind` Star
  _ -> return ()
  where constrainRhs n args res =
          let rhs = foldr KFun res $ map (fromJust . snd) args
          in tell [KVar n :~: rhs]

mkKindSubst :: [Decl a] -> TCM (Subst Kind)
mkKindSubst = join
              . fmap (solveConstrs . snd)
              . runWriterT
              . mapM constrainDecl
