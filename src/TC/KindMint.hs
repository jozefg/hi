{-# LANGUAGE LambdaCase #-}
module KindMint (kindMint) where
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Gen
import qualified Data.Map            as M
import           Source
import           TC.Util

type Minted = StateT (M.Map Name Name) TCM

mint :: Name -> Minted Name
mint n = fmap (M.lookup n) get >>= \case
  Nothing -> gen >>= \n' -> modify (M.insert n n') >> return n
  Just n  -> return n

mintType :: Type -> Minted Type
mintType = \case
  TVar Nothing n -> flip TVar n . Just . KVar <$> mint n
  TCon Nothing n -> flip TCon n . Just . KVar <$> mint n
  TApp l r -> TApp <$> mintType l <*> mintType r
  t -> return t

mintMatch :: Match a -> TCM (Match a)
mintMatch (Match n ps e ds) = Match n ps <$> mintExp e <*> mapM mintNDecl ds

mintNDecl :: NestedDecl a -> TCM (NestedDecl a)
mintNDecl = \case
  NSig (FunSig nms cxt ty) ->
    NSig . FunSig nms cxt <$> evalStateT (mintType ty) M.empty
  NTop (Top p e ds) -> NTop <$> (Top p <$> mintExp e <*> mapM mintNDecl ds)
  NFun (Fun nm ms) -> NFun . Fun nm <$> mapM mintMatch ms

mintExp :: Exp a -> TCM (Exp a)
mintExp = \case
  App a l r -> App a <$> mintExp l <*> mintExp r
  InfixApp a l n r -> InfixApp a <$> mintExp l <*> pure n <*> mintExp r
  LeftSection a e n -> LeftSection a <$> mintExp e <*> pure n
  RightSection a n e -> RightSection a n <$> mintExp e
  List a exps -> List a <$> mapM mintExp exps
  Let a ndecls e -> Let a <$> mapM mintNDecl ndecls <*> mintExp e
  If a i t e ->  If a <$> mintExp i <*> mintExp t <*> mintExp e
  Tuple a l r -> Tuple a <$> mintExp l <*> mintExp r
  Lambda a pats e -> Lambda a pats <$> mintExp e
  Case a e bs -> Case a <$> mintExp e <*> mapM mintBranch bs
  Annot a e t -> Annot a e <$> evalStateT (mintType t) M.empty
  e -> return e
  where mintBranch (Branch p e) = Branch p <$> mintExp e

mintDecl :: Decl a -> TCM (Decl a)
mintDecl = \case
  DFun (Fun nm ms) -> DFun . Fun nm <$> mapM mintMatch ms
  DTop (Top p e ds) -> DTop <$> (Top p <$> mintExp e <*> mapM mintNDecl ds)
  DClass cxt c n nds -> DClass cxt c n <$> mapM mintNDecl nds
  DInst cxt c n nds -> DInst cxt c n <$> mapM mintNDecl nds
  DData n ns cons -> do
    ns' <- mapM mintName ns
    let nmap = M.fromList ns'
        ns'' = map (fmap $ Just . KVar) ns'
    DData n ns'' <$> mapM (mintCon nmap) cons
  DSig (FunSig nms cxt ty) ->
    DSig . FunSig nms cxt <$> evalStateT (mintType ty) M.empty
  DType n vs t -> DType n vs <$> evalStateT (mintType t) M.empty
  DAssoc n -> return (DAssoc n)
  where mintCon nmap (ConD n tys) =
          ConD n <$> evalStateT (mapM mintType tys) nmap
        mintName (n, _) = (,) n <$> gen

kindMint :: [Decl a] -> TCM [Decl a]
kindMint = mapM mintDecl
