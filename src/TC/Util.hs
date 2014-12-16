{-# LANGUAGE LambdaCase #-}
module TC.Util where
import Data.Foldable (foldMap)
import qualified Data.Set as S
import           Source

data TypeError = NoSuchName Name
               | TySynCycle Name
               | OccursFail Name
               | CannotMerge
               deriving Show

patVars :: Pat a -> S.Set Name
patVars = \case
  WildP -> S.empty
  VarP _ n -> S.singleton n
  ConP _ _ pats -> foldMap patVars pats
  ListP _ pats -> foldMap patVars pats
  TupleP _ l r -> patVars l `S.union` patVars r
  _ -> S.empty

nBoundVars :: NestedDecl a -> S.Set Name
nBoundVars = \case
  NSig{} -> S.empty
  NFun (Fun n _) -> S.singleton n
  NTop (Top p _ _) -> patVars p

boundVars :: Decl a -> S.Set Name
boundVars = \case
  DFun (Fun n _) -> S.singleton n
  DTop (Top p _ _) -> patVars p
  DClass _ _ _ nds -> foldMap nBoundVars nds
  _ -> S.empty
