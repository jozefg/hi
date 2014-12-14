module Source where

data Name a = Name a String
            deriving (Eq, Show, Ord)

data Kind = Star | KFun Kind Kind

data PolyType = Forall [Name ()] Type
data Type = TFun Type Type
          | TTuple Type Type
          | TUnit
          | TVar (Name ())
          | TCon (Name ())
          | TApp Type Type
          | TList Type
          | TInt
          | TDouble
          | TBool
          | TIO
          | TChar

data Constr a

data Pat a
data Match a

data Cxt a
data Class a
data Decl a
data Expr a
