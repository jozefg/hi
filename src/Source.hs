module Source where

data Name = Name String
            deriving (Eq, Show, Ord)

data Kind = Star | KFun Kind Kind

data PolyType = Forall [Name] Type
data Type = TFun Type Type
          | TTuple Type Type
          | TUnit
          | TVar Name
          | TCon Name
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

data Binding a

data Exp a = Var a Name
           | Con a Name
           | LitChar a Char
           | LitInt a Int
           | LitString a String
           | App a (Exp a) (Exp a)
           | InfixApp a (Exp a) (Exp a) (Exp a) -- Operator in the middle
           | LeftSection a (Exp a) (Exp a) -- Operator on the right
           | RightSection a (Exp a) (Exp a) -- Operator on the left
           | List a [Exp a]
           | Let a [Binding a] (Exp a)
           | If (Exp a) (Exp a) (Exp a)
           | Tuple (Exp a) (Exp a)
           | Lambda a [Pat a] (Exp a)
           | Case a (Exp a) [(Pat a, Exp a)]
