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

data ConD = ConD Name [Type]

data Pat a = WildP
           | VarP a Name
           | ConP a Name [Pat a]
           | IntP a Int
           | CharP a Char
           | StringP a String
           | ListP a [Pat a]
           | TupleP a (Pat a) (Pat a)

data Match a = Match Name [Pat a] (Exp a) [NestedDecl a]
data Branch a = Branch (Pat a) (Exp a)

data NestedDecl a = NSig FunSig
                  | NFun (Fun a)

data Constr = Constr Name [Name]
data Cxt = Cxt [Constr]

data FunSig = FunSig [Name] Cxt Type
data Fun a = Fun Name [Match a]

data Decl a = DFun (Fun a)
            | DClass Cxt Name Name [NestedDecl a]
            | DInst Cxt Name Type [NestedDecl a]
            | DData Name [Name] [ConD]
            | DSig FunSig
            | DType Name [Name] Type
            | DAssoc Name -- Just to ensure that Name exists

data Exp a = Var a Name
           | Con a Name
           | LitChar a Char
           | LitInt a Int
           | LitString a String
           | App a (Exp a) (Exp a)
           | InfixApp a (Exp a) Name (Exp a) -- Operator in the middle
           | LeftSection a (Exp a) Name -- Operator on the right
           | RightSection a Name (Exp a) -- Operator on the left
           | List a [Exp a]
           | Let a [NestedDecl a] (Exp a)
           | If a (Exp a) (Exp a) (Exp a)
           | Tuple a (Exp a) (Exp a)
           | Lambda a [Pat a] (Exp a)
           | Case a (Exp a) [Branch a]
           | Annot a (Exp a) Type
