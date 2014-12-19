module Source where

data Name = Name String
            deriving (Eq, Show, Ord)

data Kind = Star | KFun Kind Kind | KVar Name
          deriving Eq

data PolyType = Forall [Name] Type
data Type = TUnit
          | TVar (Maybe Kind) Name
          | TCon (Maybe Kind) Name
          | TApp Type Type
          | TFun
          | TTuple
          | TList
          | TInt
          | TDouble
          | TBool
          | TIO
          | TChar
          deriving Eq
tfun :: Type -> Type -> Type
tfun l r = TApp (TApp TFun l) r

ttuple :: Type -> Type -> Type
ttuple l r = TApp (TApp TTuple l) r

tlist :: Type -> Type
tlist = TApp TList

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
                  | NTop (Top a)

data Constr = Constr Name [Name]
data Cxt = Cxt [Constr]

data FunSig = FunSig [Name] Cxt Type
data Fun a = Fun Name [Match a]
data Top a = Top (Pat a) (Exp a) [NestedDecl a]

data Decl a = DFun (Fun a)
            | DTop (Top a)
            | DClass Cxt Name Name [NestedDecl a]
            | DInst Cxt Name Type [NestedDecl a]
            | DData Name [(Name, Maybe Kind)] [ConD]
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
