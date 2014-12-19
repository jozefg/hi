{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Validate (InvalidConstruct, syntaxValidate) where
import           Control.Applicative
import           Control.Monad
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.SrcLoc (noLoc)
import qualified Source                       as S

data InvalidConstruct = InvalidConstruct SrcLoc String
type Validate = Either InvalidConstruct

notSupLoc :: SrcLoc -> String -> Either InvalidConstruct a
notSupLoc loc str = Left (InvalidConstruct loc str)

notSup :: String -> Either InvalidConstruct a
notSup str = Left (InvalidConstruct noLoc str)

instance Show InvalidConstruct where
  show (InvalidConstruct loc message) =
    "Invalid construct at " ++ show loc ++ ":\t" ++ message

nameV :: Name -> Validate S.Name
nameV (Ident n) = pure $ S.Name n
nameV (Symbol s) = pure $ S.Name s

tyV :: Type -> Validate S.Type
tyV = \case
  TyForall{} -> notSup "Explicit for-alls"
  TyFun l r -> S.tfun <$> tyV l <*> tyV r
  TyTuple Boxed [l, r] -> S.ttuple <$> tyV l <*> tyV r
  TyTuple {} -> notSup "Tuples with an arity other than 2"
  TyList t -> S.tlist <$> tyV t
  TyParArray{} -> notSup "Parallel arrays (Who uses these!?)"
  TyApp l r -> S.TApp <$> tyV l <*> tyV r
  TyVar n -> S.TVar Nothing <$> nameV n
  TyCon (UnQual n) -> S.TCon Nothing <$> nameV n
  TyCon _ -> notSup "Qualified or Special names"
  TyParen t -> tyV t
  TyInfix {} -> notSup "Infix type operators"
  TyKind{} -> notSup "Kinds"
  TyPromoted{} -> notSup "Promoted data types"
  TyEquals{} -> notSup "Equality constraints"
  TySplice{} -> notSup "Type splice"
  TyBang{} -> notSup "Type bangs"

altV :: Alt -> Validate (S.Branch SrcLoc)
altV (Alt _ pat (UnGuardedRhs e) (BDecls [])) =
  S.Branch <$> patV pat <*> expV e
altV _ = notSup "Fancy case branches"

expV :: Exp -> Validate (S.Exp SrcLoc)
expV = \case
  Var (UnQual n) -> S.Var noLoc <$> nameV n
  IPVar{} -> notSup "Implicit variables"
  Con (UnQual n) -> S.Con noLoc <$> nameV n
  Lit (Char c) -> pure (S.LitChar noLoc c)
  Lit (String s) -> pure (S.LitString noLoc s)
  Lit (Int i) -> pure (S.LitInt noLoc $ fromInteger i)
  InfixApp l (QVarOp (UnQual n)) r ->
    S.InfixApp noLoc <$> expV l <*> nameV n <*> expV r
  InfixApp l (QConOp (UnQual n)) r ->
    S.InfixApp noLoc <$> expV l <*> nameV n <*> expV r
  App l r -> S.App noLoc <$> expV l <*> expV r
  Lambda loc pats e -> S.Lambda loc <$> mapM patV pats <*> expV e
  Let (BDecls binds) e -> S.Let noLoc <$> mapM ndeclV binds <*> expV e
  If i t e -> S.If noLoc <$> expV i <*> expV t <*> expV e
  Case e alts -> S.Case noLoc <$> expV e <*> mapM altV alts
  Tuple Boxed [l, r] -> S.Tuple noLoc <$> expV l <*> expV r
  List es -> S.List noLoc <$> mapM expV es
  Paren e -> expV e
  LeftSection e (QVarOp (UnQual n)) ->
    S.LeftSection noLoc <$> expV e <*> nameV n
  LeftSection e (QConOp (UnQual n)) ->
    S.LeftSection noLoc <$> expV e <*> nameV n
  RightSection (QVarOp (UnQual n)) e ->
    S.RightSection noLoc <$> nameV n <*> expV e
  RightSection (QConOp (UnQual n)) e ->
    S.RightSection noLoc <$> nameV n <*> expV e
  ExpTypeSig loc e t -> S.Annot loc <$> expV e <*> tyV t
  _ -> notSup "One of a bajillion fancy expressions"


patV :: Pat -> Validate (S.Pat SrcLoc)
patV = \case
  PWildCard -> pure S.WildP
  PVar n -> S.VarP noLoc <$> nameV n
  PTuple Boxed [l, r] -> S.TupleP noLoc <$> patV l <*> patV r
  PTuple {} -> notSup "Fancy tuple patterns"
  PList pats -> S.ListP noLoc <$> mapM patV pats
  PLit _ (Char c) -> pure $ S.CharP noLoc c
  PLit _ (String s) -> pure $ S.StringP noLoc s
  PLit s (Int i) -> pure $ case s of
    Negative -> S.IntP noLoc (fromInteger $ -i)
    Signless -> S.IntP noLoc (fromInteger i)
  PApp (UnQual n) pats ->
    S.ConP noLoc <$> nameV n <*> mapM patV pats
  PParen p -> patV p
  PApp _ _ -> notSup "Qualified or Special names"
  _ -> notSup "Fancy pattern"

asstV :: Asst -> Validate S.Constr
asstV VarA{} = notSup "Constraint kinds"
asstV InfixA{} = notSup "Infix type operators"
asstV EqualP{} = notSup "Equality constraints"
asstV IParam{} = notSup "Implicit parameters"
asstV (ParenA a) = asstV a
asstV (ClassA (UnQual n) tys) = do
  n' <- nameV n
  tys' <- mapM tyV tys
  vars <- forM tys' $ \case
    (S.TVar _ n) -> return n
    _ -> notSup "Flexible constraints"
  return (S.Constr n' vars)
asstV ClassA{} = notSup "Qualified or Special names"

cxtV :: Context -> Validate S.Cxt
cxtV assts = S.Cxt <$> mapM asstV assts

classV :: ClassDecl -> Validate (S.NestedDecl SrcLoc)
classV (ClsDecl d) = ndeclV d
classV _ = notSup "Fancy class features"

instV :: InstDecl -> Validate (S.NestedDecl SrcLoc)
instV (InsDecl d) = ndeclV d
instV _ = notSup "Fancy instance features"


matchV :: Match -> Validate (S.Match SrcLoc)
matchV (Match _ n pats Nothing (UnGuardedRhs e) (BDecls decs)) =
  S.Match <$> nameV n
          <*> mapM patV pats
          <*> expV e
          <*> mapM ndeclV decs
matchV (Match loc _ _ _ _ _) =
  notSupLoc loc "Function clause extensions"

conV :: QualConDecl -> Validate S.ConD
conV (QualConDecl _ [] [] (ConDecl n tys)) =
  S.ConD <$> nameV n <*> mapM tyV tys
conV (QualConDecl loc _ _ _) = notSupLoc loc "Construct extensions"

sigV :: [Name] -> Type -> Validate S.FunSig
sigV n t =
  case t of
   TyForall _ cxt t ->
     S.FunSig <$> mapM nameV n <*> cxtV cxt <*> tyV t
   _ ->
     S.FunSig <$> mapM nameV n <*> pure (S.Cxt []) <*> tyV t

funV :: [Match] -> Validate (S.Fun SrcLoc)
funV ms = mapM matchV ms >>= \case
  cs@(S.Match nm _ _ _ : _) -> return (S.Fun nm cs)

ndeclV :: Decl -> Validate (S.NestedDecl SrcLoc)
ndeclV = \case
  TypeSig _ ns t -> S.NSig <$> sigV ns t
  FunBind ms -> S.NFun <$> funV ms
  PatBind _ pat (UnGuardedRhs e) (BDecls decs) -> S.NTop <$> patbV pat e decs
  _ -> notSup "Fancy nested data"

tvarV :: TyVarBind -> Validate S.Name
tvarV = \case
  UnkindedVar n -> nameV n
  _ -> notSup "Kinded variables"

patbV :: Pat -> Exp -> [Decl] -> Validate (S.Top SrcLoc)
patbV p e decs = S.Top <$> patV p <*> expV e <*> mapM ndeclV decs

declV :: Decl -> Validate (S.Decl SrcLoc)
declV = \case
  TypeDecl _ n vars t ->
    S.DType <$> nameV n
            <*> mapM (fmap (, Nothing) . tvarV) vars
            <*> tyV t
  DataDecl _ _ [] n vars cons [] ->
    S.DData <$> nameV n
            <*> mapM (fmap (, Nothing) . tvarV) vars
            <*> mapM conV cons
  ClassDecl _ cxt n [a] [] cls ->
    S.DClass <$> cxtV cxt <*> nameV n <*> tvarV a <*> mapM classV cls
  InstDecl _ Nothing [] cxt (UnQual n) [t] inst ->
    S.DInst <$> cxtV cxt <*> nameV n <*> tyV t <*> mapM instV inst
  InfixDecl _ _ _ [VarOp n] -> S.DAssoc <$> nameV n
  InfixDecl _ _ _ [ConOp n] -> S.DAssoc <$> nameV n
  TypeSig _ ns t -> S.DSig <$> sigV ns t
  FunBind ms -> S.DFun <$> funV ms
  PatBind _ pat (UnGuardedRhs e) (BDecls decs) -> S.DTop <$> patbV pat e decs
  _ -> notSup "Fancy declaration"

syntaxValidate :: Module -> Validate [S.Decl SrcLoc]
syntaxValidate (Module loc _ (_:_) _ _ _ _) = notSupLoc loc "Pragmas"
syntaxValidate (Module loc _ _ _ (Just (_:_)) _ _) = notSupLoc loc "Export lists"
syntaxValidate (Module loc _ _ _ _ (_:_) _) = notSupLoc loc "Imports"
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM declV decs
