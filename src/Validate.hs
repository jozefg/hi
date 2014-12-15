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
  TyFun l r -> S.TFun <$> tyV l <*> tyV r
  TyTuple Boxed [l, r] -> S.TTuple <$> tyV l <*> tyV r
  TyTuple {} -> notSup "Tuples with an arity other than 2"
  TyList t -> S.TList <$> tyV t
  TyParArray{} -> notSup "Parallel arrays (Who uses these!?)"
  TyApp l r -> S.TApp <$> tyV l <*> tyV r
  TyVar n -> S.TVar <$> nameV n
  TyCon (UnQual n) -> S.TVar <$> nameV n
  TyCon _ -> notSup "Qualified or Special names"
  TyParen t -> tyV t
  TyInfix {} -> notSup "Infix type operators"
  TyKind{} -> notSup "Kinds"
  TyPromoted{} -> notSup "Promoted data types"
  TyEquals{} -> notSup "Equality constraints"
  TySplice{} -> notSup "Type splice"
  TyBang{} -> notSup "Type bangs"

expV :: Exp -> Validate (S.Exp SrcLoc)
expV = undefined

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
    (S.TVar n) -> return n
    _ -> notSup "Flexible constraints"
  return (S.Constr n' vars)
asstV ClassA{} = notSup "Qualified or Special names"

cxtV :: Context -> Validate S.Cxt
cxtV assts = S.Cxt <$> mapM asstV assts

classV :: ClassDecl -> Validate (S.Decl SrcLoc)
classV = undefined

instV :: InstDecl -> Validate (S.Decl SrcLoc)
instV = undefined

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
  _ -> notSup "Fancy nested data"

tvarV :: [TyVarBind] -> Validate [S.Name]
tvarV vars = forM vars $ \case
  UnkindedVar n -> nameV n
  _ -> notSup "Kinded variables"

declV :: Decl -> Validate (S.Decl SrcLoc)
declV = \case
  TypeDecl _ n vars t ->
    S.DType <$> nameV n
            <*> tvarV vars
            <*> tyV t
  DataDecl _ _ [] n vars cons [] ->
    S.DData <$> nameV n
            <*> tvarV vars
            <*> mapM conV cons
  ClassDecl _ cxt _ [_] [] cls -> undefined
  InstDecl _ Nothing _ cxt _ tys inst -> undefined
  InfixDecl _ _ _ _ -> undefined
  TypeSig _ ns t -> S.DSig <$> sigV ns t
  FunBind ms -> S.DFun <$> funV ms
  ClassDecl loc _ _ _ _ _ -> notSupLoc loc "Type class extensions"
  InstDecl loc _ _ _ _ _ _ ->
    notSupLoc loc "Unsupported instance declaration extensions"
  DerivDecl loc _ _ _ _ _ -> notSupLoc loc "Deriving declarations"
  DataDecl loc _ _ _ _ _ _ -> notSupLoc loc "Unsupported data type extensions"
  GDataDecl loc _ _ _ _ _ _ _ -> notSupLoc loc "GADT"
  DataFamDecl loc _ _ _ _ -> notSupLoc loc "Data families"
  TypeInsDecl loc _ _ -> notSupLoc loc "Type family Instance"
  DataInsDecl loc _ _ _ _ -> notSupLoc loc "Type family instance"
  GDataInsDecl loc _ _ _ _ _ -> notSupLoc loc "Type family instance"
  DefaultDecl loc _ -> notSupLoc loc "Default declarations"
  SpliceDecl loc _ -> notSupLoc loc "Splices"
  TypeFamDecl loc _ _ _ -> notSupLoc loc "Type families"
  ClosedTypeFamDecl loc _ _ _ _ -> notSupLoc loc "Closed type families"
  PatBind loc _ _ _ -> notSupLoc loc "Pattern bindings, this seems hard"
  ForImp loc _ _ _ _ _ -> notSupLoc loc "FFI"
  ForExp loc _ _ _ _ -> notSupLoc loc "FFI"
  RulePragmaDecl loc _ -> notSupLoc loc "Pragma"
  DeprPragmaDecl loc _ -> notSupLoc loc "Pragma"
  WarnPragmaDecl loc _ -> notSupLoc loc "Pragma"
  InlineSig loc _ _ _ -> notSupLoc loc "Pragma"
  InlineConlikeSig loc _ _ -> notSupLoc loc "Pragma"
  SpecSig loc _ _ _ -> notSupLoc loc "Pragma"
  SpecInlineSig loc _ _ _ _ -> notSupLoc loc "Pragma"
  InstSig loc _ _ _ _ -> notSupLoc loc "Pragma"
  AnnPragma loc _ -> notSupLoc loc "Pragma"
  MinimalPragma loc _ -> notSupLoc loc "Pragma"

syntaxValidate :: Module -> Validate [S.Decl SrcLoc]
syntaxValidate (Module loc _ (_:_) _ _ _ _) = notSupLoc loc "Pragmas"
syntaxValidate (Module loc _ _ _ (Just (_:_)) _ _) = notSupLoc loc "Export lists"
syntaxValidate (Module loc _ _ _ _ (_:_) _) = notSupLoc loc "Imports"
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM declV decs
