{-# LANGUAGE LambdaCase #-}
module Validate (InvalidConstruct, syntaxValidate) where
import           Control.Applicative
import           Control.Monad
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.SrcLoc
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

nameValidate :: Name -> Validate S.Name
nameValidate = undefined

tyValidate :: Type -> Validate S.Type
tyValidate = \case
  TyForall{} -> notSup "Explicit for-alls"
  TyFun l r -> S.TFun <$> tyValidate l <*> tyValidate r
  TyTuple Boxed [l, r] -> S.TTuple <$> tyValidate l <*> tyValidate r
  TyTuple {} -> notSup "Tuples with an arity other than 2"
  TyList t -> S.TList <$> tyValidate t
  TyParArray{} -> notSup "Parallel arrays (Who uses these!?)"
  TyApp l r -> S.TApp <$> tyValidate l <*> tyValidate r
  TyVar n -> S.TVar <$> nameValidate n
  TyCon (UnQual n) -> S.TVar <$> nameValidate n
  TyCon _ -> notSup "Qualified or Special names"
  TyParen t -> tyValidate t
  TyInfix {} -> notSup "Infix type operators"
  TyKind{} -> notSup "Kinds"
  TyPromoted{} -> notSup "Promoted data types"
  TyEquals{} -> notSup "Equality constraints"
  TySplice{} -> notSup "Type splice"
  TyBang{} -> notSup "Type bangs"

expValidate :: Exp -> Validate (S.Exp SrcLoc)
expValidate = undefined

patValidate :: Pat -> Validate (S.Pat SrcLoc)
patValidate = \case
  PWildCard -> pure S.WildP
  PVar n -> S.VarP noLoc <$> nameValidate n
  PTuple Boxed [l, r] -> S.TupleP noLoc <$> patValidate l <*> patValidate r
  PTuple {} -> notSup "Fancy tuple patterns"
  PList pats -> S.ListP noLoc <$> mapM patValidate pats
  PLit _ (Char c) -> pure $ S.CharP noLoc c
  PLit _ (String s) -> pure $ S.StringP noLoc s
  PLit s (Int i) -> pure $ case s of
    Negative -> S.IntP noLoc (fromInteger $ -i)
    Signless -> S.IntP noLoc (fromInteger i)
  PApp (UnQual n) pats ->
    S.ConP noLoc <$> nameValidate n <*> mapM patValidate pats
  PParen p -> patValidate p
  PApp _ _ -> notSup "Qualified or Special names"
  _ -> notSup "Fancy pattern"

asstValidate :: Asst -> Validate S.Constr
asstValidate VarA{} = notSup "Constraint kinds"
asstValidate InfixA{} = notSup "Infix type operators"
asstValidate EqualP{} = notSup "Equality constraints"
asstValidate IParam{} = notSup "Implicit parameters"
asstValidate (ParenA a) = asstValidate a
asstValidate (ClassA (UnQual n) tys) = do
  n' <- nameValidate n
  tys' <- mapM tyValidate tys
  vars <- forM tys' $ \case
    (S.TVar n) -> return n
    _ -> notSup "Flexible constraints"
  return (S.Constr n' vars)
asstValidate ClassA{} = notSup "Qualified or Special names"

cxtValidate :: Context -> Validate S.Cxt
cxtValidate assts = S.Cxt <$> mapM asstValidate assts

classValidate :: ClassDecl -> Validate (S.Decl SrcLoc)
classValidate = undefined

instValidate :: InstDecl -> Validate (S.Decl SrcLoc)
instValidate = undefined

matchValidate :: Match -> Validate (S.Match SrcLoc)
matchValidate (Match _ n pats Nothing (UnGuardedRhs e) (BDecls decs)) =
  S.Match <$> nameValidate n
          <*> mapM patValidate pats
          <*> expValidate e
          <*> mapM ndeclValidate decs
matchValidate (Match loc _ _ _ _ _) =
  notSupLoc loc "Function clause extensions"

conValidate :: QualConDecl -> Validate S.ConD
conValidate (QualConDecl _ [] [] (ConDecl n tys)) =
  S.ConD <$> nameValidate n <*> mapM tyValidate tys
conValidate (QualConDecl loc _ _ _) = notSupLoc loc "Construct extensions"

sigValidate :: [Name] -> Type -> Validate S.FunSig
sigValidate n t =
  case t of
   TyForall _ cxt t ->
     S.FunSig <$> mapM nameValidate n <*> cxtValidate cxt <*> tyValidate t
   _ ->
     S.FunSig <$> mapM nameValidate n <*> pure (S.Cxt []) <*> tyValidate t

funValidate :: [Match] -> Validate (S.Fun SrcLoc)
funValidate ms = mapM matchValidate ms >>= \case
  cs@(S.Match nm _ _ _ : _) -> return (S.Fun nm cs)

ndeclValidate :: Decl -> Validate (S.NestedDecl SrcLoc)
ndeclValidate = \case
  TypeSig _ ns t -> S.NSig <$> sigValidate ns t
  FunBind ms -> S.NFun <$> funValidate ms
  _ -> notSup "Fancy nested data"

tvarValidate :: [TyVarBind] -> Validate [S.Name]
tvarValidate vars = forM vars $ \case
  UnkindedVar n -> nameValidate n
  _ -> notSup "Kinded variables"

declValidate :: Decl -> Validate (S.Decl SrcLoc)
declValidate = \case
  TypeDecl _ n vars t ->
    S.DType <$> nameValidate n
            <*> tvarValidate vars
            <*> tyValidate t
  DataDecl _ _ [] n vars cons [] ->
    S.DData <$> nameValidate n
            <*> tvarValidate vars
            <*> mapM conValidate cons
  ClassDecl _ cxt _ [_] [] cls -> undefined
  InstDecl _ Nothing _ cxt _ tys inst -> undefined
  InfixDecl _ _ _ _ -> undefined
  TypeSig _ ns t -> S.DSig <$> sigValidate ns t
  FunBind ms -> S.DFun <$> funValidate ms
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
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM declValidate decs
