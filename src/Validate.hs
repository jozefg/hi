{-# LANGUAGE LambdaCase #-}
module Validate (InvalidConstruct, syntaxValidate) where
import           Control.Applicative
import           Language.Haskell.Exts.Syntax
import qualified Source                       as S

data InvalidConstruct = InvalidConstruct (Maybe SrcLoc) String
type Validate = Either InvalidConstruct

notSupLoc :: SrcLoc -> String -> Either InvalidConstruct a
notSupLoc loc str = Left (InvalidConstruct (Just loc) str)

notSup :: String -> Either InvalidConstruct a
notSup str = Left (InvalidConstruct Nothing str)

instance Show InvalidConstruct where
  show (InvalidConstruct (Just loc) message) =
    "Invalid construct at " ++ show loc ++ ":\t" ++ message
  show (InvalidConstruct Nothing message) =
    "Invalid construct:\t" ++ message

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

cxtValidate :: Context -> Validate S.Cxt
cxtValidate = undefined

classValidate :: ClassDecl -> Validate (S.Decl SrcLoc)
classValidate = undefined

instValidate :: InstDecl -> Validate (S.Decl SrcLoc)
instValidate = undefined

matchValidate :: Match -> Validate (S.Match SrcLoc)
matchValidate = undefined

constrValidate :: QualConDecl -> Validate S.Constr
constrValidate = undefined

declValidate :: Decl -> Validate (S.Decl SrcLoc)
declValidate = \case
  TypeDecl _ _ _ t -> undefined
  DataDecl _ _ [] _ _ constrs _ -> undefined
  ClassDecl _ cxt _ [_] [] cls -> undefined
  InstDecl _ Nothing _ cxt _ tys inst -> undefined
  InfixDecl _ _ _ _ -> undefined
  TypeSig _ _ t -> undefined
  FunBind ms -> undefined
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
