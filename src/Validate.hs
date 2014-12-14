{-# LANGUAGE LambdaCase #-}
module Validate (InvalidConstruct, syntaxValidate) where
import           Language.Haskell.Exts.Syntax
import qualified Source                       as S

data InvalidConstruct = InvalidConstruct SrcLoc String
type Validate = Either InvalidConstruct

notSup :: SrcLoc -> String -> Either InvalidConstruct a
notSup loc str = Left (InvalidConstruct loc str)

instance Show InvalidConstruct where
  show (InvalidConstruct loc message) =
    "Invalid construct at " ++ show loc ++ "\t" ++ message

tyValidate :: Type -> Validate S.Type
tyValidate = undefined

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
  ClassDecl loc _ _ _ _ _ -> notSup loc "Type class extensions"
  InstDecl loc _ _ _ _ _ _ ->
    notSup loc "Unsupported instance declaration extensions"
  DerivDecl loc _ _ _ _ _ -> notSup loc "Deriving declarations"
  DataDecl loc _ _ _ _ _ _ -> notSup loc "Unsupported data type extensions"
  GDataDecl loc _ _ _ _ _ _ _ -> notSup loc "GADT"
  DataFamDecl loc _ _ _ _ -> notSup loc "Data families"
  TypeInsDecl loc _ _ -> notSup loc "Type family Instance"
  DataInsDecl loc _ _ _ _ -> notSup loc "Type family instance"
  GDataInsDecl loc _ _ _ _ _ -> notSup loc "Type family instance"
  DefaultDecl loc _ -> notSup loc "Default declarations"
  SpliceDecl loc _ -> notSup loc "Splices"
  TypeFamDecl loc _ _ _ -> notSup loc "Type families"
  ClosedTypeFamDecl loc _ _ _ _ -> notSup loc "Closed type families"
  PatBind loc _ _ _ -> notSup loc "Pattern bindings, this seems hard"
  ForImp loc _ _ _ _ _ -> notSup loc "FFI"
  ForExp loc _ _ _ _ -> notSup loc "FFI"
  RulePragmaDecl loc _ -> notSup loc "Pragma"
  DeprPragmaDecl loc _ -> notSup loc "Pragma"
  WarnPragmaDecl loc _ -> notSup loc "Pragma"
  InlineSig loc _ _ _ -> notSup loc "Pragma"
  InlineConlikeSig loc _ _ -> notSup loc "Pragma"
  SpecSig loc _ _ _ -> notSup loc "Pragma"
  SpecInlineSig loc _ _ _ _ -> notSup loc "Pragma"
  InstSig loc _ _ _ _ -> notSup loc "Pragma"
  AnnPragma loc _ -> notSup loc "Pragma"
  MinimalPragma loc _ -> notSup loc "Pragma"

syntaxValidate :: Module -> Validate [S.Decl SrcLoc]
syntaxValidate (Module loc _ (_:_) _ _ _ _) = notSup loc "Pragmas"
syntaxValidate (Module loc _ _ _ (Just (_:_)) _ _) = notSup loc "Export lists"
syntaxValidate (Module loc _ _ _ _ (_:_) _) = notSup loc "Imports"
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM declValidate decs
