{-# LANGUAGE LambdaCase #-}
module SyntaxValidate (InvalidConstruct, syntaxValidate) where
import Language.Haskell.Exts.Syntax

data InvalidConstruct = InvalidConstruct SrcLoc String

notSup :: SrcLoc -> String -> Either InvalidConstruct a
notSup loc str = Left (InvalidConstruct loc str)

instance Show InvalidConstruct where
  show (InvalidConstruct loc message) =
    "Invalid construct at " ++ show loc ++ "\t" ++ message

tyValidate :: Type -> Either InvalidConstruct ()
tyValidate = undefined

cxtValidate :: Context -> Either InvalidConstruct ()
cxtValidate = undefined

classValidate :: ClassDecl -> Either InvalidConstruct ()
classValidate = undefined

instValidate :: InstDecl -> Either InvalidConstruct ()
instValidate = undefined

matchValidate :: Match -> Either InvalidConstruct ()
matchValidate = undefined

constrValidate :: QualConDecl -> Either InvalidConstruct ()
constrValidate = undefined

declValidate :: Decl -> Either InvalidConstruct ()
declValidate = \case
  TypeDecl _ _ _ t -> tyValidate t
  TypeFamDecl loc _ _ _ -> notSup loc "Type families"
  ClosedTypeFamDecl loc _ _ _ _ -> notSup loc "Closed type families"
  DataDecl _ _ [] _ _ constrs _ -> mapM_ constrValidate constrs
  DataDecl loc _ _ _ _ _ _ -> notSup loc "Unsupported data type extensions"
  GDataDecl loc _ _ _ _ _ _ _ -> notSup loc "GADT"
  DataFamDecl loc _ _ _ _ -> notSup loc "Data families"
  TypeInsDecl loc _ _ -> notSup loc "Type family Instance"
  DataInsDecl loc _ _ _ _ -> notSup loc "Type family instance"
  GDataInsDecl loc _ _ _ _ _ -> notSup loc "Type family instance"
  ClassDecl _ cxt _ [_] [] cls -> cxtValidate cxt >> mapM_ classValidate cls
  ClassDecl loc _ _ _ _ _ -> notSup loc "Type class extensions"
  InstDecl _ Nothing _ cxt _ tys inst ->
    mapM_ tyValidate tys >> cxtValidate cxt >> mapM_ instValidate inst
  InstDecl loc _ _ _ _ _ _ ->
    notSup loc "Unsupported instance declaration extensions"
  DerivDecl loc _ _ _ _ _ -> notSup loc "Deriving declarations"
  InfixDecl _ _ _ _ -> Right ()
  DefaultDecl loc _ -> notSup loc "Default declarations"
  SpliceDecl loc _ -> notSup loc "Splices"
  TypeSig _ _ t -> tyValidate t
  FunBind ms -> mapM_ matchValidate ms
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

syntaxValidate :: Module -> Either InvalidConstruct ()
syntaxValidate (Module loc _ (_:_) _ _ _ _) = notSup loc "Pragmas"
syntaxValidate (Module loc _ _ _ (Just (_:_)) _ _) = notSup loc "Export lists"
syntaxValidate (Module loc _ _ _ _ (_:_) _) = notSup loc "Imports"
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM_ declValidate decs
