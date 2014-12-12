module SyntaxValidate (InvalidConstruct, syntaxValidate) where
import Language.Haskell.Exts.Syntax

data InvalidConstruct = InvalidConstruct SrcLoc String

instance Show InvalidConstruct where
  show (InvalidConstruct loc message) =
    "Invalid construct at " ++ show loc ++ "\n\t" ++ message

declValidate :: Decl -> Either InvalidConstruct ()
declValidate = undefined

-- Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
syntaxValidate :: Module -> Either InvalidConstruct ()
syntaxValidate (Module loc _ (_:_) _ _ _ _) =
  Left (InvalidConstruct loc "Pragmas")
syntaxValidate (Module loc _ _ _ (Just (_:_)) _ _) =
  Left (InvalidConstruct loc "Export lists")
syntaxValidate (Module loc _ _ _ _ (_:_) _) =
  Left (InvalidConstruct loc "Imports")
syntaxValidate (Module _ _ _ _ _ _ decs) = mapM_ declValidate decs
