module Assoc (checkAssocs) where
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable (foldMap)
import qualified Data.Set      as S
import           Source
import           TC.Util

assocs :: [Decl a] -> S.Set Name
assocs = foldr go S.empty
  where go (DAssoc n) s = S.insert n s
        go _ s = s

checkAssocs :: [Decl a] -> Either TypeError ()
checkAssocs ds = forM_ (S.toList vars) $ \v ->
  if S.member v globals
  then return ()
  else throwError (NoSuchName v)
  where vars = assocs ds
        globals = foldMap boundVars ds
