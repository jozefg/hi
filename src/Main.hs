{-# LANGUAGE LambdaCase #-}
module Main where
import Language.Haskell.Exts
import System.Environment
import System.Exit

runParser :: FilePath -> IO Module
runParser f =
  parseFile f >>= \case
    ParseOk a -> return a
    ParseFailed loc err -> do
      putStrLn ("Parsing failure: " ++ prettyPrint loc)
      putStrLn err
      exitFailure

main :: IO ()
main = do
  [f] <- getArgs
  _ <- runParser f
  putStrLn "Parsed OK"
