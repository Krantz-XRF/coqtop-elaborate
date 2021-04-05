module Main where

import Control.Monad
import CoqTop
import Data.Char
import Main.Utf8
import System.Environment
import System.Exit
import System.IO

printCoqTopComment :: String -> IO ()
printCoqTopComment s | all isSpace s = pure ()
printCoqTopComment s | [x] <- lines s
  = putStrLn ("(** " ++ x ++ " *)")
printCoqTopComment s = putStrLn
  ("(** [Coq Proof View]\n" ++ unlines view ++ " *)")
  where isLine xs = not (all isSpace xs) && all (== '=') (dropWhile isSpace xs)
        width = maximum $ map length $ filter (not . isLine) (lines s)
        replaceLine xs
          | isLine xs = "  " ++ replicate (width - 2) '='
          | otherwise = xs
        view = map ((" * " ++) . replaceLine) (lines s)

main :: IO ()
main = withUtf8 do
  args <- getArgs
  when (null args) do
    prog <- getProgName
    putStrLn ("usage: " ++ prog ++ " <file>")
    exitFailure
  withFile (head args) ReadMode \file -> do
    res <- coqtop =<< hGetContents file
    forM_ res \case
      Left s  -> putStr s
      Right s -> printCoqTopComment s
