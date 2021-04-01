module Main where

import Control.Monad
import CoqTop
import Data.Char
import System.Environment
import System.Exit
import System.IO

printCoqTopComment :: String -> IO ()
printCoqTopComment s | all isSpace s = pure ()
printCoqTopComment s = putStr $ unlines $ concat
  [ [ "(** Coq Proof View" ]
  , map (" * " ++) (lines s)
  , [ " *)" ]
  ]

main :: IO ()
main = do
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
