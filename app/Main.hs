module Main where

import Control.Monad
import CoqTop
import Data.Char
import Data.Maybe
import Main.Utf8
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe

data Flag
  = Version
  | Help
  | Input String
  | Output String
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h','?'] ["help"]    (NoArg Help)           "show help message"
  , Option ['V']     ["version"] (NoArg Version)        "show version"
  , Option ['o']     ["output"]  (ReqArg Output "FILE") "output to FILE"
  , Option ['i']     ["input"]   (ReqArg Input "FILE")  "input from FILE"
  ]

data Flags = Flags
  { showVersion :: Bool
  , showHelp    :: Bool
  , inputFile   :: String
  , outputFile  :: String
  }

makeFlags :: [Flag] -> IO Flags
makeFlags fs = do
  let filterInputs = mapMaybe (\case Input s -> Just s; _ -> Nothing)
  let filterOutputs = mapMaybe (\case Output s -> Just s; _ -> Nothing)
  input <- unsafeInterleaveIO case filterInputs fs of
    [x] -> pure x
    []  -> die "no input file."
    xs  -> die ("too many input files: " ++ show xs)
  output <- unsafeInterleaveIO case filterOutputs fs of
    []  -> pure "-"
    [x] -> pure x
    xs  -> die ("too many output files: " ++ show xs)
  pure Flags
    { showVersion = Version `elem` fs
    , showHelp    = Help `elem` fs
    , inputFile   = input
    , outputFile  = output
    }

showCoqTopComment :: String -> String
showCoqTopComment s | all isSpace s = ""
showCoqTopComment s | [x] <- lines s = "(** " ++ x ++ " *)"
showCoqTopComment s = "(** [Coq Proof View]\n" ++ unlines view ++ " *)"
  where isLine xs = not (all isSpace xs) && all (== '=') (dropWhile isSpace xs)
        width = maximum $ map length $ filter (not . isLine) (lines s)
        replaceLine xs
          | isLine xs = "  " ++ replicate (width - 2) '='
          | otherwise = xs
        view = map ((" * " ++) . replaceLine) (lines s)

helpHeader :: String -> String
helpHeader prog = "usage: " ++ prog ++ " [-i] <file> [-o <file>]\n\nOptions:"

description :: String
description = unlines
  [ "CoqTop Elaborate 1.0"
  , "Elaborate Coq source code with coqtop output."
  , "This is free software; see the source for copying conditions.  There is NO"
  , "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
  ]

withFileOr :: FilePath -> IOMode -> Handle -> (Handle -> IO a) -> IO a
withFileOr "-"  _ h p = p h
withFileOr file m _ p = withFile file m p

main :: IO ()
main = withUtf8 do
  args <- getArgs
  let (rawFlags, ~[], errors) = getOpt (ReturnInOrder Input) options args
  Flags{..} <- makeFlags rawFlags
  when (not (null errors) || showHelp) do
    prog <- getProgName
    die (concat errors ++ usageInfo (helpHeader prog) options)
  when showVersion (putStrLn description)
  withFile inputFile ReadMode \fileIn ->
    withFileOr outputFile WriteMode stdout \fileOut -> do
      res <- coqtop =<< hGetContents fileIn
      forM_ res \case
        Left s  -> hPutStr fileOut s
        Right s -> hPutStrLn fileOut (showCoqTopComment s)
