module CoqTop (coqtop) where

import Control.Monad
import Data.Char
import Data.List
import System.IO
import System.Process

import Language.Coq.Parser

breakStatements :: String -> [String]
breakStatements s = go (zeroPos, s) where
  go = unfoldr \src ->
    if all isSpace (snd src)
      then Nothing
      else Just (breakSentence src)

procCoqtop :: Handle -> CreateProcess
procCoqtop h = (proc "coqtop" [])
  { std_in = CreatePipe
  , std_out = UseHandle h
  , std_err = UseHandle h
  , delegate_ctlc = True
  }

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x : interleave ys xs

expectWelcome :: Handle -> IO ()
expectWelcome = void . hGetLine

expectPrompt :: Handle -> IO String
expectPrompt h = go "" where
  go xs | " < " `isPrefixOf` xs = pure (reverse xs)
        | otherwise = do c <- hGetChar h; go (c : xs)

expectPrompt_ :: Handle -> IO ()
expectPrompt_ = void . expectPrompt

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

coqtop :: String -> IO [Either String String]
coqtop (breakStatements -> srcLines) = do
  (hout, hout_write) <- createPipe
  hSetBuffering hout_write NoBuffering
  hSetNewlineMode hout universalNewlineMode
  (~(Just hin), ~Nothing, ~Nothing, _)
    <- createProcess_ "coqtop" (procCoqtop hout_write)
  expectWelcome hout
  expectPrompt_ hout
  resLines <- forM srcLines \ss -> do
    let ssLine = if last ss == '\n' then ss else ss ++ "\n"
    let n = length (lines ssLine)
    hPutStr hin ssLine; hFlush hin
    prompt <- replicateM n (expectPrompt hout)
    pure (trim (concatMap (unlines . init . lines) prompt))
  pure (interleave (map Left srcLines) (map Right resLines))
