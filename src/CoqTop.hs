module CoqTop (coqtop) where

import Control.Monad
import Data.Char
import Data.List
import System.IO
import System.Process

import Language.Coq.Parser

breakStatement :: String -> (String, String)
breakStatement = breakSentence

breakStatements :: String -> [String]
breakStatements = unfoldr \src ->
  if all isSpace src then Nothing
  else Just (breakStatement src)

procCoqtop :: CreateProcess
procCoqtop = (proc "coqtop" [])
  { std_in = CreatePipe
  , std_out = CreatePipe
  , std_err = CreatePipe
  }

while :: IO Bool -> IO a -> IO [a]
while cond act = do
  carryOn <- cond
  if carryOn
    then (:) <$> act <*> while cond act
    else pure []

interleave :: [a] -> [a] -> [a]
interleave []     ys = ys
interleave (x:xs) ys = x : interleave ys xs

coqtop :: String -> IO [Either String String]
coqtop (breakStatements -> srcLines)
  = withCreateProcess procCoqtop
  \ ~(Just hin) ~(Just hout) ~(Just herr) _ -> do
  _ <- hGetLine hout
  resLines <- forM srcLines \ss -> do
    hPutStr hin ss; hFlush hin
    _ <- hGetLine herr
    unlines <$> while (hWaitForInput hout 0) (hGetLine hout)
  pure (interleave (map Right resLines) (map Left srcLines))
