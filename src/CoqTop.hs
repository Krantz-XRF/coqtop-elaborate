module CoqTop (coqtop) where

import Control.Monad
import Data.Char
import Data.List
import System.IO
import System.Process

import Language.Coq.Parser

breakStatements :: String -> [(Bool, String)]
breakStatements s = go (zeroPos, s) where
  go = unfoldr \src ->
    if all isSpace (snd src)
      then Nothing
      else Just (breakSentence src)

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
  resLines <- forM srcLines \(ok, ss) -> do
    hPutStr hin ss; hFlush hin
    when ok $ void $ hGetLine herr
    unlines <$> while (hReady hout) (hGetLine hout)
  pure (interleave (map Right resLines) (map (Left . snd) srcLines))
