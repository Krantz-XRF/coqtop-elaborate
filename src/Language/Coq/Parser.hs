module Language.Coq.Parser where

import Control.Applicative hiding (optional)
import Control.Monad
import Language.Coq.Lexer

import Text.Parsec hiding (many, (<|>))

import Text.Parsec.Pos

peekSat :: (Char -> Bool) -> P
peekSat f = getInput >>= \case
  ""      -> pure ()
  (x : _) -> unless (f x) mzero

vernacControl :: P
vernacControl
    = reserved "Fail"
  <|> reserved "Time"
  <|> reserved "Redirect" <* coqString
  <|> reserved "Timeout" <* coqInteger

coqSentence :: P
coqSentence
    = optional vernacControl *> bullets
  <|> some sentencePart *> endOfSentence

sentencePart :: P
sentencePart
    = void (noneOf ".\"")
  <|> coqString
  <|> void (try (string ".."))
  <|> void (try (char '.' *> satisfy (`notElem` " \n\r\t")))

endOfSentence :: P
endOfSentence = char '.' *> peekSat (`elem` " \n\r\t")

breakSentence :: String -> (String, String)
breakSentence s = splitAt n s where
  sentencePos = spaces *> coqSentence *> getPosition
  finalPos = case runParser sentencePos () "<INPUT>" s of
    Right p -> p
    Left e  -> error (show e)
  sPos = scanl updatePosChar firstPos s
  n = length (takeWhile (<= finalPos) sPos) - 1
  firstPos = initialPos (sourceName finalPos)
