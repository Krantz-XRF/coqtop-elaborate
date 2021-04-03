module Language.Coq.Parser where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Language.Coq.Lexer

import Text.Parsec hiding (many, optional, (<|>))

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

zeroPos :: SourcePos
zeroPos = initialPos "<INPUT>"

breakSentence :: (SourcePos, String) -> ((Bool, String), (SourcePos, String))
breakSentence (firstPos, s) = ((ok, this), (finalPos, rest)) where
  (this, rest) = splitAt n s
  n = length (takeWhile (<= finalPos) sPos) - 1
  sentencePos
    = setPosition firstPos *> whiteSpace
    *> ((,) <$> optSentence <*> getPosition)
  optSentence = isJust <$> optional coqSentence
  ~(Right (ok, finalPos)) = runParser sentencePos () "<INPUT>" s
  sPos = scanl updatePosChar firstPos s
