{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Language.Coq.Lexer where

import Data.Char
import Data.Functor

import Control.Applicative hiding (optional)

import Text.Parsec.Language

import Text.Parsec hiding (many, (<|>))

import qualified Text.Parsec.Token as P

type P = forall st . Parsec String st ()
type Pc = forall st . Parsec String st Char

satisfyCat :: [GeneralCategory] -> Pc
satisfyCat cats = satisfy ((`elem` cats) . generalCategory)

satisfyRanges :: [(Char, Char)] -> Pc
satisfyRanges rs = satisfy (flip all rs . inRange)
  where inRange c (l, r) = l <= c && c <= r

unicodeLetter :: Pc
unicodeLetter
    = satisfyCat [LowercaseLetter, UppercaseLetter, TitlecaseLetter, OtherLetter]
  <|> satisfyRanges [('\x1D00', '\x1D7F'), ('\x1D80', '\x1DBF'), ('\x1DC0', '\x1DFF')]
  <|> oneOf "_\x00A0"

unicodeIdPart :: Pc
unicodeIdPart = satisfyCat [DecimalNumber, LetterNumber, OtherNumber] <|> char '\''

coqString :: P
coqString = void (lexeme (char '"' <* many strPart <* char '"'))
  where strPart = void (satisfy (/= '"')) <|> void (try (string "\"\""))

coqInteger :: P
coqInteger = void (lexeme (some digit))

coqDef :: LanguageDef st
coqDef = emptyDef
  { P.commentStart    = "(*"
  , P.commentEnd      = "*)"
  , P.identStart      = unicodeLetter
  , P.identLetter     = unicodeLetter <|> unicodeIdPart
  , P.reservedOpNames = [":"]
  , P.reservedNames   = ["Fail", "Time", "Redirect", "Timeout"]
  }

bullets :: P
bullets
    = void (lexeme (some (oneOf "-+*")))
  <|> void (optional sel *> lexeme (char '{'))
  <|> void (lexeme (string "}"))
  where sel = brackets identifier *> reservedOp ":"

P.TokenParser {..} = P.makeTokenParser coqDef
