module Main where

import Control.Applicative ((<$>), (<*>), (<*), (*>), ZipList(..), getZipList)
import Control.Monad (join, msum)
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

-- AST

type UnifiedPattern = [String]
type Unifiers = [(String, [String])]
data PatternPart = Tag String | Var String deriving (Show, Eq)
type Pattern = [PatternPart]
type Substitution = (Pattern, Pattern)
type Program = [Substitution]

-- Parser

separator :: GenParser Char st Char
separator = satisfy (\c -> isSpace c && c /= '\n' && c /= '\r') <?> "separator"

separators :: GenParser Char st ()
separators = skipMany separator

tagPart :: GenParser Char st PatternPart
tagPart = Tag <$> (char '\'' *> many (noneOf "'") <* char '\'') <* separators

varString :: GenParser Char st PatternPart
varString = Var <$> many1 letter <* separators

patternPart :: GenParser Char st PatternPart
patternPart = tagPart <|> varString

line :: GenParser Char st Substitution
line = (\a b -> (a, b)) <$> manyTill patternPart (separators *> string "->" <* separators) <*> many1 patternPart

program :: GenParser Char st Program
program = sepEndBy line newline

-- Pattern matching

unifyPatternPart :: PatternPart -> String -> Maybe Unifiers
unifyPatternPart (Var a) b = Just [(a, [b])]
unifyPatternPart (Tag a) b =
    if a == b
    then Just []
    else Nothing

substitutePatternPart :: Unifiers -> PatternPart -> Maybe [String]
substitutePatternPart _ (Tag a) = Just [a]
substitutePatternPart subs (Var a) = lookup a subs

fromTag :: PatternPart -> Maybe String
fromTag (Tag s) = Just s
fromTag (Var _) = Nothing

fromVar :: PatternPart -> Maybe String
fromVar (Var a) = Just a
fromVar (Tag _) = Nothing

matchSub :: UnifiedPattern -> Substitution -> Maybe [String]
matchSub input (pattern, output) =
    let (tailUnifier, patternNoTail) =
            case listToMaybe pattern >>= fromVar of
              Just var -> ([(var, take (length input - length (tail pattern)) input)], tail pattern)
              Nothing -> ([], pattern)
        unified = (++ [Just tailUnifier]) . getZipList $ unifyPatternPart <$> ZipList (reverse patternNoTail) <*> ZipList (reverse input)
    in if null tailUnifier && length input > length patternNoTail
       then Nothing
       else do
         subs <- sequence unified
         fmap join . sequence $ fmap (substitutePatternPart $ join subs) output

matchProgram :: Program -> UnifiedPattern -> Maybe [String]
matchProgram p input = msum $ fmap (matchSub input) p

runProgram :: Program -> UnifiedPattern -> String
runProgram p input =
    let output = matchProgram p input
    in case output of
      Just input' -> runProgram p input'
      Nothing -> join input

-- Main

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  content <- readFile filename
  case parse program filename content of
    Left e -> print e
    Right p -> putStrLn $ runProgram p []
