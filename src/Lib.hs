module Lib where
import           Control.Applicative (Alternative (..), many, some)
import           Data.Char           (digitToInt, isDigit)
import           Data.List           (intersperse)


newtype Parser i o = Parser { runParser :: i -> (Maybe o, i) }

instance Functor (Parser i) where
  fmap f p = Parser $ \input ->
    let (mo, remaining) = runParser p input
    in (f <$> mo, remaining)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> (Just x, input)
  pf <*> px = Parser $ \input -> case runParser pf input of
    (Just f, fRest) -> case runParser px fRest of
      (Just x, xRest) -> (Just (f x), xRest)
      (Nothing, _)    -> (Nothing, input)
    (Nothing, _) -> (Nothing, input)

satisfy :: (Char -> Bool) -> Parser String Char
satisfy predicate = Parser $ \input -> case input of
  ""       -> (Nothing, input)
  (c:rest) -> if predicate c then (Just c, rest) else (Nothing, input)

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

char :: Char -> Parser String Char
char x = satisfy (x ==)

word :: String -> Parser String String
word ""    = Parser $ \input -> (Just "", input)
word (c:s) = ((:) <$> char c) <*> word s

instance Alternative (Parser i) where
  empty = Parser $ \input -> (Nothing, input)
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    (Nothing, _) -> case runParser p2 input of
      (Nothing, _) -> (Nothing, input)
      x            -> x
    x -> x

sepBy :: Parser i v -> Parser i s -> Parser i [v]
sepBy pat sep = (:) <$> pat <*> many (sep *> pat) <|> pure []

oneOf :: String -> Parser String Char
oneOf = foldr (<|>) empty . (char <$>)

spaces :: Parser String String
spaces = some $ oneOf " \t\n"

--------------------

data LispExpression
  = LispNumber Int
  | LispSymbol String
  | LispList [LispExpression]
  deriving Show

someOneOf :: String -> Parser String String
someOneOf = some . oneOf

lispNumber :: Parser String LispExpression
lispNumber = LispNumber . read <$> someOneOf ['0'..'9']

lispSymbol :: Parser String LispExpression
lispSymbol = LispSymbol <$> someOneOf "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM-!?+*"

lispList :: Parser String LispExpression
lispList = LispList <$> (char '(' *> sepBy lispExpression spaces <* char ')')

lispExpression :: Parser String LispExpression
lispExpression = lispNumber <|> lispSymbol <|> lispList

lispWrite :: LispExpression -> String
lispWrite (LispNumber n) = show n
lispWrite (LispSymbol s) = s
lispWrite (LispList xs)  = "(" ++ concat (intersperse " " (map lispWrite xs)) ++ ")"
