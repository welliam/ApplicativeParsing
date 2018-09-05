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

instance Alternative (Parser i) where
  empty = Parser $ \input -> (Nothing, input)
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    (Nothing, _) -> case runParser p2 input of
      (Nothing, _) -> (Nothing, input)
      x            -> x
    x -> x

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \input -> case input of
  []       -> (Nothing, input)
  (c:rest) -> if predicate c then (Just c, rest) else (Nothing, input)

parseMaybeList :: (a -> Maybe b) -> Parser [a] b
parseMaybeList f = Parser $ \input -> case input of
  []       -> (Nothing, input)
  (c:rest) -> case f c of
    Nothing   -> (Nothing, input)
    justValue -> (justValue, rest)

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

token :: Eq a => a -> Parser [a] a
token x = satisfy (x ==)

multiToken :: Eq a => [a] -> Parser [a] [a]
multiToken []    = Parser $ \input -> (Just [], input)
multiToken (c:s) = ((:) <$> token c) <*> multiToken s

sepBy :: Parser i v -> Parser i s -> Parser i [v]
sepBy pat sep = (:) <$> pat <*> many (sep *> pat) <|> pure []

oneOf :: Eq a => [a] -> Parser [a] a
oneOf = foldr (<|>) empty . (token <$>)

spaces :: Parser String String
spaces = some $ oneOf " \t\n"

--------------------

data LispExpression
  = LispNumber Int
  | LispSymbol String
  | LispList [LispExpression]
  | LispLambda [String] LispExpression
  deriving Show

mapSomeOf :: (String -> a) -> String -> Parser String a
mapSomeOf f = (f <$>) . some . oneOf

lispNumber :: Parser String LispExpression
lispNumber = mapSomeOf (LispNumber . read) ['0'..'9']

lispSymbol :: Parser String LispExpression
lispSymbol = mapSomeOf LispSymbol "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM-!?+*"

lispList :: Parser String LispExpression
lispList = LispList <$> (token '(' *> sepBy lispExpression spaces <* token ')')

-- listOfSymbols :: Parser String [String]
-- listOfSymbols = LispList <$> sepBy lispSymbol spaces

-- dottedList :: Parser String [String]
-- dottedList = lispList

-- lispArguments :: Parser String LispExpression
-- lispArguments = lispSymbol <|> dottedList <|> listOfSymbols

-- lispLambda :: Parser String LispExpression
-- lispLambda = LispList <$> (char '(' *> multiToken "lambda" *>  <*> sepBy lispExpression spaces <* char ')')

lispLambda :: Parser LispExpression LispExpression
lispLambda = parseMaybe p
  where p (LispList [(LispSymbol "lambda"), (LispList maybeArgs), body])
          = fmap (`LispLambda` body) (getArgs maybeArgs)
        p _ = Nothing
        getArgs []                    = Just []
        getArgs ((LispSymbol x):rest) = (x:) <$> getArgs rest
        getArgs _                     = Nothing

lispExpression :: Parser String LispExpression
lispExpression = lispNumber <|> lispSymbol <|> lispList

lispWrite :: LispExpression -> String
lispWrite (LispNumber n) = show n
lispWrite (LispSymbol s) = s
lispWrite (LispList xs)  = "(" ++ concat (intersperse " " (map lispWrite xs)) ++ ")"
