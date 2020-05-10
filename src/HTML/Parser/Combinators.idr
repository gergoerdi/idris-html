module HTML.Parser.Combinators

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings
import Lightyear.Char

%access public export

option : (Monad m) => a -> ParserT s m a -> ParserT s m a
option x p          = p <|> pure x

optionMaybe : (Monad m) => ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p       = option Nothing (liftA Just p)

choice : (Foldable t, Monad m) => t (ParserT str m a) -> ParserT str m a
choice ps = foldr (<|>) empty ps

oneOf : List Char -> Parser Char
oneOf xs = satisfy (\x => elem x xs)

charNoCase : Char -> Parser Char
charNoCase c = satisfy ((toUpper c) ==) <|> satisfy ((toLower c) ==)

letter : Parser Char
letter = satisfy isAlpha

alphanum : Parser Char
alphanum = satisfy isAlphaNum

anyCharList : Parser (List Char)
anyCharList = many anyChar

stringNoCase : String -> Parser String
stringNoCase s = map pack (traverse charNoCase (unpack s))
