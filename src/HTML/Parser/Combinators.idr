module HTML.Parser.Combinators

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

%access public

oneOf : List Char -> Parser Char
oneOf xs = satisfy (\x => elem x xs)

charNoCase : Char -> Parser Char
charNoCase c = satisfy ((toUpper c) ==) <|> satisfy ((toLower c) ==)

letter : Parser Char
letter = satisfy isAlpha

alphanum : Parser Char
alphanum = satisfy isAlphaNum

anyChar : Parser Char
anyChar = satisfy (\c => True)

anyCharList : Parser (List Char)
anyCharList = many anyChar

stringNoCase : String -> Parser String
stringNoCase s = map pack (traverse charNoCase (unpack s))

covering
manyTill : Parser a -> Parser b -> Parser (List a)
manyTill p e = scan
    where
        scan  = (do e; return (with List []))
                    <|> do 
                        x <- p
                        xs <- scan
                        return (x::xs)

many1 : Parser a -> Parser (List a)
many1 p = do{ x <- p; xs <- many p; return (x::xs) }
              
