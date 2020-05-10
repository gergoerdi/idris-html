{-
Most of this parser was lifted from https://github.com/tauli/idris-monadic-parser and ported to Lightyear.

I am retaining the copyright notice for the project here.  Once this parser is reworked it will likely be removed.

Copyright (c) 2013, tauli
All rights reserved.
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}


module HTML.Parser.HTML5

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings
import Lightyear.Char

import HTML.Parser.Combinators

%access public export

Attribute : Type
Attribute = (String, String)

data Tag =
      TagOpen    String (List Attribute)
    | TagVoid    String (List Attribute)
    | TagClose   String
    | TagText    String
    | TagComment String

Show Tag where
  show (TagOpen    s xs) = "TagOpen "    ++ "\"" ++ s ++ "\"" ++ " " ++ show xs
  show (TagVoid    s xs) = "TagVoid "    ++ "\"" ++ s ++ "\"" ++ " " ++ show xs
  show (TagClose   s   ) = "TagClose "   ++ "\"" ++ s ++ "\""
  show (TagText    s   ) = "TagText "    ++ "\"" ++ s ++ "\""
  show (TagComment s   ) = "TagComment " ++ "\"" ++ s ++ "\""

quoted : Parser s -> Parser s
quoted p = squote p <|> dquote p

htmlSpaceList : List Char
htmlSpaceList = ['\x0009', '\x000A', '\x000C', '\x000D', '\x0020']

htmlSpace : Parser Char
htmlSpace = oneOf htmlSpaceList

htmlSpaces : Parser (List Char)
htmlSpaces = many htmlSpace

doctype : Parser ()
doctype = do
    stringNoCase "<!DOCTYPE"
    many htmlSpace
    stringNoCase "html"
    many htmlSpace
    many $ satisfy (/= '>')
    char '>'
    pure ()

comment : Parser Tag
comment = do
    string "<!--"
    c <- manyTill anyChar (string "-->")
    pure $ TagComment (pack c)

cdata : Parser Tag
cdata = do
  string "<![CDATA["
  c <- manyTill anyChar $ string "]]>"
  pure $ TagText $ pack c

text : Parser Tag
text = map (TagText . pack) $ some $ satisfy (/= '<')

attributeName : Parser String
attributeName = map pack $ some $ satisfy (\x => not $ elem x (htmlSpaceList ++ ['\x00','\x22','\x27','>','/','=']))

unqotedAVal : Parser String
unqotedAVal = map pack $ some $ satisfy (\x => not $ elem x (htmlSpaceList ++ ['\x22','\x27','=','<','>','\x60']))

quoteAVal : Char -> Parser String
quoteAVal q = between (char q) (char q) $ map pack $ many $ satisfy (/= q)

singleQuoteAVal : Parser String
singleQuoteAVal = quoteAVal '\x22'

doubleQuoteAVal : Parser String
doubleQuoteAVal = quoteAVal '\x27'

attributeValue : Parser Attribute
attributeValue = do
    n <- attributeName
    htmlSpaces
    char '='
    htmlSpaces
    v <- unqotedAVal <|> singleQuoteAVal <|> doubleQuoteAVal
    pure (n,v)

attribute : Parser Attribute
attribute = attributeValue <|> (map (\x => (x,"")) attributeName) 

tag : Parser a -> Parser a
tag = between (char '<') (char '>')

startTag : Parser Tag
startTag = do
    n <- map pack $ some alphanum
    many htmlSpace
    a <- attribute `sepBy` htmlSpace
    many htmlSpace
    opt $ char '/'
    pure $ TagOpen n a


voidTags : List String
voidTags = 
  [
    "area",
    "base",
    "br",
    "col",
    "command",
    "embed",
    "hr",
    "img",
    "input",
    "keygen",
    "link",
    "meta",
    "param",
    "source",
    "track",
    "wbr"
  ]

voidTag : Parser Tag
voidTag = do
  n <- choice $ map stringNoCase voidTags
  many htmlSpace
  a <- attribute `sepBy` htmlSpace
  many htmlSpace
  opt $ char '/'
  pure $ TagVoid n a

endTag : Parser Tag
endTag = map (TagClose . pack) $ between (char '/') (many htmlSpace) $ some alphanum

element : Parser Tag
element = comment <|> cdata <|> tag voidTag <|> tag startTag <|> tag endTag <|> text

htmlDoc : Parser (List Tag)
htmlDoc = do
    many htmlSpace
    opt doctype
    many htmlSpace
    element `sepBy` (many htmlSpace) 