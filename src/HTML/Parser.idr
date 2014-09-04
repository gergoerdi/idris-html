module HTML.Parser

import Control.Monad.State
import Control.Monad.Identity

import HTML.Parser.HTML5
import HTML.Types

%access public
%default total

private
TagState : Type
TagState = List String

private
HtmlParser : Type -> Type
HtmlParser = StateT TagState Identity

private
push : String -> HtmlParser ()
push input = do
    cur <- get
    put $ with List (input::cur)

private
close : String -> HtmlParser Bool
close tagToClose = do
    cur <- get
    case cur of
        [] => return False
        (x::xs) =>  if x == tagToClose then do
                        put $ with List xs
                        return True 
                    else 
                        return False
private
mapM : (Monad m, Traversable f, Functor f) => (a -> m b) -> f a -> m (f b)
mapM p = sequence . map p

private
toAttr : (String, String) -> HTML.Types.Attribute
toAttr (k,v) = attr k v

private
partial
toHtml : List HTML.Parser.HTML5.Tag -> HtmlParser (List Html)
toHtml xs = do
    (res, rest) <- toHtml' xs
    return res
  where
    partial
    toHtml' : List HTML.Parser.HTML5.Tag -> HtmlParser (List Html, List HTML.Parser.HTML5.Tag)
    toHtml' [] = return ([], [])
    toHtml' (x::xs) = case x of
                        TagOpen t attrs => do
                                            (children, rest) <- toHtml' xs
                                            (siblings, rest2) <- toHtml' rest
                                            return $ ((elem t (map toAttr attrs) (children))::siblings, rest2)
                        TagVoid t attrs => do
                                            (siblings, rest) <- toHtml' xs
                                            return $ ((elem t (map toAttr attrs) [])::siblings, rest)
                        TagText s => do
                            (siblings, rest) <- toHtml' xs
                            return $ ((Content s)::siblings, rest)
                        TagComment s => do
                            (siblings, rest) <- toHtml' xs
                            return $ ((Comment s)::siblings, rest)
                        TagClose closeTag => do
                            res <- close closeTag
                            if res == True then do
                                return ([], xs)
                            else do
                                (children, rest) <- toHtml' xs
                                return (children, rest)
    
private
partial
convertTagsToHtml : List HTML.Parser.HTML5.Tag -> List Html
convertTagsToHtml tags = let (a, s) = runIdentity $ runStateT (toHtml tags) [] in a
    
partial
parseHtml : String -> Either String (List Html)
parseHtml input = case parse htmlDoc input of
                    Left x => Left x
                    Right y => Right $ convertTagsToHtml y