module HTML.Parser

import Control.Monad.State
import Control.Monad.Identity

import HTML.Parser.HTML5
import HTML.Types
import Lightyear.Strings using (parse)

%access export
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
        [] => pure False
        (x::xs) =>  if x == tagToClose then do
                        put $ with List xs
                        pure True
                    else
                        pure False
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
    pure res
  where
    partial
    toHtml' : List HTML.Parser.HTML5.Tag -> HtmlParser (List Html, List HTML.Parser.HTML5.Tag)
    toHtml' [] = pure ([], [])
    toHtml' (x::xs) = case x of
                        TagOpen t attrs => do
                                            (children, rest) <- toHtml' xs
                                            (siblings, rest2) <- toHtml' rest
                                            pure ((elem t (map toAttr attrs) (children))::siblings, rest2)
                        TagVoid t attrs => do
                                            (siblings, rest) <- toHtml' xs
                                            pure ((elem t (map toAttr attrs) [])::siblings, rest)
                        TagText s => do
                            (siblings, rest) <- toHtml' xs
                            pure ((Content s)::siblings, rest)
                        TagComment s => do
                            (siblings, rest) <- toHtml' xs
                            pure ((Comment s)::siblings, rest)
                        TagClose closeTag => do
                            res <- close closeTag
                            if res then do
                                pure ([], xs)
                            else do
                                (children, rest) <- toHtml' xs
                                pure (children, rest)

private
partial
convertTagsToHtml : List HTML.Parser.HTML5.Tag -> List Html
convertTagsToHtml tags = let (a, s) = runIdentity $ runStateT (toHtml tags) [] in a
    
partial
parseHtml : String -> Either String (List Html)
parseHtml input = map convertTagsToHtml $ parse htmlDoc input
