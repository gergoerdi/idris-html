module HTML.Pretty

import HTML.Types

instance Show Attribute where
    show attr = key attr <+> "=\"" <+> value attr <+> "\""

private
renderAttributes : List Attribute -> String
renderAttributes attrs = if length attrs > 0 then " " <+> Prelude.Foldable.foldr (\e, r => e <+> " " <+> r) "" (map show attrs) else ""

private
indent : Nat -> String
indent level = Prelude.Foldable.foldr (\e, r => " " <+> r) "" [1..x]
    where
        x : Nat
        x = level * 4

private
newline : String
newline = "\r\n"

private
partial
renderHtml : Html -> String
renderHtml h = go 0 h
    where
        mutual 
            partial
            go : Nat -> Html -> String
            go level (CData c) = "<![CDATA[" <+> c <+> "]]>"
            go level (Comment c) = "<!-- " <+> c <+> " -->"
            go level (Content c) = c
            go level (Node el) = 
                if length (children el) < 1 then
                    "<" <+> tag el <+> renderAttributes (attributes el) <+> "/>"
                else
                    "<" <+> (tag el) <+> renderAttributes (attributes el) <+> ">" <+> renderChildren (level + 1) (children el) <+> "</" <+> (tag el) <+> ">"
            partial
            renderChildren : Nat -> List Html -> String
            renderChildren level children = Prelude.Foldable.foldr (\e, r => newline <+> indent level <+> e <+> r) (newline <+> indent (level - 1) <+> "") (map (go level) children)



instance Show Html where
    show h = renderHtml h

instance Show Element where
    show el = renderHtml (Node el)

instance Show HtmlDoc where
    show (MkHtmlDoc html) = foldr accum "" html 
        where
            accum : Html -> String -> String
            accum e acc = (renderHtml e) ++ newline ++ acc