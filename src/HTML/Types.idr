module HTML.Types

%access public export
%default total

Tag : Type
Tag = String

record Attribute where
    constructor MkAttribute
    key : String
    value : String

mutual
    record Element where
        constructor MkElement
        tag : Tag
        attributes : List Attribute
        children : List Html

    data Html : Type where
        CData : String -> Html
        Comment : String -> Html
        Content : String -> Html
        Node : Element -> Html

record HtmlDoc where
    constructor MkHtmlDoc
    html : List Html


attr : String -> String -> Attribute
attr k v = MkAttribute k v

elem : Tag -> List Attribute -> List Html -> Html
elem t a c = Node $ MkElement t a c

doc : List Html -> HtmlDoc
doc xs = MkHtmlDoc xs
