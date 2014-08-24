module Html.Types

%access public
%default total

Tag : Type
Tag = String

record Attribute : Type where
    MkAttribute : (key : String) ->
                  (value : String) ->
                  Attribute


mutual
    record Element : Type where
        MkElement : (tag : Tag) ->
                    (attributes : List Attribute) ->
                    (children : List Html) ->
                    Element

    data Html : Type where
        CData : String -> Html
        Comment : String -> Html
        Content : String -> Html
        Node : Element -> Html

private
emptyElement : Element
emptyElement = MkElement "" [] []

elem : Tag -> List Attribute -> List Html -> Html
elem t a c = Node $ MkElement t a c

attr : String -> String -> Attribute
attr k v = MkAttribute k v


