module HTML.Provider

import Providers

import HTML.Parser.HTML5

%language TypeProviders

readHtml : String -> IO (Provider (List Tag))
readHtml fileName = do
    contents <- readFile fileName
    pure $ case parse htmlDoc contents of
        Left x => Error x
        Right y => Provide y

readHtmlString : String -> IO (Provider (List Tag))
readHtmlString contents =
    pure $ case parse htmlDoc contents of
        Left x => Error x
        Right y => Provide y
