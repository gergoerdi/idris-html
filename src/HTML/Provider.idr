module HTML.Provider

import Providers

import HTML.Parser.HTML5

%language TypeProviders

readHtml : String -> IO (Provider (List Tag))
readHtml fileName = do
    contents <- readFile fileName
    case parse htmlDoc contents of
        Left x => return (Error x)
        Right y => return (Provide y)

readHtmlString : String -> IO (Provider (List Tag))
readHtmlString contents =
  case parse htmlDoc contents of
        Left x => return (Error x)
        Right y => return (Provide y)