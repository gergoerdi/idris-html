module HTML.Provider

import HTML.Parser.HTML5
import Lightyear.Strings using (parse)

%language TypeProviders

readHtmlString : String -> IO (Provider (List Tag))
readHtmlString contents =
    pure $ case parse htmlDoc contents of
        Left x => Error x
        Right y => Provide y

readHtml : String -> IO (Provider (List Tag))
readHtml fileName = do
    Right contents <- readFile fileName
        | Left err => pure (Error $ show err)
    readHtmlString contents
