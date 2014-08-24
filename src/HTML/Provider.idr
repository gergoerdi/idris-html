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

readString : String -> IO (Provider (List Tag))
readString contents =
  case parse htmlDoc contents of
        Left x => return (Error x)
        Right y => return (Provide y)


minimalHtml : String
minimalHtml = """
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>title</title>
    <link rel="stylesheet" href="style.css">
    <script src="script.js"></script>
  </head>
  <body>
    <!-- page content -->
  </body>
</html>
"""

--%provide (minimal : List Tag) with readString minimalHtml