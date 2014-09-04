module Main

import HTML

minimalHtml : String
minimalHtml = """
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>this is a title</title>
    <link rel="stylesheet" href="style.css">
    <script src="script.js"></script>
  </head>
  <body>
    <!-- page content -->
  </body>
</html>
"""

main : IO ()
main = 
    let x = parseHtml minimalHtml in case x of
        Left err => print err
        Right res => print (doc res)