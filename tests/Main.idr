module Main

import HTML.Provider

main : IO ()
main = do
    putStrLn "got here"
    res <- readString minimalHtml
    case res of
        Error x => print x
        Provide y => print y
    putStrLn "done"