import Parser
import Ast
import Types
import Eval

main =
  do p <- getContents
     case run expr p of
       Left err ->
         do putStr "parse error at "
            print err
       Right ast ->
         --do
           --print ast
           case infer ast of
             Left str ->
               do putStrLn str
             Right node ->
               do
                 putStrLn $ "---- Type Annotations ----\n" ++
                   ppTTerm (tExpr node)
                 case normalOrder (nodeExpr node) of
                   Left str ->
                     do putStrLn str
                   Right term ->
                     do
                       putStrLn $ "---- Value ----\n" ++
                         ppTerm term
