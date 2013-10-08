import System.Console.GetOpt
import System.Environment (getArgs)

import qualified Parser as P
import qualified Data.List as List
import Ast
import Types
import Eval

data Flag = TypeAnnot | Eval | Both | Help | Verbose
          deriving Show

showSteps [t] = ppTerm t 
showSteps (t : ts) = ppTerm t ++ " =>\n" ++ showSteps ts


options :: [OptDescr Flag]
options =
  [ Option ['a'] ["annot"] (NoArg TypeAnnot) "show the program with explicit types"
  , Option ['i'] ["interpret"] (NoArg Eval) "interpret the program"
  , Option ['b'] [] (NoArg Both) "show the program with explicit types then interpret"
  , Option ['t'] ["trace"] (NoArg Verbose) "Show each reduction step"
  , Option ['h'] ["help"]  (NoArg Help) "help"
  ]

header = "Usage: jebus OPTION"

jebusOpts :: [String] -> IO Flag
jebusOpts argv = 
  case getOpt RequireOrder options argv of
    ([flag], [], []) -> return flag
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))


execute p flag =
  do
    case (P.parse P.expr p) of
      Left str ->
        do
          putStr "parse error at "
          print str
      Right ast ->
        case flag of
          TypeAnnot ->
              case infer ast of
                Left str -> putStrLn str
                Right node -> putStrLn $ ppTTerm (tExpr node)
          Eval ->
              case infer ast of
                Left str -> putStrLn str
                Right node ->
                  case normalOrder (nodeExpr node) of
                    Left str -> putStrLn str
                    Right term -> putStrLn $  ppTerm term
                  
          Both ->
            do
              case infer ast of
                Left str -> putStrLn str
                Right node ->
                  case normalOrder (nodeExpr node) of
                    Left str -> putStrLn str
                    Right term ->
                      putStrLn $  "---- Type Annotations ----\n" ++
                        ppTTerm (tExpr node) ++ "----- Value -----\n" ++
                        ppTerm term
          Verbose ->
            case infer ast of
                Left str -> putStrLn str
                Right node ->
                  case normalOrderT (nodeExpr node) of
                    Left str -> putStrLn str
                    Right terms ->
                      do
                        let cnt = (List.length terms)-1
                        putStrLn $ showSteps terms ++ "\nPerformed " ++ show cnt ++ " beta reductions."
                  
          
main =
  do
    opt <- getArgs
    flag <- jebusOpts opt
    case flag of
      Help -> putStr $ usageInfo header options
      _ -> 
        do
          p <- getContents
          execute p flag
