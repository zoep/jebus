{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs hiding (Normal)
import System.Environment (getArgs, withArgs)

import qualified Parser as P
import qualified Data.List as List
import qualified Ast
import qualified Types
import qualified Eval

-- based on the excellent tutorial :
-- http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/

data EvalMode = Normal | Applicative
              deriving (Data, Typeable, Show, Eq)

data Options =
    Annot
  | Eval
    { trace     :: Bool
    , eval      :: EvalMode
    }
  deriving (Data, Typeable, Show, Eq)

annot :: Options
annot = Annot
        &= help "Print an explicitly typed version of the program"
        
interpret :: Options
interpret = Eval
       { trace = False &= help "show each beta reduction"
       , eval  = Normal &= help "specify evaluation strategy: normal (default) or applicative" 
       }
       &= help "Interpret the program"
       
jebusModes :: Mode (CmdArgs Options)
jebusModes = cmdArgsMode $ modes [annot, interpret]
  &= summary (_PROGRAM_INFO ++ ", " ++ _AUTHORS)
  &= help _PROGRAM_ABOUT
  &= helpArg [explicit, name "help", name "h"]
  &= program _PROGRAM_NAME

_PROGRAM_NAME = "jebus"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "The Jebus Lambda Calculus Interpreter"
_AUTHORS = "Zoe Paraskevopoulou, Nick Giannarakis 2013"

main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $
            cmdArgsRun jebusModes
    optionHandler opts

optionHandler :: Options -> IO ()
optionHandler opts@Annot{..}  = do
  p <- getContents
  annotProg p 
optionHandler opts@Eval{..}  = do
  p <- getContents
  interProg p eval trace



showSteps [t] = Types.ppTerm t 
showSteps (t : ts) = Types.ppTerm t ++ " =>\n" ++ showSteps ts

annotProg p =
  case (P.parse p) of
    Left str ->
      do
        putStr "parse error at "
        print str
    Right ast ->
      case Ast.infer ast of
        Left str -> putStrLn str
        Right node -> putStrLn $ "----------------------------\n" ++
                        Types.ppTTerm (Types.tExpr node)  

interProg p eval trace =
  case (P.parse p) of
    Left str ->
      do
        putStr "parse error at "
        print str
    Right ast ->
      case Ast.infer ast of
        Left str -> putStrLn str
        Right node ->
          if (trace) then
            do
              let f = case eval of
                    Normal -> Eval.normalOrderT
                    Applicative -> Eval.applicativeOrderT
              case f (Types.nodeExpr node) of
                Left str -> putStrLn str
                Right terms ->
                  do
                    let cnt = (List.length terms)-1
                    putStrLn $ showSteps terms ++ "\nPerformed " ++
                      show cnt ++ " beta reductions."
          else
            do
              let f = case eval of
                    Normal -> Eval.normalOrder
                    Applicative -> Eval.applicativeOrder
              case f (Types.nodeExpr node) of
                Left str -> putStrLn str
                Right term -> putStrLn $ Types.ppTerm term
