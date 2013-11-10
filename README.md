The Jebus Lambda Calculus Interpreter
=====================================

Jebus is an interpreter for a lambda calculus variation implemented in Haskell.
Jebus was created for demonstration porposes, as part of a lambda calculus course of National Technical University of Athens.


Syntax
------

   
**e** ::=  **e<sub>1</sub>  e<sub>2</sub>** <br>
     | \ **ident** . **e**   <br>
     | "let" ["rec"] **ident** "=" **e<sub>1</sub>** "in" **e<sub>2</sub>** <br>
     | "["**e<sub>1</sub>**"," **e<sub>1</sub>**"]" <br>
     | "true" | "false" <br>
     | ("if" | "ifl") **e<sub>1</sub>** "then" **e<sub>2</sub>** "else"  **e<sub>3</sub>** <br>
     |  **e<sub>1</sub> op e<sub>2</sub>** <br>
     |  **e<sub>1</sub> rop e<sub>2</sub>** <br>
     |  **e<sub>1</sub> bop e<sub>2</sub>** <br>
        
**op** ::= "+" | "-" | "\*" | "\*\*"

**rop** ::= "=" | "<" | "<=" | ">" | ">="

**bop** ::= "&&" | "||"


Typing
------

The language is strictly typed using the Hindley–Milner type system. The types are implicit in the source and they are reconstructed before the interpretation using the algorithm W for type inference. The supported types are natural numbers, booleans, function and product types. It features let-polymorphism. 


Built-in Functions
------------------

The implementation also provides the following build-in functions:

* succ : nat -> nat
* pred : nat -> nat
* iszero : nat -> bool
* not : bool -> bool
* fst : a x b -> a
* snd : a x b -> b



Usage
-----

To compile:

    $ gch jebus.hs


Jebus reads a program from the standard input. 
The following command-line options are available:  


	$ jebus [COMMAND] ... [OPTIONS]

	Common flags:
  		-h --help           Display help message
  		-V --version        Print version information

	jebus annot 
  		Prints an explicitly typed version of the program

	jebus eval [OPTIONS]
  		Interpret the program

  		-t --trace          show each beta reduction
  		-e --eval=EVALMODE  specify evaluation strategy: normal (default) or
        	                applicative
        	                
        	                



Dependencies
------------

* [Ghc](http://www.haskell.org/haskellwiki/GHC)
* [wl-pprint](http://hackage.haskell.org/package/wl-pprint)
* [cmdargs](http://hackage.haskell.org/package/cmdargs)
* [parsec](http://hackage.haskell.org/package/parsec-3.1.3)

Implementation
--------------

All the functionality is implemented using abstractions and function applications.
For more imformation about the implementation read the slides in the report directory.
