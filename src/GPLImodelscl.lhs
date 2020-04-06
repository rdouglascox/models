> module GPLImodelscl (mdlscl1stdin, mdlscl2stdin) where

> import GPLIevaluator
> import GPLIparser
> import System.IO
> import System.Console.ANSI
> import Data.List
> import System.Exit
> import ReadModels
> import PrintModels

> check x y = if x == y
>             then "\nCorrect!"
>             else "\nIncorrect!" 

Case1: formula and model only.

> mdlscl2stdin input = do
>         hSetBuffering stdout NoBuffering
>         let out = readmodel input
>         putStrLn ("\nformula: " ++ (getformula (lines input)))
>         putStr (printmodel out)
>         putStr "\nvalue: "
>         setSGR [SetColor Foreground Vivid Magenta]
>         putStrLn (show (val' (getformula (lines input)) (readmodel input)))
>         setSGR [Reset]
>         putStrLn "" 

Case2: formula, model, and value.

> mdlscl1stdin input = do
>         hSetBuffering stdout NoBuffering
>         let out = readmodel input
>         putStrLn ("\nformula: " ++ (getformula (lines input)))
>         putStr (printmodel out)
>         putStr "\nvalue: "
>         let value = (val' (getformula (lines input)) (readmodel input))
>         putStrLn (show value)
>         setSGR [SetColor Foreground Vivid Magenta]
>         putStrLn (check value (head (getanswer (lines input))))
>         setSGR [Reset]
>         putStrLn "" 


> val' y x = val x (genassignment x y) (head (parser y))
   
