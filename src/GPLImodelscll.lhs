> module GPLImodelscll (mdlscll) where

> import GPLIevaluator
> import GPLIparser
> import System.IO
> import System.Console.ANSI
> import Data.List
> import System.Exit
> import ReadModels

> mdlscll input = do
>         hSetBuffering stdout NoBuffering        
>         putStrLn ""
>         putStr input
>         putStr "value: "
>         setSGR [SetColor Foreground Vivid Magenta]
>         putStrLn (show (val' (getformula (lines input)) ( readmodel input))) 
>         setSGR [Reset]
>         putStrLn ""

> val' y x = val x (genassignment x y) (head (parser y))
   
