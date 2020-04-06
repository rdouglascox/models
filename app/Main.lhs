> import GPLIenumerator
> import GPLImodels
> import GPLImodelscl
> import System.Environment
> import System.Exit
> import System.IO
> import ReadModels

> main = do
>        args <- getArgs
>        if (("--interact" `elem` args) || (("-i" `elem` args)))
>            then do mdls
>            else do if (not (null args))
>                    then do 
>                         input <- readFile (head args)
>                         selectmdlsstdin input
>                    else do  
>                         sin <- getContents
>                         selectmdlsstdin' sin

> tomdles2stdin :: String -> Bool
> tomdles2stdin x = not (hasformula (lines x)) && not (hasvalue (lines x)) && (hasmodel (lines x)) 

> tomdles1stdin x = (hasformula (lines x)) && not (hasmodel (lines x)) && not (hasvalue (lines x)) 
> tomdlescl1stdin x = (hasformula (lines x)) && (hasmodel (lines x)) && not (hasvalue (lines x))
> tomdlescl2stdin x = (hasformula (lines x)) && (hasmodel (lines x)) && (hasvalue (lines x))

> selectmdlsstdin :: String -> IO ()
> selectmdlsstdin x | tomdles1stdin x = mdls1stdin x 
>                   | tomdles2stdin x = mdls2stdin x
>                   | tomdlescl1stdin x = mdlscl2stdin x
>                   | tomdlescl2stdin x = mdlscl1stdin x

> selectmdlsstdin' :: String -> IO ()
> selectmdlsstdin' x | tomdlescl1stdin x = mdlscl2stdin x
>                    | tomdlescl2stdin x = mdlscl1stdin x
