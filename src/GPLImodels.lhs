> module GPLImodels (mdls, mdls1stdin, mdls2stdin) where

> import GPLIevaluator
> import GPLIparser
> import System.IO
> import System.Console.ANSI
> import Data.List
> import System.Exit
> import ReadModels
> import PrintModels

The basic interactive case:

> mdls = do
>        hSetBuffering stdout NoBuffering        
>        putStr "Please enter a formula: "
>        input <- requestform
>        dom <- requestdom input
>        refassign <- requestrefs dom input
>        oneassign <- requestones dom input
>        twoassign <- requesttwos dom input
>        thrassign <- requestthrs dom input
>        putStrLn "Thank you! Here's the result:\n" 
>        putStrLn ("formula: " ++ input) 
>        putStrLn ("model:\n  domain: " ++ show dom)
>        prefs refassign
>        putStrLn "  extensions:"
>        pext1 oneassign
>        pext2 twoassign
>        pext3 thrassign
>        putStr "value: "
>        setSGR [SetColor Foreground Vivid Magenta]
>        putStrLn (show (val' input (Model dom refassign oneassign twoassign thrassign))) 
>        setSGR [Reset]
>        putStrLn ""

WITHOUT A MODEL

> mdls1stdin x = do
>           hSetBuffering stdout NoBuffering        
>           let input = getformula (lines x) 
>           putStrLn ("Formula: " ++ input) 
>           dom <- requestdom input
>           refassign <- requestrefs dom input
>           oneassign <- requestones dom input
>           twoassign <- requesttwos dom input
>           thrassign <- requestthrs dom input
>           putStrLn "Thank you! Here's the result:\n" 
>           putStrLn ("formula: " ++ input) 
>           putStrLn ("model:\n  domain: " ++ show dom)
>           prefs refassign
>           putStrLn "  extensions:"
>           pext1 oneassign
>           pext2 twoassign
>           pext3 thrassign
>           putStr "value: "
>           setSGR [SetColor Foreground Vivid Magenta]
>           putStrLn (show (val' input (Model dom refassign oneassign twoassign thrassign))) 
>           setSGR [Reset]
>           putStrLn ""


WITHOUT A FORMULA.


> mdls2stdin x = do
>           hSetBuffering stdout NoBuffering        
>           putStr "Please enter a formula: "
>           formula <- requestform
>           let out = readmodel x
>           putStrLn ("\nformula: " ++ formula)
>           putStrLn (printmodel out)
>           putStr "value: "
>           setSGR [SetColor Foreground Vivid Magenta]
>           putStrLn (show (val' formula out)) 
>           setSGR [Reset]
>           putStrLn ""



Here is the function for getting the formula with error detection.

> requestform :: IO String
> requestform = do 
>               input <- getLine
>               if null (parser input)
>               then do
>                    putStr "Not a well formed formula. Try Again!\nPlease enter a formula: "
>                    requestform
>               else do
>                    return (input)

Here is the function for getting the domain with error detection.

> requestdom :: [Char] -> IO [Int]
> requestdom xs = do
>                 putStr "Please enter the domain of the model: "
>                 input <- getLine
>                 if (null (reads input :: [([Int],[Char])]))
>                 then do
>                      putStrLn "Parse error. Please enter a list of positive integers. e.g. [1,2,3]"
>                      requestdom xs
>                 else if (not (null (snd (head (reads input :: [([Int],[Char])]))))) 
>                      then do 
>                           putStrLn "Parse error. Almost parsed! Try again!"
>                           requestdom xs
>                      else if null $ (read input :: [Int])
>                           then do
>                                putStrLn "A domain must be nonempty. Try again!"
>                                requestdom xs
>                           else do
>                                return (read input :: [Int])  
                      
Okay, here are the functions for collecting the referents for the names:

> requestrefs :: [Int] -> [Char] -> IO [(Char,Int)]
> requestrefs d xs = do
>                    refs <- mapM (requestref d) (getnames xs)
>                    return (zip (getnames xs) refs)

> requestref :: [Int] -> Char -> IO (Int)
> requestref d x = do
>                  putStr ("Please enter the referent of '" ++ [x] ++ "': ")  
>                  input <- getLine
>                  if (null (reads input :: [(Int,[Char])]))  
>                  then do
>                       putStrLn "Parse error. Please enter an integer from the domain. e.g. 1"
>                       requestref d x
>                  else if (not (null (snd (head (reads input :: [(Int,[Char])])))))                   
>                       then do 
>                            putStrLn "Parse error. Almost parsed. Try again!"
>                            requestref d x
>                            else if not ((read input :: Int) `elem` d)  
>                                 then do 
>                                      putStrLn  "Referents must be elements of the domain. Try again."
>                                      requestref d x 
>                                 else do
>                                      return (read input :: Int)




And now the function for collecting the extensions for one place predicates.

> requestones :: [Int] -> [Char] -> IO [(Char,[Int])]
> requestones d xs = do
>                  refs <- mapM (requestone d) (getpredicates1 (head(parser xs)))
>                  return (zip (getpredicates1 (head(parser xs))) refs)

> requestone :: [Int] -> Char -> IO ([Int])
> requestone d x = do
>                  putStr ("Please enter the extension of the one-place predicate '" ++ [x] ++ "': ")  
>                  input <- getLine
>                  if (null (reads input :: [([Int],[Char])]))  
>                  then do
>                       putStrLn "Parse error. Please enter a list of integers from the domain. e.g. [1,2]"
>                       requestone d x
>                  else if (not (null (snd (head (reads input :: [([Int],[Char])])))))                   
>                       then do 
>                            putStrLn "Parse error. Try again!"
>                            requestone d x
>                            else if not (and (map (\x -> x `elem` d) (read input :: [Int])))
>                                 then do
>                                      putStrLn "Extensions must be subsets of the domain. Try again!"
>                                      requestone d x
>                                 else do 
>                                      return (read input :: [Int])

And now the function for collecting the extensions for two place predicates.

> requesttwos :: [Int] -> [Char] -> IO [(Char,[(Int,Int)])]
> requesttwos d xs = do
>                    refs <- mapM (requesttwo d) (delete 'I' (getpredicates2 (head(parser xs))))
>                    return (zip (getpredicates2 (head(parser xs))) refs)

> requesttwo :: [Int] -> Char -> IO ([(Int,Int)])
> requesttwo d x = do
>                  putStr ("Please enter the extension of the two-place prediate '" ++ [x] ++ "': ")  
>                  input <- getLine
>                  if (null (reads input :: [([(Int,Int)],[Char])]))  
>                  then do
>                       putStrLn "Parse error. Please enter a list of pairs of integers from the domain. e.g. [(1,2),(2,2)]"
>                       requesttwo d x
>                  else if (not (null (snd (head (reads input :: [([(Int,Int)],[Char])])))))                   
>                       then do 
>                            putStrLn "Parse error. Almost parsed. Try again!"
>                            requesttwo d x
>                            else if not (and (map (\x -> x `elem` d) (simplifypairs (read input :: [(Int,Int)]))))
>                                 then do 
>                                      putStrLn  "Exensions must be pairs of elements of the domain. Try again!"
>                                      requesttwo d x
>                                 else do                            
>                                      return (read input :: [(Int,Int)])

> simplifypairs xs = concat (map simplify xs)
>     where simplify (x,y) = [x,y]

And now the function for collecting the extensions for three place predicates.

> requestthrs :: [Int] -> [Char] -> IO [(Char,[(Int,Int,Int)])]
> requestthrs d xs = do
>                    refs <- mapM (requestthr d) (getpredicates3 (head(parser xs)))
>                    return (zip (getpredicates3 (head(parser xs))) refs)

> requestthr :: [Int] -> Char -> IO ([(Int,Int,Int)])
> requestthr d x = do
>                  putStr ("Please enter the extension of the three-place predicate '" ++ [x] ++ "': ")  
>                  input <- getLine
>                  if (null (reads input :: [([(Int,Int,Int)],[Char])]))  
>                  then do
>                       putStrLn "Parse error. Please enter a list of triples of integers from the domain. e.g. [(1,1,1),(1,2,3)]"
>                       requestthr d x
>                  else if (not (null (snd (head (reads input :: [([(Int,Int,Int)],[Char])])))))                   then do 
>                            putStrLn "Parse error. Almost parsed. Try again!"
>                            requestthr d x
>                            else if not (and (map (\x -> x `elem` d) (simplifytriples (read input :: [(Int,Int,Int)]))))
 
>                                 then do
>                                      putStrLn "Extensions must be triples of elements of the domain. Try again!"
>                                      requestthr d x
>                                 else do
>                                      return (read input :: [(Int,Int,Int)])

> simplifytriples xs = concat (map simplify xs)
>     where simplify (x,y,z) = [x,y,z]

Here's the evaluation function we'll use.

> val' :: [Char] -> Model -> Bool
> val' y x = val x (genassignment x y) (head (parser y)) 

Here are some functions for conditionally printing bits of the model.

> prefs x = if not (null x)
>           then putStrLn ("  referents:\n    " ++ (printpairs' x))
>           else putStr ""  

> pext1 x = if not (null x)
>           then putStrLn ("    one-place:\n      " ++ printpairs x)
>           else putStr ""  

> pext2 x = if not (null x)
>           then putStrLn ("    two-place:\n      " ++ printpairs x)
>           else putStr "" 

> pext3 x = if not (null x)
>           then putStrLn ("    three-place:\n      " ++ printpairs x)
>           else putStr "" 

> printpairs xs = concat $ intersperse "\n      " (map pairs xs) 
>     where pairs x = [fst x] ++ ": " ++ show (snd x) 

> printpairs' xs = concat $ intersperse "\n    " (map pairs xs) 
>     where pairs x = [fst x] ++ ": " ++ show (snd x) 
   
