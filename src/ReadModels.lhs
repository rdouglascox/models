> module ReadModels (readmodel, getformula, getanswer, hasvalue, hasformula, hasmodel) where  

We import GPLIevaluator for our Model data type.

> import GPLIevaluator 

Which is as follows:

data Model = Model {domain :: [Int]
                   ,referents :: [(Char, Int)]
                   ,extensions' :: [(Char, [Int])]
                   ,extensions'' :: [(Char, [(Int,Int)])]
                   ,extensions''' :: [(Char, [(Int,Int,Int)])]
                   }
                   deriving (Show)

Okay, here is the most important function we export. 

> readmodel :: String -> Model
> readmodel x = Model (getdomain $ lines x) (getreferents $ lines x) (getoneplace $ lines x) (gettwoplace $ lines x) (getthreeplace $ lines x)

We also export getformula, which gets the formula.

> getformula :: [String] -> String
> getformula (x:xs) = if (take 9 x) == "formula: "
>                     then (drop 9 x) 
>                     else getformula xs 
> getformula [] = []

And we also export getanswer, which gets the answer.

> getanswer :: [String] -> [Bool]
> getanswer (x:xs) = if (take 7 x) == "value: "
>                     then [(read (drop 7 x):: Bool)]
>                     else getanswer xs 
> getanswer [] = []

Now, the basic idea is that we want `models` to be smart about the input it is
given. If it is only provided with a formula, then it should go into a kind of
interactive mode starting with the given formula. This will involve modifying
`GPLImodels'. If it only provided with a model, it should go into a kind of 
interactive mode where it asks for a formula. And if it is given a model and 
a formula then it should print the value. And, finally, if it is given a model
a formala and a value it should state whether the value is correct for the formula
and the model.

To begin with, let's just write simple check functions for "formula:", "model", and "value:". This should be easy, as none of these are indented.

> hasformula :: [String] -> Bool
> hasformula (x:xs) = if (take 8 x) == "formula:"
>                     then True
>                     else hasformula xs
> hasformula [] = False 

> hasmodel :: [String] -> Bool
> hasmodel (x:xs) = if (take 6 x) == "model:"
>                   then True
>                   else hasmodel xs
> hasmodel [] = False 

> hasvalue :: [String] -> Bool
> hasvalue (x:xs) = if (take 6 x) == "value:"
>                   then True
>                   else hasvalue xs
> hasvalue [] = False 

Cool. We export all of these.

*************

> getdomain :: [String] -> [Int]    
> getdomain (x:xs) = if (take 10 x) == "  domain: "
>                    then (read (drop 10 x) :: [Int])
>                    else getdomain xs 
> getdomain [] = []

> getreferents :: [String] -> [(Char, Int)]
> getreferents (x:xs) = if (take 12 x) == "  referents:"
>                       then readrefs xs
>                       else getreferents xs
> getreferents [] = []

> readrefs :: [String] -> [(Char, Int)]
> readrefs (x:xs) = if (take 4 x) == "    " 
>                   then (x!!4,(read [(x!!7)]::Int)):(readrefs xs)
>                   else []
> readrefs [] = []   

> getoneplace :: [String] -> [(Char, [Int])]
> getoneplace (x:xs) = if (take 14 x) == "    one-place:"
>                      then readones xs
>                      else getoneplace xs
> getoneplace [] = []

> readones :: [String] -> [(Char, [Int])]
> readones (x:xs) = if (take 6 x) == "      "
>                   then (x!!6,(read (drop 8 x) :: [Int])):(readones xs)
>                   else []
> readones [] = []  

> gettwoplace :: [String] -> [(Char, [(Int,Int)])]
> gettwoplace (x:xs) = if (take 14 x) == "    two-place:"
>                      then readtwos xs
>                      else gettwoplace xs
> gettwoplace [] = []

> readtwos :: [String] -> [(Char, [(Int,Int)])]
> readtwos (x:xs) = if (take 6 x) == "      "
>                   then (x!!6,(read (drop 8 x) :: [(Int,Int)])):(readtwos xs)
>                   else []
> readtwos [] = [] 

> getthreeplace :: [String] -> [(Char, [(Int,Int,Int)])]
> getthreeplace (x:xs) = if (take 16 x) == "    three-place:"
>                        then readthrees xs
>                        else getthreeplace xs
> getthreeplace [] = []

> readthrees :: [String] -> [(Char, [(Int,Int,Int)])]
> readthrees (x:xs) = if (take 6 x) == "      "
>                     then (x!!6,(read (drop 8 x) :: [(Int,Int,Int)])):(readthrees xs)
>                     else []
> readthrees [] = [] 


