-- First way of commenting in haskell
{-
second of comment is mutiline
-}

import Data.List 
import System.IO

-- haskell is statically type, so can't change its type after it is defined

---------------- Data Types ---------------------------------------------------------------

-- Int -2^63,2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer , this is unbounded meaning as big as memory 
-- Float 
-- Double, up to 11 points 
-- Bool, true or false 
-- Char 
-- Tuple 
------------------------------------------------------------------------------------

-- Math Functions ---------------------------------------------------------------------

sumOfNum = sum[1..10]

addNum = 23 + 123
multNum = 2 * 3
divNum = 3 / 2
modNum = mod 15 3 -- as it is a prefix operator, could also be written as 
modNum2 = 15 `mod` 4 -- aka infix operator 
negNum = 2 - (-3) -- need brackets 

{- type in eg. :t sqrt, in ghci to know is function type
    sqrt :: Floating a => a -> a
    this just means this function works with floating numbers, it will receive 'a' type 
    value and return an 'a' type value
-}
num9 = 9 :: Int 
sqrt9 = sqrt (fromIntegral num9) -- to convert int to float, but sometimes compiler is smart enough to convert
sqrt16 = sqrt 16

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
squared9v2 = 9 ^ 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999
-- sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh
-----------------------------------------------------------------------------------------

---------------------Bool--------------------------------------------------------------------

trueAndFalse = True && False
trueOrFalse = True || False 
notTrue = not(True)
-- trying typing in :t truncate or :t (+) in terminal
-----------------------------------------------------------------------------------------

-----------------------List------------------------------------------------------------------
-- list in haskell in singularly linked meaning u can only add elem infront of lists 

primeNum = [9,5,7,131,3]
morePrime = primeNum ++ [11,13]
favNum = 2:5:7:10:[] -- another of prepresenting list !!very important 
lol = [[2,3,4,33],[45,3]]

morePrime2 = 17 : morePrime
lenList = length morePrime2
revList = reverse primeNum
emptyList = null primeNum

secondElem = primeNum !! 1 -- returns elem at index=1
firstElem = head primeNum
lastElem = last primeNum

primeInit = init primeNum -- get all elems except last element 
first3values = take 3 primeNum

is5inList = 5 `elem` primeNum
maxValue = maximum primeNum
minValue = minimum primeNum

sumOfList = sum primeNum
zeroToTen = [0..10]
evenList = [2,4..100]
everyOddLetter = ['A','C'..'Z']
infiniteList10 = [10,20..] -- remember haskell is lazy, so will only use elem as needed

many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1,2,3])

listTimes2 = [n*2 | n <- [1..10]] -- use of generator 
listFilter = [n*2 | n <- [1..100], n*2 <= 40 ] -- use of filter
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,1,8,3,4,7,6]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
listBiggerThen5 = filter (>5) sumOfLists
evensUpTo20 = takeWhile (<=20) [2,4..]

-- fold applies to every item in list 
multOfList = foldl (*) 1 [2,3,4,5] -- 1 is initial value
-----------------------------------------------------------------------------------------

-----------------------List Comprehension------------------------------------------------------------------

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..]]

-----------------------------------------------------------------------------------------
-----------------------------Tuple------------------------------------------------------------
randTuple = (1,"Random tuple")
 
-- A tuple pair stores 2 values
bobSmith = ("Bob Smith",52)
 
-- Get the first value
bobsName = fst bobSmith
 
-- Get the second value
bobsAge = snd bobSmith
-- zip can combine values into tuple pairs 
names = ["Bob","Mary","Tom"]
addresses = ["123 Main","234 North","567 South"]
 
namesNAddress = zip names addresses 
-----------------------------------------------------------------------------------------

-----------------------------Functions------------------------------------------------------------

main = do 
    putStrLn "Whaz is your name?"
    name <- getLine
    putStrLn("Hello " ++ name)

addMe :: Int -> Int -> Int
addMe x y= x + y 

-- You can perform different actions based on values
whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
 
-- The default
whatAge _ = "Nothing Important"

-------recursion-----
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

isOdd :: Int -> Bool 
isOdd n 
    | n `mod` 2 == 0 = False
    | otherwise = True
    
isEven :: Int -> Bool 
isEven n = n `mod` 2 == 0

-- Use guards to define the school to output
whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
	| (age > 6) && (age <= 10) = "Elementary School"
	| (age > 10) && (age <= 14) = "Middle School"
	| (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college"
    
-- The where clause keeps us from having to repeat a calculation
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "Terrible Batting Average"
	| avg <= 0.250 = "Average Player"
	| avg <= 0.280 = "Your doing pretty good"
	| otherwise = "You're a Superstar"
	where avg = hits / atBats 

-- You can access list items by separating letters with : or get everything but
-- the first item with xs
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list contains " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first item is " ++ show x ++ " and the rest are " ++ show xs

-- We can also get values with an As pattern
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

-- ---------- HIGHER ORDER FUNCTIONS ----------
-- Passing of functions as if they are variables
 
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4]

-- Let's make map / how map works
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

-- Check if strings are equal with recursion
areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

-- PASSING A FUNCTION INTO A FUNCTION
-- (Int -> Int) says we expect a function that receives an Int and returns an Int
doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4 -- times4 is a function that is being passed in

-- RETURNING A FUNCTION FROM A FUNCTION
getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4

-- We could use this function with map as well
threePlusList = map adds3 [1,2,3,4,5]

-- ---------- LAMBDA ----------
-- How we create functions without a name
-- \ represents lambda then you have the arguments -> and result
 
dbl1To10 = map (\x -> x * 2) [1..10]