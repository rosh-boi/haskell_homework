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