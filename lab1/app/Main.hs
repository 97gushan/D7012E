-- Gustav Hansson

module Main where

getSum :: (Int, Int, [Int]) -> Int
getSum (s, i, list) = s

getStartIndex :: (Int, Int, [Int]) -> Int
getStartIndex (s, i, list) = i

getEndIndex :: (Int, Int, [Int]) -> Int
getEndIndex (s, i, list) = i + length list - 1

getList :: (Int, Int, [Int]) -> [Int]
getList (s, i, list) = list

sortSublists :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
sortSublists [] = []
sortSublists (x: xs) = 
    let smaller = sortSublists [b | b <- xs, getSum b <= getSum x]
        bigger = sortSublists [b | b <- xs, getSum b > getSum x]
    in smaller ++ [x] ++ bigger

generatePrefix :: [Int] -> Int -> [(Int, Int, [Int])]
generatePrefix [] _ = []
generatePrefix xs i = (sum xs, i,xs) : generatePrefix (init xs) i

generateSublists :: [Int] -> Int -> [(Int, Int, [Int])]
generateSublists [] _ = []
generateSublists xs i =  generatePrefix xs i ++ generateSublists (tail xs) (i+1)

smallestK :: [Int] -> Int -> [(Int, Int, [Int])]
smallestK xs k = 
    let sublists = generateSublists xs 1
        sortedLists = sortSublists sublists
    in take k sortedLists

formatList :: [Int] -> String
formatList [] = "]\n"
formatList (x: xs) = show x ++ " " ++ formatList xs 

formatOutput :: [(Int, Int, [Int])] -> String
formatOutput [] = ""
formatOutput (x: xs) = 
    show (getSum x) ++ "\t" ++ show (getStartIndex x) ++ "\t" ++ 
    show (getEndIndex x) ++ "\t[" ++ formatList (getList x) ++ formatOutput xs

formatHeaderOutput :: String
formatHeaderOutput = "\nsize \ti \tj \tsublist \n\n"

testList1 :: [Int]
testList1 = [x * (-1) ^x | x <- [1..100]]

testList2 :: [Int]
testList2 = [24,-11,-34,42,-24,7,-19,21]

testList3 :: [Int]
testList3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]

testList4 :: [Int]
testList4 = [1,-2,2,2,2,1,-2]

k1 :: Int
k1 = 15

k2 :: Int
k2 = 6

k3 :: Int
k3 = 8

list :: [Int]
list = testList1

k :: Int
k = k1

main 
    | null list = error "ERROR: list is empty"
    | k > length list = error "ERROR: the value k is larger than the length of the list"
    | otherwise = putStr(formatHeaderOutput ++ formatOutput (smallestK list k))
