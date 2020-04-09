module Main where

sortSublists :: (Integral a) => [[a]] -> [[a]]
sortSublists [] = []
sortSublists (x: xs) = 
    let smaller = sortSublists [a | a <- xs, sum a <= sum x]
        bigger = sortSublists [a | a <- xs, sum a > sum x]
    in smaller ++ [x] ++ bigger

generatePrefix :: [a] -> [[a]]
generatePrefix [] = [[]]
generatePrefix xs = xs : generatePrefix(init xs)

generateSublists :: [a] -> [[a]]
generateSublists [] = [[]]
generateSublists xs =  generatePrefix xs ++ generateSublists(tail xs)

smallestK :: (Integral a) => [a] -> Int -> [[a]]
smallestK xs k = 
    let sublists = generateSublists xs
        sortedLists = sortSublists(filter (/= []) sublists)
    in take k sortedLists

findStartIndex :: (Integral a) => [a] -> [a] -> Int
findStartIndex list sublist = 
    if take (length sublist) list == sublist then 1
    else 1 + findStartIndex (tail list) sublist  

findEndIndex :: (Integral a) => [a] -> [a] -> Int
findEndIndex list sublist = 
    if take (length sublist) list == sublist then length sublist
    else 1 + findEndIndex (tail list) sublist  

formatList :: [Int] -> String
formatList [] = "]\n"
formatList (x: xs) = show x ++ " " ++ formatList xs 

formatOutput :: [Int] -> [[Int]] -> String
formatOutput list [] = ""
formatOutput list (x: xs) = 
    show (sum x) ++ "\t" ++ show (findStartIndex list x) ++ "\t" ++ 
    show (findEndIndex list x) ++ "\t[" ++ formatList x ++ formatOutput list xs

formatHeaderOutput :: String
formatHeaderOutput = "\nsize \ti \tj \tsublist \n\n"

testList1 :: (Integral a ) => [a]
testList1 = [x * (-1) ^x | x <- [1..100]]

testList2 :: (Integral a ) => [a]
testList2 = [24,-11,-34,42,-24,7,-19,21]

testList3 :: (Integral a ) => [a]
testList3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]

testList4 :: (Integral a ) => [a]
testList4 = [1,1,-2,1,1]

k1 :: Int
k1 = 15

k2 :: Int
k2 = 6

k3 :: Int
k3 = 8

list :: (Integral a) => [a]
list = testList1

k :: Int
k = k1

main 
    | null list = error "ERROR: list is empty"
    | k > length list = error "ERROR: the value k is larger than the length of the list"
    | otherwise = putStr(formatHeaderOutput ++ formatOutput list (smallestK list k))