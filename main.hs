import System.Environment

doIt::[Int] -> Int
doIt [] = 0
doIt (a:b)
     | a `mod` 2 == 0 = a + doIt(b)
     | otherwise = doIt(b)

getIndex::Int -> [Int] -> Int
getIndex _  [] = 0
getIndex n (a:b:c)
         | notElem n (a:b:c) /= True = n
         | otherwise =  getIndex b c


allnow::[Int] -> Int
allnow [] = 0
allnow (a:b)
    | getIndex a (b) `mod` 2 == 0 && a `mod` 2 == 0 = a + allnow b
    | otherwise = allnow b
