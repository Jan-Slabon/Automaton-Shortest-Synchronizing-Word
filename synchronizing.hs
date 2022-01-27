powerset :: Integer ->[Integer]
powerset n = [1..2^n-1]

intToSet_ :: Integer->[Integer]->Integer->[Integer]
intToSet_ _ l 0 = l
intToSet_ i l n = if n `mod` 2 == 1
    then intToSet_  (i+1) (i:l) (n `div` 2)
    else intToSet_  (i+1) l (n `div` 2)

pow2 :: Integer->Integer->Integer
pow2 a b = a + 2^b

setToInt :: [Integer]->Integer
setToInt l = foldl pow2 0 l

intToSet :: Integer -> [Integer]
intToSet = intToSet_ 0 []

binariesToSets :: [Integer] -> [[Integer]]
binariesToSets list = map intToSet list

type Function = (Integer, Char, Integer)

net :: Integer->Char->Integer->Function->Integer
net n a k (q,c,p) = if ((n == q) && (a==c))
    then p+k
    else 0+k

transition :: [Function]->Char->Integer->Integer
transition l a n = foldl (net n a) 0 l

transitionInPowerAutomata :: [Function]->Char->[Integer]->[Integer]
transitionInPowerAutomata f a = map (transition f a) 

type Trace = ([Integer],String)
collapse_ :: [Integer] -> [Integer] ->[Integer]
collapse_ [] tmp = tmp
collapse_ (s:xs) tmp = collapse_ (filter (/=s) xs) $ s:tmp

collapse :: [Integer] -> [Integer]
collapse l =  collapse_ l []

singleStateTrace :: [Trace] -> String
singleStateTrace [] = "None"
singleStateTrace ((node, s):xs)
                    | ((length $ collapse node) == 1) = s
                    | otherwise = singleStateTrace xs
isSingleState :: Bool -> Trace -> Bool
isSingleState prev (node,s)
                | ((length $ collapse node) == 1) = True
                | otherwise = (prev || False)

bfs :: String -> [Function] -> [Trace] -> String
bfs alfabet func start 
            | isReached = singleStateTrace start 
            | otherwise = bfs alfabet func $ concat $ map (\a -> map (\(li, s) -> (transitionInPowerAutomata func a li, a:s)) start) alfabet 
            where isReached = foldl isSingleState False start 

main :: IO ()
main = 
    do 
        file <- readFile "automaton.txt"
        print file

--func = [(1,'b',2),(1,'a',1),(2,'b',2),(2,'a',1)]
--alfabet = "ab"
--start = [([1,2],"")]