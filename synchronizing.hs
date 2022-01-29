import Data.Char

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
setToInt = foldl pow2 0

intToSet :: Integer -> [Integer]
intToSet = intToSet_ 0 []

binariesToSets :: [Integer] -> [[Integer]]
binariesToSets  = map intToSet

type Function = (Integer, Char, Integer)
type Trace = ([Integer],String)

net :: Integer->Char->Integer->Function->Integer
net n a k (q,c,p) = if (n == q) && (a==c)
    then p+k
    else 0+k

transition :: [Function]->Char->Integer->Integer
transition l a n = foldl (net n a) 0 l

transitionInPowerAutomata :: [Function]->Char->[Integer]->[Integer]
transitionInPowerAutomata f a = map (transition f a) 


collapse_ :: [Integer] -> [Integer] ->[Integer]
collapse_ [] tmp = tmp
collapse_ (s:xs) tmp = collapse_ (filter (/=s) xs) $ s:tmp

collapse :: [Integer] -> [Integer]
collapse l =  collapse_ l []

singleStateTrace :: [Trace] -> String
singleStateTrace [] = "None"
singleStateTrace ((node, s):xs)
                    | length (collapse node) == 1 = s
                    | otherwise = singleStateTrace xs
isSingleState :: Bool -> Trace -> Bool
isSingleState prev (node,s)
                | length  (collapse node) == 1 = True
                | otherwise = prev || False

bfs :: String -> Integer -> [Function]  -> [Trace] -> String
bfs _ 0 _ _ = "None"
bfs alphabet n func start
            | isReached = singleStateTrace start 
            | otherwise = bfs alphabet (n-1) func $ concatMap (\a -> map (\(li, s) -> (transitionInPowerAutomata func a li, a:s)) start) alphabet 
            where isReached = foldl isSingleState False start 

toFunction :: [String] ->[Function]
toFunction = foldl (\l s -> (toInteger (digitToInt (head s)), s !! 1, toInteger(digitToInt (s !! 2))):l) [] 

main :: IO ()
main = 
    do 
        file <- readFile "automaton.txt"
        let fileLines  = lines file
        let size = read (head fileLines) :: Integer
        let startState = intToSet $ 2^size - 1
        let alphabet = fileLines !! 1
        let function = toFunction $ (tail.tail) fileLines
        print $ bfs alphabet (2^size -1) function [(startState, "")]
        
--func = [(1,'b',2),(1,'a',1),(2,'b',2),(2,'a',1)]
--alphabet = "ab"
--start = [([1,2],"")]