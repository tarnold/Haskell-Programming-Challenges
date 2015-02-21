import Data.List.Split
import Data.List

-- type of card conversion --------------------------------------------

cardType :: [String] -> [Int]
cardType x  =   fmap (\y -> cardValue (head y)) x

cardValue :: Char -> Int
cardValue x 
        | x == '2'   =   2
        | x == '3'   =   3 
        | x == '4'   =   4
        | x == '5'   =   5
        | x == '6'   =   6
        | x == '7'   =   7
        | x == '8'   =   8
        | x == '9'   =   9
        | x == '1'   =   10
        | x == 'J'   =   11
        | x == 'Q'   =   12
        | x == 'K'   =   13
        | x == 'A'   =   14

cardSuit :: [String] -> [Int]
cardSuit x =   fmap (\y -> suitValue (last y)) x

suitValue :: Char -> Int
suitValue x
        | x == 'H'      =   1
        | x == 'D'      =   2
        | x == 'C'      =   3
        | x == 'S'      =   4

-- straight flush ------------------------------------------------

isStraightFlush :: [String] -> Int
isStraightFlush x
            | (isFlush $ cardSuit x) + (isStraight $ cardType x) > 1000    = 900 + (highCard $ cardType x)
            | otherwise                 = 0

-- flush ---------------------------------------------------------

isFlush :: [Int] -> Int
isFlush x
        | count (head x) x == 5     =   600 + (highCard x)
        | otherwise                                         =   0

-- high card --------------------------------------------------------

highCard :: [Int] -> Int
highCard x  =  maximum x


-- pair, 3 of a kind, 4 of a kind -------------------------------------------
count:: Int -> [Int] -> Int
count b [] = 0
count b (a:as) | b == a = 1+(count b as)
               | otherwise = count b as

countPairs :: [Int] -> Int
countPairs x = maximum $ fmap (\y -> count y x) x

pair :: [Int] -> Int
pair x 
        | countPairs x == 2     =   200 + (highCard x)
        | countPairs x == 3     =   400 + (highCard x)
        | countPairs x == 4     =   800 + (highCard x)
        | otherwise             =   0


-- straight ---------------------------------------------------------

inOrder :: [Int] -> Bool
inOrder [x]     = True
inOrder (x:xs)
        | x == ((head xs) - 1)  =   inOrder xs
        | otherwise             =   False

isStraight :: [Int] -> Int
isStraight x
            | inOrder $ sort x      = 500 + (highCard x)
            | otherwise             = 0


-- 2 pairs, full house ----------------------------------------------

countDifPairs :: [Int] -> [Int]
countDifPairs x = reverse $ sort $ fmap (\y -> count y x) x


twoPair :: [Int] -> Int
twoPair x
        | (head $ countDifPairs x) == 2 && (head $ tail $ countDifPairs x) == 2         =   300 + (highCard x)
        | otherwise         =   0


fullHouse :: [Int] -> Int
fullHouse x
        | (head $ countDifPairs x) == 3 && (head $ tail $ countDifPairs x) == 2         =   700 + (highCard x)
        | otherwise         =   0

-- runner ----------------------------------------------------------

getHandValue :: [String] -> Int
getHandValue x = do
                let typeOfCard  = cardType x
                let suitOfCard = cardSuit x
                maximum  $ (highCard typeOfCard) : (pair typeOfCard) : (twoPair typeOfCard) : (fullHouse typeOfCard) : (isStraight typeOfCard) : (isFlush suitOfCard) : (isStraightFlush x) : []

findWinner :: [[String]] -> String
findWinner [x,y]
        |   (getHandValue x) > (getHandValue y)         = "Black wins"
        |   (getHandValue x) == (getHandValue y)        = "Tie"
        |   otherwise                                   = "White wins"

calculate :: [[[String]]] -> [String]
calculate [x]       =   findWinner x : []
calculate (x:xs)    =   findWinner x : calculate xs


main :: IO ()
main = do
        hands <- fmap lines $ readFile "pokerhands.txt"
        let testcases = fmap (\x -> splitEvery 5 x) $ fmap words hands
        let result = calculate testcases
        writeFile "pokerhand_output.txt" $ unlines result

