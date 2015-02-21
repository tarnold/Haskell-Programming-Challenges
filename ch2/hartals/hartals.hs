import Data.Set
import System.IO

compute :: [Int] -> [Int]
compute hartals = do
                let numTests = head hartals
                numHartals (tail hartals)


numHartals :: [Int] -> [Int]
numHartals [] = []
numHartals (x:xs) = (calcHartals x (drop 1 (take ((head xs) + 1) xs))) : numHartals (drop ((head xs) + 1) xs)


calcHartals :: Int -> [Int] -> Int
calcHartals x xs = do
                        let hartalSet = createSet x xs
                        length (Data.Set.toList hartalSet)


createSet :: Int -> [Int] -> Set Int
createSet days [x] = Data.Set.fromList (hartalDays days x)
createSet days (x:xs) = union (Data.Set.fromList (hartalDays days x)) (createSet days xs)


hartalDays :: Int -> Int -> [Int] 
hartalDays days hartals = [x | x <- [1..days], x `mod` hartals == 0 && not (friday x) && not (saturday x)]


saturday :: Int -> Bool
saturday a = a `mod` 7 == 0


friday :: Int -> Bool
friday a = (a + 1) `mod` 7 == 0


main :: IO ()
main = do
            hartals <- fmap lines (readFile "hartals.txt")
            let hData = compute (Prelude.map (\x -> read x :: Int) hartals)
            writeFile "hartals_output.txt" $ unlines (Prelude.map show hData) 
