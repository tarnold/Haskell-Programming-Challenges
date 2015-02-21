import Data.List.Utils

findPermutation :: String -> String -> String
findPermutation "" word2                    = ""
findPermutation word1 word2
            | contains [head word1] word2   = [head word1] ++ findPermutation (tail word1) word2
            | otherwise                     = "" ++ findPermutation (tail word1) word2

calculate :: [String] -> [String]
calculate []     = []
calculate (x:xs) = findPermutation x (head xs) : calculate (tail xs) 

main :: IO ()
main = do
        input <- fmap lines $ readFile "commonPermutation_input.txt"
        let solution = calculate input
        writeFile "commonPermutation_output.txt" $ unlines solution
