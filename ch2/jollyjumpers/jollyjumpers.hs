calculate :: [[Int]] -> [Bool]
calculate x         = do
                        let jolly = fmap buildJolly x
                        fmap (\x -> isJolly ((length x) - 1) x) jolly

buildJolly :: [Int] -> [Int]
buildJolly [x]          =   []
buildJolly (x:xs)       =   abs (x - (head xs)) : buildJolly xs 

isJolly :: Int -> [Int] -> Bool
isJolly num x
            | num == 0              =   True
            | num `elem` x          =   isJolly (num-1) x
            | otherwise             =   False
        

main :: IO ()
main = do
            jolly <- fmap lines $ readFile "jollyjumpers.txt"
            let testcases = fmap (\x -> fmap (\x -> read x :: Int) (words x)) jolly
            let answer = calculate testcases
            writeFile "jolly_output.txt" $ unlines (map show answer)
