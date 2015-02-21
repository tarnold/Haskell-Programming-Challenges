import Data.Char

-- build data -------------------------------------------------------------

createTable :: [String] -> [String]
createTable x   =   fmap (\x -> fmap toLower x) $ take (digitToInt $ head $ head x) (tail x)

getWords :: [String] -> [String]
getWords x   =  tail $ drop (digitToInt $ head $ head x) (tail x)


-- search directions ------------------------------------------------------



-- algorithm --------------------------------------------------------------

findWords :: [String] -> [String] -> [[String]]
findWords searchWords table     =   traverseTable 0 searchWords table

traverseTable :: Int -> [String] -> [String] -> [[String]]
traverseTable rowNum searchWords table
                    | rowNum+1 == length table   = searchRow rowNum searchWords x : []
                    | otherwise             = searchRow rowNum searchWords x : traverseTable (rowNum+1) searchWords table

searchRow :: Int -> [String] -> [String] -> [String]
searchRow rowNum searchWords table = searchChars rowNum 0 searchWords table

searchChars :: Int -> Int -> [String] -> [String] -> [String]
searchChars rowNum charNum searchWords table
                | charNum+1 == length table!!rowNum = isMatch rowNum charNum searchWords table : []
                | otherwise = isMatch rowNum charNum searchWords table : searchChars rowNum charNum searchWords table

isMatch :: Int -> Int -> [String] -> [String] -> String
isMatch rowNum charNum searchWords table 
                | wordFound rowNum charNum searchWords table    = (show rowNum) ++ " " ++ (show charNum)
                | otherwise                                     = ""

wordFound :: Int -> Int -> [String] -> [String] -> Bool
wordFound rowNum charNum [] table = False
wordFound rowNum charNum (x:xs) table
                    | foundIt rowNum charNum x table    = True
                    | otherwise                         = foundIt rowNum charNum xs table

foundIt :: Int -> Int -> String -> [String] -> Bool
foundIt rowNum charNum word table
            | straightUp rowNum charNum word table || upperRightDiag rowNum charNum word table || straightRight rowNum charNum word table || bottomRightDiag rowNum charNum word table || straightDown rowNum charNum word table || bottomLeftDiag rowNum charNum word table || straightLeft rowNum charNum word table || upperLeftDiag rowNum charNum word table   = True
            | otherwise                         = False
-- runner -----------------------------------------------------------------

calculate :: [String] -> [[String]]
calculate x = do
                let table = createTable x
                let searchWords = getWords x 
                findWords searchWords table

main :: IO ()
main = do
            input <- fmap lines $ readFile "wheresWaldorf_input.txt"
            let solution = calculate input
            print solution
