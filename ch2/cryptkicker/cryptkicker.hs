import System.IO
import Data.List
import Data.List.Utils
import Data.Char

nextLetter :: Char -> Char
nextLetter ' '  = ' '
nextLetter 'z'  = 'a'
nextLetter c    = chr (ord c + 1)

decode :: [[Char]] -> [Char] -> [Char]
decode language message
            | not . elem False $ map (\x -> contains x message) language = message
            | otherwise = decode language $ map nextLetter message
                     

main :: IO ()
main = do
            crypt <- fmap lines (readFile "cryptkicker.txt")
            let numWords = read (head crypt) :: Int
            let language = take numWords $ tail crypt
            let message = intercalate " " $ drop numWords $ tail crypt
            print language
            print message
            print (decode language message) 
