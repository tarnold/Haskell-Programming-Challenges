decode :: Char -> Char
decode x
        | x == '1'   =   '`'
        | x == '2'   =   '1'
        | x == '3'   =   '2'
        | x == '4'   =   '3'
        | x == '5'   =   '4'
        | x == '6'   =   '5'
        | x == '7'   =   '6'
        | x == '8'   =   '7'
        | x == '9'   =   '8'
        | x == '0'   =   '9'
        | x == '-'   =   '0'
        | x == '='   =   '-'
        | x == 'w'   =   'q'
        | x == 'e'   =   'w'
        | x == 'r'   =   'e'
        | x == 't'   =   'r'
        | x == 'y'   =   't'
        | x == 'u'   =   'y'
        | x == 'i'   =   'u'
        | x == 'o'   =   'i'
        | x == 'p'   =   'o'
        | x == '['   =   'p'
        | x == ']'   =   '['
        | x == '\\'  =   ']'
        | x == 's'   =   'a'
        | x == 'd'   =   's'
        | x == 'f'   =   'd'
        | x == 'g'   =   'f'
        | x == 'h'   =   'g'
        | x == 'j'   =   'h'
        | x == 'k'   =   'j'
        | x == 'l'   =   'k'
        | x == ';'   =   'l'
        | x == '\''  =   ';'
        | x == 'x'   =   'z'
        | x == 'c'   =   'x'
        | x == 'v'   =   'c'
        | x == 'b'   =   'v'
        | x == 'n'   =   'b'
        | x == 'm'   =   'n'
        | x == ','   =   'm'
        | x == '.'   =   ','
        | x == '/'   =   '.'
        | otherwise  =   ' '
        
calculate :: [String] -> [String]
calculate [x]       =   fmap decode x : []
calculate (x:xs)    =   fmap decode x : calculate xs

main :: IO ()
main = do
            message <- fmap lines $ readFile "WERTYU_input.txt"
            let solution = calculate message
            writeFile "WERTYU_output.txt" $ unlines solution
