logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0,0)
minMaxCartao str = (minimum ((setValues . words . isDigit) logCartao), maximum ((setValues . words . isDigit) logCartao)) 

setValues :: [String] -> [Double]
setValues [] = []
setValues (dia:valor:tail) = (read valor :: Double) : setValues tail

isDigit :: String -> String
isDigit [] = ""
isDigit (head:tail) | (head >= '0' && head <= '9') || head == '.' = head : isDigit tail
                    | head == ';' = aux head : isDigit tail
                    | otherwise = isDigit tail
    where aux x = if x ==  ';' then ' ' else x

filterSpaces :: String -> String
filterSpaces [] = []
filterSpaces (head:tail) = if head == ';' then ' ' : filterSpaces tail else head : filterSpaces tail

