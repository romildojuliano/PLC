logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"
logMes :: String -> String -> Double
logMes [] _ = 0
logMes _ [] = 0
logMes mes logCartao = searchFatura mes ((makeFatura . words . filterSpaces) logCartao)

searchFatura :: String -> [(Int, String, String, Double)] -> Double
searchFatura [] _ = 0
searchFatura _ [] = 0
searchFatura str ((dia, mes, empresa, valor):tail) = if str == mes then valor + searchFatura str tail else searchFatura str tail

makeFatura :: [String] -> [(Int, String, String, Double)]
makeFatura [] = []
makeFatura (dia:mes:empresa:valor:tail) = (read dia :: Int , mes, empresa, read valor :: Double) : makeFatura tail

isDigit :: String -> String
isDigit [] = ""
isDigit (head:tail) | (head >= '0' && head <= '9') || head == '.' = head : isDigit tail
                    | head == ';' = aux head : isDigit tail
                    | otherwise = isDigit tail
    where aux x = if x ==  ';' then ' ' else x

isLetter :: String -> String
isLetter [] = ""
isLetter (head:tail) = if (head >= '0' && head <= '9') || head == ';' || head == '.' then isLetter tail else head : isLetter tail 

filterSpaces :: String -> String
filterSpaces [] = []
filterSpaces (head:tail) = if head == ';' then ' ' : filterSpaces tail else head : filterSpaces tail

