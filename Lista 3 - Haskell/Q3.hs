data Comando = ParaFrente Int
      | ParaTras Int
      | Escreva Char
      deriving (Show, Eq)


interprete :: String -> [Comando] -> Char
interprete (hd:tl) [] = hd
interprete str commands = aux 0 str commands
    where aux n _ _ | (n < 0) = '0' 
          aux n [] commands = '0'
          aux n (headstring:tailstring) [] = headstring
          aux n str [command] | command == (ParaFrente val) = drop val str
                              | command == (ParaTras val) = take val str
                              | otherwise = '0'
          aux n str (headcommand:tailcommand) | headcommand == (ParaTras val)= aux (n + (ParaTras val)) str tailcommand
                                              | headcommand == (ParaFrente val) = aux (n - (ParaFrente val)) str tailcommand
                                              | headcommand == (Escreva char) = aux n (escreva str n (Escreva char)) tailcommand
                                              | otherwise = '0'


escreva :: String -> Int -> Char -> String
escreva [] _ char = char:[]
escreva str 0 char = char:str
escreva str n char = reverse(char:(reverse(take n str)))++(drop n str) -- Zoeira



