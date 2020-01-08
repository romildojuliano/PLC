escreva :: String -> Int -> Char -> String
escreva [] _ char = char:[]
escreva str 0 char = char:str
escreva str n char = reverse(char:(reverse(take n str)))++(drop n str) 