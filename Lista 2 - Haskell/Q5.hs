type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa [("Divide", 0)] = -666
executa ((command, value) : tail) = aux ((command, value):tail) 0
    where aux [] n = n
          aux ((command, value):tail) x | (command == "Divide") = if value /= 0 then aux tail (x `div` value) else -666
                                        | (command == "Multiplica") = aux tail (x * value)
                                        | (command == "Soma") = aux tail (x + value)
                                        | (command == "Subtrai") = aux tail (x - value)
                                        | otherwise = -666



