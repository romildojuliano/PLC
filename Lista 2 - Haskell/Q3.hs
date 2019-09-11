processBankOperations :: [Double] -> [(Int, Int, Int, Double)] -> [Double]
processBankOperations [] _ = []
processBankOperations accounts [] = accounts
processBankOperations (head:tail) (opCode, contaOrigem, contaDestino, valor) | opCode == 0 = processBankOperations (accessAccount (head:tail) contaDestino valor) (opCode, contaOrigem, contaDestino, valor)
                                                                             | opCode == 1 = processBankOperations (debitaValor (head:tail) contaDestino valor) (opCode, contaOrigem, contaDestino, valor) 
                                                                             | opCode == 2 = processBankOperations (accessAccount (debitaValor (head:tail) contaOrigem valor) contaDestino valor) (opCode, contaOrigem, contaDestino, valor)

accessAccount :: [Double] -> Int -> Double -> [Double]
accessAccount [] _ _ = []
accessAccount (head:tail) 0 value = (value + head) : tail
accessAccount (head:tail) posix value = head : accessAccount tail (posix-1) value

-- changeValue :: [Double] -> Int -> [Double]

debitaValor :: [Double] -> Int -> Double -> [Double]
debitaValor [] _ _ = []
debitaValor (head:tail) 0 value = if (head - value) > 0 then (head - value) : tail else head : tail
debitaValor (head:tail) posix value = head : debitaValor tail (posix-1) value
