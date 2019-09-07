
--Aluno: Romildo Juliano de Almeida Lira

--Caso geral
decrypt :: String -> [(Char, Char)] -> String
decrypt [] _ = "" -- Se for dada uma lista vazia, não retorna valor algum
decrypt (headWord:tailWord) list = auxDecrypt headWord list : decrypt tailWord list-- Compara o primeiro caractere com a lista de tuplas,
-- E continua comparando todos os caracteres com a lista de tuplas até achar a string vazia


auxDecrypt :: Char -> [(Char, Char)] -> Char
auxDecrypt _ [] =  ' ' -- Se o caractere for comparado com o fim da lista, retorna nada
auxDecrypt char ((a, b):tail) | (char == a) = b -- Se o caractere for o da posição (-> a <- , b), retorna o B
                              | (char == b) = a -- Se o caractere for o da posição (a, -> b <-), retorna o A
                              | otherwise = auxDecrypt char tail -- Caso contrário procura com o resto da lista

-- Caso específico 
decrypt' :: String -> [(Char, Char)] -> String
decrypt' [] _ = ""
decrypt' (headString:tailString) ((first, second):tailList)= auxDecrypt' headString (first, second) : decrypt' tailString tailList 

auxDecrypt' :: Char -> (Char, Char) -> Char
auxDecrypt' char (first, second) = (if char == first then second else char)