isPalindromo :: String -> Bool 
isPalindromo [] = True
isPalindromo str = (str == reverse str)