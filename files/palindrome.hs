main = interact checkPalindrome

checkPalindrome :: String -> String
checkPalindrome = unlines . map (\x -> if (x==reverse x) then "Palindrome" else "NOT") . lines

