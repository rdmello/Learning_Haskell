main = interact $ unlines . filter ((<10).length) . lines

-- shortLinesOnly :: String -> String
-- shortLinesOnly input = 
--   let allLines = lines input
--       shortLines = filter (\x -> length x < 10) allLines
--       result = unlines shortLines
--   in result
