module Main where
import Data.Char
import Data.List

validChars:: String
validChars = ['a'..'z']

letter2digit :: Char -> Int
letter2digit x =  case elemIndex (toLower x) validChars of
  Just x -> x
  Nothing -> 0
  

isScarry :: String -> Bool
isScarry str = (sum $ fmap letter2digit str) == 13

getScaryWords :: [String] -> [String]
getScaryWords = filter isScarry 
  

main :: IO ()
-- main = undefined
-- main = do
--   f <- readFile "/usr/share/dict/words"
--   putStrLn $ unlines( getScaryWords $ lines f)

-- main =
--   readFile "/usr/share/dict/words" >>= 
--   IO String -> IO [String] -> IO [String] -> IO String 
main =
  readFile "/usr/share/dict/words" >>=
  putStrLn.unlines.getScaryWords.lines
