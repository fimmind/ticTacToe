module Main where

import Field
import System.Process (callCommand)
import Data.List (isSubsequenceOf)
import Debug.Trace
import System.IO (hFlush, stdout)

split :: Char -> String -> [String]
split ch str = 
  case rest of
    []     -> [chunk]
    _:rest -> chunk : split ch rest
  where (chunk, rest) = break (== ch) str

parceSell :: (Int, Int) -> String -> Maybe (Int, Int)
parceSell (sizeX, sizeY) line =
  case filter (not . null) $ split ' ' line of
    [strX, strY] 
      | any (not . null) [xs, ys] ->
        if x >= 0 && y >= 0 && x < sizeX && y < sizeY
          then Just (x, y)
          else Nothing
        where
          xs = reads strX
          ys = reads strY
          x  = fst (head xs) - 1
          y  = fst (head ys) - 1
    _ -> Nothing

putStr' :: String -> IO ()
putStr' (s:ss) = putChar s >> putStr' ss
putStr' _    = return ()

inputSell :: Field -> Char -> IO (Int, Int)
inputSell fld whoseTurn = do
  putStr $ whoseTurn : ": "
  hFlush stdout
  line <- getLine
  case parceSell (size fld) line of
    Just sell 
      | getSell sell fld == Just ' ' -> return sell
    _ -> do
      putStrLn "Wrong input"
      inputSell fld whoseTurn

isWinner :: Field -> Char -> Int -> Bool
isWinner fld who winnigLineLen =
  any (replicate winnigLineLen who `isSubsequenceOf`) $ 
    fldArr ++ 
      fldArr' ++
      leftDiogonals fldArr ++
      leftDiogonals fldArr'
    where 
      fldArr = fieldToArray fld
      fldArr' = rotate fldArr
      leftDiogonals arr = 
        tail $ concat $ map ((\(a, b) -> [a, b]) . unzip) $ do
          i <- [0 .. length arr - winnigLineLen]
          return $ do
            j <- [0 .. min (length arr) (length (head arr)) - 1 - i]
            return ( 
              arr !! (i + j) !! j, 
              arr !! j       !! (i + j) )
      rotate arr = do 
        i <- [0 .. length (head arr) - 1]
        return 
          [ arr !! j !! i 
          | j <- [(len - 1), (len - 2) .. 0]
          ] where len = length arr

nextTurn :: Field -> Char -> Int -> IO ()
nextTurn fld whoseTurn winnigLineLen
  | whoseTurn == 'X' || whoseTurn == 'O' 
  = if isFull fld
      then putStrLn "Draw!"
      else do
        sell <- inputSell fld whoseTurn
        let fld' = setSell sell whoseTurn fld
        callCommand "clear"
        print fld'
        if isWinner fld' whoseTurn winnigLineLen
          then putStrLn $ whoseTurn : "s won!"
          else nextTurn fld' (switch whoseTurn) winnigLineLen
            where
              switch 'X' = 'O'
              switch 'O' = 'X'

playGame :: (Int, Int) -> Int -> IO ()
playGame size@(sizeX, sizeY) winnigLineLen
  | sizeX >= winnigLineLen || sizeY >= winnigLineLen = do
    let fld = field size
    callCommand "clear"
    print fld
    nextTurn fld 'X' winnigLineLen

input :: Read a => String -> a -> IO a
input str defaultValue = do
  putStr str; hFlush stdout
  line <- getLine
  if null line || all (== ' ') line
    then return defaultValue
    else return $ read line

-- ╳႐
main = do
  width <- input "Field width (3): " 3 :: IO Int
  height <- input "Field height (3): " 3 :: IO Int
  winnigLineLen <- input "Winning line length (3): " 3 :: IO Int
  playGame (width, height) winnigLineLen
