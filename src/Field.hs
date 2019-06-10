module Field (
Field,
field,
fieldToArray,
setSell,
getSell,
sizeX,
sizeY,
size,
isFull
) where


newtype Field = Field [[Char]]

 
-- ┘┐┤┼├┌└ │─ ┬ ┴
instance Show Field where
  show fld = showLine "┌┬┐" len ++ '\n' : show' fld ++ showLine "└┴┘" len
    where
      len = sizeX fld

      show' (Field [xs]) = showRow xs
      show' (Field (xs:ys)) =
        showRow xs ++ showLine "├┼┤" (length xs) ++ '\n' : show' (Field ys)

      showRow fs = "│" ++ showRow' fs ++  "│\n"
        where
          showRow' [x] = [' ', x, ' ']
          showRow' (x:xs) = showRow' [x] ++ "│" ++ showRow' xs

      showLine [left, splitter, right] size = left : showLine' [splitter, right] size
        where
          showLine'    [_       , right] 1    = "───" ++ [right]
          showLine' sr@[splitter, right] size = "───" ++ splitter : showLine' sr (size - 1)


type Size = Int

field :: (Size, Size) -> Field
field (x, y)
  | x > 1 && y > 1 = Field $ replicate y $ replicate x ' '
  | otherwise = error "Field size mast be at least 2 by 2"

fieldToArray :: Field -> [[Char]]
fieldToArray (Field fld) = fld

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = before ++ x : after
  where (before, _:after) = splitAt i xs

setSell :: (Int, Int) -> Char -> Field -> Field
setSell (x, y) ch (Field field) = 
  Field $ setAt field y $ setAt (field !! y) x ch

getSell :: (Int, Int) -> Field -> Maybe Char
getSell (x, y) (Field f) =
  if x < length (head f) &&
    y < length f &&
    x >= 0 &&
    y >= 0
      then Just $ f !! y !! x
      else Nothing

sizeX :: Field -> Size
sizeX (Field (xs:_)) = length xs
sizeX _              = 0

sizeY :: Field -> Size
sizeY (Field (xs:xss)) = (1 +) $ sizeY $ Field xss
sizeY _                = 0

size :: Field -> (Size, Size)
size fld = (sizeX fld, sizeY fld)


isFull :: Field -> Bool
isFull (Field fld) = and $ map (all (/=' ')) fld
