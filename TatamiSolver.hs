module TatamiSolver where

import Data.Char (isLetter)

-- these two function are to correctly measure the width of an entry of a grid, 
-- i.e. so that the width of "\ESC[44m55\ESC[0m" ignored the escape sequences
stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


instance (Show a) => Show (GridWithAPointer a) where
     show (GridWithAPointer(top, left, focus, right, bottom)) = show top ++ unwords(map show (reverse left)) ++ " " ++ "\ESC[44m" ++ show focus ++ "\ESC[0m" ++ " " ++ unwords(map show right) ++ "\n" ++ show bottom


put :: a -> GridWithAPointer a -> GridWithAPointer a
put focus (GridWithAPointer(top, left, _, right, bottom)) = GridWithAPointer(top, left, focus, right, bottom)

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer(top, x:xs, focus, right, bottom)) = GridWithAPointer(top, xs, x, focus:right, bottom)


moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer(top, left, focus, x:xs, bottom)) = GridWithAPointer(top, focus:left, x, xs, bottom)
moveRight (GridWithAPointer(top, left, focus, [], bottom)) = GridWithAPointer(top, left, focus, [], bottom)

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer(top, left, focus, right, bottom)) = let column = length left; (Grid rows) = top; pivot = rowAbove !! column; rowAbove = last rows; newTop = Grid (init rows); (Grid rowsBottom) = bottom; newRowsBottom = [reverse(left) ++ [focus] ++ right] ++ rowsBottom; in (GridWithAPointer(newTop, reverse(take column rowAbove), pivot, drop (column + 1) rowAbove, Grid(newRowsBottom)))

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer(top, left, focus, right, bottom))  = let (Grid newTop) = top; (Grid rows) = bottom; column = length left; rowBelow = head rows; position = rowBelow !! column; newBottom = Grid (tail rows); newTopRows = Grid (newTop ++ [reverse left ++ [focus] ++ right]); in (GridWithAPointer( newTopRows, reverse (take column rowBelow), position, drop (column + 1) rowBelow, newBottom))


putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp x (GridWithAPointer(top, left, _, right, bottom)) = put x (moveUp(GridWithAPointer(top, left, x, right, bottom)))

putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown x (GridWithAPointer(top, left, _, right, bottom)) = put x (moveDown(GridWithAPointer(top, left, x, right, bottom)))

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight x (GridWithAPointer(top, left, _, right, bottom)) = put x (moveRight(GridWithAPointer(top, left, x, right, bottom)))

putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft x (GridWithAPointer(top, left, _, right, bottom)) = put x (moveLeft(GridWithAPointer(top, left, x, right, bottom)))


cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover grid@(GridWithAPointer(Grid top, left, focus, right, Grid bottom)) = 
  if (rows * cols) `mod` 2 == 0 then 
    case solve (gridToList grid) 1 of
      Just result -> fromListToGrid result
      Nothing -> error "no valid tatami coverage"
  else emptyGrid
  where 
    rows = length top + 1 + length bottom
    cols = length left + 1 + length right
    
    solve grid num =
      case findEmpty grid of
        Nothing -> if hasAnyDifferentCorners grid then Nothing else Just grid
        Just (r,c) ->
          let 
              horizontal = if c+1 < cols && grid !! r !! (c + 1) == 0
                     then let g = setCell (setCell grid r c num) r (c + 1) num
                          in if hasAnyDifferentCorners g then Nothing else solve g (num + 1)
                     else Nothing
              vertical = if r + 1 < rows && grid !! (r + 1) !! c == 0
                     then let g = setCell (setCell grid r c num) (r + 1) c num
                          in if hasAnyDifferentCorners g then Nothing else solve g (num + 1)
                     else Nothing
          in case horizontal of
               Just result -> Just result
               Nothing -> vertical
    
    emptyGrid = GridWithAPointer (Grid [[]], [], 0, [], Grid [[]])
    
    has4DifferentTatamis grid r c = 
      r > 0 && r < rows && c > 0 && c < cols &&
      let r1 = grid !! (r - 1) !! (c - 1)
          r2 = grid !! r !! (c - 1)
          c1 = grid !! (r - 1) !! c
          c2 = grid !! r !! c
      in all (> 0) [r1, r2, c1, c2] && r1 /= r2 && r1 /= c1 && r1 /= c2 && r2 /= c1 && r2 /= c2 && c1 /= c2

    hasAnyDifferentCorners g = 
      any (\r -> any (\c -> has4DifferentTatamis g r c) [1..cols-1]) [1..rows-1]
    
    findEmpty grid =
      case [(r,c) | r <- [0..rows-1], c <- [0..cols-1], grid !! r !! c == 0] of
        [] -> Nothing
        (pos:_) -> Just pos
        
    gridToList (GridWithAPointer(Grid t, l, f, ri, Grid b)) = 
      t ++ [l ++ [f] ++ ri] ++ b

    fromListToGrid [] = error "Empty grid"
    fromListToGrid (firstRow:restRows) =
      case firstRow of
        (f:r) -> GridWithAPointer (Grid [], [], f, r, Grid restRows)
        [] -> error "Empty row"

    setCell grid r c val =
      take r grid ++
      [take c (grid !! r) ++ [val] ++ drop (c+1) (grid !! r)] ++
      drop (r+1) grid

    
