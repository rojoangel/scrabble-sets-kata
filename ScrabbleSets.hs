module ScrabbleSets
(tiles_left_in_bag
) where

import Data.List (groupBy, sort)

tiles :: [Char]
tiles = '_' : ['A'..'Z']

tile_count :: Char -> Int
tile_count '_' = 2
tile_count 'A' = 9
tile_count 'B' = 2
tile_count 'C' = 2
tile_count 'D' = 4
tile_count 'E' = 12
tile_count 'F' = 2
tile_count 'G' = 3
tile_count 'H' = 2
tile_count 'I' = 9
tile_count 'J' = 1
tile_count 'K' = 1
tile_count 'L' = 4
tile_count 'M' = 2
tile_count 'N' = 6
tile_count 'O' = 8
tile_count 'P' = 2
tile_count 'Q' = 1
tile_count 'R' = 6
tile_count 'S' = 4
tile_count 'T' = 6
tile_count 'U' = 4
tile_count 'V' = 2
tile_count 'W' = 2
tile_count 'X' = 1
tile_count 'Y' = 2
tile_count 'Z' = 1
tile_count _ = 0

left_tiles :: [Char] -> [(Int, Char)]
left_tiles ts = [(left_tile t ts, t) | t <- tiles]
    where count_tile a xs = sum [1 | x <- xs, a == x]
          left_tile t ts  = (tile_count t - count_tile t ts)

group_left_tiles_by_count :: [(Int, Char)] -> [[(Int, Char)]]
group_left_tiles_by_count ts = groupBy (\a b -> fst a == fst b) $ sort ts

check_for_errors:: [[(Int, Char)]] -> [[(Int, Char)]]
check_for_errors ts
  | (< 0) headOfHeadCount = error $ "Invalid input. More " ++
                                    [headOfHeadLetter] ++
                                    "'s have been taken from the bag than possible."
  | otherwise             = ts
  where headOfHeadCount  = fst . head $ head ts
        headOfHeadLetter = snd . head $ head ts

compact_groups :: [[(Int, Char)]] -> [(Int, [Char])]
compact_groups ts = reverse $ map (foldl (\ (_, accB) (a, b) -> (a, accB ++ [b])) (0, "")) ts

tiles_left_in_bag :: [Char] -> [(Int, [Char])]
tiles_left_in_bag = compact_groups . check_for_errors . group_left_tiles_by_count . left_tiles
