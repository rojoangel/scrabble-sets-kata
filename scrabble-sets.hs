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

count_tile :: Char -> [Char] -> Int
count_tile a xs = sum [1 | x <- xs, a == x]
