import Data.List (nub, sort)

main = interact (unlines . sort . nub . lines)
