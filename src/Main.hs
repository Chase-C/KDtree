module Main (main) where

--------------------------------------------------------------------------------

import Data.Maybe
import qualified KDtree as KD
import Vec3D

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let xs = (0, Vec3D ( 1,  1,  1))
           : (1, Vec3D (-1,  1,  1))
           : (2, Vec3D ( 1, -1,  1))
           : (3, Vec3D ( 1,  1, -1))
           : (4, Vec3D (-1, -1,  1))
           : (5, Vec3D ( 1, -1, -1))
           : (6, Vec3D (-1,  1, -1))
           : (7, Vec3D (-1, -1, -1))
           : (8, Vec3D ( 5,  1,  1))
           : (9, Vec3D ( 1,  5, -5))
           : [] :: [(Int, Vec3D)]
        tree = KD.fromList xs

    putStrLn $ KD.prettyPrint tree
    putStrLn $ show $ KD.toList tree
    putStrLn $ show $ KD.kNearestNeighbors tree (Vec3D (5, 5, 2)) 5
