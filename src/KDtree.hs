module KDtree where

---------------------------------------------------------

import Control.Applicative
import Data.Bits

import qualified Data.Foldable as F
import qualified Data.List     as L

import Vec3D

---------------------------------------------------------

data KDtree a = Node
                  { kdObject :: (a, Vec3D)
                  , kdAxis   :: Axis
                  , kdRight  :: KDtree a
                  , kdLeft   :: KDtree a
                  }
              | Leaf
                deriving (Show)

instance F.Foldable KDtree where
    foldl func i Leaf = i
    foldl func i (Node (obj, _) _ r l) =
        let a = F.foldl func i l
            b = func a obj
        in  F.foldl func b r

prettyPrint :: (Show a) => KDtree a -> String
prettyPrint Leaf = "Leaf\n"
prettyPrint (Node obj axis r l) = "Node {\n\t" ++ "Object: " ++ (show obj) ++ "\n\tAxis: " ++ (show axis) ++
    "\nLeft -\n" ++ (prettyPrint l) ++ "Right -\n" ++ (prettyPrint r) ++ "\n}\n"

---------------------------------------------------------

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

next :: Axis -> Axis
next Z = X
next x = succ x

coord :: Axis -> (Vec3D -> Float)
coord X = vX
coord Y = vY
coord Z = vZ

---------------------------------------------------------

insertList :: [(a, Vec3D)] -> KDtree a
insertList xs = insertList' xs X

insertList' :: [(a, Vec3D)] -> Axis -> KDtree a
insertList' [] _    = Leaf
insertList' xs axis = Node
                        { kdObject = m
                        , kdAxis   = axis
                        , kdRight  = insertList' r $ next axis
                        , kdLeft   = insertList' l $ next axis
                        }
    where sorted   = L.sortBy (\(_, a) (_, b) -> let c = coord axis in compare (c a) (c b)) xs
          (l, m:r) = L.splitAt (length sorted `div` 2) sorted

nearestNeighbor :: KDtree a -> Vec3D -> Maybe a
nearestNeighbor tree pos = fst <$> nearestNeighbor' tree pos

nearestNeighbor' :: KDtree a -> Vec3D -> Maybe (a, Float)
nearestNeighbor' Leaf _ = Nothing
nearestNeighbor' (Node (obj, pos) axis l r) pt =
    case candidate of
      Just (_, sd) -> if sd > (offset * offset)
                        then cmpPts candidate $ nearestNeighbor' other pt
                        else candidate
      Nothing      -> Nothing
    where offset       = coord axis pt - coord axis pos
          (sub, other) = if offset > 0 then (r, l) else (l, r)
          sqDist       = vSqLen $ vSub pos pt
          candidate    = cmpPts (Just (obj, sqDist)) (nearestNeighbor' sub pt)
          cmpPts (Just a) (Just b) = if snd a > snd b then Just b else Just a
          cmpPts (Just a) _        = Just a
          cmpPts _        (Just b) = Just b
          cmpPts _        _        = Nothing
