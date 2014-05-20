module KDtree where

---------------------------------------------------------

import Control.Applicative
import Data.Bits

import qualified Data.List as L

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

---------------------------------------------------------

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

next :: Axis -> Axis
next Z = X
next x = succ x

prev :: Axis -> Axis
prev X = Z
prev x = pred x

cood :: Axis -> (Vec3D -> Float)
cood X = vX
cood Y = vY
cood Z = vZ

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
          (r, m:l) = L.splitAt (length sorted `div` 2) sorted
