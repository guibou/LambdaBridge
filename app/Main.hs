{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (SpecialKey), SpecialKey (KeySpace))
import Linear
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)
import GHC.Float

data NodeMass = Fixed | Mass Double
  deriving (Show, Eq)

data Node = Node
  { position :: V2 Double,
    position' :: V2 Double,
    mass :: NodeMass
  }
  deriving (Show)

fixedNode p m = Node p p m

data Edge = Edge Int Int
  deriving (Show)

data World = World
  { nodes :: Map Int Node,
    edges :: [Edge]
  }
  deriving (Show)


h = sqrt 75

initWorld =
  World
    { 
      nodes =
        Map.fromList
          [ (0, fixedNode (V2 (-20) 0) Fixed),
            (1, fixedNode (V2 (-10) 0) (Mass 1)),
            (2, fixedNode (V2 0 0) (Mass 1)),
            (3, fixedNode (V2 10 0) (Mass 1)),
            (4, fixedNode (V2 20 0) Fixed),

            (5, fixedNode (V2 (-15) (-h)) (Mass 1)),
            (6, fixedNode (V2 (-5) (-h)) (Mass 1)),
            (7, fixedNode (V2 5 (-h)) (Mass 1)),
            (8, fixedNode (V2 15 (-h)) (Mass 1)),
            (9, fixedNode (V2 25 (-h)) (Mass 1))
          ],
      edges =
        [ 
          Edge 0 1,
          Edge 1 2,
          Edge 2 3,
          Edge 3 4,

          -- ZigZag shape
          Edge 0 5,
          Edge 1 5,
          Edge 1 6,
          Edge 2 6,
          Edge 2 7,
          Edge 3 7,
          Edge 3 8,
          Edge 4 8,

          -- Top
          Edge 5 6,
          Edge 6 7,
          Edge 7 8,

          -- Dangling
          Edge 8 9 
        ]
    }

g = 50

main :: IO ()
main = play (InWindow "LambdaBridge" (800, 600) (0, 0)) white 20 initWorld draw event (step' 10)

draw :: World -> Picture
draw w =
  applyViewPortToPicture viewport $
    foldMap drawNode (Map.elems (nodes w))
      <> foldMap (drawEdge (nodes w)) (edges w)

event :: Event -> World -> World
event (EventKey (SpecialKey KeySpace) _ _ _) w = w { edges = drop 1 (edges w) }
event e w = w

z = V2 0 (-1)

k = 10000
kDamp = 1

applyN 0 _ v = v
applyN n f v = applyN (n - 1) f (f v)

step' n (float2Double -> dt) = applyN n (step (dt / n))

step :: Double -> World -> World
step dt w = w {nodes = nodes'}
  where
    forces_on_nodes = Map.fromListWith (+) $ do
      Edge nodeI nodeJ <- edges w
      let dirIJ = position ((nodes w) !? nodeJ) - position (nodes w !? nodeI)

      guard $ dirIJ /= V2 0 0

      let restLength = 10
      let currentLength = norm dirIJ
      let f = k * normalize dirIJ ^* (currentLength - restLength)
      [(nodeI, f), (nodeJ, - f)]

    nodes' = Map.fromList $ do
      (nodeId, Node p pPrev m) <- Map.toList (nodes w)

      -- Add springs
      let accel = case m of
            Fixed -> 0
            Mass m ->
              let spring = (fromMaybe (V2 0 0) (Map.lookup nodeId forces_on_nodes))
                  damping = -kDamp * v
                  gravity = m * g *^ z
                  v = (p - pPrev) ^/ dt
               in (spring + damping + gravity) ^/ m
      let pPrev' = p
          p' = 2 * p - pPrev + accel ^* (dt * dt)

      pure (nodeId, Node p' pPrev' m)

drawNode (Node (V2 dx dy) _ mass) = Translate (double2Float dx) (double2Float dy) $ Color (if mass == Fixed then red else black) $ Circle 1

drawEdge nodes (Edge nodeA nodeB) = line [readPoint nodeA, readPoint nodeB]
  where
    readPoint (position . (nodes !?) -> (V2 x y)) = (double2Float x, double2Float y)

viewport = viewPortInit {viewPortScale = 10}

(!?) :: HasCallStack => Map Int p -> Int -> p
m !? i = case Map.lookup i m of
  Just r -> r
  Nothing -> error $ "Cannot find key: " <> show i
