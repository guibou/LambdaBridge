{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game (Event)
import Linear
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)

data NodeMass = Fixed | Mass Float
  deriving (Show, Eq)

data Node = Node
  { position :: V2 Float,
    mass :: NodeMass,
    velocity :: V2 Float
  }
  deriving (Show)

fixedNode p m = Node p m (V2 0 0)

data Edge = Edge Int Int
  deriving (Show)

data World = World
  { nodes :: Map Int Node,
    edges :: [Edge]
  }
  deriving (Show)

initWorld =
  World
    { nodes =
        Map.fromList
          [ (0, fixedNode (V2 0 0) Fixed),
            (1, fixedNode (V2 10 0) (Mass 1)),
            (2, fixedNode (V2 20 0) (Mass 1))
          ],
      edges =
        [ Edge 0 1,
          Edge 1 2 
        ]
    }

main :: IO ()
main = play (InWindow "LambdaBridge" (800, 600) (0, 0)) white 20 initWorld draw event step

draw :: World -> Picture
draw w =
  applyViewPortToPicture viewport $
    foldMap drawNode (Map.elems (nodes w))
      <> foldMap (drawEdge (nodes w)) (edges w)

event :: Event -> World -> World
event e w = traceShow e w

g = 9.8

z = V2 0 (-1)

k = 10

step :: Float -> World -> World
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
      (nodeId, Node p mass v) <- Map.toList (nodes w)

      -- Add springs
      let accel = case mass of
            Fixed -> 0
            Mass m ->
              let forces = m * g *^ z + (fromMaybe (V2 0 0) (Map.lookup nodeId forces_on_nodes))
               in forces ^/ m
      let v' = v + accel ^* dt
      let p' = p + v' ^* dt

      pure (nodeId, Node p' mass v')

drawNode (Node (V2 dx dy) mass velocity) = Translate dx dy $ Color (if mass == Fixed then red else black) $ Circle 1

drawEdge nodes (Edge nodeA nodeB) = line [readPoint nodeA, readPoint nodeB]
  where
    readPoint (position . (nodes !?) -> (V2 x y)) = (x, y)

viewport = viewPortInit {viewPortScale = 10}

(!?) :: HasCallStack => Map Int p -> Int -> p
m !? i = case Map.lookup i m of
  Just r -> r
  Nothing -> error $ "Cannot find key: " <> show i
