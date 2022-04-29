{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event (..))
import Linear (V2 (..), (*^))
import Linear.Metric (normalize)
import Linear (Metric(distance))
import Debug.Trace
import Data.List (delete)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

-- Drawing UI

main :: IO ()
main =
  play
    ( InWindow
        "LambdaBridgeUI"
        (1280, 1024)
        (0, 0)
    )
    white
    -- Simulation frame rate
    20
    initWorld
    draw
    event
    stepWithSubSteps

draw :: World -> Picture
draw world =
  Scale 2 2 $
    foldMap (\node -> drawNode node) world.nodes
      <> foldMap edgeDisplay world.edges
  where
    edgeDisplay (Edge nodeA nodeB restLength) = drawEdge posA posB coef
      where
        posA = getNodePosition nodeA
        posB = getNodePosition nodeB
        coef = (distance posA posB - restLength)
    getNodePosition nodeIndex = case Map.lookup nodeIndex world.nodes of
      Nothing -> error "Your bridge is insane"
      Just node -> node.position

drawNode (Node (V2 x y) _velocity state) = Translate x y image
  where
    image = case state of
      Mobile -> ThickCircle 10 2
      Fixed -> Color red (Polygon [(-10, -10), (10, -10), (10, 10), (-10, 10)])

-- drawEdge (V2 x y) (V2 x' y') = Line [(x, y), (x', y')]
drawEdge p0@(V2 x y) p1@(V2 x' y') coef = do
  let V2 radius angle = carthesianToPolar (normalize (p0 - p1))
      normalDirection = polarToCarthesian (V2 radius (angle + pi / 2))
      forceColor = mixColors (abs coef ) 1 (mixColors ((max 0 coef)) (abs (min 0 coef)) red blue) black
  Color forceColor $ Polygon
    ( map
        toPoint
        [ V2 x y + lineThickness * normalDirection,
          V2 x y - lineThickness * normalDirection,
          V2 x' y' - lineThickness * normalDirection,
          V2 x' y' + lineThickness * normalDirection
        ]
    )
  <> (Color red $ Translate ((x' + x) / 2) ((y' + y) / 2) (Scale 0.1 0.1 $ Text (show (truncate (coef * 10)))))

carthesianToPolar (V2 x y) = (V2 (sqrt (x * x + y * y)) (atan2 y x))

polarToCarthesian (V2 radius angle) = V2 (radius * cos angle) (radius * sin angle)

--
-- (x, y)   -> (r, theta)

lineThickness = 2

toPoint (V2 x y) = (x, y)

event :: Event -> World -> World
event (EventKey _ _ _ (x, y)) world = world { edges = delete closestEdge (world.edges) }
  where
    closestEdge = snd $ minimumBy (comparing fst) $ do
           e <- world.edges
           let Just nodeA = Map.lookup e.nodeA world.nodes
           let Just nodeB = Map.lookup e.nodeB world.nodes
           let center = (nodeA.position + nodeB.position) / 2
           pure (distance center (V2 x y), e)
event _event world = world

-- Bridge

data World = Bridge
  { edges :: [Edge],
    nodes :: Map Int Node
  }

data Edge = Edge
  { nodeA :: Int,
    nodeB :: Int,
    restLength :: Float
  }
  deriving (Eq)

data NodeState = Fixed | Mobile

data Node = Node
  { position :: V2 Float,
    velocity :: V2 Float,
    state :: NodeState
  }

exampleNode = Node (V2 0 0)

mkNode position = Node position (V2 0 0) Mobile

initWorld :: World
initWorld =
  Bridge
    { nodes = nodes,
      edges =
        [mkEdge i (i + 1) | i <- [0 .. 5] <> [7 .. 11]]
          <> [mkEdge i (i + 7) | i <- [0 .. 5]]
          <> [mkEdge (i + 7) (i + 1) | i <- [0 .. 5]]
          <> [mkEdge 7 13]
    }
      where
        nodes = Map.fromList $
          zip
            [0 ..]
            [ -- down layer
              (mkNode (V2 -150 0)),
              mkNode (V2 -100 0),
              mkNode (V2 -50 0),
              mkNode (V2 0 0),
              mkNode (V2 50 0),
              mkNode (V2 100 0),
              (mkNode (V2 150 0)),
              -- Top layer
              (mkNode (V2 -125 50)) {state = Fixed},
              mkNode (V2 -75 50),
              mkNode (V2 -25 50),
              mkNode (V2 25 50),
              mkNode (V2 75 50),
              (mkNode (V2 125 50)) {state = Fixed},
              -- Dangling node
              mkNode (V2 -20 -20)
            ]
        mkEdge nodeAIdx nodeBIdx = Edge nodeAIdx nodeBIdx restLength
          where
            restLength = do
              let Just nodeA = Map.lookup nodeAIdx nodes
              let Just nodeB = Map.lookup nodeBIdx nodes
               in distance nodeA.position nodeB.position

stepWithSubSteps dt w = applyN 20 (step (dt / 20)) w

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ w = w
applyN n f w = applyN (n-1) f (f w)

step :: Float -> World -> World
step dt bridge =
  bridge
    { nodes = nodes'
    }
  where
    nodes' = Map.mapWithKey applyForces bridge.nodes

    edgeForces :: Map Int (V2 Float)
    edgeForces = Map.fromListWith (+) $ do
      edge <- bridge.edges
      let Just nodeA = Map.lookup edge.nodeA bridge.nodes
      let Just nodeB = Map.lookup edge.nodeB bridge.nodes
      let l = distance nodeA.position nodeB.position

      let force = -k * (l - edge.restLength)
      let axis = normalize (nodeA.position - nodeB.position)

      [(edge.nodeA, force *^ axis) ,(edge.nodeB, -force *^ axis)]

    applyForces :: Int -> Node -> Node
    applyForces nodeId node = case node.state of
      Mobile -> node {position = newPosition, velocity = newVelocity}
      Fixed -> node
      where
        newPosition = node.position + dt *^ node.velocity
        newVelocity = node.velocity + dt *^ acceleration

        dampening = -(node.velocity) * 0.1

        edgeForce = case Map.lookup nodeId edgeForces of
                           Just edgeForce -> edgeForce
                           Nothing -> V2 0 0
        acceleration = (edgeForce + gravityForce + dampening) / massNode

massNode = 10

k = 1000

g = 45

z = V2 0 1

gravityForce = -(massNode * g * z)
