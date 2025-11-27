{-# LANGUAGE RecordWildCards #-}

module Logic
  ( initialState
  , handleInput
  ) where

import Types
import Control.Monad.State
import Control.Monad (unless)
import Data.List (partition, nub)

initialState :: GameState
initialState =
  let w = 12
      h = 8

      borderWalls =
        [ (x, 0)     | x <- [0 .. w-1] ] ++
        [ (x, h-1)   | x <- [0 .. w-1] ] ++
        [ (0, y)     | y <- [0 .. h-1] ] ++
        [ (w-1, y)   | y <- [0 .. h-1] ]

      internalWalls =
        [ (5, y) | y <- [2..5] ]

      allWalls = borderWalls ++ internalWalls

      pPos     = (1,1)
      goalPos  = (10,6)
      itemsPos = [(3,1), (4,3), (8,5)]
      enemyPos = [(6,2), (9,4)]
      totalIts = length itemsPos
  in GameState
      { playerPos  = pPos
      , hp         = 10
      , maxHp      = 10
      , items      = itemsPos
      , enemies    = enemyPos
      , walls      = allWalls
      , goal       = goalPos
      , width      = w
      , height     = h
      , collected  = 0
      , totalItems = totalIts
      , message    = "Bienvenido al nivel 1."
      , gameOver   = False
      , win        = False
      , level      = 1
      }

handleInput :: Char -> GameState -> GameState
handleInput c gs = execState (gameStep c) gs


gameStep :: Char -> State GameState ()
gameStep c = do
  gs <- get
  unless (gameOver gs) $ do
    case c of
      'w' -> movePlayer 'w' >> resolvePlayerTile
      'W' -> movePlayer 'w' >> resolvePlayerTile
      's' -> movePlayer 's' >> resolvePlayerTile
      'S' -> movePlayer 's' >> resolvePlayerTile
      'a' -> movePlayer 'a' >> resolvePlayerTile
      'A' -> movePlayer 'a' >> resolvePlayerTile
      'd' -> movePlayer 'd' >> resolvePlayerTile
      'D' -> movePlayer 'd' >> resolvePlayerTile
      'e' -> playerAttack
      'E' -> playerAttack
      _   -> modify (\st -> st { message = "Tecla desconocida." })

    afterPlayer <- get
    unless (gameOver afterPlayer) $ do
      moveEnemies
      enemyDamage



movePlayer :: Char -> State GameState ()
movePlayer dir = do
  gs@GameState{..} <- get
  let (px,py) = playerPos
      (dx,dy) = case dir of
                  'w' -> ( 0,-1)
                  's' -> ( 0, 1)
                  'a' -> (-1, 0)
                  'd' -> ( 1, 0)
                  _   -> ( 0, 0)
      nx = px + dx
      ny = py + dy
      inside = nx >= 0 && nx < width && ny >= 0 && ny < height
      noWall = (nx,ny) `notElem` walls
      newPos = if inside && noWall then (nx,ny) else playerPos
  put gs { playerPos = newPos }



resolvePlayerTile :: State GameState ()
resolvePlayerTile = do
  gs@GameState{..} <- get
  let pos = playerPos

      -- recoger item
      (picked, remainingItems) = partition (== pos) items
      newCollected             = collected + length picked

      -- chocar contra enemigo 
      (collidedEnemies, remainingEnemies) = partition (== pos) enemies
      touchedEnemy  = not (null collidedEnemies)
      hpAfterEnemy  = if touchedEnemy then hp - 1 else hp

      
      reachedGoal   = pos == goal
      allItemsTaken = newCollected == totalItems
      playerWins    = reachedGoal && allItemsTaken

      dead          = hpAfterEnemy <= 0

      newMsg
        | playerWins        = "¡Ganaste! Llegaste al tesoro con todos los objetos."
        | dead              = "Te quedaste sin HP. Game Over."
        | touchedEnemy      = "Te lanzas contra un enemigo: lo eliminas pero pierdes 1 HP."
        | not (null picked) = "Recogiste un objeto."
        | otherwise         = message

  put gs
    { items     = remainingItems
    , enemies   = remainingEnemies
    , collected = newCollected
    , hp        = hpAfterEnemy
    , gameOver  = dead || playerWins
    , win       = playerWins
    , message   = newMsg
    }



playerAttack :: State GameState ()
playerAttack = do
  gs@GameState{..} <- get
  let (px,py) = playerPos
      area = [ (px+dx, py+dy) | dx <- [-1..1], dy <- [-1..1] ]
      (killed, survivors) = partition (`elem` area) enemies
      msg'
        | null killed = "Atacas, pero no hay enemigos en tu área."
        | otherwise   = "Eliminas " ++ show (length killed) ++ " enemigo(s) a tu alrededor."
  put gs { enemies = survivors, message = msg' }



moveEnemies :: State GameState ()
moveEnemies = do
  gs@GameState{..} <- get
  let (px,py)    = playerPos
      moved      = map (moveEnemyTowards (px,py) walls width height) enemies
      newEnemies = nub moved    
  put gs { enemies = newEnemies }

moveEnemyTowards :: Position -> [Position] -> Int -> Int -> Position -> Position
moveEnemyTowards (px,py) ws w h (ex,ey) =
  let dx = signum (px - ex)
      dy = signum (py - ey)

      trySteps =
        [ (ex + dx, ey)   
        , (ex, ey + dy)   
        , (ex, ey)        
        ]

      valid (x,y) =
        x >= 0 && x < w && y >= 0 && y < h && (x,y) `notElem` ws

  in case filter valid trySteps of
       (p:_) -> p
       []    -> (ex,ey)



enemyDamage :: State GameState ()
enemyDamage = do
  gs@GameState{..} <- get
  let pos             = playerPos
      (onPlayer, rest) = partition (== pos) enemies
      wasHit          = not (null onPlayer)
      newHp           = if wasHit then hp - 1 else hp
      dead            = newHp <= 0
      newMsg
        | dead      = "Un enemigo te golpeó. Te quedaste sin HP. Game Over."
        | wasHit    = "Un enemigo te golpea, pierdes 1 HP pero lo eliminas."
        | otherwise = message
  put gs
    { hp       = newHp
    , enemies  = rest
    , gameOver = gameOver || dead
    , message  = newMsg
    }
