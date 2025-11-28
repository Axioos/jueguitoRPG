{-# LANGUAGE RecordWildCards #-}

module Logic
  ( initialState
  , handleInput
  ) where

import Types
import Control.Monad.State
import Control.Monad (unless)
import Data.List (partition)

-- --- Estado Inicial ---
initialState :: GameState
initialState =
  let w = 30
      h = 14

      borderWalls =
        [ (x, 0)     | x <- [0 .. w-1] ] ++
        [ (x, h-1)   | x <- [0 .. w-1] ] ++
        [ (0, y)     | y <- [0 .. h-1] ] ++
        [ (w-1, y)   | y <- [0 .. h-1] ]

      internalWalls =
        [ (10, y) | y <- [2..11] ] ++
        [ (x, 7)  | x <- [5..24] ] ++
        [ (20, y) | y <- [3..10] ]

      allWalls = borderWalls ++ internalWalls
      
      pA = (1, h-2)    
      pB = (w-2, 1)    
    
      questItems =
        [ ((5,2),  QuestItem), ((14,3), QuestItem), ((25,9), QuestItem) ]
      
      itemsPos =
        questItems ++
        [ ((4,10), Heart), ((18,5), Strength), ((22,3), Heart) ]

      -- Inicializar enemigos mirando hacia abajo
      initialEnemies = 
        [ ((12,4), DDown), ((18,4), DDown), ((9,11), DDown), ((22,8), DDown) ]

  in GameState
      { playerPos      = (2,2)
      , hp             = 10
      , maxHp          = 10
      , atk            = 1
      , questCollected = 0
      , totalQuest     = length questItems
      , items          = itemsPos
      , enemies        = initialEnemies
      , walls          = allWalls
      , goal           = (27,11)
      , portalA        = pA
      , portalB        = pB
      , width          = w
      , height         = h
      , message        = "Recolecta todos los Q y luego llega al tesoro T."
      , gameOver       = False
      , win            = False
      , level          = 1
      , gameFrame      = 0
      }

-- --- Manejo de Inputs ---
handleInput :: Char -> GameState -> GameState
handleInput c gs = 
    let gs' = execState (gameStep c) gs
    in gs' { gameFrame = gameFrame gs' + 1 }

gameStep :: Char -> State GameState ()
gameStep c = do
  gs <- get
  unless (gameOver gs) $ do
    case c of
      'w' -> movePlayer 'w' >> resolvePlayerTile
      's' -> movePlayer 's' >> resolvePlayerTile
      'a' -> movePlayer 'a' >> resolvePlayerTile
      'd' -> movePlayer 'd' >> resolvePlayerTile
      'e' -> playerAttack
      _   -> modify (\st -> st { message = "Tecla desconocida." })
    after <- get
    unless (gameOver after) $ do
      moveEnemies
      enemyDamage

-- --- Lógica de Movimiento ---
movePlayer :: Char -> State GameState ()
movePlayer dir = do
  gs@GameState{..} <- get
  let (x,y) = playerPos
      (dx,dy) = case dir of
        'w' -> (0,-1)
        's' -> (0,1)
        'a' -> (-1,0)
        'd' -> (1,0)
        _   -> (0,0)
      np = (x+dx, y+dy)
      valid (nx,ny) =
        nx >= 0 && nx < width &&
        ny >= 0 && ny < height &&
        (nx,ny) `notElem` walls
  put gs { playerPos = if valid np then np else playerPos }

resolvePlayerTile :: State GameState ()
resolvePlayerTile = do
  gs@GameState{..} <- get
  let pos = playerPos
      (picked, remaining) = partition (\(p,_) -> p == pos) items

      questN = length [() | (_,QuestItem) <- picked]
      hearts = length [() | (_,Heart)     <- picked]
      strs   = length [() | (_,Strength)  <- picked]

      hpGained   = hearts * 2
      atkGained  = strs
      newHpBase  = min maxHp (hp + hpGained)
      newAtk     = atk + atkGained
      newQuest   = questCollected + questN

      -- Colisión con enemigos (ignorando dirección)
      (hitEnemies, restEnemies) = partition (\(p,_) -> p == pos) enemies
      hit = not (null hitEnemies)
      newHpPrePortal = if hit then newHpBase - 1 else newHpBase

      winGameBase = pos == goal && newQuest == totalQuest
      deadBase    = newHpPrePortal <= 0

      msgHp | hpGained > 0  = "Recuperas " ++ show hpGained ++ " HP. "
            | otherwise     = ""
      msgAtk | atkGained > 0 = "ATK aumenta en " ++ show atkGained ++ ". "
             | otherwise     = ""
      msgQuest | questN > 0    = "Mision (" ++ show newQuest ++ "/" ++ show totalQuest ++ "). "
               | otherwise     = ""
      msgHit | hit           = "Te chocas con un enemigo (-1 HP). "
             | otherwise     = ""
      (posAfterPortal, msgPortal)
        | pos == portalA = (portalB, "Portal activado. ")
        | pos == portalB = (portalA, "Portal activado. ")
        | otherwise      = (pos, "")

      baseMsg = msgQuest ++ msgHp ++ msgAtk ++ msgHit ++ msgPortal
      winGame = winGameBase  
      dead    = deadBase
      finalMsg
        | winGame       = "¡Ganaste!"
        | dead          = "Game Over."
        | baseMsg == "" = message
        | otherwise     = baseMsg

  put gs
    { playerPos      = posAfterPortal
    , items          = remaining
    , enemies        = restEnemies
    , hp             = newHpPrePortal
    , atk            = newAtk
    , questCollected = newQuest
    , gameOver       = dead || winGame
    , win            = winGame
    , message        = finalMsg
    }

-- --- Combate ---
playerAttack :: State GameState ()
playerAttack = do
  gs@GameState{..} <- get
  let (x,y)  = playerPos
      radius = max 1 atk
      area   = [ (x+dx,y+dy) | dx <- [-radius..radius], dy <- [-radius..radius] ]
      (killed, alive) = partition (\(p,_) -> p `elem` area) enemies
      msg
        | null killed = "Ataque fallido."
        | otherwise   = "Eliminas " ++ show (length killed) ++ " enemigo(s)."
  put gs { enemies = alive, message = msg }

-- --- IA de Enemigos (CORREGIDA) ---
moveEnemies :: State GameState ()
moveEnemies = do
  gs@GameState{..} <- get
  let (px,py) = playerPos
      
      -- Función recursiva para mover enemigos uno a uno evitando solapamientos
      -- 'pending': Enemigos que faltan por mover
      -- 'acc': Enemigos que ya se movieron (su nueva posición)
      solveCollisions [] _ = []
      solveCollisions ((p, oldDir):pending) acc =
        let 
            -- "others" incluye los que YA se movieron (acc) y los que FALTAN (pending)
            -- Así evitamos movernos a donde está otro o a donde acaba de llegar otro.
            others = map fst acc ++ map fst pending
            
            newP   = enemyStep (px,py) walls width height others p
            newDir = determineDir p newP oldDir
            
            movedEnemy = (newP, newDir)
        in 
            movedEnemy : solveCollisions pending (movedEnemy : acc)

  -- Iniciar el cálculo secuencial
  let newEnemies = solveCollisions enemies []
      
  put gs { enemies = newEnemies }

-- Determinar hacia dónde mirar
determineDir :: Position -> Position -> Direction -> Direction
determineDir (ox, oy) (nx, ny) oldDir
  | nx > ox = DRight
  | nx < ox = DLeft
  | ny > oy = DDown
  | ny < oy = DUp
  | otherwise = oldDir

-- Calcular siguiente paso
enemyStep :: Position -> [Position] -> Int -> Int -> [Position] -> Position -> Position
enemyStep (px,py) ws w h occupied (ex,ey) =
  let dx = signum (px - ex)
      dy = signum (py - ey)
      candidates = [(ex+dx, ey), (ex, ey+dy), (ex+dx, ey+dy)]
      valid (x,y) = x >= 0 && x < w && y >= 0 && y < h 
                    && (x,y) `notElem` ws 
                    && (x,y) `notElem` occupied 
                    && (x,y) /= (ex,ey) -- Evitar quedarse quieto si puede moverse
  in case filter valid candidates of
       (p:_) -> p
       []    -> (ex,ey)

enemyDamage :: State GameState ()
enemyDamage = do
  gs@GameState{..} <- get
  let (hit, rest) = partition (\(p,_) -> p == playerPos) enemies
      wasHit      = not (null hit)
      newHp       = if wasHit then hp - 1 else hp
      dead        = newHp <= 0
      msg
        | dead      = "Golpe letal recibido. Game Over."
        | wasHit    = "Te golpean (-1 HP)."
        | otherwise = message
  put gs { enemies = rest, hp = newHp, gameOver = gameOver || dead, message = msg }