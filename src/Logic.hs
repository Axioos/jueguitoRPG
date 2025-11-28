{-# LANGUAGE RecordWildCards #-}

module Logic
  ( initialState
  , handleInput
  ) where

import Types
import Control.Monad.State
import Control.Monad (unless)
import Data.List (partition)




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
        [ ((5,2),  QuestItem)
        , ((14,3), QuestItem)
        , ((25,9), QuestItem)
        ]

      
      itemsPos =
        questItems ++
        [ ((4,10), Heart)      
        , ((18,5), Strength)  
        , ((22,3), Heart)
        ]

  in GameState
      { playerPos      = (2,2)
      , hp             = 10
      , maxHp          = 10
      , atk            = 1
      , questCollected = 0
      , totalQuest     = length questItems
      , items          = itemsPos
      , enemies        = [(12,4),(18,4),(9,11),(22,8)]
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
      }




handleInput :: Char -> GameState -> GameState
handleInput c gs = execState (gameStep c) gs

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
      _   -> modify (\st -> st { message = "Tecla desconocida. Usa WASD para moverte, E para atacar, Q para salir." })
    after <- get
    unless (gameOver after) $ do
      moveEnemies
      enemyDamage




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

      (hitEnemies, restEnemies) = partition (== pos) enemies
      hit = not (null hitEnemies)
      newHpPrePortal = if hit then newHpBase - 1 else newHpBase

     
      winGameBase = pos == goal && newQuest == totalQuest
      deadBase    = newHpPrePortal <= 0

      
      msgHp
        | hpGained == 0 = ""
        | hpGained == 2 = "Recuperas 2 puntos de vida con H. "
        | otherwise     = "Recuperas " ++ show hpGained ++ " puntos de vida con H. "

      msgAtk
        | atkGained == 0 = ""
        | atkGained == 1 = "Tu fuerza aumenta en 1 con S (ATK = " ++ show newAtk ++ "). "
        | otherwise      = "Tu fuerza aumenta en " ++ show atkGained ++ " con S (ATK = " ++ show newAtk ++ "). "

      msgQuest
        | questN == 0   = ""
        | questN == 1   = "Recolectas un objeto de misión Q (" ++ show newQuest ++ "/" ++ show totalQuest ++ "). "
        | otherwise     = "Recolectas varios objetos de misión Q (" ++ show newQuest ++ "/" ++ show totalQuest ++ "). "

      msgHit
        | not hit       = ""
        | otherwise     = "Te lanzas contra un enemigo E: lo derrotas pero pierdes 1 HP. "

    
      (posAfterPortal, msgPortal)
        | pos == portalA = (portalB, "Entras al portal y apareces en la esquina opuesta. ")
        | pos == portalB = (portalA, "Entras al portal y apareces en la esquina opuesta. ")
        | otherwise      = (pos, "")

      baseMsg = msgQuest ++ msgHp ++ msgAtk ++ msgHit ++ msgPortal

      winGame = winGameBase  
      dead    = deadBase

      finalMsg
        | winGame = "¡Ganaste! Recolectaste todos los Q y llegaste al tesoro T."
        | dead    = "Te quedaste sin HP. Game Over."
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




playerAttack :: State GameState ()
playerAttack = do
  gs@GameState{..} <- get
  let (x,y)  = playerPos
      radius = max 1 atk
      area   = [ (x+dx,y+dy) | dx <- [-radius..radius], dy <- [-radius..radius] ]
      (killed, alive) = partition (`elem` area) enemies
      msg
        | null killed = "Atacas con E, pero no alcanzas a ningún enemigo."
        | otherwise   = "Eliminas " ++ show (length killed) ++ " enemigo(s) con tu ataque E."
  put gs
    { enemies = alive
    , message = msg
    }




moveEnemies :: State GameState ()
moveEnemies = do
  gs@GameState{..} <- get
  let (px,py) = playerPos

      stepMove :: [Position] -> Position -> [Position]
      stepMove acc e =
        let newPos = enemyStep (px,py) walls width height acc e
        in newPos : acc

      newEnemies = reverse (foldl stepMove [] enemies)

  put gs { enemies = newEnemies }

enemyStep :: Position         
          -> [Position]       
          -> Int              
          -> Int              
          -> [Position]       
          -> Position         
          -> Position
enemyStep (px,py) ws w h occupied (ex,ey) =
  let dx = signum (px - ex)
      dy = signum (py - ey)

      candidates =
        [ (ex + dx, ey + dy)
        , (ex + dx, ey)
        , (ex, ey + dy)
        , (ex, ey)
        ]

      valid (x,y) =
        x >= 0 && x < w &&
        y >= 0 && y < h &&
        (x,y) `notElem` ws &&
        (x,y) `notElem` occupied

  in case filter valid candidates of
       (p:_) -> p
       []    -> (ex,ey)




enemyDamage :: State GameState ()
enemyDamage = do
  gs@GameState{..} <- get
  let (hit, rest) = partition (== playerPos) enemies
      wasHit      = not (null hit)
      newHp       = if wasHit then hp - 1 else hp
      dead        = newHp <= 0
      msg
        | dead      = "Un enemigo E te golpea. Te quedas sin HP. Game Over."
        | wasHit    = "Un enemigo E te golpea y pierdes 1 HP."
        | otherwise = message
  put gs
    { enemies = rest
    , hp      = newHp
    , gameOver = gameOver || dead
    , message = msg
    }
