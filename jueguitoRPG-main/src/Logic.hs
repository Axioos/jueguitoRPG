{-# LANGUAGE RecordWildCards #-}

module Logic
  ( initialState
  , handleInput
  , advanceAnimations
  ) where

import Types
import Control.Monad.State
import Control.Monad (unless)
import Data.List (partition)

-- --- ESTADO INICIAL ---
initialState :: GameState
initialState =
  let w = 30
      h = 14
      
      borderWalls = [ (x, 0) | x <- [0..w-1] ] ++ [ (x, h-1) | x <- [0..w-1] ] ++
                    [ (0, y) | y <- [0..h-1] ] ++ [ (w-1, y) | y <- [0..h-1] ]
      internalWalls = [ (10, y) | y <- [2..11] ] ++ [ (x, 7) | x <- [5..24] ] ++ [ (20, y) | y <- [3..10] ]
      allWalls = borderWalls ++ internalWalls
      
      pA = (1, h-2); pB = (w-2, 1)    
      questItems = [ ((5,2), QuestItem), ((14,3), QuestItem), ((25,9), QuestItem) ]
      itemsPos = questItems ++ [ ((4,10), Heart), ((18,5), Strength), ((22,3), Heart) ]
      
      mkEnemy (x,y) = Enemy { ePos=(x,y), eFrom=(x,y), eDir=DDown }
      initialEnemies = map mkEnemy [(12,4), (18,4), (9,11), (22,8)]

  in GameState
      { playerPos = (2,2), playerFrom = (2,2), playerDir = DDown
      , playerAtkFrame = 0, actionTimer = 0, hp = 10, maxHp = 10, atk = 1
      , questCollected = 0, totalQuest = length questItems
      , items = itemsPos, enemies = initialEnemies, walls = allWalls
      , goal = (27,11), portalA = pA, portalB = pB
      , width = w, height = h, message = "Recolecta los Q y llega a la T."
      , gameOver = False, win = False, level = 1, gameFrame = 0
      }

-- --- GESTIÓN DE TIEMPO ---
advanceAnimations :: GameState -> GameState
advanceAnimations gs@GameState{..} = 
    let nextAtk   = if playerAtkFrame > 0 then playerAtkFrame - 1 else 0
        nextAct   = if actionTimer > 0    then actionTimer - 1    else 0
        nextFrame = if nextAct > 0 then gameFrame + 1 else 0
    in gs { gameFrame = nextFrame, playerAtkFrame = nextAtk, actionTimer = nextAct }

-- --- INPUT ---
handleInput :: Char -> GameState -> GameState
handleInput c gs
  | c `elem` "wsade" =
      if actionTimer gs < 2
         then
           let 
               gsReady = gs { playerFrom = playerPos gs, actionTimer = 0 }
               gsNext  = execState (gameStep c) gsReady
           in 
               gsNext { actionTimer = moveDuration }
         else gs

  | otherwise = execState (gameStep c) gs

gameStep :: Char -> State GameState ()
gameStep c = do
  gs <- get
  unless (gameOver gs || actionTimer gs > 0) $ do
    case c of
      'w' -> movePlayer DUp    >> resolvePlayerTile
      's' -> movePlayer DDown  >> resolvePlayerTile
      'a' -> movePlayer DLeft  >> resolvePlayerTile
      'd' -> movePlayer DRight >> resolvePlayerTile
      'e' -> playerAttack
      _   -> return ()
    
    after <- get
    unless (gameOver after) $ do
       if c `elem` "wsade" then do moveEnemies; enemyDamage else return ()

-- --- LÓGICA JUGADOR ---
movePlayer :: Direction -> State GameState ()
movePlayer dir = do
  gs@GameState{..} <- get
  let (x,y) = playerPos
      (dx,dy) = case dir of DUp->(0,-1); DDown->(0,1); DLeft->(-1,0); DRight->(1,0)
      np = (x+dx, y+dy)
      valid (nx,ny) = nx>=0 && nx<width && ny>=0 && ny<height && (nx,ny) `notElem` walls
      newPos = if valid np then np else playerPos

  put gs { playerPos = newPos, playerFrom = playerPos, playerDir = dir }

resolvePlayerTile :: State GameState ()
resolvePlayerTile = do
  gs@GameState{..} <- get
  let pos = playerPos
      (picked, remaining) = partition (\(p,_) -> p == pos) items
      
      questN = length [() | (_,QuestItem) <- picked]
      hearts = length [() | (_,Heart)     <- picked]
      strs   = length [() | (_,Strength)  <- picked]
      
      (hitEnemies, _) = partition (\e -> ePos e == pos) enemies
      hit = not (null hitEnemies)
      
      hpGained = hearts * 2
      newHp = min maxHp (hp + hpGained) - (if hit then 1 else 0)
      newAtk = atk + strs
      newQuest = questCollected + questN
      
      winGame = pos == goal && newQuest == totalQuest
      
      (finalPos, msgPortal) 
          | pos == portalA = (portalB, "Portal activado! ")
          | pos == portalB = (portalA, "Portal activado! ")
          | otherwise      = (pos, "")

      finalFrom = if finalPos /= pos then finalPos else playerFrom

      msgHp | hpGained > 0 = "Salud +" ++ show hpGained ++ ". "
            | otherwise    = ""
            
      msgItem | questN > 0 = "Mision (" ++ show newQuest ++ "/" ++ show totalQuest ++ "). "
              | strs > 0   = "Fuerza aumentada! "
              | otherwise  = ""

      msgHit | hit       = "Te chocas con enemigo (-1 HP). "
             | otherwise = ""

      newMsg = msgHit ++ msgPortal ++ msgHp ++ msgItem

  put gs { playerPos = finalPos, playerFrom = finalFrom, items = remaining, hp = newHp
         , atk = newAtk, questCollected = newQuest
         , gameOver = newHp <= 0 || winGame, win = winGame
         , message = newMsg }

playerAttack :: State GameState ()
playerAttack = do
  gs@GameState{..} <- get
  put gs { playerAtkFrame = 6, playerFrom = playerPos }
  let (x,y) = playerPos
      area = [(x+dx,y+dy) | dx <- [-atk..atk], dy <- [-atk..atk]]
      (killed, alive) = partition (\e -> ePos e `elem` area) enemies
  unless (null killed) $ modify (\st -> st { enemies = alive, message = "Enemigo eliminado!" })

-- --- LÓGICA ENEMIGOS ---
moveEnemies :: State GameState ()
moveEnemies = do
  gs@GameState{..} <- get
  let (px,py) = playerPos
      occupied = map ePos enemies

      solve [] _ = []
      solve (e:pending) acc =
         let p = ePos e
             others = map ePos acc ++ map ePos pending
             newP = enemyStep (px,py) walls width height others p
             newDir = determineDir p newP (eDir e)
             newEnemy = e { ePos = newP, eFrom = p, eDir = newDir }
         in newEnemy : solve pending (newEnemy : acc)

  put gs { enemies = solve enemies [] }

determineDir (ox,oy) (nx,ny) old | nx>ox=DRight | nx<ox=DLeft | ny>oy=DDown | ny<oy=DUp | otherwise=old
enemyStep (px,py) ws w h occ (ex,ey) =
  let dx=signum(px-ex); dy=signum(py-ey); cands=[(ex+dx,ey),(ex,ey+dy),(ex+dx,ey+dy)]
      valid (x,y) = x>=0 && x<w && y>=0 && y<h && (x,y)`notElem`ws && (x,y)`notElem`occ && (x,y)/=(ex,ey)
  in case filter valid cands of (p:_) -> p; [] -> (ex,ey)

enemyDamage :: State GameState ()
enemyDamage = do
  gs <- get
  let hit = any (\e -> ePos e == playerPos gs) (enemies gs)
  if hit then put gs { hp = hp gs - 1, gameOver = (hp gs - 1) <= 0, message = "Te golpearon!" } else return ()