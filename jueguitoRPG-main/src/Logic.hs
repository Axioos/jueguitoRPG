{-# LANGUAGE RecordWildCards #-}

module Logic
  ( initialState -- Exporta el estado inicial (fallback puro)
  , loadInitialState -- Exporta la acción IO de carga de archivo
  , handleInput
  , advanceAnimations
  ) where

import Types
import Control.Monad.State
import Control.Monad (unless)
import Data.List (partition)
import System.IO
import Control.Exception (catch, SomeException)
import Data.Char (isSpace)

-- --- PARSING DEL MAPA GRID ---

data ParsedMap = ParsedMap
  { pmWidth :: Int
  , pmHeight :: Int
  , pmPlayerPos :: Position
  , pmGoal :: Position
  , pmPortalA :: Position
  , pmPortalB :: Position
  , pmWalls :: [Position]
  , pmItems :: [(Position, ItemKind)]
  , pmEnemies :: [Position]
  } deriving (Show)

parseGridContent :: String -> ParsedMap
parseGridContent content = 
    let 
        rawLines = lines content

        isHeaderComment l = case dropWhile isSpace l of
                              "" -> True
                              ('#':c:_) -> isSpace c
                              _ -> False

        mapStart = dropWhile isHeaderComment rawLines
        rawMap = filter (not . null) mapStart
        
        h = length rawMap
        w = if h > 0 then length (head rawMap) else 0

        emptyMap = ParsedMap w h (0,0) (0,0) (0,0) (0,0) [] [] []
        
        (finalMap, portalPositions) = foldl parseRow (emptyMap, []) (zip [0..h-1] rawMap)

        finalMapWithPortals = case portalPositions of
                                [p1, p2] -> finalMap { pmPortalA = p1, pmPortalB = p2 }
                                _        -> finalMap 
    in finalMapWithPortals

parseRow :: (ParsedMap, [Position]) -> (Int, String) -> (ParsedMap, [Position])
parseRow (pm, portals) (y, row) = 
    foldl (parseCell y) (pm, portals) (zip [0..pmWidth pm - 1] row)

parseCell :: Int -> (ParsedMap, [Position]) -> (Int, Char) -> (ParsedMap, [Position])
parseCell y (pm, portals) (x, char) =
    let pos = (x, y)
        pmWallUpdated = if char == '#' 
                        then pm { pmWalls = pos : pmWalls pm } 
                        else pm
            
    in case char of
        'J' -> (pmWallUpdated { pmPlayerPos = pos }, portals)
        'E' -> (pmWallUpdated { pmEnemies = pos : pmEnemies pm }, portals)
        'T' -> (pmWallUpdated { pmItems = (pos, QuestItem) : pmItems pm }, portals)
        'S' -> (pmWallUpdated { pmItems = (pos, Heart) : pmItems pm }, portals)
        'A' -> (pmWallUpdated { pmItems = (pos, Strength) : pmItems pm }, portals)
        'F' -> (pmWallUpdated { pmGoal = pos }, portals)
        'P' -> (pmWallUpdated, pos : portals)
        _   -> (pmWallUpdated, portals)

mapToGameState :: ParsedMap -> GameState
mapToGameState ParsedMap{..} =
  let 
      questItems = [k | (_,k) <- pmItems, k == QuestItem]
      
      mkEnemy (x,y) = Enemy { ePos=(x,y), eFrom=(x,y), eDir=DDown }
      initialEnemies = map mkEnemy pmEnemies

  in GameState
      { playerPos = pmPlayerPos, playerFrom = pmPlayerPos, playerDir = DDown
      , playerAtkFrame = 0, actionTimer = 0, hp = 10, maxHp = 10, atk = 1
      , questCollected = 0, totalQuest = length questItems
      , items = pmItems, enemies = initialEnemies, walls = pmWalls
      , goal = pmGoal, portalA = pmPortalA, portalB = pmPortalB
      , width = pmWidth, height = pmHeight, message = "Recolecta los T y llega a la F."
      , gameOver = False, win = False, level = 1, gameFrame = 0
      }

-- Función principal para cargar el estado inicial (IO)
loadInitialState :: IO GameState
loadInitialState = do
    putStrLn "Cargando nivel 1 desde assets/levels/level1.txt (Grid Map)..."
    catch 
        (do
            content <- readFile "assets/levels/level1.txt"
            let parsedMap = parseGridContent content
            return $ mapToGameState parsedMap
        ) 
        (\e -> do
            let err = show (e :: SomeException)
            putStrLn $ "Error al cargar el nivel: " ++ err
            putStrLn "Cargando estado inicial por defecto (Hardcoded)."
            return initialState -- Usa el estado puro de fallback
        )

-- --- ESTADO INICIAL DE FALLBACK (PURO) ---
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
      , width = w, height = h, message = "Recolecta los T y llega a la F."
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