module Types
  ( Position
  , Direction(..)
  , ItemKind(..)
  , Enemy(..)
  , GameState(..)
  , moveDuration 
  ) where

type Position = (Int, Int)

-- --- CONFIGURACIÓN ---
-- Duración del movimiento en frames.
-- 4 frames = Movimiento rápido y ágil (aprox 60-80ms)
moveDuration :: Int
moveDuration = 4

data Direction = DDown | DUp | DLeft | DRight
  deriving (Show, Eq)

data ItemKind
  = QuestItem   
  | Heart       
  | Strength    
  deriving (Show, Eq)

data Enemy = Enemy
  { ePos  :: Position
  , eFrom :: Position
  , eDir  :: Direction
  } deriving (Show, Eq)

data GameState = GameState
  { playerPos      :: Position
  , playerFrom     :: Position
  , playerDir      :: Direction
  , playerAtkFrame :: Int
  , actionTimer    :: Int
  , hp             :: Int
  , maxHp          :: Int
  , atk            :: Int
  , questCollected :: Int
  , totalQuest     :: Int
  , items          :: [(Position, ItemKind)]
  , enemies        :: [Enemy]
  , walls          :: [Position]
  , goal           :: Position
  , portalA        :: Position
  , portalB        :: Position
  , width          :: Int
  , height         :: Int
  , message        :: String
  , gameOver       :: Bool
  , win            :: Bool
  , level          :: Int
  , gameFrame      :: Int 
  } deriving (Show)