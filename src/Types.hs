module Types
  ( Position
  , Direction(..)
  , ItemKind(..)
  , GameState(..)
  ) where

type Position = (Int, Int)

-- Direcciones para la animación
data Direction = DUp | DLeft | DDown | DRight
  deriving (Show, Eq)

data ItemKind
  = QuestItem   
  | Heart       
  | Strength    
  deriving (Show, Eq)

data GameState = GameState
  { playerPos      :: Position
  , hp             :: Int
  , maxHp          :: Int
  , atk            :: Int
  , questCollected :: Int
  , totalQuest     :: Int
  , items          :: [(Position, ItemKind)]
  , enemies        :: [(Position, Direction)] -- Ahora guarda Posición y Dirección
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