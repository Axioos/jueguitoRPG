module Types
  ( Position
  , ItemKind(..)
  , GameState(..)
  ) where

type Position = (Int, Int)


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
  , enemies        :: [Position]
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
  } deriving (Show)
