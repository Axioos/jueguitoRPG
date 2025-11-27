module Types
  ( Position
  , GameState(..)
  ) where

type Position = (Int, Int)

data GameState = GameState
  { playerPos  :: Position
  , hp         :: Int
  , maxHp      :: Int
  , items      :: [Position]
  , enemies    :: [Position]
  , walls      :: [Position]
  , goal       :: Position
  , width      :: Int
  , height     :: Int
  , collected  :: Int
  , totalItems :: Int
  , message    :: String
  , gameOver   :: Bool
  , win        :: Bool
  , level      :: Int
  } deriving (Show)
