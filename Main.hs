module Main where

import Types
import Logic
import Render
import System.IO

gameLoop :: GameState -> IO ()
gameLoop gs = do
  render gs
  if gameOver gs
    then do
      putStrLn ""
      if win gs
        then putStrLn "Completaste el nivel."
        else putStrLn " Game Over."
      putStrLn "Presiona ENTER para salir..."
      hSetEcho stdin True
      hSetBuffering stdin LineBuffering
      _ <- getLine
      return ()
    else do
      c <- getChar
      if c `elem` "qQ"
        then do
          putStrLn "\nSaliendo del juego."
        else do
          let gs' = handleInput c gs
          gameLoop gs'

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  gameLoop initialState
