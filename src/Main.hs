{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SDL
import SDL.Image (load, InitFlag(..), initialize)
import Control.Monad (unless)
import Control.Monad.State (execState)
import Linear (V2(..))
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Exception (catch, SomeException)

import Types
import Logic
import Render (renderSDL)

-- --- Inicialización ---
main :: IO ()
main = do
  SDL.initializeAll
  SDL.Image.initialize [InitPNG]
  
  window <- createWindow "Dungeon RPG" defaultWindow { windowInitialSize = V2 960 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  orcTexture <- loadTexture renderer "assets/enemigo/orc_walk.png"

  putStrLn "--- Juego Iniciado ---"
  appLoop renderer orcTexture initialState
  
  mapM_ destroyTexture orcTexture
  destroyRenderer renderer
  destroyWindow window
  quit

loadTexture :: Renderer -> FilePath -> IO (Maybe Texture)
loadTexture r path = catch loadIt handleErr
  where
    loadIt = do
        surface <- SDL.Image.load path
        texture <- createTextureFromSurface r surface
        freeSurface surface
        return (Just texture)

    handleErr :: SomeException -> IO (Maybe Texture)
    handleErr _ = do
        putStrLn $ "Error crítico: No se encuentra " ++ path
        return Nothing

-- --- Bucle Principal ---
appLoop :: Renderer -> Maybe Texture -> GameState -> IO ()
appLoop renderer texOrc gs = do
  events <- pollEvents
  let eventPayloads = map eventPayload events
      quitSignal = any (== QuitEvent) eventPayloads
  
  let commands = mapMaybe getKey [ c | KeyboardEvent c <- eventPayloads ]
      command  = listToMaybe commands

      gsNext = case command of
                 Just c  -> handleInput c gs
                 Nothing -> gs

  renderSDL renderer texOrc gsNext
  
  case command of 
      Just _ -> putStrLn $ "HP: " ++ show (hp gsNext) ++ " | " ++ message gsNext
      Nothing -> return ()

  delay 50
  unless (quitSignal || gameOver gsNext) (appLoop renderer texOrc gsNext)
  
  if gameOver gsNext 
     then putStrLn "=== FIN DEL JUEGO ==="
     else return ()

-- --- Utilerías ---
getKey :: KeyboardEventData -> Maybe Char
getKey KeyboardEventData{ keyboardEventKeysym = Keysym{..}, keyboardEventKeyMotion = Pressed } =
  case keysymKeycode of
    KeycodeW -> Just 'w'
    KeycodeA -> Just 'a'
    KeycodeS -> Just 's'
    KeycodeD -> Just 'd'
    KeycodeE -> Just 'e' 
    KeycodeQ -> Just 'q' 
    _        -> Nothing 
getKey _ = Nothing