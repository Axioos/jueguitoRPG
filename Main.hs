{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SDL
import Control.Monad (unless)
import Control.Monad.State (execState)
import Linear (V2(..))
import Data.Maybe (listToMaybe, mapMaybe)

import Types
import Logic
import Render (renderSDL)

-- --- Inicialización ---
main :: IO ()
main = do
  initializeAll
  
  -- Crear ventana
  window <- createWindow "Dungeon RPG" defaultWindow { windowInitialSize = V2 960 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  putStrLn "--Juego iniciado--"
  
  -- Arrancar bucle
  appLoop renderer initialState
  
  destroyRenderer renderer
  destroyWindow window
  quit

-- --- Bucle del Juego (Game Loop) ---
appLoop :: Renderer -> GameState -> IO ()
appLoop renderer gs = do
  -- 1. Capturar eventos
  events <- pollEvents
  let eventPayloads = map eventPayload events
      quitSignal = any (== QuitEvent) eventPayloads
  
  -- 2. Lógica de Input
  -- Usamos mapMaybe para filtrar solo las teclas validas
  let commands = mapMaybe getKey [ c | KeyboardEvent c <- eventPayloads ]
      command  = listToMaybe commands

      gsNext = case command of
                 Just c  -> handleInput c gs
                 Nothing -> gs

  -- 3. Renderizado
  renderSDL renderer gsNext
  
  -- 4. Logs en consola
  case command of 
      Just _ -> putStrLn $ "HP: " ++ show (hp gsNext) ++ " | " ++ message gsNext
      Nothing -> return ()

  -- 5. Control de FPS y Recursión
  delay 20 -- ~50 FPS
  unless (quitSignal || gameOver gsNext) (appLoop renderer gsNext)
  
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
    _        -> Nothing -- Cualquier otra tecla se ignora
getKey _ = Nothing      -- Soltar tecla (Released) se ignora