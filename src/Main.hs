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

-- --- INICIALIZACIÓN ---
main :: IO ()
main = do
  SDL.initializeAll
  SDL.Image.initialize [InitPNG]
  
  window <- createWindow "Dungeon RPG" defaultWindow { windowInitialSize = V2 960 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  -- Carga de Assets
  texOrc   <- loadTexture renderer "assets/enemigo/orc_walk.png"
  texPWalk <- loadTexture renderer "assets/jugador/player_walk.png"
  texPAtk  <- loadTexture renderer "assets/jugador/player_atack.png"

  putStrLn "--- Juego Iniciado ---"
  putStrLn "Controles: WASD (Mover) | E (Atacar) | Q (Salir)"
  
  appLoop renderer texOrc texPWalk texPAtk initialState
  
  -- Limpieza
  mapM_ destroyTexture [t | Just t <- [texOrc, texPWalk, texPAtk]]
  destroyRenderer renderer
  destroyWindow window
  quit

loadTexture :: Renderer -> FilePath -> IO (Maybe Texture)
loadTexture r path = catch loadIt handleErr
  where
    loadIt = do
        s <- SDL.Image.load path
        t <- createTextureFromSurface r s
        freeSurface s
        return (Just t)
    handleErr :: SomeException -> IO (Maybe Texture)
    handleErr _ = do
        putStrLn $ "Error: Falta textura " ++ path
        return Nothing

-- --- BUCLE PRINCIPAL (GAME LOOP) ---
appLoop :: Renderer -> Maybe Texture -> Maybe Texture -> Maybe Texture -> GameState -> IO ()
appLoop renderer tOrc tWalk tAtk gs = do
  events <- pollEvents
  let quitSignal = any (== QuitEvent) (map eventPayload events)
  
  -- 1. Capturar Input
  let commands = mapMaybe getKey [ c | KeyboardEvent c <- map eventPayload events ]
      command  = listToMaybe commands
      
      gsInput  = case command of
                   Just c  -> handleInput c gs
                   Nothing -> gs

  -- 2. Avanzar Tiempo
  let gsNext = advanceAnimations gsInput

  -- 3. Renderizar
  renderSDL renderer tOrc tWalk tAtk gsNext
  
  -- 4. LOGS (Restaurado): Imprimir solo si hubo una tecla válida
  case command of 
      Just _ -> putStrLn $ "HP: " ++ show (hp gsNext) ++ " | " ++ message gsNext
      Nothing -> return ()
  
  delay 50 
  unless (quitSignal || gameOver gsNext) (appLoop renderer tOrc tWalk tAtk gsNext)
  
  if gameOver gsNext then putStrLn "=== GANASTE ===" else return ()

-- --- UTILERÍAS ---
getKey :: KeyboardEventData -> Maybe Char
getKey KeyboardEventData{ keyboardEventKeysym = Keysym{..}, keyboardEventKeyMotion = Pressed } =
  case keysymKeycode of
    KeycodeW -> Just 'w'; KeycodeA -> Just 'a'; KeycodeS -> Just 's'; KeycodeD -> Just 'd'; KeycodeE -> Just 'e'; KeycodeQ -> Just 'q'; _ -> Nothing 
getKey _ = Nothing