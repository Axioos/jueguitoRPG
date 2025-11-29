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
import Logic (initialState, loadInitialState, handleInput, advanceAnimations) -- Importa loadInitialState
import Render (renderSDL)
import qualified Menu
import Menu (MenuState, MenuOption(..), initialMenuState, handleMenuInput, renderMenu, selectedOption)

-- --- TIPOS DE ESTADO GLOBAL ---
data AppState = AppState
  { menuState :: MenuState
  , gameState :: GameState
  , inMenu    :: Bool
  , showingInstructions :: Bool
  }

-- --- INICIALIZACIÓN ---
main :: IO ()
main = do
  SDL.initializeAll
  SDL.Image.initialize [InitPNG]
  
  window <- createWindow "Dungeon RPG" defaultWindow { windowInitialSize = V2 960 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  -- Carga de Assets del Juego
  texOrc   <- loadTexture renderer "assets/enemigo/orc_walk.png"
  texPWalk <- loadTexture renderer "assets/jugador/player_walk.png"
  texPAtk  <- loadTexture renderer "assets/jugador/player_atack.png"
  
  -- Carga de Assets del Menú
  texMenuBg   <- loadTexture renderer "assets/fondo.png"
  texMenuLogo <- loadTexture renderer "assets/logo.png"

  putStrLn "=== DUNGEON RPG ==="
  putStrLn "Menú: W/S (Navegar) | E (Seleccionar)"
  putStrLn "Juego: WASD (Mover) | E (Atacar) | Q (Volver al menú)"
  
  let appState = AppState
        { menuState = initialMenuState
        , gameState = initialState -- Se usa el estado puro (fallback) para inicializar
        , inMenu = True
        , showingInstructions = False
        }
  
  mainLoop renderer texOrc texPWalk texPAtk texMenuBg texMenuLogo appState
  
  -- Limpieza
  mapM_ destroyTexture [t | Just t <- [texOrc, texPWalk, texPAtk, texMenuBg, texMenuLogo]]
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

-- --- BUCLE PRINCIPAL ---
mainLoop :: Renderer -> Maybe Texture -> Maybe Texture -> Maybe Texture -> Maybe Texture -> Maybe Texture -> AppState -> IO ()
mainLoop renderer tOrc tWalk tAtk tMenuBg tMenuLogo appState = do
  events <- pollEvents
  let quitSignal = any (== QuitEvent) (map eventPayload events)
  
  let commands = mapMaybe getKey [ c | KeyboardEvent c <- map eventPayload events ]
      command  = listToMaybe commands

  -- Procesamiento según estado
  newAppState <- if showingInstructions appState
    then handleInstructionsInput command appState
    else if inMenu appState
      then handleMenuState renderer command appState
      else handleGameState renderer tOrc tWalk tAtk command appState

  -- Renderizado
  if showingInstructions newAppState
    then renderInstructions renderer
    else if inMenu newAppState
      then Menu.renderMenu renderer tMenuBg tMenuLogo (menuState newAppState)
      else renderSDL renderer tOrc tWalk tAtk (gameState newAppState)

  delay 50
  unless quitSignal (mainLoop renderer tOrc tWalk tAtk tMenuBg tMenuLogo newAppState)

-- --- MANEJO DEL MENÚ ---
handleMenuState :: Renderer -> Maybe Char -> AppState -> IO AppState
handleMenuState renderer (Just 'e') appState@AppState{..} = do
  case selectedOption menuState of
    Menu.Play -> do
      putStrLn "=== Iniciando Juego ==="
      loadedState <- loadInitialState -- AHORA LLAMA A LA ACCIÓN IO
      return appState { inMenu = False, gameState = loadedState } -- USA EL ESTADO CARGADO

    Menu.Quit -> do
      putStrLn "=== Saliendo del Juego ==="
      error "Quit selected"

handleMenuState _ (Just c) appState@AppState{..} = do
  let newMenuState = handleMenuInput c menuState
  return appState { menuState = newMenuState }

handleMenuState _ Nothing appState = return appState

-- --- MANEJO DE INSTRUCCIONES ---
handleInstructionsInput :: Maybe Char -> AppState -> IO AppState
handleInstructionsInput (Just 'q') appState = do
  putStrLn "=== Volviendo al Menú ==="
  return appState { showingInstructions = False }
handleInstructionsInput _ appState = return appState

renderInstructions :: Renderer -> IO ()
renderInstructions renderer = do
  rendererDrawColor renderer $= V4 15 15 25 255
  clear renderer
  
  -- Título
  rendererDrawColor renderer $= V4 200 180 50 255
  fillRect renderer (Just $ Rectangle (P (V2 300 30)) (V2 360 60))
  
  -- Bloques de texto (simulados con rectángulos)
  let instructions = 
        [ (100, "CONTROLES:")
        , (150, "W A S D - Movimiento")
        , (180, "E - Atacar enemigos")
        , (210, "Q - Salir / Volver")
        , (260, "OBJETIVO:")
        , (310, "Recolecta los items naranjas (Q)")
        , (340, "Llega a la meta azul (T)")
        , (370, "Evita los enemigos verdes!")
        ]
  
  rendererDrawColor renderer $= V4 200 200 200 255
  mapM_ (\(y, _) -> fillRect renderer (Just $ Rectangle (P (V2 100 y)) (V2 760 25))) instructions
  
  -- Indicación para volver
  rendererDrawColor renderer $= V4 255 100 100 255
  fillRect renderer (Just $ Rectangle (P (V2 280 420)) (V2 400 40))
  
  present renderer

-- --- MANEJO DEL JUEGO ---
handleGameState :: Renderer -> Maybe Texture -> Maybe Texture -> Maybe Texture -> Maybe Char -> AppState -> IO AppState
handleGameState renderer tOrc tWalk tAtk (Just 'q') appState = do
  putStrLn "=== Volviendo al Menú ==="
  return appState { inMenu = True, menuState = initialMenuState }

handleGameState _ _ _ _ (Just c) appState@AppState{..} = do
  let gsInput = handleInput c gameState
      gsNext  = advanceAnimations gsInput
  
  -- Log solo si cambió el mensaje
  if message gsNext /= message gameState && not (null (message gsNext))
    then putStrLn $ "HP: " ++ show (hp gsNext) ++ " | " ++ message gsNext
    else return ()
  
  -- Si el juego terminó, volver al menú
  if gameOver gsNext
    then do
      if win gsNext
        then putStrLn "=== ¡VICTORIA! Volviendo al menú ==="
        else putStrLn "=== Game Over. Volviendo al menú ==="
      return appState { inMenu = True, menuState = initialMenuState, gameState = initialState } -- Usa el estado puro (fallback) para reset
    else return appState { gameState = gsNext }

handleGameState _ _ _ _ Nothing appState@AppState{..} = do
  let gsNext = advanceAnimations gameState
  return appState { gameState = gsNext }

-- --- UTILIDADES ---
getKey :: KeyboardEventData -> Maybe Char
getKey KeyboardEventData{ keyboardEventKeysym = Keysym{..}, keyboardEventKeyMotion = Pressed } =
  case keysymKeycode of
    KeycodeW -> Just 'w'
    KeycodeA -> Just 'a'
    KeycodeS -> Just 's'
    KeycodeD -> Just 'd'
    KeycodeE -> Just 'e'
    KeycodeQ -> Just 'q'
    _ -> Nothing 
getKey _ = Nothing