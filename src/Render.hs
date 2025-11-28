{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render (renderSDL) where

import SDL
import Linear (V4(..), V2(..))
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Data.List (lookup)
import Types

-- --- Configuración Visual ---
tileSize :: CInt
tileSize = 32

-- --- Función Principal de Renderizado ---
renderSDL :: Renderer -> GameState -> IO ()
renderSDL renderer gs@GameState{..} = do
  -- 1. Limpiar pantalla (Fondo negro)
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- 2. Dibujar mapa
  let coords = [ (x, y) | x <- [0..width-1], y <- [0..height-1] ]
  mapM_ (drawCell renderer gs) coords

  -- 3. Mostrar frame
  present renderer

-- --- Dibujado de Celdas Individuales ---
drawCell :: Renderer -> GameState -> (Int, Int) -> IO ()
drawCell renderer GameState{..} (x, y) = do
  let pos = (x,y)
      
      -- Selección de color según el tipo de objeto
      color 
        | pos == playerPos     = V4 255 255 0 255   -- Jugador (Amarillo)
        | pos == goal          = V4 0 0 255 255     -- Meta (Azul)
        | pos == portalA       = V4 0 255 255 255   -- Portales (Cyan)
        | pos == portalB       = V4 0 255 255 255
        | pos `elem` walls     = V4 128 128 128 255 -- Paredes (Gris)
        | pos `elem` enemies   = V4 255 0 0 255     -- Enemigos (Rojo)
        | otherwise            = checkItems pos items

  -- Definir y pintar rectángulo
  let rect = Rectangle (P (V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize))) 
                       (V2 tileSize tileSize)

  rendererDrawColor renderer $= color
  fillRect renderer (Just rect)

-- --- Auxiliares ---
checkItems :: Position -> [(Position, ItemKind)] -> V4 Word8
checkItems pos itemList = 
    case lookup pos itemList of
        Just QuestItem -> V4 255 165 0 255  -- Misión (Naranja)
        Just Heart     -> V4 0 255 0 255    -- Vida (Verde)
        Just Strength  -> V4 255 0 255 255  -- Fuerza (Magenta)
        Nothing        -> V4 20 20 20 255   -- Suelo vacío