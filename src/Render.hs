{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render (renderSDL) where

import SDL
import Linear (V4(..), V2(..))
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Data.List (find, lookup)
import Types

-- --- Configuración Visual ---
tileSize :: CInt
tileSize = 32

spriteSize :: CInt
spriteSize = 64

orcScale :: CInt
orcScale = 3

-- --- Función Principal ---
renderSDL :: Renderer -> Maybe Texture -> GameState -> IO ()
renderSDL renderer texOrc gs@GameState{..} = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  let coords = [ (x, y) | x <- [0..width-1], y <- [0..height-1] ]
  mapM_ (drawCell renderer texOrc gs) coords

  present renderer

-- --- Dibujado por Celda ---
drawCell :: Renderer -> Maybe Texture -> GameState -> (Int, Int) -> IO ()
drawCell renderer texOrc GameState{..} (x, y) = do
  let pos = (x,y)
      baseRect = Rectangle (P (V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize))) 
                           (V2 tileSize tileSize)

  -- 1. Capa Fondo
  let backColor 
        | pos == playerPos     = V4 255 255 0 255
        | pos == goal          = V4 0 0 255 255
        | pos == portalA       = V4 0 255 255 255
        | pos == portalB       = V4 0 255 255 255
        | pos `elem` walls     = V4 128 128 128 255
        | otherwise            = checkItems pos items

  rendererDrawColor renderer $= backColor
  fillRect renderer (Just baseRect)

  -- 2. Capa Enemigos
  case find (\(p, _) -> p == pos) enemies of
    Just (_, dir) -> drawEnemy renderer texOrc baseRect gameFrame dir
    Nothing       -> return ()

-- --- Dibujado de Enemigo (Animado) ---
drawEnemy :: Renderer -> Maybe Texture -> Rectangle CInt -> Int -> Direction -> IO ()
drawEnemy renderer Nothing destRect _ _ = do
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRect renderer (Just destRect)

drawEnemy renderer (Just tex) (Rectangle (P (V2 x y)) (V2 w h)) frame dir = do
    
    -- DIRECCIONES ---
    let rowIdx = case dir of
                   DDown  -> 0  -- Fila 0: Abajo
                   DUp    -> 1  -- Fila 1: Arriba
                   DLeft  -> 2  -- Fila 2: Izquierda
                   DRight -> 3  -- Fila 3: Derecha
        
        cols = 6
        currentCol = fromIntegral (frame `mod` cols)
        srcRow     = fromIntegral rowIdx
        
        srcRect = Rectangle (P (V2 (currentCol * spriteSize) (srcRow * spriteSize))) 
                            (V2 spriteSize spriteSize)

    -- Escalado
    let newW = w * orcScale
        newH = h * orcScale
        diffX = (newW - w) `div` 2
        diffY = (newH - h) `div` 2
        
        scaledDest = Rectangle (P (V2 (x - diffX) (y - diffY))) (V2 newW newH)

    textureBlendMode tex $= BlendAlphaBlend 
    copy renderer tex (Just srcRect) (Just scaledDest)

checkItems :: Position -> [(Position, ItemKind)] -> V4 Word8
checkItems pos itemList = 
    case lookup pos itemList of
        Just QuestItem -> V4 255 165 0 255
        Just Heart     -> V4 0 255 0 255
        Just Strength  -> V4 255 0 255 255
        Nothing        -> V4 20 20 20 255