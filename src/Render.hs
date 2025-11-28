{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render (renderSDL) where

import SDL
import Linear (V4(..), V2(..), (^*))
import Foreign.C.Types (CInt)
import Data.Word (Word8)
import Data.List (find, lookup)
import Types

-- --- CONFIGURACIÓN VISUAL ---
tileSize :: CInt
tileSize = 32

spriteSize :: CInt
spriteSize = 64

orcScale :: CInt
orcScale = 3
playerScale :: CInt
playerScale = 3

-- --- FUNCIÓN PRINCIPAL DE RENDERIZADO ---
renderSDL :: Renderer -> Maybe Texture -> Maybe Texture -> Maybe Texture -> GameState -> IO ()
renderSDL renderer texOrc texPWalk texPAtk gs@GameState{..} = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  let coords = [ (x, y) | x <- [0..width-1], y <- [0..height-1] ]
  
  -- 1. Capa Fondo (Suelo)
  mapM_ (drawBackground renderer gs) coords

  -- 2. Capa Enemigos (Interpolados)
  mapM_ (drawEnemyInterpolated renderer texOrc actionTimer gameFrame) enemies

  -- 3. Capa Jugador (Interpolado)
  drawPlayerInterpolated renderer texPWalk texPAtk playerPos playerFrom playerDir playerAtkFrame gameFrame actionTimer

  present renderer

-- --- DIBUJADO DE FONDO ---
drawBackground :: Renderer -> GameState -> (Int, Int) -> IO ()
drawBackground renderer GameState{..} (x, y) = do
  let pos = (x,y)
      destRect = Rectangle (P (V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize))) 
                           (V2 tileSize tileSize)
      color 
        | pos == goal    = V4 0 0 255 255
        | pos == portalA || pos == portalB = V4 0 255 255 255
        | pos `elem` walls = V4 128 128 128 255
        | otherwise      = checkItems pos items
  
  rendererDrawColor renderer $= color
  fillRect renderer (Just destRect)

-- --- MATEMÁTICAS DE INTERPOLACIÓN (LERP) ---
getInterpolatedPixel :: Position -> Position -> Int -> (CInt, CInt)
getInterpolatedPixel (curX, curY) (oldX, oldY) timer =
    let 
        progress :: Float
        progress = 1.0 - (fromIntegral timer / fromIntegral moveDuration)

        startX = fromIntegral oldX * fromIntegral tileSize :: Float
        startY = fromIntegral oldY * fromIntegral tileSize :: Float
        endX   = fromIntegral curX * fromIntegral tileSize :: Float
        endY   = fromIntegral curY * fromIntegral tileSize :: Float

        pStart = V2 startX startY
        pEnd   = V2 endX endY
        
        -- Interpolación vectorial: Start + (End - Start) * Progress
        V2 finalX finalY = pStart + (pEnd - pStart) ^* progress
    in 
        (round finalX, round finalY)

-- --- DIBUJADO DE ENEMIGOS ---
drawEnemyInterpolated :: Renderer -> Maybe Texture -> Int -> Int -> Enemy -> IO ()
drawEnemyInterpolated renderer texOrc timer gFrame Enemy{..} = do
    let (pixelX, pixelY) = getInterpolatedPixel ePos eFrom timer
    
    -- Mapeo Orco: 0=Abajo, 1=Arriba, 2=Izq, 3=Der
    let rowIdx = case eDir of DDown->0; DUp->1; DLeft->2; DRight->3
    
    drawTexture renderer texOrc pixelX pixelY gFrame rowIdx orcScale

-- --- DIBUJADO DE JUGADOR ---
drawPlayerInterpolated :: Renderer -> Maybe Texture -> Maybe Texture -> Position -> Position -> Direction -> Int -> Int -> Int -> IO ()
drawPlayerInterpolated renderer texWalk texAtk curPos oldPos dir atkFrame gameFrame timer = do
    
    -- Mapeo Jugador: 0=Abajo, 1=Izq, 2=Der, 3=Arriba
    let rowIdx = case dir of DDown->0; DLeft->1; DRight->2; DUp->3

    if atkFrame > 0 
       then do
          -- Ataque: Sin interpolación de movimiento (estático)
          let (staticX, staticY) = (fromIntegral (fst curPos) * tileSize, fromIntegral (snd curPos) * tileSize)
          case texAtk of
              Just t -> drawTexture renderer (Just t) staticX staticY (6 - atkFrame) rowIdx playerScale
              Nothing -> return ()
       else do
          -- Caminar: Con interpolación suave
          let (pixelX, pixelY) = getInterpolatedPixel curPos oldPos timer
          case texWalk of
              Just t -> drawTexture renderer (Just t) pixelX pixelY gameFrame rowIdx playerScale
              Nothing -> return ()

-- --- FUNCIÓN GENÉRICA DE TEXTURA ---
drawTexture :: Renderer -> Maybe Texture -> CInt -> CInt -> Int -> CInt -> CInt -> IO ()
drawTexture _ Nothing _ _ _ _ _ = return ()
drawTexture renderer (Just tex) x y frame rowIdx scale = do
    let cols = 6
        currentCol = fromIntegral (frame `mod` cols)
        
        srcRect = Rectangle (P (V2 (currentCol * spriteSize) (rowIdx * spriteSize))) 
                            (V2 spriteSize spriteSize)
        
        destW = tileSize * scale
        destH = tileSize * scale
        
        -- Centrado del sprite grande sobre la celda pequeña
        diffX = (destW - tileSize) `div` 2
        diffY = (destH - tileSize) `div` 2
        
        finalRect = Rectangle (P (V2 (x - diffX) (y - diffY))) (V2 destW destH)

    textureBlendMode tex $= BlendAlphaBlend 
    copy renderer tex (Just srcRect) (Just finalRect)

-- --- AUXILIARES ---
checkItems :: Position -> [(Position, ItemKind)] -> V4 Word8
checkItems pos itemList = 
    case lookup pos itemList of
        Just QuestItem -> V4 255 165 0 255
        Just Heart     -> V4 0 255 0 255
        Just Strength  -> V4 255 0 255 255
        Nothing        -> V4 20 20 20 255