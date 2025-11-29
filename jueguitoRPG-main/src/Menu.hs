{-# LANGUAGE RecordWildCards #-}

module Menu
  ( MenuState(..)
  , MenuOption(..)
  , initialMenuState
  , handleMenuInput
  , renderMenu
  ) where

import qualified SDL
import SDL (Renderer, Rectangle(..), Point(..), ($=), clear, present, fillRect, rendererDrawColor, copy)
import Linear (V4(..), V2(..))
import Foreign.C.Types (CInt)
import Data.Word (Word8)

-- --- TIPOS ---
data MenuOption = Play | Quit
  deriving (Show, Eq, Enum, Bounded)

data MenuState = MenuState
  { selectedOption :: MenuOption
  , menuActive     :: Bool
  } deriving (Show)

-- --- ESTADO INICIAL ---
initialMenuState :: MenuState
initialMenuState = MenuState
  { selectedOption = Play
  , menuActive     = True
  }

-- --- MANEJO DE INPUT ---
handleMenuInput :: Char -> MenuState -> MenuState
handleMenuInput c ms@MenuState{..}
  | c == 'w' || c == 's' = ms { selectedOption = cycleThroughOptions selectedOption (c == 'w') }
  | c == 'e' = ms  -- Enter selecciona (manejado en Main)
  | otherwise = ms

cycleThroughOptions :: MenuOption -> Bool -> MenuOption
cycleThroughOptions current isUp =
  let allOptions = [minBound :: MenuOption .. maxBound]
      currentIdx = fromEnum current
      newIdx = if isUp 
               then (currentIdx - 1) `mod` length allOptions
               else (currentIdx + 1) `mod` length allOptions
  in toEnum newIdx

-- --- RENDERIZADO DEL MENÚ ---
renderMenu :: Renderer -> Maybe SDL.Texture -> Maybe SDL.Texture -> MenuState -> IO ()
renderMenu renderer mbBackground mbLogo MenuState{..} = do
  -- Fondo con imagen o color sólido
  case mbBackground of
    Just bgTex -> do
      SDL.clear renderer
      SDL.copy renderer bgTex Nothing Nothing  -- Renderizar imagen de fondo completo
    Nothing -> do
      rendererDrawColor renderer $= V4 20 20 30 255
      clear renderer

  -- Logo del juego (centrado arriba)
  case mbLogo of
    Just logoTex -> do
      let logoRect = Rectangle (P (V2 280 40)) (V2 400 120)  -- Ajusta tamaño según necesites
      SDL.copy renderer logoTex Nothing (Just logoRect)
    Nothing -> do
      -- Título de respaldo si no hay logo
      rendererDrawColor renderer $= V4 200 180 50 255
      fillRect renderer (Just $ Rectangle (P (V2 320 50)) (V2 320 80))

  -- Opciones del menú
  let options = [Play, Quit]
      startY = 240
      spacing = 100

  mapM_ (\(opt, idx) -> drawMenuOption renderer opt (selectedOption == opt) (startY + idx * spacing)) 
        (zip options [0..])

  present renderer

-- --- DIBUJADO DE ELEMENTOS ---
drawMenuOption :: Renderer -> MenuOption -> Bool -> CInt -> IO ()
drawMenuOption renderer option isSelected yPos = do
  let text = case option of
        Play -> "JUGAR"
        Quit -> "SALIR"
  
  let color = if isSelected 
              then V4 255 255 100 255  -- Amarillo brillante
              else V4 150 150 150 255  -- Gris

  rendererDrawColor renderer $= color
  
  let width = if isSelected then 300 else 250
      height = if isSelected then 60 else 50
      xPos = 480 - (width `div` 2)
      
      optionRect = Rectangle (P (V2 xPos yPos)) (V2 width height)
  
  fillRect renderer (Just optionRect)

  -- Dibujar texto dentro del rectángulo
  drawText renderer text (xPos + (width `div` 2)) (yPos + (height `div` 2)) isSelected

  -- Indicador de selección
  when isSelected $ do
    rendererDrawColor renderer $= V4 255 50 50 255
    let indicatorLeft = Rectangle (P (V2 (xPos - 40) (yPos + 15))) (V2 20 20)
        indicatorRight = Rectangle (P (V2 (xPos + width + 20) (yPos + 15))) (V2 20 20)
    fillRect renderer (Just indicatorLeft)
    fillRect renderer (Just indicatorRight)

-- Dibuja texto usando rectángulos pixelados (estilo retro)
drawText :: Renderer -> String -> CInt -> CInt -> Bool -> IO ()
drawText renderer text centerX centerY isSelected = do
  let color = if isSelected 
              then V4 50 50 50 255    -- Negro sobre amarillo
              else V4 255 255 255 255 -- Blanco sobre gris
  
  rendererDrawColor renderer $= color
  
  let charWidth = 8
      charHeight = 10
      spacing = 2
      totalWidth = fromIntegral (length text) * (charWidth + spacing)
      startX = centerX - (totalWidth `div` 2)
      startY = centerY - (charHeight `div` 2)
  
  mapM_ (\(ch, idx) -> drawChar renderer ch (startX + idx * (charWidth + spacing)) startY charWidth charHeight) 
        (zip text [0..])

-- Dibuja un carácter simple usando rectángulos
drawChar :: Renderer -> Char -> CInt -> CInt -> CInt -> CInt -> IO ()
drawChar renderer ch x y w h = do
  case ch of
    'J' -> drawJ renderer x y w h
    'U' -> drawU renderer x y w h
    'G' -> drawG renderer x y w h
    'A' -> drawA renderer x y w h
    'R' -> drawR renderer x y w h
    'S' -> drawS renderer x y w h
    'L' -> drawL renderer x y w h
    'I' -> drawI renderer x y w h
    _   -> return ()

-- Funciones para dibujar cada letra (estilo pixel art)
drawJ :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawJ r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 (x+w`div`2) y)) (V2 2 (h*3`div`4)))
  fillRect r (Just $ Rectangle (P (V2 x (y+h*3`div`4))) (V2 (w`div`2) 2))
  fillRect r (Just $ Rectangle (P (V2 x (y+h*2`div`4))) (V2 2 (h`div`4)))

drawU :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawU r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 (x+w-2) y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x (y+h-2))) (V2 w 2))

drawG :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawG r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x (y+h-2))) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 (x+w-2) (y+h`div`2))) (V2 2 (h`div`2)))
  fillRect r (Just $ Rectangle (P (V2 (x+w`div`2) (y+h`div`2))) (V2 (w`div`2) 2))

drawA :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawA r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 (x+w-2) y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x (y+h`div`2))) (V2 w 2))

drawR :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawR r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 x (y+h`div`2))) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 (x+w-2) y)) (V2 2 (h`div`2)))
  fillRect r (Just $ Rectangle (P (V2 (x+w`div`2) (y+h`div`2))) (V2 2 (h`div`2)))

drawS :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawS r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 (h`div`2)))
  fillRect r (Just $ Rectangle (P (V2 x (y+h`div`2))) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 (x+w-2) (y+h`div`2))) (V2 2 (h`div`2)))
  fillRect r (Just $ Rectangle (P (V2 x (y+h-2))) (V2 w 2))

drawL :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawL r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x (y+h-2))) (V2 w 2))

drawI :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO ()
drawI r x y w h = do
  fillRect r (Just $ Rectangle (P (V2 x y)) (V2 w 2))
  fillRect r (Just $ Rectangle (P (V2 (x+w`div`2-1) y)) (V2 2 h))
  fillRect r (Just $ Rectangle (P (V2 x (y+h-2))) (V2 w 2))

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()