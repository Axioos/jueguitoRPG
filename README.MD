# Requisitos del Sistema

Se asume que ya dispones de:
* **Ubuntu** 
* **GHC & Cabal** 

## Dependencias del Proyecto (Importante)
Aunque tengas Haskell instalado, necesitas las **librerías de desarrollo de C para SDL2** para que el juego pueda compilarse. Sin esto, `cabal build` fallará.

Ejecuta el siguiente comando en tu terminal:

```bash
sudo apt update
sudo apt install libsdl2-dev pkg-config build-essential git
```
### Inicialicar proyecto

```bash
cabal update
cabal build
```

#### Arrancar el proyecto
```bash
cabal run
```