# Requisitos del Sistema

Se asume que ya dispones de:
* **Una distribución Linux** 
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
#####🕹️ Controles
* Moverse: W A S D
* Atacar: E
* Salir / Volver al Menú: Q 
* Navegar en Menús W: (Arriba) / S (Abajo)
* Confirmar en Menús: E
