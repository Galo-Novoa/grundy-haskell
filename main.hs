import Parsing
import Data.Char
import Control.Monad
import Control.Applicative ((<|>))
import System.IO (hFlush, stdout)

type Row = Int
type Board = [(Int, Row)]

-- Muestra el tablero de forma legible
showBoard :: Board -> String
showBoard board = unlines [ show n ++ " : " ++ replicate size '*' | (n, size) <- board ]

-- Parser para números
number :: Parser Int
number = token nat

-- Parser para tuplas del tipo (a,b)
tuple :: Parser (Int, Int)
tuple = do
  symbol "("
  a <- number
  symbol ","
  b <- number
  symbol ")"
  return (a, b)

-- Busca una fila por número
findRow :: Int -> Board -> Maybe Row
findRow n = lookup n

-- Divide una fila en dos partes diferentes
splitRow :: Int -> Int -> Int -> Board -> Maybe Board
splitRow rowNum a b board
  | a <= 0 || b <= 0 || a == b = Nothing
  | otherwise = case findRow rowNum board of
      Nothing -> Nothing
      Just size | size /= a + b -> Nothing
                | otherwise ->
                  let -- Reemplazar la fila dividida por las dos nuevas filas
                      newBoard = map (\(n, s) -> if n == rowNum then (n, a) else (n, s)) board
                      maxNum = maximum (map fst newBoard)
                      -- Agregar la segunda fila nueva
                      finalBoard = newBoard ++ [(maxNum + 1, b)]
                  in Just finalBoard

-- Comprueba si quedan movimientos posibles
hasMoves :: Board -> Bool
hasMoves = any (\row -> snd row >= 3)

-- Verifica si el movimiento es válido según las reglas del juego
isValidMove :: Int -> (Int, Int) -> Board -> Bool
isValidMove rowNum (a, b) board =
  case findRow rowNum board of
    Just size -> a > 0 && b > 0 && a /= b && a + b == size && size >= 3
    Nothing -> False

-- Juego principal
playGrundy :: IO ()
playGrundy = do
  let initialBoard = [(1, 10)]  
  putStrLn "¡Bienvenido al Juego de Grundy!\n"
  putStrLn "Reglas:"
  putStrLn "-Dos jugadores se turnan para dividir una fila en 2 filas de diferentes tamaños"
  putStrLn "-Las divisiones deben sumar exactamente el tamaño original"
  putStrLn "-El juego termina cuando solo quedan filas de tamaño 1 o 2"
  putStrLn "-Gana el último jugador que puede hacer un movimiento válido\n"
  putStrLn "Tablero inicial:"
  putStrLn $ showBoard initialBoard
  gameLoop initialBoard 1

-- Bucle principal del juego
gameLoop :: Board -> Int -> IO ()
gameLoop board player
  | not (hasMoves board) = do
      putStrLn $ "\n¡Jugador " ++ show (3 - player) ++ " Gana!"
      putStrLn $ "Jugador " ++ show player ++ " Pierde porque no hay más movimientos posibles"
      showMenu
  | otherwise = do
      putStrLn $ "\n--- Turno del Jugador " ++ show player ++ " ---"
      putStr "Elija fila: "
      hFlush stdout
      rowInput <- getLine
      case parse number rowInput of
        [(rowNum, "")] -> case findRow rowNum board of
          Just size | size >= 3 -> do
            putStr "Ingrese división (ej: (3,5)): "
            hFlush stdout
            splitInput <- getLine
            case parse tuple splitInput of
              [((a, b), "")] -> 
                if isValidMove rowNum (a, b) board
                  then case splitRow rowNum a b board of
                    Just newBoard -> do
                      putStrLn "\nTablero actual:"
                      putStrLn $ showBoard newBoard
                      gameLoop newBoard (3 - player)
                    Nothing -> do
                      putStrLn "Error inesperado al realizar el movimiento."
                      gameLoop board player
                  else do
                    putStrLn "Movimiento inválido: las partes deben ser diferentes, >0 y sumar el tamaño original."
                    gameLoop board player
              _ -> do
                putStrLn "Formato inválido. Use (a,b) con a ≠ b."
                gameLoop board player
          _ -> do
            putStrLn "Fila no existe o no se puede dividir (debe ser ≥3)."
            gameLoop board player
        _ -> do
          putStrLn "Entrada inválida. Ingrese un número de fila."
          gameLoop board player

-- Menú principal
showMenu :: IO ()
showMenu = do
  putStrLn "\n=== MENÚ PRINCIPAL ==="
  putStrLn "1. Jugar"
  putStrLn "2. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStrLn "\nReiniciando juego...\n"
      playGrundy
    "2" -> do
      putStrLn "¡Gracias por jugar!"
    _ -> do
      putStrLn "Opción inválida. Por favor seleccione 1 o 2."
      showMenu

main :: IO ()
main = do
  putStrLn "JUEGO DE GRUNDY"
  putStrLn "==============="
  showMenu