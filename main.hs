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

-- Reemplaza una fila con un nuevo tamaño
replaceRow :: Int -> Row -> Board -> Board
replaceRow n newSize = map (\(k, s) -> if k == n then (k, newSize) else (k, s))

-- Divide una fila en dos partes diferentes
splitRow :: Int -> Int -> Int -> Board -> Maybe Board
splitRow rowNum a b board
  | a <= 0 || b <= 0 || a == b = Nothing
  | otherwise = case findRow rowNum board of
      Nothing -> Nothing
      Just size | size /= a + b -> Nothing
                | otherwise ->
                  let filtered = filter ((/= rowNum) . fst) board
                      maxNum = if null board then 0 else maximum (map fst board)
                      newRows = [(maxNum + 1, a), (maxNum + 2, b)]
                  in Just (filtered ++ newRows)

-- Comprueba si quedan movimientos posibles
hasMoves :: Board -> Bool
hasMoves = any ((>= 3) . snd)

-- Juego principal
playGrundy :: IO ()
playGrundy = do
  let initialBoard = [(1, 8)]
  putStrLn "¡Bienvenido al Juego de Grundy!\n"
  putStrLn "Tablero inicial:"
  putStrLn $ showBoard initialBoard
  gameLoop initialBoard 1

-- Bucle principal del juego
gameLoop :: Board -> Int -> IO ()
gameLoop board player
  | not (hasMoves board) = do
      putStrLn $ "Jugador " ++ show (3 - player) ++ " Gana !"
      putStrLn $ "Jugador " ++ show player ++ " Pierde porque no hay más movimientos posibles"
  | otherwise = do
      putStrLn $ "Jugador " ++ show player
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
              [((a, b), "")] -> case splitRow rowNum a b board of
                Just newBoard -> do
                  putStrLn $ showBoard newBoard
                  gameLoop newBoard (3 - player)
                Nothing -> do
                  putStrLn "Movimiento inválido: partes deben ser diferentes, >0 y sumar el tamaño original."
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

main :: IO ()
main = playGrundy