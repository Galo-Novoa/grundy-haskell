import Data.Char
import Control.Monad
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

type Row = Int
type Board = [(Int, Row)]

-- Muestra el tablero de forma legible
showBoard :: Board -> String
showBoard board = unlines [ show n ++ " : " ++ replicate size '*' | (n, size) <- board ]

-- Parser simple para números usando readMaybe
parseNumber :: String -> Maybe Int
parseNumber input = case reads input of
                     [(n, "")] -> Just n
                     _ -> Nothing

-- Parser simple para tuplas del tipo (a,b)
parseTuple :: String -> Maybe (Int, Int)
parseTuple input = case input of
                    '(':rest -> case span (/= ',') rest of
                                 (aStr, ',':bRest) -> case span (/= ')') bRest of
                                                      (bStr, ")") -> do
                                                        a <- parseNumber aStr
                                                        b <- parseNumber bStr
                                                        return (a, b)
                                                      _ -> Nothing
                                 _ -> Nothing
                    _ -> Nothing

-- Busca una fila por número
findRow :: Int -> Board -> Maybe Row
findRow n = lookup n

-- Divide una fila en dos partes diferentes, insertando la nueva fila después de la original
splitRow :: Int -> Int -> Int -> Board -> Maybe Board
splitRow rowNum a b board
  | a <= 0 || b <= 0 || a == b = Nothing
  | otherwise = case findRow rowNum board of
      Nothing -> Nothing
      Just size | size /= a + b -> Nothing
                | otherwise ->
                  let -- Renumerar todo el tablero para mantener números consecutivos
                      -- Primero, reemplazar la fila dividida por las dos nuevas en la posición correcta
                      allSizes = concatMap (\(n, s) -> 
                        if n == rowNum 
                          then [a, b]  -- Dividir esta fila en dos
                          else [s])     -- Mantener las otras filas
                        board
                      
                      -- Crear nuevo tablero con números consecutivos 1,2,3,4...
                      newBoard = zip [1..] allSizes
                  in Just newBoard

-- Comprueba si quedan movimientos posibles
hasMoves :: Board -> Bool
hasMoves = any (\row -> snd row >= 3)

-- Verifica si el movimiento es válido según las reglas del juego
isValidMove :: Int -> (Int, Int) -> Board -> Bool
isValidMove rowNum (a, b) board =
  case findRow rowNum board of
    Just size -> a > 0 && b > 0 && a /= b && a + b == size && size >= 3
    Nothing -> False

-- Lee input con reintento en caso de error
readInputWithRetry :: String -> (String -> Maybe a) -> (a -> Bool) -> String -> IO a
readInputWithRetry prompt parser validator errorMsg = do
  putStr prompt
  hFlush stdout
  input <- getLine
  case parser input of
    Just value | validator value -> return value
    _ -> do
      putStrLn errorMsg
      readInputWithRetry prompt parser validator errorMsg

-- Juego principal
playGrundy :: IO ()
playGrundy = do
  let initialBoard = [(1, 10)]  
  putStrLn "¡Bienvenido al Juego de Grundy!"
  gameLoop initialBoard 1

-- Bucle principal del juego
gameLoop :: Board -> Int -> IO ()
gameLoop board player
  | not (hasMoves board) = do
      putStrLn $ "\n¡Jugador " ++ show (3 - player) ++ " gana!"
      putStrLn $ "Jugador " ++ show player ++ " pierde porque no hay más movimientos posibles"
      showMenu
  | otherwise = do
      putStrLn $ "\n--- Turno del Jugador " ++ show player ++ " ---"
      putStrLn $ showBoard board
      
      -- Leer fila con reintento
      rowNum <- readInputWithRetry 
        "Elija fila: " 
        parseNumber
        (\n -> case findRow n board of 
                Just size -> size >= 3 
                Nothing -> False)
        "La fila elegida no existe o no se puede dividir (debe tener más de dos monedas). Ingrese otro número:"
      
      -- Leer división con reintento
      division <- readInputWithRetry
        "Ingrese división (ej: (3,7)): "
        parseTuple
        (\(a, b) -> isValidMove rowNum (a, b) board)
        "Movimiento inválido: Divida la fila en dos partes de diferente tamaño. Ingrese otra división:"
      
      let (a, b) = division
      case splitRow rowNum a b board of
        Just newBoard -> do
          gameLoop newBoard (3 - player)
        Nothing -> do
          putStrLn "Error inesperado al realizar el movimiento."
          gameLoop board player

-- Mostrar reglas del juego
showRules :: IO ()
showRules = do
  putStrLn "\n=== REGLAS DEL JUEGO DE GRUNDY ==="
  putStrLn "- En cada turno, uno de los dos jugadores elige una fila de monedas (representadas por asteriscos) y luego la divide en dos filas de diferentes tamaños."
  putStrLn "- Debe especificar el tamaño de las dos filas nuevas, asegurándose de que ambas sean de tamaño mayor a cero y diferentes entre sí."
  putStrLn "- El juego termina cuando solo quedan filas de tamaño 1 o 2."
  putStrLn "- Gana el último jugador que pudo hacer un movimiento válido."
  putStrLn "\nPresione Enter para continuar..."
  _ <- getLine
  showMenu

-- Menú principal
showMenu :: IO ()
showMenu = do
  putStrLn "\n=== MENÚ PRINCIPAL ==="
  putStrLn "1. Jugar"
  putStrLn "2. Ver reglas"
  putStrLn "3. Salir"
  putStr "\nSeleccione una opción: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStrLn "\nIniciando juego...\n"
      playGrundy
    "2" -> showRules
    "3" -> do
      putStrLn "¡Gracias por jugar!"
    _ -> do
      putStrLn "Opción inválida. Por favor, seleccione 1, 2 o 3."
      showMenu

main :: IO ()
main = do
  putStrLn "\nJUEGO DE GRUNDY"
  putStrLn "==============="
  showMenu