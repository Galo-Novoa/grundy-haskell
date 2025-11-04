{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
import Data.Char
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import System.Console.ANSI (clearScreen)

type Row = Int
type Board = [(Int, Row)]

-- Muestra el tablero de forma legible
showBoard :: Board -> String
showBoard board = unlines [ show n ++ " : " ++ replicate size '*' ++ " (" ++ show size ++ ")" | (n, size) <- board ]

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
findRow = lookup

-- Divide una fila en dos partes diferentes, insertando la nueva fila después de la original
splitRow :: Int -> Int -> Int -> Board -> Maybe Board
splitRow rowNum a b board
  | a <= 0 || b <= 0 || a == b = Nothing
  | otherwise = case findRow rowNum board of
      Nothing -> Nothing
      Just size | size /= a + b -> Nothing
                | otherwise ->
                  let allSizes = concatMap (\(n, s) ->
                        if n == rowNum
                          then [a, b]
                          else [s]) board
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

-- Tipos de modo de juego
data GameMode = Original | Libre Int deriving (Show, Read)

-- Mostrar modo de juego en español
showMode :: GameMode -> String
showMode Original = "Original (10 monedas)"
showMode (Libre n) = "Libre (" ++ show n ++ " monedas)"

-- Función principal para jugar
playGrundy :: GameMode -> IO ()
playGrundy mode = do
  let initialBoard = case mode of
                      Original -> [(1, 10)]
                      Libre n -> [(1, n)]
  putStrLn "¡Bienvenido al Juego de Grundy!"
  putStrLn $ "Modo: " ++ showMode mode
  let initialState = [show initialBoard, "1", show mode, "True", "", "-1"]
  gameLoop initialState

-- Selección de modo de juego (directa)
selectGameMode :: IO GameMode
selectGameMode = selectGameModeLoop ""

selectGameModeLoop :: String -> IO GameMode
selectGameModeLoop errorMsg = do
  clearScreen
  putStrLn "\n=== CONFIGURACIÓN - MODO DE JUEGO ==="
  putStrLn "1. Original (10 monedas iniciales)"
  putStrLn "2. Libre (elige la cantidad inicial)\n"

  -- Mostrar mensaje de error justo antes del input
  if not (null errorMsg) 
    then putStrLn errorMsg
    else return ()

  putStr "Seleccione una opción: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> return Original
    "2" -> do
      putStr "\nIngrese la cantidad inicial de monedas: "
      hFlush stdout
      input <- getLine
      case parseNumber input of
        Just n | n >= 3 && n <= 30 -> return (Libre n)
        Just n | n > 30 -> selectGameModeLoop "Demasiadas monedas! El máximo permitido es 30."
        _ -> selectGameModeLoop "Cantidad inválida. Debe ser un número entre 3 y 30."
    _ -> selectGameModeLoop "Opción inválida. Seleccione 1 o 2."

-- Menú principal
showMenu :: GameMode -> String -> IO ()
showMenu currentMode message = do
  clearScreen
  putStrLn "\n=== MENÚ PRINCIPAL ==="
  putStrLn $ "Modo actual: " ++ showMode currentMode
  
  -- Mostrar mensaje si no está vacío
  if not (null message)
    then putStrLn $ "\n" ++ message
    else return ()
  
  putStrLn "\n1. Jugar"
  putStrLn "2. Ver reglas"
  putStrLn "3. Cambiar modo de juego"
  putStrLn "4. Salir"
  putStr "\nSeleccione una opción: "
  hFlush stdout
  option <- getLine
  case option of
    "1" -> do
      putStrLn "\nIniciando juego...\n"
      playGrundy currentMode
    "2" -> showRules currentMode
    "3" -> do
      newMode <- selectGameMode
      showMenu newMode ""
    "4" -> do
      putStrLn "¡Gracias por jugar!\n"
    _ -> do
      showMenu currentMode "Opción inválida. Seleccione entre 1 y 4."

-- Bucle principal del juego (versión con lista simple)
gameLoop :: [String] -> IO ()
gameLoop [boardStr, playerStr, modeStr, firstMoveStr, errorMsg, selectedRowStr] = do
  let board = read boardStr :: Board
      player = read playerStr :: Int
      currentMode = read modeStr :: GameMode
      firstMove = read firstMoveStr :: Bool
      selectedRow = read selectedRowStr :: Int
  
  if not (hasMoves board)
    then do
      putStrLn $ "\n¡Jugador " ++ show (3 - player) ++ " gana!"
      putStrLn $ "Jugador " ++ show player ++ " pierde porque no hay más movimientos posibles"
      putStrLn "\nPresione Enter para volver al menú..."
      _ <- getLine
      showMenu currentMode ""
    else do
      clearScreen
      putStrLn $ "\n--- Turno del Jugador " ++ show player ++ " ---"
      putStrLn $ showBoard board
      
      if firstMove 
        then putStrLn "(Presione 'q' en cualquier momento de la partida para volver al menú)\n"
        else return ()

      if not (null errorMsg)
        then putStrLn errorMsg
        else return ()

      putStr "Elija fila: "
      hFlush stdout
      input <- getLine
      case input of
        "q" -> showMenu currentMode ""
        _ -> case parseNumber input of
          Just rowNum | case findRow rowNum board of
                         Just size -> size >= 3
                         Nothing -> False -> 
            -- Pasar a askDivision con la fila seleccionada
            askDivision [boardStr, playerStr, modeStr, "False", "", show rowNum]
          _ -> 
            gameLoop [boardStr, playerStr, modeStr, "False", "La fila elegida no existe o no se puede dividir (debe tener más de dos monedas).", "-1"]

-- Función para manejar la división
askDivision :: [String] -> IO ()
askDivision [boardStr, playerStr, modeStr, firstMoveStr, errorMsg, selectedRowStr] = do
  let board = read boardStr :: Board
      player = read playerStr :: Int
      currentMode = read modeStr :: GameMode
      rowNum = read selectedRowStr :: Int
  
  clearScreen
  putStrLn $ "\n--- Turno del Jugador " ++ show player ++ " ---"
  putStrLn $ showBoard board
  putStrLn $ "Fila seleccionada: " ++ show rowNum
  
  if not (null errorMsg)
    then putStrLn errorMsg
    else return ()
  
  putStr "Ingrese división (ej: (3,7)): "
  hFlush stdout
  divisionInput <- getLine
  case divisionInput of
    "q" -> showMenu currentMode ""
    _ -> case parseTuple divisionInput of
      Just division | isValidMove rowNum division board -> do
        let (a, b) = division
        case splitRow rowNum a b board of
          Just newBoard -> 
            let newState = [show newBoard, show (3 - player), modeStr, "False", "", "-1"]
            in gameLoop newState
          Nothing -> 
            askDivision [boardStr, playerStr, modeStr, firstMoveStr, "Error inesperado al realizar el movimiento.", selectedRowStr]
      _ -> 
        askDivision [boardStr, playerStr, modeStr, firstMoveStr, "Movimiento inválido. Divida la fila en dos partes de diferente tamaño cuya suma coincida con el tamaño de la línea elegida.", selectedRowStr]

-- Mostrar reglas del juego
showRules :: GameMode -> IO ()
showRules currentMode = do
  clearScreen
  putStrLn "\n=== REGLAS DEL JUEGO DE GRUNDY ==="
  putStrLn "- En cada turno, uno de los dos jugadores elige una fila de monedas (representadas por asteriscos) y luego la divide en dos filas de diferentes tamaños."
  putStrLn "- Debe especificar el tamaño de las dos filas nuevas, asegurándose de que ambas sean de tamaño mayor a cero y diferentes entre sí."
  putStrLn "- El juego termina cuando solo quedan filas de tamaño 1 o 2."
  putStrLn "- Gana el último jugador que pudo hacer un movimiento válido."
  putStrLn "- En modo Libre, la cantidad inicial de monedas puede ser entre 3 y 30."
  putStrLn "\nPresione Enter para volver al menú..."
  _ <- getLine
  showMenu currentMode ""

main :: IO ()
main = do
  putStrLn "\nJUEGO DE GRUNDY"
  putStrLn "==============="
  showMenu Original ""  -- Mensaje vacío inicial