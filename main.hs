import Parsing
import System.Random (randomRIO)
import System.Control.ANSI

printRow :: Int -> IO ()
printRow 1 = do putChar '*'
printRow n = do putChar '*'
                putChar ' '
                printRow (n - 1)

main :: IO ()
main = do
    n <- randomRIO (1, 10) :: IO Int
    printRow n
    putChar '\n'

printRows :: [Int] -> IO ()
printRows [] = return ()
printRows (x:xs) = do
    printRow x
    putChar '\n'
    printRows xs