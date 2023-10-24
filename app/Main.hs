module Main where

import qualified Data.Map as M
import GeneticPRESIDENT
import Control.Monad

main :: IO ()
main = do

    input1 <- readFile "inputs/randomInputs/input1.txt"
    input2 <- readFile "inputs/randomInputs/input2.txt"
    input3 <- readFile "inputs/randomInputs/input3.txt"
    input4 <- readFile "inputs/randomInputs/input4.txt"
    input5 <- readFile "inputs/randomInputs/input5.txt"
    input6 <- readFile "inputs/randomInputs/input6.txt"
    input7 <- readFile "inputs/randomInputs/input7.txt"
    input8 <- readFile "inputs/randomInputs/input8.txt"

    putStrLn $ show mutFactor ++ "% mutation"
    {-
    -- Input 1:
    -- Candidates: 10
    -- Supporters: 5
    -- States: 20

    sols1 <- replicateM 50 $ do
        geneticPresident input1 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 1. --"
    putStrLn "--------------"

    let sols1' = filter (/= 0) sols1
    let totalSeats1 = totalSeats input1

    putStrLn $ "\ntotalSeats: " ++ show totalSeats1

    putStrLn $ "\nMin: " ++ show (minimum sols1')
    putStrLn $ "Max: " ++ show (maximum sols1')
    putStrLn $ "Zeroes: " ++ show (length sols1 - length sols1')
    putStrLn $ "Mean: " ++ show (mean sols1')
    putStrLn $ "Variance: " ++ show (variance sols1')

    -- Input 2:
    -- Candidates: 6
    -- Supporters: 2
    -- States: 20

    sols2 <- replicateM 50 $ do
        geneticPresident input2 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 2. --"
    putStrLn "--------------"

    let sols2' = filter (/= 0) sols2
    let totalSeats2 = totalSeats input2

    putStrLn $ "\ntotalSeats: " ++ show totalSeats2

    putStrLn $ "\nMin: " ++ show (minimum sols2')
    putStrLn $ "Max: " ++ show (maximum sols2')
    putStrLn $ "Zeroes: " ++ show (length sols2 - length sols2')
    putStrLn $ "Mean: " ++ show (mean sols2')
    putStrLn $ "Variance: " ++ show (variance sols2')
    -}
    -- Input 3:
    -- Candidates: 20
    -- Supporters: 7
    -- States: 40

    sols3 <- replicateM 1000 $ do
        geneticPresident input3 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 3. --"
    putStrLn "--------------"

    let sols3' = filter (/= 0) sols3
    let totalSeats3 = totalSeats input3

    putStrLn $ "\ntotalSeats: " ++ show totalSeats3

    putStrLn $ "\nMin: " ++ show (minimum sols3')
    putStrLn $ "Max: " ++ show (maximum sols3')
    putStrLn $ "Zeroes: " ++ show (length sols3 - length sols3')
    putStrLn $ "Mean: " ++ show (mean sols3')
    putStrLn $ "Variance: " ++ show (variance sols3')

    -- Input 4:
    -- Candidates: 8
    -- Supporters: 5
    -- States: 30
    {-
    sols4 <- replicateM 50 $ do
        geneticPresident input4 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 4. --"
    putStrLn "--------------"

    let sols4' = filter (/= 0) sols4
    let totalSeats4 = totalSeats input4

    putStrLn $ "\ntotalSeats: " ++ show totalSeats4

    putStrLn $ "\nMin: " ++ show (minimum sols4')
    putStrLn $ "Max: " ++ show (maximum sols4')
    putStrLn $ "Zeroes: " ++ show (length sols4 - length sols4')
    putStrLn $ "Mean: " ++ show (mean sols4')
    putStrLn $ "Variance: " ++ show (variance sols4')

    -- Input 5:
    -- Candidates: 14
    -- Supporters: 2
    -- States: 40

    sols5 <- replicateM 50 $ do
        geneticPresident input5 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 5. --"
    putStrLn "--------------"

    let sols5' = filter (/= 0) sols5
    let totalSeats5 = totalSeats input5

    putStrLn $ "\ntotalSeats: " ++ show totalSeats5

    putStrLn $ "\nMin: " ++ show (minimum sols5')
    putStrLn $ "Max: " ++ show (maximum sols5')
    putStrLn $ "Zeroes: " ++ show (length sols5 - length sols5')
    putStrLn $ "Mean: " ++ show (mean sols5')
    putStrLn $ "Variance: " ++ show (variance sols5')
    -}
    -- Input 6:
    -- Candidates: 30
    -- Supporters: 5
    -- States: 45

    sols6 <- replicateM 1000 $ do
        geneticPresident input6 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 6. --"
    putStrLn "--------------"

    let sols6' = filter (/= 0) sols6
    let totalSeats6 = totalSeats input6

    putStrLn $ "\ntotalSeats: " ++ show totalSeats6

    putStrLn $ "\nMin: " ++ show (minimum sols6')
    putStrLn $ "Max: " ++ show (maximum sols6')
    putStrLn $ "Zeroes: " ++ show (length sols6 - length sols6')
    putStrLn $ "Mean: " ++ show (mean sols6')
    putStrLn $ "Variance: " ++ show (variance sols6')

    -- Input 7:
    -- Candidates: 25
    -- Supporters: 10
    -- States: 70

    sols7 <- replicateM 1000 $ do
        geneticPresident input7 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 7. --"
    putStrLn "--------------"

    let sols7' = filter (/= 0) sols7
    let totalSeats7 = totalSeats input7

    putStrLn $ "\ntotalSeats: " ++ show totalSeats7

    putStrLn $ "\nMin: " ++ show (minimum sols7')
    putStrLn $ "Max: " ++ show (maximum sols7')
    putStrLn $ "Zeroes: " ++ show (length sols7 - length sols7')
    putStrLn $ "Mean: " ++ show (mean sols7')
    putStrLn $ "Variance: " ++ show (variance sols7')

    -- Input 8:
    -- Candidates: 20
    -- Supporters: 5
    -- States: 100

    sols8 <- replicateM 1000 $ do
        geneticPresident input8 20 500
    
    putStrLn "\n\n--------------"
    putStrLn "-- Input 8. --"
    putStrLn "--------------"

    let sols8' = filter (/= 0) sols8
    let totalSeats8 = totalSeats input8

    putStrLn $ "\ntotalSeats: " ++ show totalSeats8

    putStrLn $ "\nMin: " ++ show (minimum sols8')
    putStrLn $ "Max: " ++ show (maximum sols8')
    putStrLn $ "Zeroes: " ++ show (length sols8 - length sols8')
    putStrLn $ "Mean: " ++ show (mean sols8')
    putStrLn $ "Variance: " ++ show (variance sols8')

frequency :: [Int] -> M.Map Int Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

prettyPrint :: M.Map Int Int -> String
prettyPrint m = M.foldlWithKey f "Appearances:\n" m
    where f result k a = result ++ "" ++ show k ++ " -> " ++ show a ++ "\n"

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

variance :: [Int] -> Float
variance xs = sum (zipWith (*) aux2 aux2) / fromIntegral (length xs)
    where meant = mean xs
          aux1 = map fromIntegral xs
          aux2 = [x - meant | x <- aux1]
