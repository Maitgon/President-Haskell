module InputGen where

import System.Random

nCandidates :: Int
nCandidates = 20

nSupporters :: Int 
nSupporters = 5

totalStates :: Int
totalStates = 100

candidatesString :: Int -> Int -> String
candidatesString total supp = "Candidates:\n" ++ bottomPart
    where bottomPart = candidatesStringAux total supp 'A'

nextChar :: Char -> Char
nextChar a = toEnum (fromEnum a + 1)

candidatesStringAux :: Int -> Int -> Char -> String
candidatesStringAux total supp curr
    | total == 1 = [curr,'\n','A','\n'] ++ candidatesStringAux 0 supp 'B'
    | total == 0 && supp /= 1 = [curr, ','] ++ candidatesStringAux 0 (supp-1) (nextChar curr)
    | total == 0 && supp == 1 = [curr]
    | otherwise = [curr,','] ++ candidatesStringAux (total - 1) supp (nextChar curr)


formState :: Int -> Int -> StdGen -> (String, StdGen)
formState n total gen = (firstPart ++ secondPart, snd $ split new)
    where firstPart  = "\nState" ++ show n ++ " : " ++ show num ++ " seats\n"
          (num, new) = uniformR (10 :: Int, 20 :: Int) gen
          secondPart = unlines $ formStateAux total new 'A'

formStateAux :: Int -> StdGen -> Char -> [String]
formStateAux total gen curr
    | total == 0 = ["Z : " ++ show free]
    | otherwise  = ([curr] ++ " : " ++ show votes) : formStateAux (total-1) new (nextChar curr)
        where (votes, new) = if total == nCandidates
                             then uniformR (10 :: Int, 25 :: Int) gen
                             else uniformR (10 :: Int, 30 :: Int) gen
              (free, _)    = uniformR (0 :: Int, 10 :: Int) gen


formStateAll :: Int -> StdGen -> String
formStateAll n gen
    | n == totalStates = currState
    | otherwise        = currState ++ formStateAll (n+1) new
        where (currState, new) = formState n nCandidates gen





main :: IO ()
main = do
    let start = "Input 8.\n\n"
    let secondPart = candidatesString nCandidates nSupporters
    gen <- newStdGen
    let states = formStateAll 1 gen
    let final = start ++ secondPart ++ "\n" ++ states
    writeFile "inputs/randomInputs/input8.txt" final