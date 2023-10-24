module GeneticPRESIDENT where

import Data.List.Split
import qualified Data.Map as M
import Data.List
import System.Random
import Control.Monad

type Nombre = String

data President = President {
      partidos :: [Nombre]
    , ganador  :: Nombre
    , apoyos   :: [Nombre]
    , estados  :: [Estado]
} deriving (Show, Eq)

-- Devuelve los posibles ganadores de cada estado.
posibles :: President -> M.Map Nombre [Nombre]
posibles presi = M.map (\x -> if ganador presi `elem` x then [ganador presi] else x) aux
    where aux  = M.fromList $ zip (map nombre $ estados presi)
                                  (map posiblesGanadores $ estados presi)

type SeatsDistribution = M.Map Nombre Int

data Estado = Estado {
      nombre       :: Nombre
    , seats        :: Int
    , seatsPartido :: SeatsDistribution
    , seatsNuevos  :: Int
} deriving (Show, Eq)

-- Devuelve los posibles ganadores de un estado
posiblesGanadores :: Estado -> [Nombre]
posiblesGanadores (Estado _ s sP sN) = M.keys posibles
    where max = maximumBy (\x y -> compare (snd x) (snd y)) $ M.toList sP
          posibles = M.filterWithKey (\k x -> x > snd max - sN || x == snd max - sN && ordLex (fst max) k) sP

type Solucion = M.Map Nombre Nombre

-- Para ver si la solucion es valida para nuestra entrada
isValidSol :: Solucion -> President -> Bool 
isValidSol sol presi = M.null $ M.filterWithKey (\k x -> sol M.! k `notElem` x) posi
    where posi = posibles presi

-- makes a random solution from a President instance
initialization :: President -> IO Solucion
initialization presi = do
    let posi = M.toList $ posibles presi
    sol <- initializationAux posi []
    return $ M.fromList sol

initializationAux :: [(Nombre, [Nombre])] -> [(Nombre,Nombre)] -> IO [(Nombre,Nombre)]
initializationAux [] estSol = return estSol
initializationAux ((x,y):xs) estSol = do
    gen <- newStdGen
    let sol1 = (x, takeOne gen y)
    initializationAux xs (sol1:estSol)

-- Evaluates a solution
solValue :: President -> Solucion -> Int
solValue presi sol = winValue seatsDist presi
    where initVal = M.fromList $ zip (partidos presi) [0..]
          estados1 = map nombre $ estados presi
          seats1 = map seats $ estados presi
          statesSeats = M.fromList $ zip estados1 seats1
          seatsDist = auxSolValue (M.toList sol) statesSeats

-- Auxiliar function for solValue
auxSolValue :: [(String, String)] -> M.Map Nombre Int -> M.Map Nombre Int
auxSolValue [] _ = M.empty
auxSolValue (x:xs) seatsState = M.insertWith (+) key1 val1 (auxSolValue xs seatsState)
    where key1 = snd x
          val1 = seatsState M.! fst x

-- Given a Map with the number of seats each candidate gets
-- It outputs the number of seats our candidate will get with the pacts.
winValue :: M.Map Nombre Int -> President -> Int
winValue seatsDist presi = if bested then 0 else total
    where ganador1 = ganador presi
          apoyos1 = apoyos presi
          bested = M.foldrWithKey (\k a b -> seatsDist M.! k > seatsDist M.! ganador1 
                                            && k `elem` apoyos1 
                                            || b) False seatsDist
          filter1 = M.filterWithKey (\k _ -> k == ganador1 || k `elem` apoyos1) seatsDist
          total = M.foldr (+) 0 filter1

-- Making a solution out of 2 other solutions.
makeChild :: Solucion -> Solucion -> StdGen -> Solucion
makeChild sol1 sol2 gen = M.fromList $ makeChildAux (M.toList sol1) (M.toList sol2) gen

-- Auxiliar function of makeChild
makeChildAux :: [(String, String)] -> [(String, String)] -> StdGen -> [(String, String)]
makeChildAux [] _ _ = []
makeChildAux _ [] _ = []
makeChildAux ((x1,x2):xs) ((_,y2):ys) gen  = if pick == 1 
                                             then (x1,x2) : makeChildAux xs ys newGen
                                             else (x1,y2) : makeChildAux xs ys newGen
    where (pick, newGen) = randomR (1 :: Int, 2 :: Int) gen

----------------
-- Mutations. --
----------------

mutFactor :: Double
mutFactor = 0.15


mutation :: Solucion -> President -> IO Solucion
mutation sol presi = do
    let states = estados presi
    gen <- newStdGen
    let win = ganador presi
    let newSol = M.fromList $ mutationAux sol states win gen
    return newSol

mutationAux :: Solucion -> [Estado] -> Nombre -> StdGen -> [(Nombre, Nombre)]
mutationAux _ [] _ _          = []
mutationAux sol (e:est) win gen = (name, newWin) : mutationAux sol est win new2
    where name = nombre e
          posi = posiblesGanadores e
          (muta, new) = random gen
          (new1, new2) = System.Random.split new
          oldWin = sol M.! name
          newWin = if muta < mutFactor && oldWin /= win
                   then takeOneDifferent new1 oldWin posi
                   else oldWin
          

---------------------------
-- Funciones auxiliares. --
---------------------------

-- '<=' para strings
ordLex :: String -> String -> Bool
ordLex [] [] = True
ordLex [] _ = False 
ordLex _ [] = False
ordLex (x:xs) (y:ys) = x <= y || ordLex xs ys

-- A function to take a random element different to the one given.
-- Non empty list.
takeOneDifferent :: StdGen -> Nombre -> [Nombre] -> Nombre
takeOneDifferent _ _ [x] = x
takeOneDifferent ran win posi = newPosi !! new
        where newPosi = delete win posi
              n = length newPosi
              (new, _) = randomR (0, n-1) ran

-- A function to take a random element from a list
-- Non empty list
takeOne :: StdGen -> [Nombre] -> Nombre
takeOne ran posi = posi !! new
    where n = length posi
          (new, _) = randomR (0, n-1) ran

-- Get gaps between probabilites in a list:
-- 5 element list : [1.0, 1.8, 2.4, 2.8, 3.0]
-- Used for selecting the best parents with higher probability.
probabilitiesDown :: Int -> Int -> Float -> [Float]
probabilitiesDown 0 _ _ = []
probabilitiesDown m n f = newF : probabilitiesDown (m-1) n newF
    where newF = f + (fromIntegral m / fromIntegral n)

-- Select one of the parents at random
-- but need to be fed a weighted list
-- modified as the list that probabilitiesDown gives.
randomParents :: [Float] -> Float -> Int -> Int
randomParents [] _ pos = pos
randomParents (x:prob) f pos
    | x < f = randomParents prob f (pos+1)
    | otherwise = pos


--------------------------
-- Parsing de entradas. --
--------------------------

-- Convierte la string de un estado a la estructura de datos Estado
toState :: String -> Estado
toState estado = Estado nombre1 seats1 seatsPartido3 seatsNuevos2
    where x:estado1 = lines estado
          divide1 = words x
          (nombre1, seats1) = (head divide1, (read :: String -> Int) (divide1 !! 2))
          (seatsPartido1, seatsNuevos1) = (init estado1, words (last estado1))
          seatsNuevos2 = (read :: String -> Int) $ seatsNuevos1 !! 2
          seatsPartido2 = map ((\[x,_,y] -> (x, (read :: String -> Int) y)) . words) seatsPartido1
          seatsPartido3 = M.fromList seatsPartido2

totalSeats :: String -> Int
totalSeats input = sum (map seats states)
    where _:_:xs = splitOn "\n\n" input
          states = map toState xs
    


--splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
--splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

geneticPresident :: String -> Int -> Int -> IO Int
geneticPresident input popu iter = do
    -- Input reading.
    let _:i1:xs = splitOn "\n\n" input
    let [_,partidos1,ganador2,apoyos1] = lines i1
    let partidos2 = splitOn "," partidos1
    let apoyos2 = splitOn "," apoyos1
    let estado2 = map toState xs
    let president = President partidos2 ganador2 apoyos2 estado2
    --print president
    --print $ posibles president

    --Initialization
    init <- replicateM popu $ initialization president
    --print init
    --print $ map (solValue president) init

    -- Looping
    let loop 0 sol = (pure . reverse) $ sortOn (solValue president) sol
        loop n sol = do
            let ordSol = reverse $ sortOn (solValue president) sol
            let prob = probabilitiesDown popu popu 0.0
            newPopu <- replicateM (popu-1) $ do
                gen <- newStdGen
                let [next1, next2] = take 2 $ randomRs (0.0 , last prob) gen
                let parent1 = randomParents prob next1 0
                let parent2' = randomParents prob next2 0
                let parent2 = if parent2' == parent1 then parent2' `div` 2 else parent2'
                let child = makeChild (ordSol !! parent1) (ordSol !! parent2) gen

                -- Mutation here

                mutation child president

                --return child

            -- We return the best child too
            loop (n-1) (head ordSol : newPopu)
    
    best <- head <$> loop iter init
    --print best
    return $ solValue president best

--main :: IO ()
--main = do
--    input1 <- readFile "inputs/randomInputs/input1.txt"
--    geneticPresident input1 20 20
