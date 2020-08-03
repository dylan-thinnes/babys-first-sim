{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module AdderN where

import Sim
import Data.Functor.Compose
import Data.List (partition)
import Data.Bits

-- TESTING
main :: IO ()
main = testAll 6

-- Turn an integer into bits
toBits :: Int -> Integer -> [Bool]
toBits size n = testBit n <$> [size-1,size-2..0]

bound :: Int -> Int
bound bits = 2 ^ bits - 1

withPrefixes :: (a -> String -> b) -> String -> [a] -> [b]
withPrefixes f prefix as = zipWith f as $ (prefix <>) . show <$> ([0..] :: [Integer])

-- TESTING THE ADDER
data Input = Input { size :: Int, inp :: Integer }
data Result = Result
    { input :: Input
    , expected :: WireState
    , real :: WireState
    , matching :: Bool
    }

testAll :: Int -> IO ()
testAll size = do
    let inputs = map (Input size) [0..fromIntegral $ bound size]
    let results = map testOne inputs
    mapM_ printResult results
    putStrLn "FINAL REPORT:"
    let (valid, invalid) = partition matchingResult results
    putStrLn $ unwords ["Valid:", show $ length valid]
    putStrLn $ unwords ["Invalid:", show $ length invalid]
    mapM_ printResult invalid

-- Creating and handling results
matchingResult :: Result -> Bool
matchingResult Result {..} = matching

printResult :: Result -> IO ()
printResult Result {..} = do
    let Input {..} = input
    putStrLn $ unwords ["Inputs:", show inp, show size, show (bound size)]
    putStrLn $ unwords ["Expected:", show expected]
    putStrLn $ unwords ["Real:    ", show real]
    putStrLn $ unwords ["Match:", show matching]

-- Test the adder with single values
testOne :: Input -> Result
testOne input@(Input {..}) =
    let expected = WireState $ fromIntegral inp == bound size
        real = runOne size inp
        matching = real == expected
     in Result {..}

-- Run the adder with single values
runOne :: Int -> Integer -> WireState
runOne size inp =
    let inpWires = withPrefixes unsafe "test-x" $ toBits size inp
        circuit = boolify andN 
            (  inpWires
            :> unsafe False "test-out"
            :> HNil
            )
        (state, _) = stepClean $ runCircuitM circuit
     in wireFromState state "test-out"

