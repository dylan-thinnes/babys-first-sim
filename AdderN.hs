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
data Input = Input { size :: Int, cin :: Bool, x :: Int, y :: Int }
data Result = Result
    { input :: Input
    , expectedInt :: Int
    , expected :: ([WireState], WireState)
    , real :: ([WireState], WireState)
    , matching :: Bool
    }

testAll :: Int -> IO ()
testAll size = do
    let inputs = do
        cin <- [True, False]
        x <- [0..bound size]
        y <- [0..bound size]
        pure $ Input {..}
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
    putStrLn $ unwords ["Inputs:", show cin, show x, show y]
    putStrLn $ unwords ["Expected Sum:", show expectedInt]
    putStrLn $ unwords ["Expected:", show expected]
    putStrLn $ unwords ["Real:    ", show real]
    putStrLn $ unwords ["Match:", show matching]

-- Test the adder with single values
testOne :: Input -> Result
testOne input@(Input {..}) =
    let expectedInt = x + y + fromEnum cin
        expected = (map WireState $ toBits size $ fromIntegral expectedInt, WireState $ expectedInt > bound size)
        real = runOne size cin (fromIntegral x) (fromIntegral y)
        matching = real == expected
     in Result {..}

-- Run the adder with single values
runOne :: Int -> Bool -> Integer -> Integer -> ([WireState], WireState)
runOne size cin x y =
    let bitsX = toBits size x
        bitsY = toBits size y
        circuit = runAdderN
        (state, _) = stepClean $ runCircuitM circuit
        out = wireFromState state <$> withPrefixes (const WireId) "test-sum" bitsX
        cout = wireFromState state "test-cout"

        abSums = vzip3 (withPrefixes unsafe "test-x" bitsX)
                       (withPrefixes unsafe "test-y" bitsY)
                       (withPrefixes unsafe "test-sum" $ repeat False)

        runAdderN = boolify adderN 
            (  unsafe cin "test-cin"
            :> Compose abSums
            :> unsafe False "test-cout"
            :> HNil
            )

     in (out, cout)
