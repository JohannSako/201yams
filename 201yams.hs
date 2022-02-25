{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.List (isPrefixOf)
import Text.Printf (printf)

checkDiceValue :: [Char] -> Bool
checkDiceValue "0" = True
checkDiceValue "1" = True
checkDiceValue "2" = True
checkDiceValue "3" = True
checkDiceValue "4" = True
checkDiceValue "5" = True
checkDiceValue "6" = True
checkDiceValue _ = False

checkDiceValueWithoutZero :: [Char] -> Bool
checkDiceValueWithoutZero "1" = True
checkDiceValueWithoutZero "2" = True
checkDiceValueWithoutZero "3" = True
checkDiceValueWithoutZero "4" = True
checkDiceValueWithoutZero "5" = True
checkDiceValueWithoutZero "6" = True
checkDiceValueWithoutZero _ = False

getOccurenceOfNumber :: [Int] -> Int -> Int
getOccurenceOfNumber [] expected = 0
getOccurenceOfNumber (fst:next) expected
    | fst == expected = 1 + (getOccurenceOfNumber next expected)
    | otherwise = (getOccurenceOfNumber next expected)

makeBinomialLaw :: Int -> Int -> Int -> Float
makeBinomialLaw otherDice x end
    | x == end = 0.0
    | otherwise = (fromIntegral (product[1..otherDice]) / fromIntegral (product[1..x] * (product[1..otherDice - x])) * (1/6)^x  * (5/6)^(otherDice - x)) + (makeBinomialLaw otherDice (x + 1) end)

checkPairProbability :: [Int] -> Int -> IO()
checkPairProbability numbers expected = printf "Chances to get a %d pair: %.2f%%\n" expected ((makeBinomialLaw (5 - (getOccurenceOfNumber numbers expected)) (2 - (if (getOccurenceOfNumber numbers expected) > 2 then 2 else (getOccurenceOfNumber numbers expected))) (6 - (getOccurenceOfNumber numbers expected))) * 100)

checkThreeProbability :: [Int] -> Int -> IO()
checkThreeProbability numbers expected = printf "Chances to get a %d three-of-a-kind: %.2f%%\n" expected ((makeBinomialLaw (5 - (getOccurenceOfNumber numbers expected)) (3 - (if (getOccurenceOfNumber numbers expected) > 3 then 3 else (getOccurenceOfNumber numbers expected))) (6 - (getOccurenceOfNumber numbers expected))) * 100)

checkFourProbability :: [Int] -> Int -> IO()
checkFourProbability numbers expected = printf "Chances to get a %d four-of-a-kind: %.2f%%\n" expected ((makeBinomialLaw (5 - (getOccurenceOfNumber numbers expected)) (4 - (if (getOccurenceOfNumber numbers expected) > 4 then 3 else (getOccurenceOfNumber numbers expected))) (6 - (getOccurenceOfNumber numbers expected))) * 100)

computeFull :: Int -> Int -> Float
computeFull full dice = (fromIntegral (product[1..dice]) / (fromIntegral (product[1..full]) * fromIntegral (product[1..(dice - full)])) / 6^dice) * 100

checkFullProbability :: [Int] -> Int -> Int -> IO()
checkFullProbability numbers fst_expected snd_expected
    | fst_expected == snd_expected = exitWith (ExitFailure 84)
    | otherwise = printf "Chances to get a %d full of %d: %.2f%%\n" fst_expected snd_expected (computeFull (3 - (if (getOccurenceOfNumber numbers fst_expected) > 3 then 3 else (getOccurenceOfNumber numbers fst_expected))) (5 - ((if (getOccurenceOfNumber numbers fst_expected) > 3 then 3 else (getOccurenceOfNumber numbers fst_expected)) + (if (getOccurenceOfNumber numbers snd_expected) > 2 then 2 else (getOccurenceOfNumber numbers snd_expected)))))

isInList :: [Int] -> Int -> Int
isInList [] value = 0
isInList (first:next) value
    | first == value = 1
    | otherwise = isInList next value

computeStraight :: [Int] -> Int -> Float
computeStraight numbers dice = (fromIntegral (product[1..dice])) / 6^dice

checkStraightProbability :: [Int] -> Int -> IO()
checkStraightProbability numbers 5 = printf "Chances to get a 5 straight: %.2f%%\n" ((computeStraight numbers (5 - (isInList numbers 1) - (isInList numbers 2) - (isInList numbers 3) - (isInList numbers 4) - (isInList numbers 5))) * 100)
checkStraightProbability numbers 6 = printf "Chances to get a 6 straight: %.2f%%\n" ((computeStraight numbers (5 - (isInList numbers 2) - (isInList numbers 3) - (isInList numbers 4) - (isInList numbers 5) - (isInList numbers 6))) * 100)
checkStraightProbability numbers _ = exitWith (ExitFailure 84)

checkYamsProbability :: [Int] -> Int -> IO()
checkYamsProbability numbers expected = printf "Chances to get a %d yams: %.2f%%\n" expected ((makeBinomialLaw (5 - (getOccurenceOfNumber numbers expected)) (5 - (getOccurenceOfNumber numbers expected)) (6 - (getOccurenceOfNumber numbers expected))) * 100)

findCombinationExpectetion :: [Int] -> String -> IO()
findCombinationExpectetion numbers rule
    | length rule == 6 && isPrefixOf "pair_" rule && (checkDiceValueWithoutZero [rule!!5]) = checkPairProbability numbers (read [rule!!5] :: Int)
    | length rule == 7 && isPrefixOf "three_" rule && (checkDiceValueWithoutZero [rule!!6]) = checkThreeProbability numbers (read [rule!!6] :: Int)
    | length rule == 6 && isPrefixOf "four_" rule && (checkDiceValueWithoutZero [rule!!5]) = checkFourProbability numbers (read [rule!!5] :: Int)
    | length rule == 8 && isPrefixOf "full_" rule && (checkDiceValueWithoutZero [rule!!5]) && [rule!!6] == "_" && (checkDiceValueWithoutZero [rule!!7]) = checkFullProbability numbers (read [rule!!5] :: Int) (read [rule!!7] :: Int)
    | length rule == 10 && isPrefixOf "straight_" rule && (checkDiceValueWithoutZero [rule!!9]) = checkStraightProbability numbers (read [rule!!9] :: Int)
    | length rule == 6 && isPrefixOf "yams_" rule && (checkDiceValueWithoutZero [rule!!5]) = checkYamsProbability numbers (read [rule!!5] :: Int)
    | otherwise = exitWith (ExitFailure 84)

checkArguments :: String -> String -> String -> String -> String -> String -> IO()
checkArguments d1 d2 d3 d4 d5 c
    | not (checkDiceValue d1) = exitWith (ExitFailure 84)
    | not (checkDiceValue d2) = exitWith (ExitFailure 84)
    | not (checkDiceValue d3) = exitWith (ExitFailure 84)
    | not (checkDiceValue d4) = exitWith (ExitFailure 84)
    | not (checkDiceValue d5) = exitWith (ExitFailure 84)
    | otherwise = findCombinationExpectetion ([read d1 :: Int, read d2 :: Int, read d3 :: Int, read d4 :: Int, read d5 :: Int]) (c)

printHelper :: IO()
printHelper = putStrLn ("USAGE\n"
                     ++ "    ./201yams d1 d2 d3 d4 d5 c\n\n"
                     ++ "DESCRIPTION\n"
                     ++ "    d1      value of the first die (0 if not thrown)\n"
                     ++ "    d2      value of the second die (0 if not thrown)\n"
                     ++ "    d3      value of the third die (0 if not thrown)\n"
                     ++ "    d4      value of the fourth die (0 if not thrown)\n"
                     ++ "    d5      value of the fifth die (0 if not thrown)\n"
                     ++ "    c       expected combination")

main :: IO()
main = do
    args <- getArgs
    if length args == 6
        then checkArguments (args!!0) (args!!1) (args!!2) (args!!3) (args!!4) (args!!5)
        else 
            if length args == 1 && (args!!0) == "-h"
            then printHelper
            else exitWith (ExitFailure 84)