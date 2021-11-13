module BigNumber (BigNumber, scanner, output, somaBN) where
import Data.Char ( intToDigit, digitToInt )

-- TYPE DEFINITION

{-
Absolute value of a number, represented by a list of its decimal digits
-}
type AbsoluteNum = [Int]

{-
Bool -> True if it's a negative number and False otherwise
AbsoluteNum -> Absolute value of the number (list of the number's digits)
-}
type BigNumber = (Bool, AbsoluteNum)



-- INPUT/OUTPUT FUNCTIONS

{-
Converts a string into a BigNumber. A negative number is represented using a '-' at the start of the string
-}
scanner :: String -> BigNumber
scanner numString
              | head numString == '-' = (True, [digitToInt c | c <- tail numString])
              | otherwise = (False, [digitToInt c | c <- numString])

{-
Converts a BigNumber into a string. A negative number is represented using a '-' at the start of the string
-}
output :: BigNumber -> String
output bigNum
          | isNegative = "-" ++ numString
          | otherwise = numString
          where isNegative = fst bigNum
                numList = snd bigNum
                numString = [intToDigit n | n <- numList]



-- AUXILIARY SUM FUNCTIONS

sumAbsolute :: AbsoluteNum -> AbsoluteNum -> AbsoluteNum
sumAbsolute a b = getSum (sumCarries (zipWith sumWithCarry normalizedA normalizedB))
-- The zip combines the modular sum of the digits with the corresponding carry

  where lenDiff = length a - length b
        normalizedA = reverse a ++ replicate (-lenDiff) 0 -- The numbers should have the same size
        normalizedB = reverse b ++ replicate lenDiff 0 -- They are reversed to make calculations easier

sumWithCarry :: Int -> Int -> (Int, Int)
sumWithCarry x y = (total`mod`10, total`div`10)
  where total = x + y

{-
Uses the carries calculated previously and adds them in the corresponding digit
-}
sumCarries :: [(Int, Int)] -> [(Int, Int)]
sumCarries xs = foldl sumCarry [] (xs ++ [(0, 0)])

  where sumCarry :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
        sumCarry [] pair = [pair]
        sumCarry acc (soma, carry) = init acc ++ [(prevSoma, 0)] ++ [sumWithCarry (soma + carry*10) prevCarry]
          where (prevSoma, prevCarry) = last acc

{-
Extracts the digits of the sum and puts them in the right order again
-}
getSum :: [(Int, Int)] -> AbsoluteNum
getSum xs
        | head num == 0 = tail num
        | otherwise = num
        where num = reverse [fst tup | tup <- xs]


-- MAIN ARITHMETIC FUNCTIONS


somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN a b
        | isANeg == isBNeg = (isANeg, sumAbsolute absoluteA absoluteB)
        | otherwise = a
        where isANeg = fst a
              isBNeg = fst b
              absoluteA = snd a
              absoluteB = snd b
