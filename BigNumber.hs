module BigNumber (BigNumber, scanner, output, somaBN, subBN, mulBN, divBN, safeDivBN) where
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
              | snd num == [0] = (False, [0]) -- A -0 is stored as a regular 0
              | otherwise = num

  where num = if head numString == '-'
              then (True, trimZeros [digitToInt c | c <- tail numString])
              else (False, trimZeros [digitToInt c | c <- numString])

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



-- AUXILIARY ABSOLUTENUM FUNCTIONS

{-
Removes trailing zeros from the integer list
-}
trimZeros :: AbsoluteNum -> AbsoluteNum
trimZeros xs
        | null num = [0]
        | otherwise = num
        where num = dropWhile (==0) xs

{-
Two AbsoluteNums are normalizing by making their lists the same size and reverting them, to make calculations easier
-}
normalize :: AbsoluteNum -> AbsoluteNum -> (AbsoluteNum, AbsoluteNum)
normalize a b = (normalizedA, normalizedB)
  where lenDiff = length a - length b
        normalizedA = reverse a ++ replicate (-lenDiff) 0
        normalizedB = reverse b ++ replicate lenDiff 0

{-
Extracts the digits of a sum/sub and puts them in the right order again, while removing carries and excessive 0s
-}
getResult :: [(Int, Int)] -> AbsoluteNum
getResult xs
        | null num = [0]
        | otherwise = num
        where num = dropWhile (==0) (reverse [fst tup | tup <- xs])


{-
Compares two AbsoluteNums and returns True if the first one is greater than the second
-}
isGreaterThan :: AbsoluteNum -> AbsoluteNum -> Bool
isGreaterThan a b
                | length a /= length b = length a > length b
                | null cmp = False -- They're equal
                | otherwise = uncurry (>) (head cmp) -- check greatest different digit
                where cmp = dropWhile (uncurry (==)) (zip a b)




-- AUXILIARY SUM FUNCTIONS


sumAbsolute :: AbsoluteNum -> AbsoluteNum -> AbsoluteNum
-- The zip combines the modular sum of the digits with the corresponding carry
sumAbsolute a b = getResult (sumCarries (zipWith sumWithCarry normalizedA normalizedB))
  where (normalizedA, normalizedB) = normalize a b


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



-- AUXILIARY SUB FUNCTIONS

{-
Subtracts a smaller AbsoluteNum (2nd argument) from a greater AbsoluteNum (1st argument)
-}
subAbsolute :: AbsoluteNum -> AbsoluteNum -> AbsoluteNum
subAbsolute a b = getResult (subCarries (zipWith subWithCarry normalizedA normalizedB))
  where (normalizedA, normalizedB) = normalize a b


subWithCarry :: Int -> Int -> (Int, Int)
subWithCarry x y
              | diff >= 0 = (diff, 0)
              | otherwise = (diff + 10, 1)
              where diff = x - y


{-
Uses the carries calculated previously and subtracts them in the corresponding digit
-}
subCarries :: [(Int, Int)] -> [(Int, Int)]
subCarries = foldl subCarry []

  where subCarry :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
        subCarry [] pair = [pair]
        subCarry acc (sub, carry) = init acc ++ [(prevSub, 0)] ++ [subWithCarry (sub - carry*10) prevCarry]
          where (prevSub, prevCarry) = last acc




-- AUXILIARY MUL FUNCTIONS

{-
Multiplies an AbsoluteNum by another one, by multiplying each of the latter's digits and then adding everything accordingly
-}
mulAbsolute :: AbsoluteNum -> AbsoluteNum -> AbsoluteNum
-- The reverse allows us to reuse sumAbsolute
mulAbsolute a b = foldl (\acc num -> sumAbsolute acc (reverse num)) [0] digitMuls
  where normalizedA = reverse a
        normalizedB = reverse b
        digitMuls = mulAllDigits normalizedA normalizedB


mulWithCarry :: Int -> Int -> (Int, Int)
mulWithCarry x y = (total`mod`10, total`div`10)
  where total = x * y

{-
Multiplies an AbsoluteNum by a given digit
-}
mulByDigit :: AbsoluteNum -> Int -> AbsoluteNum
-- Reverse it again to continue making calculations afterwards
mulByDigit num dig = reverse (getResult (sumCarries (map (`mulWithCarry` dig) num)))

{-
Multiplies the first number by all the digits in the second one and returns the results in a list of AbsoluteNums
-}
mulAllDigits :: AbsoluteNum -> AbsoluteNum -> [AbsoluteNum]
-- The 0's at the start account for the position of the digit being multiplied
mulAllDigits a b = [replicate i 0 ++ (rawMultiplications !! i) | i <- [0..end]]
  where rawMultiplications = map (\x -> a `mulByDigit` x) b
        end = length rawMultiplications - 1



-- AUXILIARY DIV FUNCTIONS

{-
Divides two AbsoluteNums by recursively subtracting the denominator from the numerator and counting the quotient,
until the denominator is greater than the numerator
-}
divAbsolute :: AbsoluteNum -> AbsoluteNum -> (AbsoluteNum, AbsoluteNum)
divAbsolute num denom
            | isDenomGreater = ([0], num)
            | otherwise = (sumAbsolute [1] nextQ, nextR)
  where isDenomGreater = isGreaterThan denom num
        (nextQ, nextR) = divAbsolute (subAbsolute num denom) denom



-- MAIN ARITHMETIC FUNCTIONS


somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN a b
        | isANeg == isBNeg = (isANeg, sumAbsolute absoluteA absoluteB)
        | isAGreater = (isANeg, subAbsolute absoluteA absoluteB)
        | isBGreater = (isBNeg, subAbsolute absoluteB absoluteA)
        | otherwise = (False, [0]) -- Simetric

  where isANeg = fst a
        isBNeg = fst b
        absoluteA = snd a
        absoluteB = snd b
        isAGreater = isGreaterThan absoluteA absoluteB
        isBGreater = isGreaterThan absoluteB absoluteA

subBN :: BigNumber -> BigNumber -> BigNumber
subBN a b
        | isANeg /= isBNeg = (isANeg, sumAbsolute absoluteA absoluteB)
        | isAGreater = (isANeg, subAbsolute absoluteA absoluteB)
        | isBGreater = (isBNeg, subAbsolute absoluteB absoluteA)
        | otherwise = (False, [0]) -- Equal

  where isANeg = fst a
        isBNeg = fst b
        absoluteA = snd a
        absoluteB = snd b
        isAGreater = isGreaterThan absoluteA absoluteB
        isBGreater = isGreaterThan absoluteB absoluteA


mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN a b
        | isANeg == isBNeg = (False, result)
        | otherwise = (True, result)

  where isANeg = fst a
        isBNeg = fst b
        result = mulAbsolute (snd a) (snd b)

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN a b = ((False, resultQ), (False, resultR))
  where (resultQ, resultR) = divAbsolute (snd a) (snd b)


safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN a b
        | b == scanner "0" = Nothing
        | otherwise = Just (divBN a b)
