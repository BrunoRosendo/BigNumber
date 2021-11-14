module BigNumber (BigNumber, scanner, output, somaBN, subBN) where
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



-- AUXILIARY ABSOLUTENUM FUNCTIONS

{-
Two AbsoluteNums are normalizing by making their lists the same size and reverting them, to make calculations easier.
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


isGreaterThan :: AbsoluteNum -> AbsoluteNum -> Bool
isGreaterThan a b
                | length a /= length b = length a > length b
                | null cmp = False -- They're equal
                | otherwise = uncurry (>) (head cmp) -- check greatest different digit
                where cmp = dropWhile (uncurry (==)) (zip a b)




-- AUXILIARY SUM FUNCTIONS


sumAbsolute :: AbsoluteNum -> AbsoluteNum -> AbsoluteNum
sumAbsolute a b = getResult (sumCarries (zipWith sumWithCarry normalizedA normalizedB))
-- The zip combines the modular sum of the digits with the corresponding carry
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
Subtracts a smaller AbsoluteNum (2nd argument) from a greater AbsoluteNUm (1st argument)
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

