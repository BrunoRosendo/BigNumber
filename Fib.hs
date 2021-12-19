import BigNumber

-- REGULAR FIBONACCI FUNCTIONS (EX1)

--mesma estrutura de um fatorial recursivo
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n   | n < 0 = error "negative numbers not accepted"
           | otherwise = fibRec(n-1) + fibRec(n-2)

fibLista :: (Integral a) => a -> a
fibLista n   | n < 0 = error "negative numbers not accepted"
             | otherwise = lista !! fromIntegral n
  where lista = 0 : 1 : map(\x -> lista !! (x - 1) + lista !! (x - 2)) [2..fromIntegral n]


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n
              | n < 0 = error "negative numbers not accepted"
              | otherwise = listainf !! fromIntegral n
   where listainf = map fst (iterate(\(x,y) -> (y,x+y)) (0,1))



-- BIGNUMBER FIBONACCI FUNCTION(EX3)

fibRecBN :: BigNumber -> BigNumber
fibRecBN (False,[0]) = (False,[0])
fibRecBN (False,[1]) = (False,[1])
fibRecBN bign   | fst bign = error "negative numbers not accepted"
                | otherwise = somaBN (fibRecBN(subBN bign (scanner "1"))) (fibRecBN(subBN bign (scanner "2")))


fibListaBN :: BigNumber -> BigNumber
fibListaBN bign  | fst bign = error "negative numbers not accepted"
                 | otherwise = nthBN lista bign
                 where lista = scanner "0" : scanner "1" : map(\x -> somaBN 
                                          (nthBN lista (subBN x (scanner "1")))
                                          (nthBN lista (subBN x (scanner "2"))))
                                          (rangeBN (scanner "2") bign)


fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN bign
                  | fst bign = error "negative numbers not accepted" -- fst bign checks for negative
                  | otherwise = nthBN listainfbn bign
  where listainfbn = map fst
          (iterate
            (\ (x, y) -> (y, somaBN x y)) ((False, [0]), (False, [1])))


-- AUXILIARY BIGNUMBER FUNCTIONS

nthBN :: [BigNumber] -> BigNumber -> BigNumber
nthBN xs bign | fst bign =  error "negative index"
nthBN [] _  = error "index too large"
nthBN (x:_) (False,[0]) = x
nthBN (_:xs) bign = nthBN xs (subBN bign (scanner "1"))

rangeBN :: BigNumber -> BigNumber -> [BigNumber]
rangeBN cur max
        | isGreaterThan maxAbs curAbs = cur :rangeBN (somaBN cur (scanner "1")) max
        | otherwise = [max]
  where curAbs = snd cur
        maxAbs = snd max
