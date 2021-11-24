import BigNumber

--mesma estrutura de um fatorial recursivo
--basta introduzir os 2 casos base e
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n   | n < 0 = error "negative numbers not accepted"
           | otherwise = fibRec(n-1) + fibRec(n-2)

fibLista :: (Integral a) => a -> a
fibLista n   | n < 0 = error "negative numbers not accepted"
             |otherwise = lista !! fromIntegral(n)
			  where lista = 0 : 1 : map(\x -> lista !! (x - 1) + lista !! (x - 2)) [2..fromIntegral(n)]


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n   | n < 0 = error "negative numbers not accepted"
					 | otherwise = listainf !! fromIntegral(n)
					   where listainf = map fst (iterate(\(x,y) -> (y,x+y)) (0,1))


fibRecBN :: BigNumber -> BigNumber
fibRecBN (False,[0]) = (False,[0])
fibRecBN (False,[1]) = (False,[1])
fibRecBN bign   | fst bign = error "negative numbers not accepted"
                | otherwise = somaBN (fibRecBN(subBN bign (scanner "1"))) (fibRecBN(subBN bign (scanner "2")))


nthBN :: [BigNumber] -> BigNumber -> BigNumber
nthBN xs bign | (fst bign) =  error " negative index"
nthBN [] _  = error "index too large"
nthBN (x:_) (False,[0]) =  x
nthBN (_:xs) bign = nthBN xs (subBN bign (scanner "1"))

createList :: BigNumber  -> BigNumber  -> [BigNumber ]
createList cur max
        | isGreaterThan abs2 abs1 = cur :createList (somaBN cur (scanner "1")) max
        | otherwise = [max]
		where abs1 = snd cur
		      abs2 = snd max

fibListaBN :: BigNumber -> BigNumber
fibListaBN bign  | fst bign = error "negative numbers not accepted"
                 |otherwise = nthBN lista bign
                 where aux = createList (scanner "2") bign
                       lista = scanner "0" : scanner "1" : map(\x -> somaBN (nthBN lista (subBN x (scanner "1"))) (nthBN lista (subBN x (scanner "2")))) aux


fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN bign   | fst bign = error "negative numbers not accepted"
                          |otherwise = nthBN listainfbn bign
                           where listainfbn = [x | x <- map fst (iterate(\(x,y) -> (y,somaBN x y)) ((False,[0]),(False,[1])))]