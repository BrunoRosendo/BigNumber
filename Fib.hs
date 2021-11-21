import BigNumber

--mesma estrutura de um fatorial recursivo
--basta introduzir os 2 casos base e
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-1) + fibRec(n-2)

fibLista :: (Integral a) => a -> a
fibLista n = lista !! fromIntegral(n)
            where lista = 0 : 1 : map(\x -> lista !! (x - 1) + lista !! (x - 2)) [2..fromIntegral(n)]

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = listainf !! fromIntegral(n)
                     where listainf = map fst (iterate(\(x,y) -> (y,x+y)) (0,1))                                                                                                                     


fibRecBN :: BigNumber -> BigNumber
fibRecBN (False,[0]) = (False,[0])
fibRecBN (False,[1]) = (False,[1])
fibRecBN bign = somaBN (fibRecBN(subBN bign (scanner "1"))) (fibRecBN(subBN bign (scanner "2")))


nthBN :: [BigNumber] -> BigNumber -> BigNumber
nthBN xs bign | (fst bign) =  error " negative index"
nthBN [] _  = error "index too large"
nthBN (x:_) (False,[0]) =  x
nthBN (_:xs) bign = nthBN xs (subBN bign (scanner "1"))


--fibListaBN :: BigNumber -> BigNumber
--fibListaBN bign = nthBN listabn bign
                  --where listabn = (False,[0]) : (False,[1])  : map(\x -> nthBN listabn (subBN x (scanner "1")) + nthBN listabn (subBN x (scanner "1")) [2..fromIntegral(read (output bign) :: Int)]

        

fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN bign = nthBN listainfbn bign
                          where listainfbn = [x | x <- map fst (iterate(\(x,y) -> (y,somaBN x y)) ((False,[0]),(False,[1])))]