import BigNumber

--mesma estrutura de um fatorial recursivo
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

fibListaBN :: BigNumber -> BigNumber
fibListaBN bign = scanner(show(lista !! fromIntegral(read (output bign) :: Int)))
                  where lista = 0 : 1 : map(\x -> lista !! (x - 1) + lista !! (x - 2)) [2..fromIntegral(read (output bign) :: Int)]

fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN bign = scanner(show(listainf !! fromIntegral(read (output bign) :: Int)))
                          where listainf = map fst (iterate(\(x,y) -> (y,x+y)) (0,1))