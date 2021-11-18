

--mesma estrutura de um fatorial recursivo
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-1) + fibRec(n-2)

fibLista :: Int -> Int
fibLista n = lista !! n
            where lista = 0 : 1 : map(\x -> lista !! (x - 1) + lista !! (x - 2)) [2..n]

fibListaInfinita :: Int -> Int
fibListaInfinita n = listainf !! n
            where listainf = map fst (iterate(\(x,y) -> (y,x+y)) (0,1))