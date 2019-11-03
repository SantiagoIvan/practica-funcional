import Text.Show.Functions
f1 :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
f1 a1 a2 a3 = a1 a3 == a2 a3

f2 :: (Eq b , Num a) => (a -> b) -> (a -> b) -> a -> Bool
f2 a1 a2 a3 = a1 (a3 + 1) == a2 (a3 + 1)

f3 :: (Eq b,Num b) => (a -> b) -> (a -> b ) -> (a) -> Bool
f3 a1 a2 a3 = (a1 a3) + 1 == (a2 a3) + 1

f4 :: [a] -> Int
f4 a1 = length a1 + 4

f5 :: Num a => [a] -> a
f5 a1 = head a1 + 4

f6 :: Num a => (b->a) -> b -> b -> a
f6 a1 a2 a3 = (a1 a2) + (a1 a3)

fCondicional :: (a->Bool) -> (a->b) -> (a->b) -> a -> b
fCondicional f1 f2 f3 x
 | f1 x = f2 x
 | otherwise = f3 x

transformarCond :: (a->Bool) -> (a->b) -> (a->b) -> [a] -> [b]
transformarCond f1 f2 f3 l = map (fCondicional f1 f2 f3) l

fAuxiliar :: (a->Bool) -> [a] -> [a]
fAuxiliar a [] = []
fAuxiliar a (x:xs) | a x = fAuxiliar a xs
 | otherwise = x : fAuxiliar a xs

fPrincipal :: (a->Bool) -> (a-> Bool) -> [a] -> [a]
fPrincipal a b xs = fAuxiliar a ( fAuxiliar b xs)

aparearListas _ [] = []
aparearListas [] _ = []
aparearListas (x:xs) (y:ys) = (x,y): aparearListas xs ys
seleccionarListaMenor lista1 lista2 | length lista1 > length lista2 = lista2
                                    | otherwise = lista1
transformarTupla f (x,y) = (f x , f y)
fstMayorsnd (x,y) = x > y
compararListas f lista1 lista2 | length (((filter fstMayorsnd).(map (transformarTupla f))) listasApareadas) > (div (length listasApareadas) 2) = lista1
                               | otherwise = lista2
                               where listasApareadas = aparearListas lista1 lista2

funcion :: (a->Bool) -> (a->b) -> [a] -> [b]
funcion x y z = (map y . filter x) z

funcion2 ::(Ord c,Num c, Num a) =>  a -> (a -> c) -> [a] -> a
funcion2 x _ [] = x
funcion2 x y (z:zs)
    | y z > 0   = z + funcion2 x y zs
    | otherwise = funcion2 x y zs

funcion3 :: (b -> Bool) -> (a) -> [a] -> (Int -> (Int,Bool) -> b) -> Int -> Int
funcion3 a b c d e | (a . d e) (1, True) = 0
                   | otherwise           = length (b:c) + e

funcion4 :: (Ord b, Num b) => ((a -> b) -> b -> Bool) -> (a -> b) -> [a] -> Bool
funcion4 x y = (> 10) . head . filter (x y) . map y

funcion5 :: (Ord b) => a -> [[b]] -> [(a -> b)] -> Bool
funcion5 x y z = (head y) > (map (\n -> n x) z)
