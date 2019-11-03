import Text.Show.Functions

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

pertenece [] v = False
pertenece (x:xs) v = x == v || pertenece xs v

interseccion [] lista2 = []
interseccion (x:xs) lista2 | pertenece lista2 x = x:(interseccion xs lista2)
                           | otherwise = interseccion xs lista2

eliminarMayoresA19 lista = filter (<19) lista
sumaLoca [] = []
sumaLoca (x:xs) | even x = (x+2):(sumaLoca xs)
                | otherwise =(x+1): sumaLoca xs
transformadaLoca = sumaLoca.eliminarMayoresA19
productList [] = 1
productList (x:xs) = x*(productList xs)

maximo [] = 0
maximo [x] = x
maximo (x:y:xs) = max (maximo xs) (max x y)

menoresA [] n = []
menoresA (x:xs) n | x< n = x: (menoresA xs n)
                  | otherwise = menoresA xs n
promedio lista = div (sum lista)  (length lista)
promedios lista = map promedio lista

diferencias [] = []
diferencias [x] = []
diferencias (x:xs) = abs ((x-(head xs))) : (diferencias xs)

sinRepetidos [] = []
sinRepetidos (x:xs) = x:(sinRepetidos (filter (/=x) xs))

sinExtremos lista = filter ((minimum lista)/=) (filter ((maximum lista)/=) lista)

sinPuntas n lista= take ((length lista)-2*n) (drop n lista)

sonTodosIguales [] = True
sonTodosIguales [x] = True
sonTodosIguales (x:y:xs) = x == y && (sonTodosIguales xs)

esPar 0 = True
esPar 1 = False
esPar n = esPar (n-2)

dividir dividendo divisor | dividendo < divisor = 0
                          | otherwise = 1+(dividir (dividendo-divisor) divisor)
resto dividendo divisor   | dividendo<divisor = dividendo
                          | otherwise = resto (dividendo-divisor) divisor
devolverParDivMod dividendo divisor = (dividir dividendo divisor,resto dividendo divisor)

esPrimo 1 = False
esPrimo n = length (filter ((0==).(resto n)) [1..n]) <=2
siguientePrimo n = head (filter esPrimo [n+1..])

primosQueDividenA n = ((filter ((0==).(resto n))).(filter esPrimo)) [1..n]
factoresPrimos 1 = []
factoresPrimos n = (head (primosQueDividenA n)):(factoresPrimos (((dividir n).head) (primosQueDividenA n)))

tieneEspacios frase = any (' '==) frase
obtenerPalabras listaDePalabras = filter (not.tieneEspacios) listaDePalabras

palabrasLargas listaDePalabras= filter ((>7).length) (obtenerPalabras listaDePalabras)

longitudesDeNombres'  = ((map length).(filter ((flip elem ['A'..'Z']).head)).obtenerPalabras)
vocales = ['a','e','i','o','u']
consonantes = filter (not.(flip elem vocales)) ['a'..'z']
cantidadDeVocales  = length.(filter (flip elem vocales))
cantidadDeConsonantes = length.(filter (flip elem consonantes))
esVocalosa palabra = not (tieneEspacios palabra) && (cantidadDeVocales palabra )>(cantidadDeConsonantes palabra)

lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

aparearListas [] [] = []
aparearListas (indice:xs) (cantidadDeLluvia:ys) =(indice,cantidadDeLluvia): (aparearListas xs ys)
diasLluviosos  lluvias = ((map fst).(filter ((0/=).snd)).(aparearListas [1..(length lluvias)])) lluviasEnero
