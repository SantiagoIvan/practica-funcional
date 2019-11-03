import Text.Show.Functions

existsAny f (x1,x2,x3) = f x1 || f x2 || f x3
mejor f1 f2 n = max (f1 n) (f2 n)
aplicarPar f (x,y) = (f x, f y)
parDeFns f1 f2 x = (f1 x, f2 x)

sumarLista :: Num a => [a] -> a
sumarLista [] = 0
sumarLista (x:xs) = x+sumarLista xs

cabeza :: Num a => [a] -> a
cabeza (x:_)= x

filtrador ::(a->Bool) -> [a] -> [a]
filtrador f [] = []
filtrador f (x:xs)  | f x = x : (filtrador f xs)
                    | otherwise = (filtrador f xs)

frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
promedio :: Fractional a => [a] -> a
promedio lista= (sumarLista lista)/fromInteger(toInteger(length lista))
frecuenciaCardiacaMinuto t = frecuenciaCardiaca !! (div t 10)
frecuenciaCardiacaMomento t = take (div t 10) frecuenciaCardiaca

esCapicua lista = ((reverse.concat) lista) == (concat lista)
duracionLlamadas = (("horarioReducido", [20,10,25,15]), ("horarioNormal", [10,5,8,2,9,10]))
funcionSobreDuracionLLamadas f | (f.snd.fst) duracionLlamadas > (f.snd.snd) duracionLlamadas = (fst.fst) duracionLlamadas
                               | otherwise = (fst.snd) duracionLlamadas
cuandoHabloMasMinutos = funcionSobreDuracionLLamadas sum
cuandoHizoMasLlamadas =funcionSobreDuracionLLamadas length

esMultiploDeAlguno n  = any ((0==).mod n)
promedios = map promedio
promediosSinAplazos =map (promedio.filter(>4))
mejoresNotas listaDeListas= map maximum listaDeListas
aprobo lista= ((>4).minimum) lista
aprobaron listaDeListas = (length.(filter aprobo)) listaDeListas
divisores n = filter ((0==).(mod n)) [1..n]
exists f lista = any f lista
hayAlgunNegativo lista _= any (<0) lista
aplicarFuncion v f = f v
aplicarFunciones listaDeF v = map (aplicarFuncion v) listaDeF
sumaF listaDeF = (sum.aplicarFunciones listaDeF)
subirHabilidades cantidad listaDeNumeros = map ((min 12).(+cantidad)) listaDeNumeros
flimitada f n = min 12 (max 0 (f n))
cambiarHabilidad f lista = map (flimitada f) lista
primerosPares lista = takeWhile ((0==).(flip mod 2)) lista
primerosDivisores n lista = takeWhile ((0==).(mod n)) lista
primerosNoDivisores n lista = takeWhile (not.((0==).(mod n))) lista

crecimientoAnual edad | edad <= 10 = 24 - (edad * 2 )
                      | edad <= 15 = 4
                      | edad <=17 = 2
                      | edad <= 19 = 1
                      | otherwise = 0
crecimientoEntreEdades edad1 = abs.(+(-(crecimientoAnual edad1))).crecimientoAnual
alturasEnUnAnio edad listaDeAlturas = map (+(crecimientoAnual edad)) listaDeAlturas

lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]
rachasLluvia lista = ((takeWhile (0/=)).(dropWhile (0==))) lista
sumaFold lista = foldl (+) 0 lista
productoria lista = foldl (*) 1 lista
dispersion lista = ((+(foldl1 (max)  lista)).(*(-1)).(foldl1 (min))) lista
