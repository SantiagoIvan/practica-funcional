import Text.Show.Functions

esMultiploDe n1 = (0==).(mod n1)
esPar n = esMultiploDe n 2
cubo n = n*n*n
area b h = b*h
esBisiesto año | (esMultiploDe año 400) = True
               | esMultiploDe año 4 && not (esMultiploDe año 100) = True
               | otherwise = False
maximoDeTres n1 n2  = max n1.max n2
minimoDeTres n1 n2  = min n1.min n2
dispersion n1 n2 n3 = maximoDeTres n1 n2 n3 - minimoDeTres n1 n2 n3
diasParejos  n1 n2 n3 = dispersion n1 n2 n3 < 30
diasLocos n1 n2 n3 = dispersion n1 n2 n3> 100
diasNormales n1 n2 n3 = not(diasParejos n1 n2 n3) && not(diasLocos n1 n2 n3)
pesoPino altura  | altura <= 300 = altura*3
                 | otherwise = (300 * 3) + (altura - 300)*2
esPesoUtil kg = kg < 100 && kg > 400
sirvePino = esPesoUtil.pesoPino

siguiente  = (+1)
mitad = (/2)
inversa = (1/)
triple = (*3)
esPositivo = (>0)
inversaRaizCuadrada = inversa.sqrt
incrementaCuadrado n1 = (+n1).(^2)
esResultadoPar n1 = esPar.(n1^)
fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z

aplicar (funcion1,funcion2) elemento = (funcion1 elemento, funcion2 elemento)
cuentaBizarra (n1,n2) | n1>n2 = n1+n2
                      | n2-n1 < 10 = n1*n2
                      | otherwise = n2-n1
esNotaBochazo = (<4)
aprobo tupla = not((esNotaBochazo.fst) tupla || (esNotaBochazo.snd) tupla)
promociono (x,y) = x+y>=14 && x>6 && y>6

esMayorDeEdad= (>21).snd
modificarPrimerElemento tupla | (esPar.fst) tupla =((*2) (fst tupla), snd tupla)
                              | otherwise = (fst tupla, snd tupla)
modificarSegundoElemento tupla | (esPar.fst) tupla =((*2) (fst tupla), snd tupla)
                               | otherwise = (fst tupla, snd tupla)

calcular tupla = modificarSegundoElemento.modificarPrimerElemento
