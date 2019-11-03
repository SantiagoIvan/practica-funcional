import Text.Show.Functions
import Data.List

data Cliente = Cliente{
    nombre :: String,
    resistencia :: Int,
    amigos :: [Cliente],
    bebidasTomadas :: [Bebida]
} deriving(Show)

instance Eq Cliente where
  (==) c1 c2 = nombre c1 == nombre c2
  (/=) c1 c2 = nombre c1 /= nombre c2

nuevaResistencia res cliente =(Cliente (nombre cliente) (res) (amigos cliente) (bebidasTomadas cliente))
nuevoAmigo amigo cliente | amigo /= cliente && ((not.(elem amigo)) (amigos cliente)) = (Cliente (nombre cliente) (resistencia cliente) (amigo:(amigos cliente )) (bebidasTomadas cliente))
                         | otherwise = cliente
disminuirResistencia cantidad cliente = (Cliente (nombre cliente) ((resistencia cliente)-cantidad) (amigos cliente) (bebidasTomadas cliente))
aumentarResistencia cantidad cliente = (Cliente (nombre cliente) (cantidad+(resistencia cliente)) (amigos cliente) (bebidasTomadas cliente))
agregarAlNombre agregado cliente = (Cliente (agregado++(nombre cliente)) (resistencia cliente) (amigos cliente) (bebidasTomadas cliente))
disminuirResistenciaAAmigos cantidad cliente = (Cliente (nombre cliente) (resistencia cliente) (map (disminuirResistencia cantidad) (amigos cliente)) (bebidasTomadas cliente))
agregarBebidaTomada b c = (Cliente (nombre c) (resistencia c) (amigos c) (b:(bebidasTomadas c)))

type Bebida = Cliente -> Cliente
grogxd :: Bebida
grogxd = (nuevaResistencia 0).(agregarBebidaTomada grogxd)

jarraLoca :: Bebida
jarraLoca = (disminuirResistenciaAAmigos 10).(disminuirResistencia 10).(agregarBebidaTomada jarraLoca)

klussener :: String -> Bebida
klussener gusto = (disminuirResistencia (length gusto)).(agregarBebidaTomada (klussener gusto))

tintico :: Bebida
tintico cliente = (((aumentarResistencia.length.amigos) cliente).(agregarBebidaTomada tintico)) cliente

soda :: Int -> Bebida
replicar n texto = foldl (++) ("") (replicate n texto)
soda fuerza = (agregarAlNombre ("e"++(replicar fuerza "r")++"p")).(agregarBebidaTomada (soda fuerza))

rescatate :: Int -> Cliente -> Cliente
rescatate horas cliente | horas >3 = aumentarResistencia 200 cliente
                         | otherwise = aumentarResistencia 100 cliente

rodri = Cliente { nombre = "Rodrigo", resistencia = 55, amigos = [], bebidasTomadas = [tintico] }
marcos = Cliente { nombre = "Marcos", resistencia = 40, amigos = [rodri], bebidasTomadas = [klussener "guinda"] }
cristian = Cliente { nombre = "Cristian", resistencia = 2, amigos = [], bebidasTomadas = [jarraLoca,grogxd] }
ana = Cliente { nombre = "Ana", resistencia = 120, amigos = [marcos,rodri], bebidasTomadas = [] }
robertoCarlos = Cliente { nombre = "Roberto Carlos", resistencia = 165, amigos = [], bebidasTomadas = [] }
chuckNorris = Cliente { nombre = "Chuck", resistencia = 1000, amigos = [ana], bebidasTomadas = (sodasInfinitas 1)}

sodasInfinitas n = (soda n):(sodasInfinitas (n+1))

comoEsta :: Cliente -> String
comoEsta cliente | resistencia cliente > 50 = "fresco"
                 | (length.amigos) cliente > 1 = "piola"
                 | otherwise = "duro"

realizarAcciones [] cliente = cliente
realizarAcciones (x:xs) cliente = realizarAcciones xs (x cliente)

tomarTragos :: Cliente -> [Bebida] -> Cliente
tomarTragos cliente listaDeTragos = realizarAcciones listaDeTragos cliente

dameOtro :: Bebida
dameOtro cliente= ((head.bebidasTomadas) cliente) cliente

seBancaLaToma bebida = (>0).resistencia.bebida
cualesPuedeTomar cliente listaDeBebidas = filter (flip seBancaLaToma cliente) listaDeBebidas

data Itinerario = Itinerario { nombreItinerario :: String, duracion :: Double, acciones :: [Cliente -> Cliente]} deriving (Show)

mezclaExplosiva = Itinerario { nombreItinerario = "Mezcla Explosiva", duracion = 2.5, acciones = [grogxd,grogxd,klussener "huevo",klussener "frutilla"] }
itinerarioBasico = Itinerario { nombreItinerario = "Itinerario Basico", duracion = 5.0, acciones = [jarraLoca,klussener "chocolate",rescatate 2,klussener "huevo"] }
salidaDeAmigos = Itinerario { nombreItinerario = "Salida De Amigos", duracion = 1.0, acciones = [soda 1,tintico,nuevoAmigo robertoCarlos,jarraLoca] }

maximoEntreItinerarios it1 it2 | intensidad it1 > intensidad it2 = it1
                               | otherwise = it2

hacerItinerario itinerario = realizarAcciones (acciones itinerario)
intensidad itinerario = (genericLength.acciones) itinerario / (duracion itinerario)
hacerItinerarioMasIntenso itinerarios = hacerItinerario (foldl1 (maximoEntreItinerarios) itinerarios)

test1 = comoEsta.(nuevoAmigo rodri).(nuevoAmigo ana)
test2 = (klussener "huevo").(rescatate 2).(klussener "chocolate").jarraLoca
test3 = hacerItinerario mezclaExplosiva marcos
test4 = hacerItinerario salidaDeAmigos rodri
test5 = soda 3 marcos
test6 = tomarTragos rodri [soda 1,soda 2]
test7 = tomarTragos marcos [klussener "huevo", tintico, jarraLoca]
test8 = dameOtro ana
test9 = dameOtro marcos
test10 = dameOtro ana
test11 = (dameOtro.(soda 1)) rodri
test12 = cualesPuedeTomar rodri [grogxd, tintico, klussener "frutilla"]
test13 = cualesPuedeTomar rodri [grogxd, tintico, klussener "fruuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuutilla"]
test14 = hacerItinerario salidaDeAmigos rodri
test15 = hacerItinerarioMasIntenso [salidaDeAmigos,itinerarioBasico, mezclaExplosiva] rodri
test16 = ((jarraLoca).(nuevoAmigo ana)) robertoCarlos
