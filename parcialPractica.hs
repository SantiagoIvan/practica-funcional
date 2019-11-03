import Text.Show.Functions
import Data.List
type Grito = (String,Int,Bool)

onomatopeya :: Grito -> String
onomatopeya (a,_,_) = a
intensidad :: Grito -> Int
intensidad (_,a,_) = a
mojoLaCama :: Grito -> Bool
mojoLaCama (_,_,a) = a

nivelDeTerror :: Grito -> Int
nivelDeTerror = length.onomatopeya

energiaDeGrito :: Grito -> Int
energiaDeGrito grito | mojoLaCama grito = (*) ((^2) (intensidad grito)) (nivelDeTerror grito)
                     | otherwise = (+) (intensidad grito) ((*3) (nivelDeTerror grito))

type Nene = (String, Int, Double)

type Monstruo = Nene -> Grito
mojaLaCamaSegunAltura :: (Ord a,Num a) => a-> a-> a->Bool
mojaLaCamaSegunAltura limiteInferior limiteSuperior altura = altura > limiteInferior && altura < limiteSuperior

vocalesDelNombre = filter (flip elem vocales)
replicarCharSegunLongitud char = (flip replicate char).length
vocales = "aeiou"
abecedario = "abcdefghijklmnñopqrstuvwxyz"

sullivan :: Monstruo
sullivan (nombre, edad, _) = ( ((++"GH").(replicarCharSegunLongitud 'A')) nombre, div 20 edad, edad<3)
randallBoggs :: Monstruo
randallBoggs (nombre, _, h) = ("¡Mamadera!",(length.vocalesDelNombre) nombre,  0.8>h && h<1.2 )
chuckNorris :: Monstruo
chuckNorris (_,_,_) = (abecedario,1000,True)
osito :: Monstruo
osito (_,edad,_) = ("uf",edad,False)

pam :: [(a->b)] -> a -> [b]
pam listaDeFunciones valor = map (flip ($) valor) listaDeFunciones

gritos monstruos nene = pam monstruos nene
campamentoDeExploradores :: [Nene]
monstruos :: [Monstruo]
monstruos = [sullivan,osito,randallBoggs]
campamentoDeExploradores = [("ana",3,1.2),("kevin",25,2.0),("kaka",2,1.2)]
produccionEnergeticaDeGritos mostros = (sum.(map energiaDeGrito).(foldl (++) []).(map (gritos mostros)))

type Risa = (Int,Int)
duracion :: Risa -> Int
duracion = fst
intenso :: Risa -> Int
intenso = snd

energiaDeRisa :: Risa -> Int
energiaDeRisa risa = ((^(intenso risa)).duracion) risa

type Comediante = Nene -> Risa
capusotto (_,edad,_) = (v,v) where v = (*2) edad
risas comediantes nene = pam comediantes nene
produccionEnergeticaDeRisas comediantes = (sum.(map energiaDeRisa).(foldl (++) []).(map (risas comediantes)))

produccionEnergetica productores funcionEnergia ataqueGrupal= (sum.(map funcionEnergia).(foldl (++) []).(map (ataqueGrupal productores)))
