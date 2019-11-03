import Text.Show.Functions
type CriterioDeEstudio = Parcial -> Bool
data Alumno = Alumno {
    nombre :: String,
    fechaDeNacimiento :: (Int,Int,Int),
    legajo :: Int,
    materias :: [String],
    criterioDeEstudio :: CriterioDeEstudio
}  deriving (Show)
data Parcial = Parcial {
  materia :: String,
  cantidadDePreguntas :: Int
}deriving (Show)

cabulero :: CriterioDeEstudio
cabulero  = (odd.length.materia)
hijoDelRigor :: Int -> CriterioDeEstudio
hijoDelRigor n = (>n).cantidadDePreguntas
estudioso :: CriterioDeEstudio
estudioso _ = True

nico = Alumno {
  nombre = "nico",
  fechaDeNacimiento = (30,10,2002),
  legajo = 14929,
  materias = ["hola"],
  criterioDeEstudio = (hijoDelRigor 5)
}

f h j = sum . map j . filter h
