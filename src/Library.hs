module Library where
import PdePreludat

--Defino mis tipos
type Cansancio = Number
type Stress = Number
type Idioma = String
type Minutos = Number
type Marea = String
type Indice = Turista -> Number
type Tour = [Excursion]
type Espiritualidad = Number
type Excursion = Turista -> Turista -- Modelar para agregar mas funciones sin cambiar las existentes

--Defino mis turistas
data Turista = UnTurista{
    nivelCansancio :: Cansancio,
    nivelStress :: Stress,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving Show

--Funciones para delegar
estaIdioma :: Idioma -> [Idioma] -> Bool
estaIdioma idioma idiomas = elem idioma idiomas

agregarIdioma :: Idioma -> [Idioma] -> [Idioma]
agregarIdioma idioma idiomas
    |estaIdioma idioma idiomas = idiomas
    |otherwise = idioma:idiomas

subirStress :: Turista -> Number -> Turista
subirStress turista stress = turista{nivelStress = nivelStress turista + stress}

estaAcompañado :: Turista -> Bool
estaAcompañado turista = viajaSolo turista

obtenerCansancio :: Turista -> Cansancio
obtenerCansancio turista = nivelCansancio turista

obtenerStress :: Turista -> Stress
obtenerStress turista = nivelStress turista

--Defino mis excursiones
irPlaya :: Excursion
irPlaya turista 
    |viajaSolo turista = turista {nivelCansancio = nivelCansancio turista - 5, nivelStress = nivelStress turista * 0.9}
    |otherwise = turista {nivelStress = (nivelStress turista - 1)*0.9}

apreciarPaisaje :: String -> Excursion --String -> Turista -> Turista. paisaje turista = turista
apreciarPaisaje paisaje turista = turista {nivelStress = (nivelStress turista - length paisaje)*0.9}

hablarIdioma :: Idioma -> Excursion
hablarIdioma idioma turista = turista {idiomas = agregarIdioma idioma (idiomas turista), viajaSolo = False, nivelStress = nivelStress turista*0.9}

caminar :: Minutos -> Excursion
caminar minutos turista = turista {nivelCansancio = nivelCansancio turista + minutos/4, nivelStress = (nivelStress turista - minutos/4)*0.9}

paseoBarco :: Marea -> Excursion
paseoBarco "Fuerte" turista = turista {nivelCansancio = nivelCansancio turista + 10, nivelStress = (nivelStress turista + 6)*0.9}
paseoBarco "Tranquila" turista = hablarIdioma "Aleman" (apreciarPaisaje "mar" (caminar 10 turista))

--Defino mis tours
completo :: Tour
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, irPlaya , hablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour --Cuando a ladoB le doy una excursion, me devuelve un Tour, es decir, [Excursion]
ladoB excursion = [paseoBarco "Tranquila", excursion, caminar 120] --De la misma manera funciona paseoBarco, ya que al darle "Tranquila" devuelve una Excursion

islaVecina :: Marea -> Tour
islaVecina "Fuerte" = [paseoBarco "Fuerte", apreciarPaisaje "cascada", paseoBarco "Fuerte"]
islaVecina marea = [paseoBarco marea, irPlaya, paseoBarco marea]

hacerExcursion :: Turista -> Excursion -> Turista
hacerExcursion turista excursion = excursion turista

hacerTurismo :: Turista -> Tour -> Turista
hacerTurismo turista tour = foldl (hacerExcursion) turista tour

esConvincente :: Turista -> Indice -> Tour -> Bool
esConvincente turista indice tour = elem True (excursionesDesestresantes tour indice turista) && not(estaAcompañado turista)

convenceTurista :: [Tour] -> Indice -> Turista ->  Bool
convenceTurista tours indice turista = elem True (map (esConvincente turista indice) tours) 

obtenerConvencidos :: [Tour] -> [Turista] -> [Turista]
obtenerConvencidos _ [] = []
obtenerConvencidos tour (x:xs) 
    |convenceTurista tour stress x = x:obtenerConvencidos tour xs
    |otherwise = obtenerConvencidos tour xs

playaInfinita :: Tour 
playaInfinita = irPlaya:playaInfinita 

--Defino mis indices
stress :: Indice
stress turista = nivelStress turista

cansancio :: Indice
cansancio turista = nivelCansancio turista

idiomasI :: Indice
idiomasI turista = length(idiomas turista)

--Defino mi deltaExcursion
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = (indice.excursion) turista - indice turista

--Mis turistas
ana :: Turista
ana = UnTurista 0 21 False ["Español"]

beto :: Turista
beto = UnTurista 15 15 True ["Aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["Aleman", "Catalan"]

--Aprendió un idioma: deltaExcursionSegun idiomasI ana (hablarIdioma "Aleman")
--Excursiones desestresantes
esDesestresante :: Indice -> Turista -> Excursion -> Bool
esDesestresante indice turista excursion
    |deltaExcursionSegun indice turista excursion < -2 = True
    |otherwise = False

excursionesDesestresantes :: [Excursion] -> Indice -> Turista -> [Bool]
excursionesDesestresantes excursiones indice turista = map (esDesestresante indice turista) excursiones

