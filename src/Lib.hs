module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = Personaje{
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadDeVida :: Int
} deriving (Show, Eq)

espina :: Personaje
espina = Personaje {
    nombre = "Espina",
    poderBasico = "Bola de Espinas",
    superPoder = "Granada de espinas",
    superPoderActivo = True,
    cantidadDeVida = 4800
}

pamela :: Personaje
pamela = Personaje {
    nombre = "Pamela",
    poderBasico = "Lluvia de tuercas sanadoras",
    superPoder = "torreta curativa ",
    superPoderActivo = False,
    cantidadDeVida = 9600
}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = sacarVida unPersonaje 1000
                           
sacarVida :: Personaje -> Int -> Personaje
sacarVida unPersonaje vida | vida >= cantidadDeVida unPersonaje = unPersonaje {cantidadDeVida = 0}
                           | vida < cantidadDeVida unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje - vida}

sumarVida :: Personaje -> Int -> Personaje
sumarVida unPersonaje vida = (unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + vida})

mitadVida :: Personaje -> Personaje
mitadVida unPersonaje = unPersonaje{cantidadDeVida = div (cantidadDeVida unPersonaje) 2}

-- Al principio habia hecho que se le aplique a unPersonaje el efecto de sumarVida o mitadVida, pero despues decidi poner otro personaje que podría ser el contrincante o el aliado, y segun el tipo, entonces se le aplica el sumarVida aliado o mitadVida al contrincante.   
lluviaDeTuercas :: Personaje -> String -> Personaje -> Personaje
lluviaDeTuercas unPersonaje tipo otroPersonaje | tipo == "sanadoras" = sumarVida otroPersonaje 800
                                               | tipo  == "dañinas" = mitadVida otroPersonaje
                                               | otherwise = otroPersonaje

-- En este punto interprete que todas las modificaciones se le hacen a unPersonaje y no a otroPersonaje que vendria a ser el contrincante. 
granadaDeEspinas :: Int -> Personaje -> Personaje ->Personaje
granadaDeEspinas radio unPersonaje otroPersonaje | radio > 3 && cantidadDeVida otroPersonaje < 800 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui"  ,superPoderActivo = False, cantidadDeVida = 0})
                                                 | radio  > 3 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui" })
                                                 | otherwise bolaEspinosa unPersonaje

-- Considero que se pasa un personaje que ya es el aliado.

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje  = unPersonaje {superPoderActivo = True, cantidadDeVida = (cantidadDeVida unPersonaje) * 2}


-- atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar a su contrincante con el súper y con el básico. Si no, no hará nada.

-- atacar :: Personaje -> 

quienesEstanEnLasUltimas :: [Personaje] -> [String]
quienesEstanEnLasUltimas unosPersonajes = map nombre . (filter estaEnLasUltimas) unosPersonajes

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas personaje = cantidadDeVida personaje < 800

-- Falta fijarme errores, ya que no me los estaria tirando, y terminar el punto de reportes.

