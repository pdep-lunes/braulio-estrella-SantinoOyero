module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = Personaje{
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    cantidadDeVida :: Number
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

sacarVida :: Personaje -> Number -> Personaje
sacarVida unPersonaje vida | vida >= cantidadDeVida unPersonaje = unPersonaje {cantidadDeVida = 0}
                           | vida < cantidadDeVida unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje - vida}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = sacarVida unPersonaje 1000

sumarVida :: Personaje -> Number -> Personaje
sumarVida unPersonaje vida = (unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + vida})

mitadVida :: Personaje -> Personaje
mitadVida unPersonaje = unPersonaje{cantidadDeVida = div (cantidadDeVida unPersonaje) 2}

-- Al principio habia hecho que se le aplique a unPersonaje el efecto de sumarVida o mitadVida, pero despues decidi poner otro personaje que podría ser el contrincante o el aliado, y segun el tipo, entonces se le aplica el sumarVida aliado o mitadVida al contrincante.   
lluviaDeTuercas :: Personaje -> String -> Personaje -> Personaje
lluviaDeTuercas unPersonaje tipo otroPersonaje | tipo == "sanadoras" = sumarVida otroPersonaje 800
                                               | tipo  == "dañinas" = mitadVida otroPersonaje
                                               | otherwise = otroPersonaje

-- En este punto interprete que todas las modificaciones se le hacen a unPersonaje y no a otroPersonaje que vendria a ser el contrincante. 
granadaDeEspinas :: Number -> Personaje -> Personaje ->Personaje
granadaDeEspinas radio unPersonaje otroPersonaje | radio > 3 && cantidadDeVida otroPersonaje < 800 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui"  ,superPoderActivo = False, cantidadDeVida = 0})
                                                 | radio  > 3 = (unPersonaje {nombre = nombre unPersonaje ++ "Espinas estuvo aqui" })
                                                 | otherwise = bolaEspinosa otroPersonaje

-- Considero que se pasa un personaje que ya es el aliado.
torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje  = unPersonaje {superPoderActivo = True, cantidadDeVida = (cantidadDeVida unPersonaje) * 2}

-- Hacemos un ejemplo de lo que sería un ataque de un personaje a otro, para poder utilizar el superPoder
atacaConBasico :: Personaje -> Personaje -> Personaje
atacaConBasico unPersonaje otroPersonaje = bolaEspinosa otroPersonaje
   
atacaConPoder :: Number -> Personaje -> Personaje -> Personaje
atacaConPoder x unPersonaje otroPersonaje = granadaDeEspinas x unPersonaje otroPersonaje

atacarConElSuperPoder :: Number -> Personaje -> Personaje -> Personaje
atacarConElSuperPoder x unPersonaje otroPersonaje
  | superPoderActivo unPersonaje = atacaConBasico unPersonaje (atacaConPoder x unPersonaje otroPersonaje)
  | otherwise = otroPersonaje
    
quienesEstanEnLasUltimas :: [Personaje] -> [String]
quienesEstanEnLasUltimas unosPersonajes = (map nombre . filter estaEnLasUltimas) unosPersonajes


estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas personaje = cantidadDeVida personaje < 800
