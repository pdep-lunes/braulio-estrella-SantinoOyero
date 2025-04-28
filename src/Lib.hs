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

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = sacarVida unPersonaje 1000
                           

sacarVida :: Personaje -> Int -> Personaje
sacarVida unPersonaje vida = | vida > cantidadDeVida = (unPersonaje {cantidadDeVida = = 0})
                             | vida < cantidadDeVida = (unPersonaje {cantidadDeVida = cantidadDeVida - vida})


sumarVida :: Personaje -> Int -> Personaje
sumarVida unPersonaje vida = (unPersonaje {cantidadDeVida = cantidadDeVida + vida})

mitadVida :: Personaje -> Personaje
mitadVida unPersonaje = unPersonaje{cantidadDeVida = cantidadDeVida / 2}

lluviaDeTuercas :: Personaje -> String -> Personaje
lluviaDeTuercas unPersonaje tipo = | tipo == "sanadoras" = sumarVida unPersonaje 800
                                   | tipo  == "dañinas" = mitadVida unPersonaje
                                   | otherwise = unPersonaje

granadaDeEspinas :: 
