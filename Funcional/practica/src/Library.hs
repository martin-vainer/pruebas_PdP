module Library where
import PdePreludat

-- ################## CLASE 7/6/2023: SISTEMA DE TIPOS. ##################

data Persona = Persona {
    nombre :: String,
    edad :: Number
} deriving (Show, Eq)

pepe = Persona {
    nombre = "Jose",
    edad = 20
}

pepe2 = Persona {
    nombre = "Pepe",
    edad = 21
}

-- -- HACER QUE SOLO COMPARE LA EDAD DE LAS PERSONAS Y NO COMPARE TAMBEIN LOS NOMBRES:
-- -- LE ESTOY DICIENDO A HASKELL COMO QUIERO YO COMPARAR PERSONAS.
-- instance Ord Persona where
--     (<=) persona1 persona2 = (<=) (edad persona1) (edad persona2)
-- -- definiendo el menor igual, ya no hace falta definir el mayor igual, ya que haskell es capo y sabe.
-- -- porque no es menor igual va a ser mayor igual y así.

-- --hacer un poema de amor
-- amor:: Persona -> Persona -> String
-- amor persona1 persona2 = nombre persona1 ++ " ama a " ++ nombre persona2
-- noAmas:: Persona -> Persona -> String
-- noAmas persona1 persona2 = nombre persona1 ++ " no ama a " ++ nombre persona2
-- amaALaMama:: Persona -> String
-- amaALaMama persona = amor persona (Persona "Mama" 100)

doble :: Number -> Number
doble numero = numero + numero
