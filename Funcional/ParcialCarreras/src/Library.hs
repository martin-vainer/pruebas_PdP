module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Auto = Auto {
    color::String,
    velocidad::Number,
    distancia::Number
} deriving(Show, Eq, Ord)


-- Carrera = [(Auto, Number)]

