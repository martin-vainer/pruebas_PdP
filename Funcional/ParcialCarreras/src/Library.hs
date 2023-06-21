module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Auto = Auto {
    color::String,
    velocidad::Number,
    distancia::Number,
    poderes::[PowerUp] 
} deriving(Show, Ord)

instance Eq Auto where
    (/=) auto1 auto2 = color auto1 /= color auto2

rapido = Auto {
    color = "Rojo",
    velocidad = 100,
    distancia = 0,
    poderes = []
}

lento = Auto {
    color = "Azul",
    velocidad = 50,
    distancia = 0,
    poderes = []
}

type Carrera = [Auto]

--PUNTO 1:
--1.a
distanciaEntreAutos :: Auto -> Auto -> Number
distanciaEntreAutos auto1 auto2 = abs (distancia auto1 - distancia auto2)

estaCerca:: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && (<10) (distanciaEntreAutos auto1 auto2)

--1.b
--Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
-- vaTranquilo:: Auto -> Carrera -> Bool
-- vaTranquilo auto carrera = not (any (estaCerca auto) carrera) && all (leGana auto) carrera

vaTranquilo:: Auto -> Carrera -> Bool
vaTranquilo auto carrera = not $ any (estaCerca auto) carrera && all (leGana auto) carrera

leGana :: Auto -> Auto -> Bool
leGana auto1 auto2 = (>) (distancia auto1) (distancia auto2)

--1.c
type Puesto = Number

puestoEnCarrera :: Auto -> Carrera -> Puesto
puestoEnCarrera auto carrera = (1+) . length . (filter (leGana auto)) $ carrera

--PUNTO 2:
--2.a
type Tiempo = Number
type AutoAndado = Auto -> Auto

correrXTiempo:: Tiempo -> AutoAndado
correrXTiempo tiempo auto = auto {
    distancia = (+) ((*tiempo) . velocidad $ auto) (distancia auto)
}

--2.b.I
type Modificador = (Number -> Number)

--A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.
modificarVelocidad :: Modificador -> Auto -> Auto
modificarVelocidad modificador auto = auto {
    velocidad = modificador (velocidad auto)
}
--2.b.II 
--Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.
bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad cantidad auto = modificarVelocidad (max 0 . flip (-) cantidad) auto
-- el (max 0 . flip (-) cantidad) actua como modificador.

--PUNTO 3:

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto:: PowerUp
terremoto autoPrincipal carrera = afectarALosQueCumplen (estaCerca autoPrincipal) (bajarVelocidad 50) carrera

miguelitos :: Number -> PowerUp
miguelitos bajaDeVelocidad autoPrincipal carrera = afectarALosQueCumplen (leGana autoPrincipal) (bajarVelocidad bajaDeVelocidad) carrera

--este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.
jetPack :: Tiempo -> PowerUp
jetPack tiempo autoPrincipal carrera =
    (correrXTiempo tiempo . modificarVelocidad (*2)) autoPrincipal : filter (/= autoPrincipal) carrera

-- Punto 4:
--simular una carrera:
{-
A partir de todo lo construido hasta ahora queremos finalmente simular una carrera, para lo cual se provee una lista de eventos, que son funciones que permiten ir de un estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen dichos eventos. Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada auto asociado al color del auto en cuestión.
Se pide:
Desarrollar la función:
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada evento uno detrás del otro, partiendo del estado de la carrera recibido.
-}


--PUNTO 4:
-- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Number, String)] 
-- simularCarrera carrera eventos = zip [1..] (map mostrarCarrera (aplicarEventos carrera eventos))

-- aplicarEventos :: Carrera -> [Carrera -> Carrera] -> [Carrera]
-- aplicarEventos carrera eventos = foldl (\c e -> e c) [carrera] eventos


-- mostrarCarrera :: Carrera -> String
-- mostrarCarrera carrera = unlines (map mostrarAuto carrera)

-- mostrarAuto :: Auto -> String
-- mostrarAuto auto = color auto ++ " - Distancia: " ++ show (distancia auto) ++ " - Velocidad: " ++ show (velocidad auto)

-- -- Ejemplo de uso:
-- carreraEjemplo :: Carrera
-- carreraEjemplo = [
--     Auto "Rojo" 50 0 [],
--     Auto "Azul" 40 0 [],
--     Auto "Verde" 60 0 []
--   ]

-- eventosEjemplo :: [Carrera -> Carrera]
-- eventosEjemplo = [
--     terremoto,
--     miguelitos 20,
--     jetPack 5
--   ]

-- resultadoEjemplo :: [(Number, String)]
-- resultadoEjemplo = simularCarrera carreraEjemplo eventosEjemplo
