import Text.Show.Functions
import Data.List

data Atraccion = Atraccion {nombre :: String, alturaMin:: Double, duracion :: Int, opiniones :: [String], estaEnMantenimiento :: Bool, reparaciones:: [Reparacion]} deriving Show

data Reparacion = Reparacion {dias:: Int, trabajo:: Trabajo} deriving Show

type Trabajo = Atraccion -> Atraccion

vueltaAlMundo :: Atraccion
vueltaAlMundo = Atraccion "vueltaAlMundo" 120  4  ["genial"] True  [ Reparacion 5 (ajusteDeTornilleria 3)]

montania :: Atraccion
montania = Atraccion "montania" 120 5 ["barbaro"] True [Reparacion 3 (engrase 3), Reparacion 4 (ajusteDeTornilleria 10)]

--- Punto 1
scoring :: Atraccion -> Double
scoring atraccion | (>10).duracion $ atraccion = 100
                   | (<3).length.reparaciones $ atraccion = calcularScore atraccion
                   | otherwise = (10*). alturaMin $ atraccion

calcularScore :: Atraccion -> Double
calcularScore atraccion = ((10*).genericLength.nombre) atraccion  +  ((2*). genericLength.opiniones) atraccion                

-- Punto 2

ajusteDeTornilleria :: Int -> Trabajo
ajusteDeTornilleria tornillos atraccion = atraccion { duracion = min 10 (duracion atraccion + tornillos)}

{--
ghci> vueltaAlMundo
Atraccion {nombre = "vueltaAlMundo", alturaMin = 120.0, duracion = 4, opiniones = ["genial"], estaEnMantenimiento = False, reparaciones = [Reparacion {dias = 5, trabajo = <function>}]}
--}


engrase :: Double -> Trabajo
engrase cantGrasa  = agregarOpinion "para valientes" . modificarAltura cantGrasa

agregarOpinion :: String -> Trabajo
agregarOpinion opinion atraccion = atraccion {opiniones =  opiniones atraccion ++ [opinion]}

modificarAltura :: Double -> Trabajo
modificarAltura cant atraccion = atraccion { alturaMin = alturaMin atraccion + (0.1*cant)}

{--
ghci> engrase 10 vueltaAlMundo
Atraccion {nombre = "vueltaAlMundo", alturaMin = 121.0, duracion = 4, opiniones = ["genial","para valientes"], estaEnMantenimiento = False, reparaciones = [Reparacion {dias = 5, trabajo = <function>}]}
--}


mantenimientoElectrico :: Trabajo
mantenimientoElectrico atraccion = atraccion { opiniones = (take 2.opiniones) atraccion}

mantenimientoBasico :: Trabajo
mantenimientoBasico atraccion = engrase 10 .ajusteDeTornilleria 8 $ atraccion

{--
ghci> mantenimientoBasico vueltaAlMundo
Atraccion {nombre = "vueltaAlMundo", alturaMin = 121.0, duracion = 10, opiniones = ["genial","para valientes"], estaEnMantenimiento = False, reparaciones = [Reparacion {dias = 5, trabajo = <function>}]}
--}

-- Punto 3

medaMiedito :: Atraccion -> Bool
medaMiedito atraccion = any ((>4).dias) . reparaciones $ atraccion

{--
ghci> medaMiedito vueltaAlMundo
True
--}

cerramos :: Atraccion -> Bool
cerramos atraccion = (>7).cantDiasDeReparacion $ atraccion

cantDiasDeReparacion :: Atraccion -> Int
cantDiasDeReparacion atraccion = foldl (\sem repa -> sem + dias repa) 0 . reparaciones $ atraccion

{--
ghci> cerramos vueltaAlMundo
False
--}

type Parque = [Atraccion]

disneyNoExists :: Parque -> Bool
disneyNoExists parque = all (null.reparaciones). filter ((>5).length.nombre) $ parque

{--
ghci> disneyNoExists [vueltaAlMundo]
False
--}

-- Punto 4

tieneReparacionesPiolas :: Atraccion -> Bool
tieneReparacionesPiolas atraccion = sonPiolas (reparaciones atraccion) atraccion

sonPiolas :: [Reparacion] -> Atraccion -> Bool
sonPiolas (repa:otraRepa:resto) atraccion = scoring ((trabajo repa) atraccion) < scoring ((trabajo otraRepa) atraccion) && sonPiolas (otraRepa:resto) atraccion


{--
ghci> tieneReparacionesPiolas montania
False
--}

realizarTrabajosPendientes:: Atraccion -> Atraccion
realizarTrabajosPendientes atraccion = colocarFueraMantenimiento . eliminarReparaciones . aplicarReparaciones $ atraccion

aplicarReparaciones :: Atraccion -> Atraccion
aplicarReparaciones atraccion = foldl  (\atrac repa -> (trabajo repa) atrac) atraccion . reparaciones $ atraccion

eliminarReparaciones :: Atraccion -> Atraccion
eliminarReparaciones atraccion = atraccion {reparaciones = []}

colocarFueraMantenimiento :: Atraccion -> Atraccion
colocarFueraMantenimiento atraccion = atraccion {estaEnMantenimiento = False}


{--
ghci> realizarTrabajosPendientes montania
Atraccion {nombre = "montania", alturaMin = 120.3, duracion = 10, opiniones = ["barbaro","para valientes"], estaEnMantenimiento = False, reparaciones = []}
--}