maximo [x] = x
maximo (x:xs) = x `max` maximo xs

cantidadDeElementos lista = foldl contar 0  lista

contar :: Integer -> Integer -> Integer
contar sem _  = sem + 1

cantidadDeElementos' lista = foldl (\sem _ -> sem + 1) 0 lista


cantidadElementosConFoldr lista = foldr contarFoldr 0 lista

contarFoldr :: Integer -> Integer -> Integer
contarFoldr _ sem = sem + 1

cantidadElementosConFoldr' lista = foldr (\_ sem -> sem + 1) 0 lista

----maximo de la lista

maximoConFoldl (cab: cola) = foldl max cab cola 

maximoConFoldr (cab:cola) = foldr max cab  cola


data Flor = Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Int} deriving Show

rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta=  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

flores = [rosa, violeta, jazmin,  orquidea]

maximaFlorSegun ::   (  Flor  -> Int   ) -> [Flor] -> String
maximaFlorSegun  f  lista = (nombreFlor . maximaFlor f ) lista


maximaFlor  :: (Flor -> Int) -> [Flor] -> Flor
maximaFlor _ [flor] = flor
maximaFlor f (flor: flores) | f flor >= (f.maximaFlor f) flores = flor 
                            | otherwise = maximaFlor f flores 


{-
ghci> maximaFlorSegun cantidadDeDemanda  flores
"rosa"
-}

{-
ghci> maximaFlorSegun (length.nombreFlor) flores
"orquidea"
-}

{-
ghci> maximaFlorSegun ((`mod` 4). cantidadDeDemanda )  flores 
"orquidea"
-}



------- 1b

estanOrdenadas :: [Flor] -> Bool
estanOrdenadas [] = True
estanOrdenadas [_] = True
estanOrdenadas (unaFlor:otraFlor: resto) = cantidadDeDemanda unaFlor > cantidadDeDemanda otraFlor && estanOrdenadas(otraFlor:resto)

{-
ghci> estanOrdenadas flores
True
-}


cantidadDeElementosBis :: [a] ->Int
cantidadDeElementosBis lista = foldl (\sem _ -> sem  + 1)    0   lista


cantidadDeElementosBis' lista = foldr  (\_ sem -> sem + 1) 0  lista    

{-
ghci> cantidadDeElementosBis' [(4, 6), (2, 0)]
2
-}

masGastador :: [(String, Integer)] -> (String, Integer)
masGastador (emple: empleados) = foldl maximoEmple  emple  empleados

maximoEmple :: (String, Integer) -> (String, Integer) -> (String, Integer)
maximoEmple unEmple otroEmple | snd unEmple > snd otroEmple = unEmple
                              | otherwise = otroEmple


masGastador' (emple:empleados) = foldr maximoEmple emple empleados


monto :: [(String, Integer)] -> Integer
monto empleados = foldl  (\sem (_, gasto)->  gasto + sem)  0 empleados

monto' empleados = foldr (\(_, gasto) sem -> gasto  sem) 0 empleados

{-
foldl (\sem f ->  f sem ) 2 [(3+), (*2), (5+)]
15

foldl (flip ($)) 2 [(3+), (*2), (5+)]
15
-}

{-
foldr (\f sem -> f sem) 2 [(3+), (*2), (5+)]
17

foldr ($) 2 [(3+), (*2), (5+)]
-}


type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]

data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  200000 ["ing. en sistemas", "contador"], 
            Proy "restaurante" 50000 ["cocinero", "adm. de empresas", "contador"],
            Proy "ventaChurros" 10000 ["cocinero"] ]


maximoProSegun :: (Proyecto  -> Int  )  -> [Proyecto] -> Proyecto
maximoProSegun f  (unProyecto:proyectos) = foldl (maximoSegun f)  unProyecto  proyectos

maximoSegun :: (Proyecto -> Int) -> Proyecto -> Proyecto -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto = unProyecto
                                      | otherwise = otroProyecto

{-
 -----a-----
 ghci> maximoProSegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}

--------b--------
ghci> maximoProSegun (length. profesionales)  proyectos
Proy {nombre = "restaurante", inversionInicial = 50000, profesionales = ["cocinero","adm. de empresas","contador"]}

---------c----------
ghci> maximoProSegun (length.words. nombre)  proyectos 
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}

-}                                     

maximoProSegun' :: (Proyecto  -> Int  )  -> [Proyecto] -> Proyecto
maximoProSegun' f  (unProyecto:proyectos) = foldr (maximoSegun f)  unProyecto  proyectos
