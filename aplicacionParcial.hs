import Text.Show.Functions

suma :: Integer -> Integer -> Integer
suma x y = x + y

suma' :: (Integer, Integer) -> Integer
suma' (x, y) = x + y  

esImparSigSumaSiete :: Integer -> Bool
esImparSigSumaSiete nro = (odd.siguiente.suma 7) nro


siguiente :: Integer -> Integer
siguiente nro = nro + 1


data Figura = Circulo { radio :: Double } | Rectangulo { base :: Double , altura :: Double }

area :: Figura -> Double
area (Circulo radio) = pi * radio^2
area (Rectangulo base altura ) = base * altura

circulo = Circulo 7 
rectangulo= Rectangulo 4 5


data Punto = Plano {coorX :: Double , coorY :: Double } | 
            Espacio { coorX :: Double , coorY :: Double, coorZ :: Double }

distancia :: Punto -> Double
distancia (Plano x y) = sqrt (x^2 + y^2)
distancia (Espacio x y z) = sqrt (x^2 + y^2 + z^2)

puntoPlano :: Punto
puntoPlano = Plano 3 6

puntoEspacio :: Punto
puntoEspacio = Espacio 4 5 8

----  1 a -----------
esNotaBochazo :: Integer -> Bool
esNotaBochazo nro = nro < 6

-----2 -----

aprobo :: (Integer, Integer) -> Bool
aprobo (nota1, nota2) = (not. esNotaBochazo) nota1 && (not.esNotaBochazo) nota2


---- 3------
promociono :: (Integer, Integer) -> Bool
promociono (nota1,  nota2) = (nota1 >= 8) && (nota2 >= 8)


-- (not.esNotaBochazo.fst) (5,8)

data Empleado = Comun {basico :: Double, nombre:: String}|
                Jefe {basico :: Double, antiguedad :: Double, nombre:: String}

calcularSueldo :: Empleado -> Double
calcularSueldo (Comun neto  _) = neto
calcularSueldo (Jefe neto  anti _ ) = neto  + plus anti

plus :: Double -> Double
plus antigue = antigue * 10000

ana :: Empleado
ana = Comun 60000 "ana"
julia :: Empleado
julia = Jefe 100000 3 "julia"

--- 3----
data Bebida = Cafe {nombreBebida :: String} | Gaseosa {sabor ::String , azucar :: Integer}

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "capuchino") = True
esEnergizante (Gaseosa "pomelo" azucar) = azucar > 10 
esEnergizante   _  = False
