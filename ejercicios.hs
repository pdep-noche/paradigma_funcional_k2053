siguiente :: Integer -> Integer
siguiente nro = nro + 1

calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

doble nro = nro * 2             

-- Declaratividad II 
calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (primero, segundo) | even primero && odd segundo = (doble primero, siguiente segundo)
                             | even primero && even segundo = (doble primero, segundo)
                             | odd primero && odd segundo = (primero, siguiente segundo)
                             | otherwise = (primero, segundo)
-- Declaratividad I
calcular'' :: (Integer, Integer) -> (Integer, Integer)
calcular'' (primero, segundo) = (duplicarPar primero, sumarUnoImpar segundo)

duplicarPar :: Integer -> Integer
duplicarPar nro | even nro = doble nro
                | otherwise = nro

sumarUnoImpar :: Integer -> Integer
sumarUnoImpar nro | odd nro = siguiente nro
                  | otherwise = nro                

-- Declaratividad II 
and'' :: Bool ->Bool -> Bool 
and'' primerCondi segCondi | primerCondi = segCondi
                           | otherwise = False

-- Declaratividad  III
and''' :: Bool ->Bool -> Bool 
and''' primerCond segCondi | not primerCond =  False
                           | not segCondi  = False
                           | otherwise = True  

-- con Pattern Matching --
--Declaratividad I
and''''  :: Bool ->Bool -> Bool   
and'''' True segundCond = segundCond
and''''  _ _ = False                      

-- Declaratividad I
or' :: Bool ->Bool -> Bool 
or'  False segCond = segCond
or' _ _ = True

-- Declaratividad II
or'' :: Bool ->Bool -> Bool 
or'' True _ = True
or'' _  True = True
or'' _ _ = False

type Alumno = (String, Nota, Nota, Nota)
type Nota = Integer

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1, nota2, nota3) = nota1  `max` (nota2 `max`  nota3)


cuadruple :: Integer -> Integer
cuadruple nro = doble(doble nro) 

esMayorA :: Integer -> Bool
esMayorA  nro = doble (siguiente(2 + nro)) > 10


-- (\nro -> nro * 3)

-- (\nro -> nro + 1)
-- (\nro otroNum ->nro +  otroNum)
--  (\nro  -> nro +  2 )