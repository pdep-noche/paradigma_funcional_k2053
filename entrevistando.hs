
data Postulante = UnPostulante {nombre :: String, edad :: Double, remuneracion :: Double, conocimientos :: [String]} | Estudiante {legajo :: String, conocimientos:: [String]}  deriving Show 

pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto González" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

type Requisito =  Postulante -> Bool
--- 1 a
tieneConocimientos :: Puesto -> Requisito
tieneConocimientos unPuesto postulante = all (`elem` (conocimientos postulante)). conocimientoRequeridos $ unPuesto

-- 1 b
edadAceptable :: Double -> Double -> Requisito
edadAceptable edadMin edadMax postulante = edad postulante >= edadMin && edad postulante <= edadMax

--- 1 C
sinArreglo :: Requisito
sinArreglo postulante = not . terminaCon (nombre postulante) $ apellidoDueno

terminaCon :: Nombre -> Nombre -> Bool
terminaCon nombre apellido = (== apellido).last.words $ nombre 

---2 --

preseleccion :: [Postulante] -> [Requisito] -> [Postulante]
preseleccion postulantes requisitos = filter (cumpleTodos requisitos)  postulantes 

cumpleTodos :: [Requisito] -> Postulante -> Bool
cumpleTodos requisitos postulante = all ( $ postulante) requisitos


{--
ghci> preseleccion [pepe, tito] [edadAceptable 30 40, tieneConocimientos jefe, sinArreglo]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
--}

{--
ghci> preseleccion [pepe, tito] [edadAceptable 30 40, tieneConocimientos jefe, sinArreglo, (not.elem "repite logica".conocimientos)]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
--}


{--
actualizarPostulantes  :: [Postulante] -> [Postulante]
actualizarPostulantes postulantes = [ incrementarEdad.aumentarSueldo 27 $ unPostulante  | unPostulante <- postulantes]


actualizarPostulantes' :: [Postulante] -> [Postulante]
actualizarPostulantes' postulantes = map (incrementarEdad.aumentarSueldo 27) postulantes

--}

-- actualizarPostulantes (repeat pepe)


-------------- 4
type Conocimiento = String
capacitar :: Postulante -> Conocimiento -> Postulante
capacitar  (UnPostulante nom edad remuneracion conocimientos ) conocimiento = UnPostulante nom edad remuneracion (conocimiento: conocimientos)
capacitar (Estudiante legajo conocimientos) unConocimiento = Estudiante legajo (unConocimiento: (init conocimientos)) 


{--

ghci> capacitar (Estudiante "3233544" [ "Haskell", "Prolog"] )  "Python"
Estudiante {legajo = "3233544", conocimientos = ["Python","Haskell"]}
--}

ana = Estudiante "4534543" ["python"]
capacitacion :: Puesto -> Postulante -> Postulante
capacitacion puesto postulante =  foldl capacitar postulante . conocimientoRequeridos $ puesto

{--
ghci> capacitacion jefe ana
Estudiante {legajo = "4534543", conocimientos = ["Wollok"]}
--}