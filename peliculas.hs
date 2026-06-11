psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"
juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer] 60

data Pelicula = Pelicula { nombrePelicula :: String, genero:: String, duracion:: Int, origen :: String} deriving Show

data Usuario = Usuario { nombre :: String, categoria:: String, edad:: Int, paisResidencia :: String, peliculasVistas:: [Pelicula], estadoDeSalud:: Int} deriving Show

--- 2----
ver :: Pelicula -> Usuario -> Usuario
ver pelicula usuario = usuario { peliculasVistas = peliculasVistas usuario ++ [pelicula]}

--- 3---
premiar :: [Usuario]  -> [Usuario]
premiar usuarios = map premiarUsuario usuarios

premiarUsuario :: Usuario -> Usuario
premiarUsuario usuario  | cumpleCondiciones usuario =  subeCategoria usuario
                        | otherwise = usuario


cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (>20). length. peliculasQueNoSean "Estados Unidos".peliculasVistas $ usuario

peliculasQueNoSean :: String -> [Pelicula] -> [Pelicula]
peliculasQueNoSean pais peliculas = filter ((pais /=).origen)  peliculas

subeCategoria:: Usuario -> Usuario
subeCategoria usuario =  usuario {categoria = nuevaCategoria.categoria $ usuario} 

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria  _ = "premiun" 



