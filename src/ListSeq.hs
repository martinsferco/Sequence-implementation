module ListSeq where


import Seq
import Par
import Data.List.NonEmpty (append)


instance Seq [] where
  
  emptyS     = emptyList
  singletonS = singletonList
  lengthS    = lengthList
  nthS       = nthList
  tabulateS  = tabulateList
  mapS       = mapList
  filterS    = filterList
  appendS    = appendList
  takeS      = takeList
  dropS      = dropList
  showtS     = showtList
  showlS     = showlList
  joinS      = joinList
  reduceS    = reduceList
  scanS      = scanList
  fromList   = fromListList 


-- * lista
emptyList :: [a]
emptyList = []

-- * lista
singletonList :: a -> [a]
singletonList x = [x]

-- * lista
lengthList :: [a] -> Int
lengthList = length

-- * lista
nthList :: [a] -> Int -> a
nthList (x:xs) 0 = x
nthList (x:xs) n = nthList xs (n-1)

-- * lista
tabulateList :: (Int -> a) -> Int -> [a] 
tabulateList f k = tabulateListAux f k 0

  where
    tabulateListAux f 0 i = emptyList
    tabulateListAux f k i = let
                              (fi, ft) = f i ||| tabulateListAux f (k - 1) (i + 1) 
                            in 
                              fi : ft

-- * lista
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = let
                      (fx, fxs) = f x ||| mapList f xs
                   in
                       fx : fxs

-- * lista
filterList :: (a -> Bool) -> [a] -> [a]
filterList p [] = []
filterList p (x:xs) = let
                         (px, pxs) = p x ||| filterList p xs
                      in
                         if px then x : pxs else pxs

-- * lista
appendList :: [a] -> [a] -> [a]
appendList = (++)

-- * lista
takeList :: [a] -> Int -> [a]
takeList xs k = take k xs

-- * lista
dropList :: [a] -> Int -> [a]
dropList xs k = drop k xs

-- * lista
showtList :: [a] -> TreeView a ([a])
showtList []  = EMPTY
showtList [x] = ELT x
showtList xs  = let
                    l = div (lengthList xs) 2 
                    (tl, dl) = takeList xs l ||| dropList xs l
                in
                    NODE tl dl

-- * lista
showlList :: [a] -> ListView a ([a])
showlList [] = NIL
showlList (x:xs) = CONS x xs

-- * lista
joinList :: [[a]] -> [a]
joinList [] = []
joinList [xs] = xs
joinList (xs:xss) = appendList xs (joinList xss)

reduceList :: (a -> a -> a) -> a -> [a] -> a
reduceList op b [] = b
reduceList op b l  = let v = red op b l
                     in  op b v

red :: (a -> a -> a) -> a -> [a] -> a
red op b [x] = x
red op b l   = let l' = contract op l
               in  red op b l'

contract :: (a -> a -> a) -> [a] -> [a]
contract op (x : y : zs) =  let (xy, r) = op x y ||| contract op zs
                            in  xy : r
contract _  l            =  l

-- Ejemplos - BORRARLOS ------------------------------------------------
concatStrings :: String -> String -> String
concatStrings a b = "(" ++ a ++ " + " ++ b ++ ")"

ejemploSeq :: [String]
ejemploSeq = fromListList ["x0", "x1", "x2", "x3", "x4", "x5"]

ejemploSeq' :: [String]
ejemploSeq' = fromListList ["x0", "x1", "x2", "x3", "x4", "x5", "x6"]
-- ---------------------------------------------------------------------

-- ? Preguntar por orden de reduccion
scanList :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanList op b []   =  ([], b)
scanList op b [x]  =  ([b], op b x)
scanList op b l    =  let
                        -- Contraigo l y llamo a scan recursivamente sobre la lista contraida
                        l'       = contract op l
                        (ls, lr) = scanList op b l'

                        -- Uno el resultado de scan en una unica lista ls'
                        ls' = appendList ls [lr]
                        
                        -- Expando a partir del operador, la lista original, el llamado recursivo
                        -- y un indice inicial y la longitud final que debe tener el resultado
                        r = expand op l ls' 0 (lengthList l + 1)

                      in
                        -- Separamos al resultado en dos partes acorde a la especificacion de scan
                        separateScan r

-- Si puede avanzar dos lo hace, devolviendo la lista vacia en caso de tener menos de dos elementos.
advanceTwo :: [a] -> [a]
advanceTwo (x : y : zs) = zs
advanceTwo _            = []

getFirst :: [a] -> a
getFirst (x : xs) = x

-- Dada una lista, la separa en una tupla (ls, v) donde ls son todos los elementos menos el ultimo, que es v.
separateScan :: [a] -> ([a], a)
separateScan [x]      = ([], x)
separateScan (x : xs) = let (ls, v) = separateScan xs
                        in  (x : ls, v)

-- x:y:zs es la lista original, x':zs' es la contraida
-- Termina cuando m es 0, es decir, ya se completo todo el resultado.
expand :: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
expand _  _  _       _ 0 = []
expand op s (x':xs') k m =
  if even k then
              x' : (expand op s (x':xs') (k + 1) (m - 1))
            else
              (op x' (getFirst s)) : (expand op (advanceTwo s) xs' (k + 1) (m - 1))

-- * lista
fromListList :: [a] -> [a]
fromListList = id
