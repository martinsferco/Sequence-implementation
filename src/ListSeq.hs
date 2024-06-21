module ListSeq where


import Seq
import Par


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


emptyList :: [a]
emptyList = []


singletonList :: a -> [a]
singletonList x = [x]


lengthList :: [a] -> Int
lengthList = length


nthList :: [a] -> Int -> a
nthList (x:xs) 0 = x
nthList (x:xs) n = nthList xs (n-1)


tabulateList :: (Int -> a) -> Int -> [a] 
tabulateList f k = tabulateListAux f k 0

  where
    tabulateListAux f 0 i = emptyList
    tabulateListAux f k i = let
                              (fi, ft) = f i ||| tabulateListAux f (k - 1) (i + 1) 
                            in 
                              fi : ft


mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = let
                      (fx, fxs) = f x ||| mapList f xs
                   in
                       fx : fxs


filterList :: (a -> Bool) -> [a] -> [a]
filterList p []     = []
filterList p (x:xs) = let
                         (px, pxs) = p x ||| filterList p xs
                      in
                         if px then x : pxs else pxs


appendList :: [a] -> [a] -> [a]
appendList = (++)


takeList :: [a] -> Int -> [a]
takeList xs k = take k xs


dropList :: [a] -> Int -> [a]
dropList xs k = drop k xs


showtList :: [a] -> TreeView a ([a])
showtList []  = EMPTY
showtList [x] = ELT x
showtList xs  = let
                    l = div (lengthList xs) 2 
                    (tl, dl) = takeList xs l ||| dropList xs l
                in
                    NODE tl dl


showlList :: [a] -> ListView a ([a])
showlList []     = NIL
showlList (x:xs) = CONS x xs


joinList :: [[a]] -> [a]
joinList []       = []
joinList [xs]     = xs
joinList (xs:xss) = appendList xs (joinList xss)


reduceList :: (a -> a -> a) -> a -> [a] -> a
reduceList op b [] = b
reduceList op b l  = let v = red op l
                     in  op b v


red :: (a -> a -> a) -> [a] -> a
red op [x] = x
red op l   = let l' = fst (contractListLen op l)
             in  red op l'


-- contractListLen devuelve la lista contraida y la longitud de la lista original
--  (la longitud es util para scan, no asi para reduce, donde la ignoraremos)
contractListLen :: (a -> a -> a) -> [a] -> ([a], Int)
contractListLen op (x : y : zs) =  let (xy, (r, len)) = op x y ||| contractListLen op zs
                                   in  (xy : r, len + 2)
contractListLen _  l@[x]        =  (l,  1)
contractListLen _  []           =  ([], 0)


scanList :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanList op b []   =  ([], b)
scanList op b [x]  =  ([b], op b x)
scanList op b l    =  let
                        -- Llamamos a la funcion auxiliar que no hace separaciones innecesarias.
                        l' = scanWithoutSeparation op b l
                      in
                        -- Separamos al resultado en dos partes acorde a la especificacion de scan
                        separateScan l'


scanWithoutSeparation :: (a -> a -> a) -> a -> [a] -> [a]
scanWithoutSeparation op b []  =  [b]
scanWithoutSeparation op b [x] =  [b, op b x]
scanWithoutSeparation op b l   =  let
                                    (l', len) = contractListLen op l
                                    ls'       = scanWithoutSeparation op b l'
                                  in 
                                    expandList op l ls' 0 (len + 1)


expandList :: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
expandList _  _  _       _ 0 = []
expandList op s (x':xs') k m =
  if even k then x' : (expandList op s (x':xs') (k + 1) (m - 1))
            else
              let (operatedElements, recExpand) = op x' (getFirst s) |||
                                                  expandList op (advanceTwo s) xs' (k + 1) (m - 1)
              in  operatedElements : recExpand


separateScan :: [a] -> ([a], a)
separateScan [x]      = ([], x)
separateScan (x : xs) = let (ls, v) = separateScan xs
                        in  (x : ls, v)


advanceTwo :: [a] -> [a]
advanceTwo (x : y : zs) = zs
advanceTwo _            = []


getFirst :: [a] -> a
getFirst (x : xs) = x


fromListList :: [a] -> [a]
fromListList = id