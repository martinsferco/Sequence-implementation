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
reduceList op b xs = op b (reduceListAux op xs)

reduceListAux :: (a -> a -> a) -> [a] -> a
reduceListAux op [x]            = x
-- reduceListAux op  =   


-- ? Preguntar por orden de reduccion
scanList :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanList = undefined

-- * lista
fromListList :: [a] -> [a]
fromListList = id
