module ListSeq where


import Seq


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
tabulateList = undefined

mapList :: (a -> b) -> [a] -> [b]
mapList = map

filterList :: (a -> Bool) -> [a] -> [a]
filterList = filter

appendList :: [a] -> [a] -> [a]
appendList = (++)

takeList :: [a] -> Int -> [a]
takeList _ 0      = []
takeList [] _     = []
takeList (x:xs) n = x : (takeList xs (n-1))

dropList :: [a] -> Int -> [a]
dropList xs 0     = xs
dropList [] _     = []
dropList (x:xs) n = dropList xs (n-1)

showtList :: [a] -> TreeView a ([a])
showtList []  = EMPTY
showtList [x] = ELT x
showtList xs  = let
                    l = div (lengthList xs) 2 
                in
                    NODE (takeList xs l) (dropList xs l)


showlList :: [a] -> ListView a ([a])
showlList [] = NIL
showlList (x:xs) = CONS x xs

joinList :: [[a]] -> [a]
joinList [] = []
joinList [xs] = xs
joinList (xs:xss) = xs ++ joinList xss

reduceList :: (a -> a -> a) -> a -> [a] -> a
reduceList = undefined

scanList :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanList = undefined

fromListList :: [a] -> [a]
fromListList = id
