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
nthList = undefined

tabulateList :: (Int -> a) -> Int -> [a] 
tabulateList = undefined

mapList :: (a -> b) -> [a] -> [b]
mapList = undefined 

filterList :: (a -> Bool) -> [a] -> [a]
filterList = undefined

appendList :: [a] -> [a] -> [a]
appendList = undefined

takeList :: [a] -> Int -> [a]
takeList = undefined

dropList :: [a] -> Int -> [a]
dropList = undefined

showtList :: [a] -> TreeView a ([a])
showtList = undefined

showlList :: [a] -> ListView a ([a])
showlList = undefined

joinList :: [[a]] -> [a]
joinList = undefined

reduceList :: (a -> a -> a) -> a -> [a] -> a
reduceList = undefined

scanList :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanList = undefined

fromListList :: [a] -> [a]
fromListList = undefined
