module ArrSeq where

import Seq

import qualified Arr as A
import Arr ((!))
import Arr (Arr, fromList) 


instance Seq Arr where
  emptyS     = emptyArr  
  singletonS = singletonArr
  lengthS    = lengthArr
  nthS       = nthArr
  tabulateS  = tabulateArr
  mapS       = mapArr
  filterS    = filterArr
  appendS    = appendArr
  takeS      = takeArr
  dropS      = dropArr
  showtS     = showtArr
  showlS     = showlArr
  joinS      = joinArr
  reduceS    = reduceArr
  scanS      = scanArr
  fromList   = fromListArr


emptyArr :: Arr a
emptyArr = A.empty

singletonArr :: a -> Arr a
singletonArr x = A.fromList [x]

lengthArr :: Arr a -> Int
lengthArr = A.length

nthArr :: Arr a -> Int -> a
nthArr ar i = ar ! i

tabulateArr :: (Int -> a) -> Int -> Arr a 
tabulateArr = A.tabulate

mapArr :: (a -> b) -> Arr a -> Arr b
mapArr = undefined 

filterArr :: (a -> Bool) -> Arr a -> Arr a
filterArr = undefined

appendArr :: Arr a -> Arr a -> Arr a
appendArr = undefined

takeArr :: Arr a -> Int -> Arr a
takeArr = undefined

dropArr :: Arr a -> Int -> Arr a
dropArr = undefined

showtArr :: Arr a -> TreeView a (Arr a)
showtArr = undefined

showlArr :: Arr a -> ListView a (Arr a)
showlArr = undefined

joinArr :: Arr (Arr a) -> Arr a
joinArr = undefined

reduceArr :: (a -> a -> a) -> a -> Arr a -> a
reduceArr = undefined

scanArr :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanArr = undefined

fromListArr :: [a] -> Arr a
fromListArr = undefined
