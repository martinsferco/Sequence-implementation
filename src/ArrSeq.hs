module ArrSeq where

import Seq
import Par 

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


-- * lista
emptyArr :: Arr a
emptyArr = A.empty

-- * lista
singletonArr :: a -> Arr a
singletonArr x = A.fromList [x]

-- * lista
lengthArr :: Arr a -> Int
lengthArr = A.length

-- * lista
nthArr :: Arr a -> Int -> a
nthArr ar i = ar ! i

-- * lista
tabulateArr :: (Int -> a) -> Int -> Arr a 
tabulateArr = A.tabulate

-- * lista
mapArr :: (a -> b) -> Arr a -> Arr b
mapArr f ar = tabulateArr (\i -> f (nthArr ar i)) (lengthArr ar)  

-- * lista
filterArr :: (a -> Bool) -> Arr a -> Arr a
filterArr p ar = case showtArr ar of
                  EMPTY     -> emptyArr
                  ELT x     -> if p x then singletonArr x else emptyArr
                  NODE l r  -> let (l', r') = filterArr p l ||| filterArr p r
                               in  appendArr l' r'

-- * lista
appendArr :: Arr a -> Arr a -> Arr a
appendArr ar1 ar2 = tabulateArr appendAux (l1 + l2)

  where 
      (l1, l2) = (lengthArr ar1, lengthArr ar2)
      appendAux i = if i < l1 then nthArr ar1 i
                              else nthArr ar2 (i - l1)



-- !! corregir
takeArr :: Arr a -> Int -> Arr a
takeArr ar k = A.subArray 0 k ar 

-- !! corregir
dropArr :: Arr a -> Int -> Arr a
dropArr ar k = A.subArray k (lengthArr ar - k) ar 

-- * lista
showtArr :: Arr a -> TreeView a (Arr a)
showtArr ar = case lengthArr ar of
                  0 -> EMPTY
                  1 -> ELT (nthArr ar 0)
                  k -> let
                          s = div k 2
                       in
                          NODE (takeArr ar s) (dropArr ar s)

-- * lista
showlArr :: Arr a -> ListView a (Arr a)
showlArr ar = case lengthArr ar of 
                  0 -> NIL
                  k -> CONS (nthArr ar 0) (dropArr ar 1)

-- * lista
joinArr :: Arr (Arr a) -> Arr a
joinArr = A.flatten

reduceArr :: (a -> a -> a) -> a -> Arr a -> a
reduceArr op b ar = undefined

reduceArrAux :: (a -> a -> a) -> Arr a -> a
reduceArrAux op ar | lengthArr ar == 1   = nthArr ar 0
                   | even (lengthArr ar) = reduceArrAux op (tabulateArr (\i -> op (nthArr ar (2 * i)) (nthArr ar (2 * i + 1))) (div (lengthArr ar) 2))
                   | otherwise           = reduceArrAux op (tabulateArr (\i -> function i) (div (lengthArr ar) 2 + 1))

    where
      function i = if i /= (div (lengthArr ar) 2 ) then op (nthArr ar (2 * i)) (nthArr ar (2 * i + 1)) else nthArr ar (2 * i)

-- ? Preguntar por orden de reduccion
scanArr :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanArr = undefined

-- * lista
fromListArr :: [a] -> Arr a
fromListArr = A.fromList
