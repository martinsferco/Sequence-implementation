module ArrSeq where

import Seq
import Par 

import qualified Arr as A
import Arr (Arr, fromList, (!)) 


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
mapArr f ar = tabulateArr (\i -> f (nthArr ar i)) (lengthArr ar)  


filterArr :: (a -> Bool) -> Arr a -> Arr a
filterArr p ar = case showtArr ar of
                  EMPTY     -> emptyArr
                  ELT x     -> if p x then singletonArr x else emptyArr
                  NODE l r  -> let (l', r') = filterArr p l ||| filterArr p r
                               in  appendArr l' r'


appendArr :: Arr a -> Arr a -> Arr a
appendArr ar1 ar2 = tabulateArr appendAux (l1 + l2)

  where 
      (l1, l2) = (lengthArr ar1, lengthArr ar2)
      appendAux i = if i < l1 then nthArr ar1 i
                              else nthArr ar2 (i - l1)


takeArr :: Arr a -> Int -> Arr a
takeArr ar k | k < 0     = emptyArr
             | k > l     = ar
             | otherwise = A.subArray 0 k ar

             where l = lengthArr ar


dropArr :: Arr a -> Int -> Arr a
dropArr ar k | k < 0     = ar
             | k > l     = emptyArr
             | otherwise = A.subArray k (lengthArr ar - k) ar 

             where l = lengthArr ar


showtArr :: Arr a -> TreeView a (Arr a)
showtArr ar = case lengthArr ar of
                  0 -> EMPTY
                  1 -> ELT (nthArr ar 0)
                  k -> let
                          s = div k 2
                       in
                          NODE (takeArr ar s) (dropArr ar s)


showlArr :: Arr a -> ListView a (Arr a)
showlArr ar = case lengthArr ar of 
                  0 -> NIL
                  k -> CONS (nthArr ar 0) (dropArr ar 1)


joinArr :: Arr (Arr a) -> Arr a
joinArr = A.flatten


reduceArr :: (a -> a -> a) -> a -> Arr a -> a
reduceArr op b ar = if lengthArr ar == 0 then b else op b (reduceArrAux op ar)


reduceArrAux :: (a -> a -> a) -> Arr a -> a
reduceArrAux op ar | l == 1    = nthArr ar 0
                   | otherwise = reduceArrAux op (contractArr op ar)

                   where l = lengthArr ar


contractArr :: (a -> a -> a) -> Arr a -> Arr a
contractArr op ar | even l    = tabulateArr opi      (div l 2)
                  | otherwise = tabulateArr oddOpi (div l 2 + 1)
    
    where
      opi i = op (nthArr ar (2 * i)) (nthArr ar (2 * i + 1))   
      l = lengthArr ar
      oddOpi i = if i /= div l 2  then opi i
                                  else nthArr ar (2 * i)


scanArr :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanArr op b s | lengthArr s == 0 = (emptyArr,b) 
               | lengthArr s == 1 = (singletonArr b, op b (nthArr s 0))
               | otherwise         = let
                                        sc            = contractArr op s                          -- contraemos la entrada
                                        (ss, ts)      = scanArr op b sc                           -- llamamos recursivamente
                                        s'            = appendArr ss (singletonArr ts)       
                                        
                                        expandArr i   = if even i then nthArr s' (div i 2)
                                                                  else op (nthArr s' (div i 2))
                                                                          (nthArr s (i - 1))

                                        r             = tabulateArr expandArr (lengthArr s + 1)   -- expandimos el resultado
                                      in
                                        (takeArr r (lengthArr r - 1), nthArr r (lengthArr r - 1)) -- dividimos el resultado


fromListArr :: [a] -> Arr a
fromListArr = A.fromList