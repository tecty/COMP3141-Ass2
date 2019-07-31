{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  -- Char 
  Char :: String -> RE Char
  -- Seq
  Seq  :: RE a -> RE b ->  RE (a, b)
  -- Choose
  Choose ::  RE a -> RE a -> RE a -- type family
  -- Star
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail  = failure
match (Char cs) = do 
  x <- readCharacter
  if x `elem` cs 
    then pure x
    else failure
match (Seq a b) = do 
  ra <- match a 
  rb <- match b 
  pure (ra, rb)
match (Choose a b) = match a <|> match b
match (Star a) = 
  (:) <$> match a <*> match (Star a) <|>  pure []
match (Action f a) = f <$> match a 


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs =  Action (uncurry (:)) (Seq x xs)  

string :: String -> RE String
string [] = Action (const "") Empty
string (x:xs) = Choose (Char [x] `cons` string xs) Fail

rpt :: Int -> RE a -> RE [a]
rpt 0 re = Action (const []) Empty
rpt n re = Choose (re `cons` rpt (n-1) re) Fail
-- rpt n re = error "Not implement"

rptRange :: (Int, Int) -> RE a -> RE [a]
-- rptRange (x,y) re = error "Not implement"
rptRange (x,y) re = (choose . map (`rpt` re)) [y,y-1..x]

option :: RE a -> RE (Maybe a)
option re = Choose (Action Just re) (Action (const Nothing) Empty)
-- option re = Action (\x -> Maybe x) re 

plus :: RE a -> RE [a]
plus re = re `cons` Star re

choose :: [RE a] -> RE a
-- choose res = error "not implemnt"
choose = foldr Choose Fail
-- choose (re:res) = Choose re (choose res)
-- choose [] = Fail