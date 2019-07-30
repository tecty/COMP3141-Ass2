{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  -- Char 
  Char :: RE Char 
  -- Seq
  Seq  :: RE (RE a, RE b)
  -- Choose
  Choose ::  RE (RE a, RE b)
  -- Star
  Star :: RE (RE a)
  Action :: (a -> b) -> RE a -> RE b
match :: (Alternative f, Monad f) => RE a -> Hare f a
match (Empty _) = pure None
match (Fail _) = failure
match (Char cs) = do 
  x <- readCharacter
  guard (x `elem` cs)
  pure (Character x)
match (Seq a b) = do 
  ra <- match a 
  rb <- match b  
  pure (Tuple ra rb)
match (Choose a b) = 
  match a <|> match b 
match (Star a) = 
  addFront <$> match a <*> match (Star a) <|> pure (Repetition [])
  where 
    addFront x (Repetition xs) = Repetition (x:xs)
    addFront _ _ = error "impoosible"


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = error "'cons' unimplemented"

string :: String -> RE String
string xs = error "'string' unimplemented"

rpt :: Int -> RE a -> RE [a]
rpt n re = error "'rpt' unimplemented"

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = error "'rptRange' unimplemented"

option :: RE a -> RE (Maybe a)
option re = error "'option' unimplemented"

plus :: RE a -> RE [a]
plus re = error "'plus' unimplemented"

choose :: [RE a] -> RE a
choose res = error "'choose' unimplemented"

