{-# Language 
 ScopedTypeVariables
,RankNTypes
,TypeApplications
,KindSignatures
,TypeSynonymInstances
,DataKinds
,TypeFamilies
,FlexibleInstances
,MultiParamTypeClasses
#-}

module IVecPartOne where

import Data.Proxy
import qualified Data.Vector as V
import Control.Monad.ST

import Container
import BoundedInt
import Sized
import List

type RawIVec = V.Vector 

----
-- Sized API for RawIVec

instance HasSize (RawIVec a) where
 type SizeType (RawIVec a) = Nat

instance Sized (RawIVec a) where
 getSize = toNat . V.length

----
-- Container API for RawIVec

instance Indexed RawIVec where
 type Index RawIVec = Int

-- unsafe updates used
-- can use safe updates and reimplement type checked bounds version
-- but at the moment, these are used by that implementation.
instance Insert RawIVec where
 insert  v bi a = replace v [(bi,a)]
 replace v xs = V.unsafeUpd v xs

instance Modifiable RawIVec where
 modify v bi f = insert v bi (f (access v bi))
 update v [] = v
 update v ((bi,f):xs) = update (modify v bi f) xs

instance Accessible RawIVec where
 access  v i      = V.unsafeIndex v i
 collect v []     = []
 collect v (n:ns) = access v n : collect v ns

----
-- IVec

data IVec (n :: Nat) a = IVec (RawIVec a)

toIVec'' :: IsNat n => Proxy (n :: Nat) -> [a] -> IVec n a 
toIVec'' p xs | getSize xs == (getNat p) = IVec (V.fromList xs)
           | otherwise = error e
 where e = "\n\ttoIVec, passed wrong lengthed list; " ++ "\n\t of length; " ++ show (length xs) ++ "\n\t /= " ++ show (show (fromNat (getNat p)))

toIVec' :: IsList (List n a) => Proxy n -> [a] -> IVec n a 
toIVec' p = toIVec . (toList p)

toIVec :: IsList (List n a) => List n a -> IVec n a 
toIVec xs = IVec (V.fromList (toUnsized xs))

----
-- Sized API for IVec

instance HasSize (IVec n a) where
 type SizeType (IVec n a) = Nat

instance IsNat n => Sized (IVec n a) where
 getSize _ = getNat (Proxy @n)

instance IsNat n => HasSized (RawIVec a) (n :: Nat) where
 type SizedVersion (RawIVec a) n = IVec n a

----
-- Container API for IVec

instance IsNat n => Indexed (IVec n) where
 type Index (IVec n) = BoundedInt n 

instance IsNat n => Insert (IVec n) where
 insert  (IVec v) (BoundedInt i) a = IVec $ insert v i a 
 replace (IVec v) xs = IVec $ replace v xs'
  where
   xs' = let (ys,zs) = unzip xs in zip (fromBoundedInts (Proxy @n) ys) zs


instance IsNat n => Modifiable (IVec n) where
 modify (IVec v) (BoundedInt i) f = IVec $ insert v i (f (access v i))
 update (IVec v) xs = IVec $ update v xs'
  where
   xs' = let (ys,zs) = unzip xs in zip (fromBoundedInts (Proxy @n) ys) zs
-- ALERT test fusion of zip and unzip

instance IsNat n => Accessible (IVec n) where
 access  (IVec v) (BoundedInt i) = access v i
 collect (IVec v)  xs = collect v (fromBoundedInts (Proxy @n) xs)

 -- rewrite this;
-- FUUUUXL
-- having the index as a Nat is bad!!
-- wait
-- isnt it that bounded int uses the nat for the bounds?
-- and so wraps an int?
-- the danger would be having the convert from 
-- a nat as an accessor index 
-- retrived from type level
-- it seems ok though... only the bound is Nat...
 
