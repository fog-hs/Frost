{-# Language
 TypeSynonymInstances
,FlexibleInstances
,MultiParamTypeClasses
,GADTs
,TypeApplications
,ScopedTypeVariables
,RankNTypes
,TypeFamilies
#-}

module IVec (module IVec,module IVecPartOne) where

import Data.Proxy
import IVecPartOne
import Sized
import Nat
import Container

instance IsNat n => ToSized (RawIVec a) n where
 toSized n | getNat (Proxy @n) == n = IVec 
           | otherwise = error "toSized given wrong Nat as size, use IVec constructor directly"

instance IsNat n => HasUnsized (IVec n a) where
 type UnsizedVersion (IVec n a) = RawIVec a

instance IsNat n => ToUnsized (IVec n a) where
 toUnsized (IVec v) = v

type family IsIVec x where
 IsIVec (IVec n a) = (IsNat n,ToUnsized (IVec n a),ToSized (RawIVec a) n)

----
{-
instance IsNat n => Insert (IVec n) where
 insert  v bi a = replace v [(bi,a)]
 replace (IVec v) xs = IVec $ V.unsafeUpd v (map (\(a,b) -> (fromBoundedInt a,b)) xs)

instance IsNat n => Modifiable (IVec n) where
 modify v bi f = insert v bi (f (access v bi))
 update v [] = v
 update v ((bi,f):xs) = update (modify v bi f) xs

instance IsNat n => Accessible (IVec n)	 where
 access (IVec v) (BoundedInt i) = V.unsafeIndex v i
 collect v []     = []
 collect v (n:ns) = access v n : collect v ns
-}

