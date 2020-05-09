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

module MVec (module MVec,module MVecPartOne) where

import Data.Proxy
import MVecPartOne
import Sized
import Nat
import Container

instance IsNat n => ToSized (RawMVec a) n where
 toSized n | getNat (Proxy @n) == n = MVec 
           | otherwise = error "toSized given wrong Nat as size, use MVec constructor directly"

instance IsNat n => HasUnsized (MVec n a) where
 type UnsizedVersion (MVec n a) = RawMVec a

instance IsNat n => ToUnsized (MVec n a) where
 toUnsized (MVec v) = v

type family IsMVec x where
 IsMVec (MVec n a) = (IsNat n,ToUnsized (MVec n a),ToSized (RawMVec a) n)



