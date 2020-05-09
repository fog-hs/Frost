{-# Language
 KindSignatures
,TypeOperators
,DataKinds
,PolyKinds
,GADTs
,TypeFamilies
,UndecidableInstances
,TypeFamilyDependencies
,FlexibleInstances
,RankNTypes,TypeApplications
,ScopedTypeVariables
,ConstraintKinds
#-}
module Nat where
import Data.Proxy
import GHC.Exts (Any) -- type family Any :: k	
import qualified GHC.TypeNats
data family Sing (a :: k)

class SingI (a :: k) where
 sing :: Sing a

class SingE (a :: k) where
 type Demote a :: *
 fromSing :: Sing a -> Demote (Any :: k)

class (SingI a,SingE a) => SingRep a
instance (SingI a,SingE a) => SingRep a

--

type Nat = [()]

type IsNat (n :: Nat) = SingI n

showNat :: Nat -> String
showNat = show . length

toNat :: Int -> Nat
toNat n = replicate n ()

fromNat :: Nat -> Int
fromNat = length

type Zero = ('[] :: Nat)
zero' :: Nat
zero' = []

type Succ = (:) '()
succ' :: Nat -> Nat
succ' = (:) ()

instance Enum Nat where
 succ = succ'
 pred [] = error "pred Zero"
 pred xs = tail xs
 toEnum = toNat
 fromEnum = fromNat

data instance Sing (a :: Nat) where
 SingZero :: Sing Zero
 SingSucc :: SingRep n => Sing n -> Sing (Succ n)

instance SingI Zero where
 sing = SingZero

instance SingRep n => SingI (Succ n) where
 sing = SingSucc sing

instance SingE (a :: Nat) where
 type Demote a = Nat
 fromSing SingZero = []
 fromSing (SingSucc n) = () : (fromSing n)

getNat :: forall n. IsNat n => Proxy n -> Nat
getNat _ = fromSing (sing :: Sing n)

{-
*Main> fromSing (sing :: Sing Zero)
[]
*Main> fromSing (sing :: Sing (Succ Zero))
[()]
-}

type family ToNat (n :: GHC.TypeNats.Nat) :: Nat where
 ToNat 0 = Zero
 ToNat n = Succ (ToNat (n GHC.TypeNats.- 1))
 

