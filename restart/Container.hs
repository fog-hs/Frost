{-# Language
 TypeFamilies
,ConstraintKinds
,MultiParamTypeClasses
#-}
module Container where
import BoundedInt

class Indexed (v :: * -> *) where
 type Index v :: *
 
class Indexed v => Accessible v where
 access  :: v a ->  Index v  -> a
 collect :: v a -> [Index v] -> [a]

class Indexed v => Insert v where
 insert  :: v a -> Index v -> a  -> v a
 replace :: v a -> [(Index v, a)]  -> v a

class Indexed v => Modifiable v where
 modify :: v a -> Index v -> (a -> a)  -> v a
 update :: v a -> [(Index v,(a -> a))] -> v a

type Container c = (Accessible c,Insert c,Modifiable c)

----
-- moadic varients, for mutable containers

-- unsafeRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
class Indexed v => AccessibleM m v where
 accessM  :: v a ->  Index v  -> m a
 collectM :: v a -> [Index v] -> m [a]

-- unsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
class Indexed v => InsertM m v where
 insertM  :: v a ->   Index v -> a   -> m ()
 replaceM :: v a -> [(Index v,   a)] -> m ()

-- unsafeModify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
class Indexed v => ModifiableM m v where
 modifyM :: v a ->   Index v -> (a -> a)  -> m ()
 updateM :: v a -> [(Index v,  (a -> a))] -> m ()
