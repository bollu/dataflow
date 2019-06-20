\begin{code}
 {-# LANGUAGE Arrows #-}
 {-# LANGUAGE TypeFamilies #-}
 {-# LANGUAGE GADTs #-}
 {-# LANGUAGE DataKinds #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import FreeExamples
import NaiveExamples
-- import Data.Vector
import Control.Monad.Writer
import Control.Monad.State


-- | Width of the vector instruction
newtype Width = Width Int


-- | Type family that normalizes tuples
type family Flatten a  where
  -- length 1
   Flatten () = ()
   -- length 2
   Flatten (a, b) = (Flatten a, Flatten b)
   -- length 3
   Flatten ((a, b), c) = (Flatten a, Flatten b, Flatten c)
   Flatten (a, (b, c)) = (Flatten a, Flatten b, Flatten c)

type Id = String
type Ix = Expr Int
type Length = Expr Int

data Code = Skip
  | Code :>>: Code
  | For Id Exp Code
  | Allocate Id Length
  | Write Id Exp Exp
  deriving(Eq, Show, Ord)


data Value =
  IntVal Int
  | FloatVal Float
  | BoolVal Bool
  deriving(Eq, Show, Ord)

data Exp =
  Var Id
  | Literal Value
  | Index Id Exp
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Mod Exp Exp
  | Div Exp Exp
  | Eq  Exp Exp
  | Gt  Exp Exp
  | LEq Exp Exp
  | Min Exp Exp
  | IfThenElse Exp Exp Exp 
  deriving(Show, Eq, Ord)

instance Num Exp where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  fromInteger = Literal . IntVal . fromInteger
  abs = error "no abs on Exp"
  signum = error "no signum on Exp"


data Expr a = E { unE :: Exp } deriving(Eq, Show, Ord)

liftExp2 :: (Exp -> Exp -> Exp) -> (Expr a -> Expr a -> Expr a)
liftExp2 f (E e) (E e') =  E (f e e')

instance Num (Expr a) where
  (+) = liftExp2 (:+:)
  (-) = liftExp2 (:-:)
  (*) = liftExp2 (:*:)
  fromInteger = E . Literal . IntVal . fromInteger
  abs = error "no abs on Expr"
  signum = error "no signum on Expr"


class Expable a where
  toExp :: a -> Exp
  fromExp :: Exp -> a

instance Expable Exp where
  toExp = id
  fromExp = id

instance Expable (Expr a) where
 toExp = unE 
 fromExp = E

instance Semigroup Code where
  (<>) = (:>>:)

instance Monoid Code where
  mempty = Skip

newtype CM a = CM  {runCM :: Integer -> (Integer, Code, a) }

instance Functor CM where
  fmap f cm =
    CM $ \i ->
      let (i', c, a) = runCM cm i
      in (i', c, f a)

instance Applicative CM where
  pure = return

  cma2b <*> cma = do
   a <- cma
   a2b <- cma2b
   return $ a2b a

instance Monad CM where
  return a = CM $ \i -> (i, mempty, a)
  cm >>= f = CM $ \i ->
    let (i', c', a') = runCM cm i
        (i'', c'', a'') = runCM (f a') i'
    in (i'', c' <> c'', a'')

-- | Generate a new ID
newID :: CM Id
newID = CM $ \i -> (i+1, mempty,  ("v-" <> show i))

-- | Append a section of code
appendCode :: Code -> CM ()
appendCode c = CM $ \i -> (i, c, ())

-- | Run a CM to extract out the code. Useful to generate code
-- | and then transplant to another location while ensure we 
-- | do not create overlapping IDs
extractCMCode :: CM () -> CM Code
extractCMCode cm =
  CM $ \i ->
    let (i', c, _) = runCM cm i
    in (i', mempty, c)

-- | Generate code from the CM
genCMCode :: CM () -> Code
genCMCode cm = let (_, c, _) = runCM cm 0 in c 
  

-- | Provide a way to get values from indeces
data Pull a = Pull Length (Ix -> a)

-- | Accept a write function (continuation) and codegen performing writes
data Push a = Push ((Ix -> a -> CM ()) -> CM ()) Length

map :: (a -> b) -> Pull a -> Pull b
map f (Pull l ixf) = Pull l (f . ixf)

index :: Pull a -> Ix -> a
index (Pull _ ixf) i = ixf i

zipWith :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
zipWith f (Pull l1 ixf1) (Pull l2 ixf2) = Pull (E $ Min (unE l1) (unE l2)) (\i -> f (ixf1 i) (ixf2 i))

{-
halve :: Pull a -> (Pull a, Pull a)
halve (Pull l ixf) = (Pull l2 ixf, Pull (l - l2) ixf')
  where
    l2 = l `div` 2
    ixf' i = ixf (i + l2)

reverse :: Pull a -> Pull a
reverse (Pull l ixf) = Pull l (\ix -> ixf (l - 1 - ix))

rotate :: Length -> Pull a -> Pull a
rotate r (Pull l ixf) = Pull l (\ix -> ixf ((ix+r) `mod_` l))
-}

ixMap :: (Ix -> Ix) -> Push a -> Push a
ixMap f (Push p l) = Push (\k -> p (\i a -> k (f i) a)) l

push :: Pull a -> Push a
push (Pull n ixf) = Push (\k -> for_ n $ \i -> k i (ixf i)) n

allocate :: Length -> CM (CMMem a)
allocate n = do
 id <- newID
 return $ CMMem id n


pull :: Expable a => Push a -> CM (Pull a)
pull (Push p n) = do 
  arr <- allocate n
  p $ write arr
  return $ Pull n (\i -> cmIndex arr i)

(+++) :: Push a -> Push a -> Push a
(Push p1 l1) +++ (Push p2 l2) = Push r (l1 + l2)
  where r k = do p1 k
                 p2 (\i a -> k (l1 + i) a)

-- Array creation
generate :: Length -> (Ix -> a) -> Push a
generate n ixf = Push (\k -> for_ n $ \i -> k i (ixf i)) n



data CMMem a = CMMem Id Length

-- |Memory operations
write :: Expable a => CMMem a -> Ix -> a -> CM ()
write (CMMem id _) ix v =
  appendCode $ Write id (unE ix) (toExp v)

-- | Why do we use Expable?
cmIndex :: Expable a => CMMem a -> Ix -> a
cmIndex (CMMem id len) ix = fromExp $ Index id (unE ix)

-- for loop
for_ :: Expable a => Expr Int -> (a -> CM ()) -> CM ()
for_ lim f = do
  id <- newID
  code <- extractCMCode $ f (fromExp (Var id))
  appendCode $ For id (toExp lim) code

-- | Defunctionalized push array
data PushT b  where
  Generate :: Length -> (Ix -> b) -> PushT b
  Use :: Expable b => CMMem b -> PushT b
  Map :: Expable a => (a -> b) -> PushT a -> PushT b
  Append :: Length -> PushT b -> PushT b -> PushT b

apply :: PushT a ->  (Ix -> a -> CM ()) -> CM ()
apply (Generate l ix2v) k = 
  for_ l (\ix -> k ix (ix2v ix))
apply (Use cmem@(CMMem _ n)) k = for_ n $ \ix -> k ix (cmIndex cmem ix)
apply (Map f p) k = apply p (\i a -> k i (f a))
apply (Append l p1 p2) k =
   apply p1 k >>
   apply p2 (\i a -> k (l + i) a)
   
mainArr :: IO ()
-- mainArr = mapM_ (print) (traceProcessor (initProcessor program))
mainArr = do
        runNaiveExamples
        runFreeExamples

-- | Write a pushT into a CM
writePushT :: Expable a => PushT a -> CMMem a -> CM ()
writePushT p cmem = apply p $ \ix val -> (write cmem ix val)


main :: IO ()
main = do
  let vec1 = CMMem "v1" 10
  let vec2 = CMMem "v2" 10
  let code = genCMCode $ writePushT (Map (\x -> x+1 :: Exp) (Use vec1)) vec2
  print code
  
\end{code}
