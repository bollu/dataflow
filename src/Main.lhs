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

type Id = String
type Ix = Exp
type Length = Exp

data Code = Skip
  | Code :>>: Code
  | For Id Exp Code
  | Allocate Id Length
  | Write Id Ix Exp
  deriving(Eq, Show, Ord)


data Value =
  IntVal Int
  | FloatVal Float
  | BoolVal Bool
  deriving(Eq, Show, Ord)

data Exp =
  Var Id
  | Literal Value
  | Index Id Ix
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

--- for loop
for_ :: Exp -- ^ Limit of the loop. Variable goes from 0 <= v <= limit
  -> (Exp -> CM ()) -- ^ Function that receives the loop induction variable and generates the loop body
  -> CM ()
for_ lim f = do
  id <- newID
  code <- extractCMCode $ f (Var id)
  appendCode $ For id lim code

-- | A chunk of linear memory with an ID and a length attached to it
data CMMem = CMMem Id Length

-- | Generate an index expression into the CMMem
cmIndex :: CMMem -> Ix -> Exp
cmIndex (CMMem name _) ix = Index name ix

-- | Generate a write statement into the CMMem
cmWrite ::CMMem -- ^ Array to be written
  -> Ix -- ^ Index to write to
  -> Exp -- ^ Value to write
  -> CM ()
cmWrite (CMMem name _) ix v =
  appendCode $ Write name ix v
-- | Defunctionalized push array
data PushT where
  Generate :: Length -> (Ix -> Exp) -> PushT
  Use :: CMMem -> PushT
  Map :: (Exp -> Exp) -> PushT -> PushT
  Append :: Length -> PushT -> PushT -> PushT


-- | Generate code from a pushT given an index and an expression for
-- | the value at that index
apply :: PushT ->  (Ix -> Exp -> CM ()) -> CM ()
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
writePushT ::PushT -> CMMem -> CM ()
writePushT p cmem = apply p $ \ix val -> (cmWrite cmem ix val)


main :: IO ()
main = do
  let vec1 = CMMem "v1" 10
  let vec2 = CMMem "v2" 10
  let code = genCMCode $ writePushT ((Use vec1)) vec2
  print code
  
\end{code}
