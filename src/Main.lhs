\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Main where
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import FreeExamples
import NaiveExamples
import Control.Monad.Writer
import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

-- | Width of the vector instruction
newtype Width = Width Int

-- | Identifier
type Id = String

-- | Index expression
type Ix = Exp

-- | Length expression
type Length = Exp


-- | Eliminate superfluous skip expressions in the code.
elimSkip :: Code -> Code
elimSkip (Skip :>>: c) = elimSkip c
elimSkip (c :>>: Skip) = elimSkip c
elimSkip (c :>>: c') = elimSkip c :>>: elimSkip c'
elimSkip c = c

data Value =
  IntVal Int
  | FloatVal Float
  | BoolVal Bool
  deriving(Eq, Ord)

instance Pretty Value where
  pretty (IntVal i) = pretty i
  pretty (FloatVal f) = pretty f
  pretty (BoolVal b) = pretty b

instance Show Value where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

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
  deriving(Eq, Ord)

prettySexp :: [Doc a] -> Doc a
prettySexp as = nest 2 .  parens .  hsep $ as

instance Pretty Exp where
  pretty (Var id) = pretty id
  pretty (Literal l) = pretty l
  pretty (Index id ix) = pretty id <> brackets (pretty ix)
  pretty (e :+: e') = prettySexp $ [pretty "+", pretty e, pretty e']

instance Show Exp where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Num Exp where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  fromInteger = Literal . IntVal . fromInteger
  abs = error "no abs on Exp"
  signum = error "no signum on Exp"

data Code =
  Skip
  | Code :>>: Code
  | For Id Exp Code
  | Allocate Id Length
  | Write Id Ix Exp
  deriving(Eq, Ord)

instance Pretty Code where
  pretty Skip = pretty "skip"
  pretty (c :>>: c') = vsep $ [pretty c, pretty c']
  pretty (For id lim body) =
    prettySexp [pretty "for", prettySexp [pretty id, pretty "<=", pretty lim], pretty body]
  pretty (Allocate id len) = prettySexp $ [pretty "alloc", pretty id, pretty len]
  pretty (Write id ix exp) = prettySexp $ [pretty id <> brackets (pretty ix), pretty ":=", pretty exp]

instance Show Code where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty


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
newID :: String -> CM Id
newID name = CM $ \i -> (i+1, mempty,  (name <> "-" <> show i))

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
  id <- newID "%iv"
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

-- | Compute the length of a PushT
pushTLen :: PushT -> Length
pushTLen (Generate l _ ) = l
pushTLen (Use (CMMem _ l)) = l
pushTLen (Map _ p) = pushTLen p
pushTLen (Append l p1 p2) = pushTLen p1 + pushTLen p2

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

-- | Code generate the allocation of an array and return a handle
-- | to the alocated array
allocate :: Length -> CM (CMMem)
allocate l  = do
  id <- newID "#arr"
  appendCode $ Allocate id l
  return (CMMem id l)

mainArr :: IO ()
-- mainArr = mapM_ (print) (traceProcessor (initProcessor program))
mainArr = do
        runNaiveExamples
        runFreeExamples

-- | Materialize an array, and return a handle to the materialized array
toVector :: PushT -> CM (CMMem)
toVector p = do
  -- | How do I get the length of the array I need to materialize?
  writeloc <- allocate (pushTLen p)
  apply p $ \ix val -> (cmWrite writeloc ix val)
  return $ writeloc

-- | Materialize an array and ignore the handle
toVector_ :: PushT -> CM ()
toVector_ p = toVector p >> pure ()



main :: IO ()
main = do
  let vec1 = CMMem "src" 10
  let code = elimSkip $ genCMCode $ toVector_ (Use vec1)
  print code
  
\end{code}
