\begin{code}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Flow.FreeArrow where
import Prelude()
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category as C

data FreeA (eff :: * -> * -> *) a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a1 b1 -> FreeA eff a2 b2 -> FreeA eff (a1, a2) (b1, b2)
    Choice :: FreeA eff a b -> FreeA eff (Either a d) (Either b d)

class Show2 eff where
  show2 :: eff a b -> String

instance (Show2 eff, Show2 (FreeA eff)) => Show2 (FreeA eff) where
  show2 (Pure f) = show "pure"
  show2 (Effect eff) = show2 eff
  show2 (Seq a b) = "(" ++ show2 a ++ ">>>" ++ show2 b ++ ")"
  show2 (Par a b) = "(" ++ show2 a ++ "|||" ++ show2 b ++ ")"
  show2 (Choice a) = "(??" ++ show2 a ++ "??)"


effect :: eff a b -> FreeA eff a b
effect = Effect

instance Category (FreeA eff) where
    id = Pure id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Pure
    first f = Par f id
    second f = Par id f
    (***) = Par


instance ArrowChoice (FreeA eff) where
    left = Choice

newtype Addr = Addr Int
instance Show Addr where
    show (Addr i) = "0x" ++ show i

newtype Imm a = Imm a deriving(Eq)
instance Show a => Show (Imm a) where
    show (Imm i) = "imm-" ++ show i

newtype Reg a = Reg a deriving(Eq)
instance Show a => Show (Reg a) where
    show (Reg a) = "reg-" ++ show a

-- MIPS register file: 
-- http://www.cs.uwm.edu/classes/cs315/Bacon/Lecture/HTML/ch05s03.html
-- replicate MIPS ISA: 
-- http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html
-- Show a => ... is a hack, FIXME!
data Node i o where
    -- be a source of some immediate data
    NImm :: Show a => Imm a -> Node () a
    -- register, do I need this? or do only memory ops allow one to refer to a new regiser? Think
    NReg :: Show a => Reg a -> Node () a
    -- add two register values
    NAdd :: Node (Reg Int, Reg Int) (Reg Int)
    -- add register and immediate
    NAddI :: Node (Reg Int, Imm Int) (Reg Int)
    -- write some data into memory
    NIWriteMem :: a -> Node a Addr

type Program a b = FreeA Node a b

deriving instance (Show i, Show o) => Show (Node i o)
instance Show2 Node where
\end{code}
