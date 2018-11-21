\begin{code}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Free where
import Prelude()
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category as C

data FreeA eff a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a1 b1 -> FreeA eff a2 b2 -> FreeA eff (a1, a2) (b1, b2)
    Choice :: FreeA eff a b -> FreeA eff (Either a d) (Either b d)

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

newtype Imm = Imm Int deriving(Eq)
instance Show Imm where
    show (Imm i) = "imm-" ++ show i

newtype Reg = Reg Int deriving(Eq)
instance Show Reg where
    show (Reg i) = "reg-" ++ show i



-- MIPS register file: http://www.cs.uwm.edu/classes/cs315/Bacon/Lecture/HTML/ch05s03.html
-- replicate MIPS ISA: http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html
data Node i o where
    -- be a source of some immediate data
    NImm :: a -> Node ()  (Imm a)
    -- register, do I need this? or do only memory ops allow one to refer to a new regiser? Think
    NReg :: a -> Node () (Reg a)
    -- add two register values
    NAdd :: Node (Reg Int, Reg Int) (Reg Int)
    -- add register and immediate
    NAddI :: Node (Reg Int, Imm Int) (Reg Int)


    -- write some data into memory
    NIWriteMem :: a -> Node a Addr


instance (Show i, Show o) => Show (Node i o) where
    show (NImm i) = show i
    show (NReg r) = show r
    show NAdd = show "add"
    show NAddI = show "addi"


\end{code}
