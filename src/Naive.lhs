\begin{code}
module Naive where
import Data.Semigroup
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)

newtype Addr = Addr Int deriving (Show, Eq, Ord)

newtype Value = Value Int deriving (Eq, Show, Ord)
-- We either have no gating enabled on an instruction, or we are gated,
-- in which case, we need to wait for the gating result
data Gating = NoGating | Gating (Maybe Bool) deriving (Show, Eq, Ord)

-- Tells us if a value can be fired or not.
data FireState = CanFire | CantFire deriving(Show, Eq)

-- Pessimism 
instance Semigroup FireState where
    CanFire <> CanFire = CanFire
    _ <> _ = CantFire

boolToFireState :: Bool -> FireState
boolToFireState True = CanFire
boolToFireState False = CantFire

fireStateToFilter :: FireState -> Bool
fireStateToFilter CanFire = True
fireStateToFilter CantFire = False


-- A value that is not gated is always active
gatingToFireState :: Gating -> FireState
gatingToFireState NoGating = CanFire
gatingToFireState (Gating (Just True)) = CanFire
gatingToFireState _ = CantFire

-- Sources for an instruction.
data Source = SourceImm Value | SourceAddr Addr deriving(Eq, Show)

data OpCodeBinary = Add | Mul | Leq | Eq | Xor | Or | Not 
    deriving(Eq, Show)

-- types of instructions 
data Inst =  Inst {
    opcode :: OpCodeBinary,
    source1 :: Source,
    source2 :: Source,
    dest :: Addr,
    gating :: Gating
} deriving(Eq, Show)

newtype Memory = Memory (M.Map Addr Value)

-- Tell us if we can read (ie, can fire) a memory state
canFireMemAddr :: Memory -> Addr -> FireState
canFireMemAddr (Memory mem) addr = boolToFireState $ mem M.!? addr == Nothing

-- Tell us if we can fire a source.
-- If we have a memory address we are waiting for, check if it
-- can be fired.
-- A constant can always be fired
canFireSource :: Memory -> Source -> FireState
canFireSource mem (SourceAddr addr)  = canFireMemAddr mem addr
canFireSource _ _ = CanFire

memWrite :: Memory -> Addr -> Value -> Memory
memWrite (Memory mem) addr v = Memory $ M.insert addr v mem 

memRead :: Memory -> Addr -> Value
memRead (Memory mem) addr = mem M.! addr



-- Check if an instruction can be fired. An instruction can be fired if
-- all of its arguments are available 
canFireInst :: Memory -> Inst -> FireState
canFireInst mem i = 
    canFireSource mem (source1 i) <> 
    canFireSource mem (source2 i) <>
    (gatingToFireState (gating i))

data ProcessorState = ProcessorState {
    memory :: Memory,
    insts :: [Inst]
}

-- Convert a source to a value, by either reading from memory,
-- or propogating the constant value
sourceToVal :: Source -> Memory -> Value
sourceToVal (SourceImm v) _ = v
sourceToVal (SourceAddr a) mem = memRead mem a

liftBinArithToValue :: (Int -> Int -> Int) -> Value -> Value -> Value
liftBinArithToValue  f (Value i) (Value j) = Value (f i j)

liftBinBooleanToValue :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
liftBinBooleanToValue f   =
    liftBinArithToValue (\i1 i2 -> boolToInt (f (intToBool i1) (intToBool i2)))  where
        intToBool 1 = True
        intToBool 0 = False

        boolToInt True = 1
        boolToInt False = 1

evalInst :: Inst -> Memory -> Memory
evalInst (Inst opcode src1 src2 dest _) mem = memWrite mem dest outv where
    v1 = sourceToVal src1 mem
    v2 = sourceToVal src2 mem
    outv = case opcode of
            Add -> liftBinArithToValue (+) v1 v2

stepProcessor :: ProcessorState -> Maybe ProcessorState 
stepProcessor ps = 
    do 
        let ais = filter ((== CanFire) . (canFireInst (memory ps))) (insts ps)
        curi <- listToMaybe ais
        let mem' = evalInst  curi (memory ps) 

        let insts' = drop 1 (insts ps)
        return $ ProcessorState mem' insts'
\end{code}
