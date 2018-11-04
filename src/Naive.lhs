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

canFire :: FireState -> Bool
canFire CanFire = True
canFire CantFire = False


-- A value that is not gated is always fireable
-- A value that is gated needs to be enabled, and must have
-- the gating to be set to false
gatingToFireState :: Gating -> FireState
gatingToFireState NoGating = CanFire
gatingToFireState (Gating (Just True)) = CanFire
gatingToFireState _ = CantFire

-- Sources for an instruction.
data Source = SourceImm Value | SourceAddr Addr deriving(Eq, Show)

newtype InstId = InstId Int deriving(Eq,Show, Ord)

-- Destinations for an instruction, either an address or another instruction
-- in the case of control
data Dest = DestInst InstId | DestAddr Addr deriving(Eq, Show)

data OpCodeBinary = Add | Leq | ITE
    deriving(Eq, Show)

-- types of instructions 
data Inst =  Inst {
    opcode :: OpCodeBinary,
    source1 :: Source,
    source2 :: Source,
    dest :: Dest,
    gating :: Gating
} deriving(Eq, Show)

newtype Memory = Memory (M.Map Addr Value) deriving(Show)

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

memInit :: Memory
memInit = Memory M.empty

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


newtype InstMap = InstMap (M.Map InstId Inst) deriving(Show)

setInstGating :: InstId -> Bool -> InstMap -> InstMap
setInstGating iid g (InstMap im) =
  InstMap $ M.adjust
        (\inst -> inst { gating = Gating (Just g) }) iid im

getFireableInst :: InstMap -> Maybe (InstId, Inst)
getFireableInst (InstMap im) =
  let im' = M.filter (canFire . gatingToFireState . gating) im
  in if M.null im'
  then Nothing
  else Just $ M.elemAt 0 im'

removeInst :: InstId -> InstMap -> InstMap
removeInst iid (InstMap im) = InstMap $ M.delete iid im
    
data ProcessorState = ProcessorState {
    memory :: Memory,
    insts :: InstMap
} deriving(Show)

-- Convert a source to a value, by either reading from memory,
-- or propogating the constant value
sourceToVal :: Source -> Memory -> Value
sourceToVal (SourceImm v) _ = v
sourceToVal (SourceAddr a) mem = memRead mem a

liftBinArithToValue :: (Int -> Int -> Int) -> Value -> Value -> Value
liftBinArithToValue  f (Value i) (Value j) = Value (f i j)

  
intToBool :: Int -> Bool
intToBool 1 = True
intToBool 0 = False

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 1

liftBinBooleanToValue :: (Bool -> Bool -> Bool)
                      -> Value -> Value -> Value
liftBinBooleanToValue f =
  liftBinArithToValue
    (\i1 i2 -> boolToInt (f (intToBool i1) (intToBool i2)))

evalInst :: Inst -> ProcessorState -> ProcessorState
evalInst (Inst Add src1 src2 (DestAddr destaddr) _) ps = 
    ps { memory = memWrite (memory ps) destaddr outv } where
        v1 = sourceToVal src1 (memory ps)
        v2 = sourceToVal src2 (memory ps)
        outv = liftBinArithToValue (+) v1 v2
            
evalInst (Inst ITE src1 _ (DestInst iid) _) ps = 
    ps { insts = insts' } where
        (Value vcond) = sourceToVal src1 (memory ps)
        insts' = setInstGating  iid (intToBool vcond) (insts ps)

stepProcessor :: ProcessorState -> Maybe ProcessorState 
stepProcessor ps = 
    do 
        (curid, curi) <- getFireableInst (insts ps)
        let ps' = evalInst  curi ps
        let insts' = removeInst curid (insts ps)
        
        return $ ps' {insts = insts'}

initProcessor :: M.Map InstId Inst -> ProcessorState
initProcessor insts = ProcessorState {
    memory = memInit,
    insts = InstMap insts
}

traceProcessor :: ProcessorState -> [ProcessorState]
traceProcessor ps = 
    case stepProcessor ps of
        Just ps' -> ps':traceProcessor ps'
        Nothing -> []
\end{code}
