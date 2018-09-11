\begin{code}
module Prelim where

-- All notation follows paper: 
-- A preliminary Architecture for a Basic Data-Flow Processor 

-- [_] -> element that performs computation
-- (_) -> data
--- CAPS -> unit that performs routing / IO

--  *---------------[Operational unit]<---------------------------*                             
--  |                                                             |                             
--  |                             [Decision unit]<-------------*  |                         
--  |                                     *                    |  |              
--  |                                     |                    |  |
--  |                                (control packet)          |  |
--  |                                     |                    |  |
--  |                                     |                    |  |      
-- (data packet)                          v                    |  |  
--  |                                CONTROL NETWORK           |  |                                         
--  |                                     |                    |  |                          
--  |                                     |                    |  |                          
--  |                                     |                    |  |                          
--  |                                     |                    |  |                          
--  |                                     |                    |  |                          
--  |                                     v                    |  |                          
--  |                              |Instruction Cell 0|        ^  ^                                            
--  *------------->DISTRIBUTION--->|Instruction Cell 1|----->ARBITRATION                                                      
--                  NETWORK        |Instruction Cell n|        NETWORK                                                      
--                 
-- The OpCode of an instruction cell.
data InstCode = ICOperator | ICDecider


-- An instruction cell organization:
-- 
--  |Instruction register|  (holds opcodes)
--  |Operand register 1  | 
--  |Operand register 2  | 

newtype Address = Address Int 

newtype OperandRegister a = OR a

data OpCode = OpCode
data PredCode = PredCode
data BoolCode = BoolCode
data ResultTag = ResultTag

data InstructionRegister = 
    IROp OpCode Address |
    IRPred PredCode ResultTag Address |
    IRBool BoolCode ResultTag Address

-- The latent instruction cell that is executed.
data InstCell = InstCell


-- The active operational unit.
data OpUnit = OpUnit

-- operational unit --distribution network-> instruction cell
data DataPacket = DataPacket


-- decision unit --control packet--> control network --> instruction cell
data ControlPacket = CPGate | CPValue

data Processor = Processor {
    procICells :: [InstCell],
    procOpUnits :: [OpUnit]
}

\end{code}
