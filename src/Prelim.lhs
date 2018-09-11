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


-- An INSTRUCTION CELL organization:
-- 
--  OLD:
--  *----------------------*
--  | Instruction register |
--  | Operand register 1   | (holds opcodes)
--  | Operand register 2   | 
--  *----------------------*

-- NEW:
--  *----------------------*
--  | Instruction register |
--  | Operand register 1   | (holds opcodes)
--  | Operand register 2   | 
--  *----------------------*

newtype Address = Address Int 

newtype OperandRegister a = OR a

data OpCode = OpCode
data PredCode = PredCode
data BoolCode = BoolCode
data ResultTag = RTGate | RTValue

data InstructionRegister = 
     IROp OpCode Address 
    | IRPred PredCode ResultTag Address 
    | IRBool BoolCode ResultTag Address

data GatingCode = 
    GCNo 
    | GCTrue
    | GCFalse
    | GCConstant


data ControlReceiver
data DataReceiver

-- This int can be float as well, or whatever
data OperandRegisterData =  ORData GatingCode Int
data OperandRegisterBool = ORBool GatingCode ControlReceiver ResultTag DataReceiver



-- The latent instruction cell that is executed.
-- This is very strange, I don't understand why it's built this way
data InstCell = IC_IDD InstructionRegister OperandRegisterData OperandRegisterData
            | IC_IBB InstructionRegister OperandRegisterBool OperandRegisterBool
            | IC_IID InstructionRegister InstructionRegister OperandRegisterData
            | IC_IIB InstructionRegister InstructionRegister OperandRegisterBool


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
