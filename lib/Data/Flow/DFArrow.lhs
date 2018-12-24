\begin{code}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Arrows #-}
module Data.Flow.DFArrow where
import Prelude()
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category as C



newtype Addr = Addr Int deriving(Eq)
instance Show Addr where
    show (Addr i) = "0x" ++ show i

newtype Imm = Imm Int deriving(Eq)
instance Show Imm where
    show (Imm i) = "imm-" ++ show i

-- DF = dataflow. Consider nodes in the dataflow graph
data NodePrim = AddImm2 Imm Imm Addr | Root | Seq NodePrim NodePrim deriving(Eq, Show)

--http://hackage.haskell.org/package/opaleye-0.6.7003.1/docs/src/Opaleye.Internal.QueryArr.html#QueryArr 
newtype DFArr i o = DFArr ((i, NodePrim) -> (o, NodePrim))

-- Function to run an NodePrim arrow
runInstArr :: DFArr i o -> (i, NodePrim) -> (o, NodePrim)
runInstArr (DFArr f) = f

instance C.Category DFArr where
 (.) :: DFArr b c -> DFArr a b -> DFArr a c
 (.) (DFArr fbc) (DFArr fab) = DFArr (fbc . fab)

 id :: DFArr i i
 id = DFArr id

instance Arrow DFArr where
    arr :: (b -> c) -> DFArr b c
    arr f = DFArr $ \(i, dfn) -> (f i, dfn)

    first :: DFArr b c -> DFArr (b, x) (c, x)
    first (DFArr fbc) =
        DFArr $ \((b, x), dfn) -> 
        let (c, dfn') = fbc (b, dfn) in 
        ((c, x), dfn')

-- Sources
type SourceArr = DFArr
type Source = DFArr ()

-- Destinations
type DestArr = DFArr
type Dest = DFArr ()

-- Something that denotes that we are waiting for a destination
data NeedDest a = NeedDest (a -> NodePrim)



srcimm :: Int -> Source Imm
srcimm i = DFArr (\((), prim) -> (Imm i, prim))

srcaddr :: Addr -> Source Addr
srcaddr a = DFArr (\((), prim) -> (a, prim))


addimm2 :: DFArr (Imm, Imm) (NeedDest Addr)
addimm2 = DFArr (\((imm1, imm2), prim) -> (NeedDest (\addr -> AddImm2 imm1 imm2 addr), prim))


-- Cut out the middlemen
addimm2' :: Addr -> DFArr (Imm, Imm) Addr
addimm2' addr = DFArr (\((imm1, imm2), prim) -> (addr, Seq (AddImm2 imm1 imm2 addr) prim))


destmem :: Addr -> DFArr (NeedDest Addr) Addr
destmem addr = DFArr (\(NeedDest f, prim)  -> 
    let prim' = (f addr) in
        (addr, prim'))

seal :: DFArr () b -> DFArr () ()
seal a = (C..)  (arr (const ())) a

prepare :: DFArr () () -> NodePrim
prepare arr = snd $ runInstArr arr ((), Root)

p1 :: NodePrim
p1 = prepare $ 
    proc () -> do
                imm1 <- srcimm 10 -< ()
                imm2 <- srcimm 20 -< ()
                dest2 <-  addimm2' (Addr 10) -< (imm1, imm2)
                returnA -< ()

p1' :: NodePrim
p1' = prepare $ undefined
        



-- prepareQuery converts a query to a SQL string, so it's
-- probably the most important function in Opaleye for me
\end{code}
