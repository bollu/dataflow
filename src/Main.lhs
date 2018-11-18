\begin{code}
 {-# LANGUAGE Arrows #-}

module Main where
import Prelim
import Naive
import qualified Arrow as A
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow

add :: Source -> Source -> Dest -> Gating -> Inst
add = Inst Add

imm :: Int -> Source
imm = SourceImm . Value

destmem :: Int -> Dest
destmem = DestAddr . Addr

p :: Arrow a => a i o -> a i o
p = proc a ->  do 
                returnA -< a

program :: M.Map InstId Inst
program =
   M.fromList $ map (\(k, v) -> (InstId k, v))
     [(1, add (imm 10) (imm 20) (destmem 1) NoGating)]

main :: IO ()
-- main = mapM_ (print) (traceProcessor (initProcessor program))
main = do
        print A.p1

\end{code}
