 \begin{code}

module Main where
import Prelim
import Naive
import Data.Traversable
import qualified Data.Map.Strict as M

add :: Source -> Source -> Dest -> Gating -> Inst
add = Inst Add

imm :: Int -> Source
imm = SourceImm . Value

destmem :: Int -> Dest
destmem = DestAddr . Addr

program :: M.Map InstId Inst
program =
   M.fromList $ map (\(k, v) -> (InstId k, v))
     [(1, add (imm 10) (imm 20) (destmem 1) NoGating)]

main :: IO ()
main = mapM_ (print) (traceProcessor (initProcessor program))

\end{code}
