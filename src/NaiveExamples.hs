 {-# LANGUAGE Arrows #-}
module NaiveExamples(runNaiveExamples) where
import Data.Flow.Naive
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import Control.Monad


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


runNaiveExamples :: IO ()
runNaiveExamples = do
  let p = initProcessor program
  let sts = traceProcessor p
  forM_ sts (\st -> putStrLn . show $ st) 
