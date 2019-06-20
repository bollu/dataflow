 {-# LANGUAGE Arrows #-}
module FreeExamples(runFreeExamples) where
-- import Data.Flow.Prelim
-- import Data.Flow.FreeArrow
-- import Data.Traversable
-- import qualified Data.Map.Strict as M
-- import Control.Arrow

-- TODO: allow not having to pass -< () in arrow syntax

runFreeExamples :: IO ()
runFreeExamples = putStrLn "free examples"

-- immp :: a -> Program () (Imm a)
-- immp a = Effect (NImm  (Imm a))
--
-- regp :: a -> Program () (Reg a)
-- regp a = Effect (NReg  (Reg a))
--
-- addp :: Program (Reg Int, Reg Int) (Reg Int)
-- addp = Effect $ NAdd
--
-- addreg :: Program () ()
-- addreg = proc () -> do
--   a <- regp 10 -< ()
--   b <- regp 20 -< ()
--   plus <- addp -< (a, b)
--   returnA -< ()
--
-- runFreeExamples :: IO ()
-- runFreeExamples = do
--   print "free examples"
--   putStrLn . show2 $ addreg
