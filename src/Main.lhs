\begin{code}
 {-# LANGUAGE Arrows #-}

module Main where
import Prelim
import Naive
import qualified Arrow as A
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import FreeExamples
import NaiveExamples

main :: IO ()
-- main = mapM_ (print) (traceProcessor (initProcessor program))
main = do
        runNaiveExamples
        runFreeExamples
\end{code}
