module FreeExamples(runFreeExamples) where
import Prelim
import qualified Arrow as A
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import Free

runFreeExamples :: IO ()
runFreeExamples = print "free examples"
