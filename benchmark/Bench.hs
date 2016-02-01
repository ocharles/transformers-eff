{-# LANGUAGE FlexibleContexts #-}

import qualified BenchMe
import qualified BenchTransformers
import qualified Criterion
import qualified Criterion.Main as Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [Criterion.bgroup "right1" $
     let numbers = take 10000 (cycle [2,3,5,7])
     in map ($ numbers)
            [Criterion.bench "effect-interpreters" .
             Criterion.whnf BenchMe.right1
            ,Criterion.bench "transformers" .
             Criterion.whnf BenchTransformers.right1]
    ,Criterion.bgroup "right2" $
     let numbers = take 10000 (cycle [2,3,5,7]) ++ [11]
     in map ($numbers)
            [Criterion.bench "effect-interpreters" .
             Criterion.whnf BenchMe.right2
            ,Criterion.bench "transformers" .
             Criterion.whnf BenchTransformers.right2]]
