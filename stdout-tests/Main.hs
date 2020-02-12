{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import System.Environment (getArgs)

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Prim as BS
import qualified System.IO as IO
import qualified System.Random as R
import qualified System.Random.SplitMix as SM

random64 :: R.RandomGen g => g -> (Word64, g)
random64 = R.random

defaultSequence ::
     R.RandomGen g => (g -> (Word64, g)) -> g -> (BS.Builder, g)
defaultSequence f gen =
  let (r1, gen1) = f gen
      (r2, gen2) = f gen1
      (r3, gen3) = f gen2
      (r4, gen4) = f gen3
   in ( BS.primFixed
          (BS.word64Host BS.>*< BS.word64Host BS.>*< BS.word64Host BS.>*< BS.word64Host)
          (r1, (r2, (r3, r4)))
      , gen4)

-- | Generate a sequence for stress-testing splittable RNGs.
--
-- Hans Georg Schaathun. 2015. Evaluation of splittable pseudo-random
-- generators. Journal of Functional Programming, Vol. 25.
-- https://doi.org/10.1017/S095679681500012X
splitSequence :: R.RandomGen g => (g -> (Word64, g)) -> g -> (BS.Builder, g)
splitSequence f gPrev =
  let (gNext, g) = R.split gPrev
      (gL, gR) = R.split g
      (gLL, gLR) = R.split gL
      (gRL, gRR) = R.split gR
      rLL = fst $ f gLL
      rLR = fst $ f gLR
      rRL = fst $ f gRL
      rRR = fst $ f gRR
   in ( BS.primFixed
          (BS.word64Host BS.>*< BS.word64Host BS.>*< BS.word64Host BS.>*< BS.word64Host)
          (rLL, (rLR, (rRL, rRR)))
      , gNext)

spew :: R.RandomGen g => IO.Handle -> g -> (g -> (BS.Builder, g)) -> IO ()
spew h initialGen f = do
  ref <- newIORef initialGen
  forever $ do
    gen <- readIORef ref
    let (v, gen') = f gen
    writeIORef ref gen'
    BS.hPutBuilder h v

main :: IO ()
main = do
  let stdout = IO.stdout
  IO.hSetBinaryMode stdout True
  IO.hSetBuffering stdout (IO.BlockBuffering Nothing)
  args <- getArgs
  case args of
    ["random"] ->
      spew stdout (R.mkStdGen 1337) (defaultSequence random64)
    ["random-split"] ->
      spew stdout (R.mkStdGen 1337) (splitSequence random64)
    ["splitmix"] ->
      spew stdout (SM.mkSMGen 1337) (defaultSequence SM.nextWord64)
    ["splitmix-split"] ->
      spew stdout (SM.mkSMGen 1337) (splitSequence SM.nextWord64)
