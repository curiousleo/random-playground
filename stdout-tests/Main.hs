{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import System.Environment (getArgs)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BSE
import qualified Data.ByteString.Builder.Prim as BSP
import qualified System.IO as IO
import qualified System.Random as Rand
import qualified System.Random.SplitMix as SplitMix

random64 :: Rand.RandomGen g => g -> (Word64, g)
random64 = Rand.random

randomSplit :: Rand.RandomGen g => g -> (g -> (Word64, g)) -> (BS.Builder, g)
randomSplit gPrev f =
  let (gNext, g) = Rand.split gPrev
      (gL, gR) = Rand.split g
      (gLL, gLR) = Rand.split gL
      (gRL, gRR) = Rand.split gR
      rLL = fst $ f gLL
      rLR = fst $ f gLR
      rRL = fst $ f gRL
      rRR = fst $ f gRR
   in ( BSP.primFixed
          (BSP.word64Host BSP.>*< BSP.word64Host BSP.>*< BSP.word64Host BSP.>*<
           BSP.word64Host)
          (rLL, (rLR, (rRL, rRR)))
      , gNext)

write64 :: Rand.RandomGen g => g -> (g -> (Word64, g)) -> IO.Handle -> IO ()
write64 initialGen f out = do
  ref <- newIORef initialGen
  forever $ do
    gen <- readIORef ref
    let (v, gen') = f gen
    writeIORef ref gen'
    BS.hPutBuilder out (BSE.word64Host v)

writeSplit64 ::
     Rand.RandomGen g => g -> (g -> (Word64, g)) -> IO.Handle -> IO ()
writeSplit64 initialGen f out = do
  ref <- newIORef initialGen
  forever $ do
    gen <- readIORef ref
    let (v, gen') = randomSplit gen f
    writeIORef ref gen'
    BS.hPutBuilder out v

main :: IO ()
main = do
  let stdout = IO.stdout
  IO.hSetBinaryMode stdout True
  IO.hSetBuffering stdout (IO.BlockBuffering Nothing)
  args <- getArgs
  case args of
    ["random"] ->
      write64 (Rand.mkStdGen 1337) random64 stdout
    ["random-split"] ->
      writeSplit64 (Rand.mkStdGen 1337) random64 stdout
    ["splitmix"] ->
      write64 (SplitMix.mkSMGen 1337) SplitMix.nextWord64 stdout
    ["splitmix-split"] ->
      writeSplit64 (SplitMix.mkSMGen 1337) SplitMix.nextWord64 stdout
