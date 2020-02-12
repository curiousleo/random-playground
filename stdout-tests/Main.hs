{-# LANGUAGE BangPatterns #-}

module Main where

-- import Lib

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64, Word32, Word8, Word16)
import System.Environment (getArgs)
-- import Data.Massiv.Array

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Prim as BSP
import qualified Data.ByteString.Builder.Extra as BSE
import qualified System.IO as IO
import qualified System.Random as Rand
import qualified System.Random.MWC as MWC
import qualified System.Random.SplitMix as SplitMix

random64 :: Rand.RandomGen g => g -> (Word64, g)
random64 = Rand.random

stdGenSplit :: Rand.StdGen -> (BS.Builder, Rand.StdGen)
stdGenSplit gPrev =
  let (gNext, g) = Rand.split gPrev
      (gL, gR) = Rand.split g
      (gLL, gLR) = Rand.split gL
      (gRL, gRR) = Rand.split gR
      rLL = fst $ random64 gLL
      rLR = fst $ random64 gLR
      rRL = fst $ random64 gRL
      rRR = fst $ random64 gRR
   in (BSP.primFixed (BSP.word64Host BSP.>*< BSP.word64Host BSP.>*< BSP.word64Host BSP.>*< BSP.word64Host) (rLL, (rLR, (rRL, rRR))), gNext)

smGenSplit :: SplitMix.SMGen -> (BS.Builder, SplitMix.SMGen)
smGenSplit gPrev =
  let (gNext, g) = Rand.split gPrev
      (gL, gR) = Rand.split g
      (gLL, gLR) = Rand.split gL
      (gRL, gRR) = Rand.split gR
      rLL = fst $ SplitMix.nextWord64 gLL
      rLR = fst $ SplitMix.nextWord64 gLR
      rRL = fst $ SplitMix.nextWord64 gRL
      rRR = fst $ SplitMix.nextWord64 gRR
   in (BSP.primFixed (BSP.word64Host BSP.>*< BSP.word64Host BSP.>*< BSP.word64Host BSP.>*< BSP.word64Host) (rLL, (rLR, (rRL, rRR))), gNext)
main :: IO ()

main = do
  let !stdout = IO.stdout
      -- !sm64Gen = SplitMix.newSMGen
      -- !sz = Sz1 1048576

  IO.hSetBinaryMode stdout True
  IO.hSetBuffering stdout (IO.BlockBuffering Nothing)

  args <- getArgs
  case args of
    ["random"] -> do
        ref <- newIORef $ Rand.mkStdGen 1337
        forever $ do
            !gen <- readIORef ref
            let !(v, gen') = random64 gen
            -- let (gen', arr) = randomArraySPureSeq gen random64 sz :: (Rand.StdGen, Array P Ix1 Word64)
            writeIORef ref gen'
            -- let bs = fromByteArray Seq $ toByteArray arr :: Array P Ix1 Word64
            -- let bs' = toBuilder BS.word64LE bs
            -- BS.hPutBuilder stdout bs'
            BS.hPutBuilder stdout (BSE.word64Host v)
    ["random-split"] -> do
        ref <- newIORef $ Rand.mkStdGen 1337
        forever $ do
            !gen <- readIORef ref
            let !(v, gen') = stdGenSplit gen
            -- let (gen', arr) = randomArraySPureSeq gen random64 sz :: (Rand.StdGen, Array P Ix1 Word64)
            writeIORef ref gen'
            -- let bs = fromByteArray Seq $ toByteArray arr :: Array P Ix1 Word64
            -- let bs' = toBuilder BS.word64LE bs
            -- BS.hPutBuilder stdout bs'
            BS.hPutBuilder stdout v
    ["splitmix"] -> do
        !ref <- newIORef $ SplitMix.mkSMGen 1337
        forever $ do
          !gen <- readIORef ref
          let !(v, gen') = SplitMix.nextWord64 gen
          writeIORef ref gen'
          BS.hPutBuilder stdout (BSE.word64Host v)
    ["splitmix-split"] -> do
        !ref <- newIORef $ SplitMix.mkSMGen 1337
        forever $ do
          !gen <- readIORef ref
          let !(v, gen') = smGenSplit gen
          writeIORef ref gen'
          BS.hPutBuilder stdout v
    ["mwc"] -> do
        !gen <- MWC.create
        forever $ do
          !v <- MWC.uniform gen
          BS.hPutBuilder stdout (BSE.word64Host v)

