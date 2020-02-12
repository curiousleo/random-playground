{-# LANGUAGE BangPatterns #-}

module Main where

-- import Lib

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64, Word32, Word8, Word16)
import System.Environment (getArgs)
import Data.Massiv.Array

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified System.IO as IO
import qualified System.Random as Rand
import qualified System.Random.MWC as MWC
import qualified System.Random.SplitMix as SplitMix

random64 :: Rand.RandomGen g => g -> (Word64, g)
random64 = Rand.random

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
            BS.hPutBuilder stdout (BS.word64BE v)
    ["splitmix"] -> do
        !ref <- newIORef $ SplitMix.mkSMGen 1337
        forever $ do
          !gen <- readIORef ref
          let !(v, gen') = SplitMix.nextWord64 gen
          writeIORef ref gen'
          BS.hPutBuilder stdout (BS.word64BE v)
    ["mwc"] -> do
        !gen <- MWC.create
        forever $ do
          !v <- MWC.uniform gen
          BS.hPutBuilder stdout (BS.word64BE v)

