module Main where

import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import System.Environment (getArgs)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified System.IO as IO
import qualified System.Random as Rand
import qualified System.Random.MWC as MWC
import qualified System.Random.SplitMix as SplitMix

main :: IO ()
main = do
  let stdout = IO.stdout
  IO.hSetBinaryMode stdout True
  IO.hSetBuffering stdout (IO.BlockBuffering Nothing)
  args <- getArgs
  case args of
    ["random"] -> forever $ do
        v <- Rand.randomIO
        BS.hPut stdout (BS.singleton v)
    ["splitmix"] -> do
        ref <- newIORef $ SplitMix.mkSMGen 1337
        forever $ do
          gen <- readIORef ref
          let (v, gen') = SplitMix.nextWord64 gen
          writeIORef ref gen'
          BS.hPutBuilder stdout (BS.word64BE v)
    ["mwc"] -> do
        gen <- MWC.create
        forever $ do
          v <- MWC.uniform gen
          BS.hPut stdout (BS.singleton v)

