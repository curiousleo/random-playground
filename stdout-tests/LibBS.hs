{-# LANGUAGE BangPatterns #-}

module LibBS where

import Data.ByteString
import Data.ByteString.Builder.Prim
import Data.ByteString.Builder
import Data.ByteString.Builder.Internal
import Data.ByteString.Builder.Prim.Internal
import Foreign.Ptr (plusPtr)

-- | Create a 'Builder' that encodes a sequence generated from a seed value
-- using a 'BoundedPrim' for each sequence element.
{-# INLINE primUnfoldrBounded' #-}
primUnfoldrBounded' :: BoundedPrim b -> (a -> Maybe (b, a)) -> a -> Builder
primUnfoldrBounded' w f x0 =
    builder $ fillWith x0
  where
    fillWith x k !(BufferRange op0 ope0) =
        go (f x) op0
      where
        go !Nothing        !op         = do let !br' = BufferRange op ope0
                                            k br'
        go !(Just (y, x')) !op
          | op `plusPtr` bound <= ope0 = runB w y op >>= go (f x')
          | otherwise                  = return $ bufferFull bound op $
              \(BufferRange opNew opeNew) -> do
                  !opNew' <- runB w y opNew
                  fillWith x' k (BufferRange opNew' opeNew)
    bound = sizeBound w
