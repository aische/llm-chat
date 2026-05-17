module LLM.Core.Abort
  ( AbortSignal,
    newAbortSignal,
    abort,
    isAborted,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A cooperative cancellation signal.
-- Create one with 'newAbortSignal', pass it into 'ChatEnv', and call
-- 'abort' from any thread to request cancellation at the next checkpoint.
newtype AbortSignal = AbortSignal (IORef Bool)

-- | Create a fresh signal (not yet aborted).
newAbortSignal :: IO AbortSignal
newAbortSignal = AbortSignal <$> newIORef False

-- | Fire the signal. Idempotent — calling it more than once is harmless.
abort :: AbortSignal -> IO ()
abort (AbortSignal ref) = writeIORef ref True

-- | Check whether the signal has been fired.
isAborted :: (MonadIO m) => AbortSignal -> m Bool
isAborted (AbortSignal ref) = liftIO $ readIORef ref
