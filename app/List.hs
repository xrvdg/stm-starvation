{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, TVar, atomically, readTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let l = [(x, x) | x <- [1 :: Integer .. 400_000]]
  list <- mapM newTVarIO l
  print "Initialized the List"
  -- Read the list once to be sure it's fully evaluated
  readTList list
  Async.concurrently_ (readTList list) (readTList list)

-- | irrevocable counter to keep track of transaction restart.
restartCounter :: ThreadId -> IORef Integer -> STM ()
restartCounter  threadId counter = unsafePerformIO $ do
  r <- readIORef counter
  print (threadId, r)
  hFlush stdout
  modifyIORef' counter (+ 1)
  return (return ())

readTList :: [TVar a] -> IO ()
readTList ltvar = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  atomically $ do
    restartCounter threadId counter
    mapM_ readTVar ltvar
