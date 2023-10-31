{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, atomically)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import qualified ListT
import qualified StmContainers.Map as STMMap
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let l = [(x,x) | x <- [1 :: Integer .. 100_000]]
  stmmap <- STMMap.newIO
  atomically $ mapM_ (\(x, y) -> STMMap.insert x y stmmap) l
  print "Initialized the STM Map."
  hFlush stdout
  -- Check that a read from a single thread does work
  readMap stmmap
  -- read the stm map from two threads concurrently
  foldr1 Async.concurrently_ [readMap stmmap, readMap stmmap]

-- | irrevocable counter to keep track of transaction restart.
restartCounter :: ThreadId -> IORef Integer -> STM ()
-- restartCounter threadId counter = pure ()
restartCounter  threadId counter = unsafePerformIO $ do
  r <- readIORef counter
  print (threadId, r)
  hFlush stdout
  modifyIORef' counter (+ 1)
  return (return ())

readMap :: (Show a, Show b) => STMMap.Map a b -> IO ()
readMap stmmap = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  _ <- atomically $ do
    restartCounter threadId counter
    ListT.toList (STMMap.listT stmmap)
  return ()
