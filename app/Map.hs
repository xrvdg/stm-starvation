{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import qualified ListT
import qualified StmContainers.Map as STMMap
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  let (l :: [(Integer, Integer)]) = [(x, x) | x <- [1 .. 100_000]]
  stmmap <- STMMap.newIO
  atomically $ mapM_ (\(x, y) -> STMMap.insert x y stmmap) l
  let g = readMap stmmap
  let h = readMap stmmap
  let i = readMap stmmap
  g
  print "done with g"
  hFlush stdout
  Async.race_ h i

readMap :: (Show a, Show b) => STMMap.Map a b -> IO ()
readMap stmmap = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  _ <- atomically $ do
    p threadId counter
    ListT.toList (STMMap.listT stmmap)
  return ()
  where
    p threadId counter = unsafePerformIO $ do
      r <- readIORef counter
      print (threadId, r)
      hFlush stdout
      modifyIORef' counter (+ 1)
      return (return ())
