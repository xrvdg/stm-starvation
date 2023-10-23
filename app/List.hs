{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent (myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import System.IO (hFlush, stdout)


readTvars :: [TVar a] -> IO ()
readTvars ltvar = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  _ <- atomically (p threadId counter *> mapM readTVar ltvar)
  return ()
  where
    p threadId counter = unsafePerformIO $ do
      r <- readIORef counter
      print (threadId, r)
      hFlush stdout
      modifyIORef' counter (+ 1)
      return (return ())

main :: IO ()
main = do
  let l = [(x, x) | x <- [1 :: Integer .. 400_000]]
  ltvar <- mapM newTVarIO l
  let g = readTvars ltvar
  let h = readTvars ltvar
  let i = readTvars ltvar
  g
  print "done with g"
  hFlush stdout
  Async.race_ h i