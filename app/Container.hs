{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Foreign (Storable)
import GHC.IO (unsafePerformIO)
import qualified ListT
import qualified StmContainers.Map as STMMap
import System.IO (hFlush, stdout)

data List a = (Storable a) => Cons {-# UNPACK #-} !a {-# UNPACK #-} !(List a)

h :: List Int
h = Cons 5 h

readTvars :: [TVar a] -> IO ()
readTvars ltvar = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  atomically (p threadId counter *> mapM readTVar ltvar)
  return ()
  where
    p threadId counter = unsafePerformIO $ do
      r <- readIORef counter
      print (threadId, r)
      hFlush stdout
      modifyIORef' counter (+ 1)
      return (return ())

-- main :: IO ()
-- main = do
--   -- let l = [1 .. 2_00_000]
--   let l = [(x, x) | x <- [1 .. 2_00_000]]
--   -- todo nested tvar
--   ltvar <- mapM newTVarIO l
--   let g = readTvars ltvar
--   let h = readTvars ltvar
--   let i = readTvars ltvar
--   g
--   print "done with g"
--   hFlush stdout
--   Async.race_ h i

readTTvars :: (Traversable f) => TVar (f (TVar a)) -> IO ()
readTTvars tltvar = do
  threadId <- myThreadId
  counter <- newIORef (0 :: Integer)
  _ <- atomically $ do
    p threadId counter
    ltvar <- readTVar tltvar
    mapM readTVar ltvar
  return ()
  where
    p threadId counter = unsafePerformIO $ do
      r <- readIORef counter
      print (threadId, r)
      hFlush stdout
      modifyIORef' counter (+ 1)
      return (return ())

-- main :: IO ()
-- main = do
--   -- let l = [1 .. 2_00_000]
--   let l = [(x, x) | x <- [1 .. 2_00_000]]
--   -- todo nested tvar
--   ltvar <- mapM newTVarIO l
--   tltvar <- newTVarIO ltvar
--   let g = readTTvars tltvar
--   let h = readTTvars tltvar
--   let i = readTTvars tltvar
--   g
--   print "done with g"
--   hFlush stdout
--   Async.race_ h i

main :: IO ()
main = do
  -- let l = [1 .. 2_00_000]
  let (l :: [(Integer, Integer)]) = [(x, x) | x <- [1 .. 100_000]]
  -- todo nested tvar
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
  list <- atomically $ do
    p threadId counter
    ListT.toList (STMMap.listT stmmap)
  -- print list
  return ()
  where
    p threadId counter = unsafePerformIO $ do
      r <- readIORef counter
      print (threadId, r)
      hFlush stdout
      modifyIORef' counter (+ 1)
      return (return ())
