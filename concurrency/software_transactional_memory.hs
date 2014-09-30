import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM hiding (check) 

-- http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html
-- http://www.haskell.org/haskellwiki/Simple_STM_example
-- https://www.fpcomplete.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory

-- $ runhaskell software_transactional_memory


{- TVar -}

type Lock = TVar Bool

newLock :: Bool -> STM Lock
newLock available = newTVar available

release :: Lock -> STM ()
release lock = writeTVar lock True

acquire :: Lock -> STM ()
acquire lock = do available <- readTVar lock
                  check available
                  writeTVar lock False

-- retry :: STM a
-- "may block the thread, until one of the TVars that it has read from has been udpated"

check :: Bool -> STM ()
check True  = return ()
check False = retry

-- orElse :: STM a -> STM a -> STM a
-- @t `orElse` r@ tries t; if t retries, it tries r; if r retries, it retries.
-- ~ logical OR


{- atomically -}

-- newTVar :: a -> SMT (TVar a) 
-- readTVar :: TVar a -> SMT a
-- writeTVar :: TVar a -> a -> SMT a
milliSleep = threadDelay . (*) 1000
atomRead = atomically . readTVar
atomPrint x = atomRead x >>= print
atomApply f x =  atomically $  readTVar x >>= (f >>> return) >>= writeTVar x

-- atomically :: STM a -> IO a
-- only atomically can lift a STM into an IO
--
-- atomicity :- other threads see state only before/after transaction 
-- isolation :- other threads have no effect on transaction
--
-- forkIO :: IO () -> IO ThreadId
-- forkIO spaawns Haskell threads
-- Haskell threads only take 100 bytes of memory
-- Haskell threads are managed only by the GHC runtime
-- only one Haskell thread can run at once

runThreads = do
 shared <- atomically $ newTVar 0
 before <- atomRead shared
 putStrLn $ "Before: " ++ show before

 threadA <- myThreadId
 threadB <- forkIO $ replicateM_ 25 $ atomPrint         shared >> milliSleep 20
 threadC <- forkIO $ replicateM_ 10 $ atomApply ((+) 2) shared >> milliSleep 50
 threadD <- forkIO $ replicateM_ 20 $ atomApply pred    shared >> milliSleep 25

 milliSleep 800
 after <- atomRead shared
 putStrLn $ "After: " ++ show after

 putStrLn ""
 mapM_ print [threadA, threadB, threadC, threadD]

 putStrLn ""
 setNumCapabilities 2
 getNumCapabilities >>= print

 putStrLn ""
 mapM_ (threadCapability >=> (fst >>> return) >=> print) [threadA, threadB, threadC, threadD]

-- threadCapability returns the capability (i.e. physical processor) on which the thread is running


main = runThreads
