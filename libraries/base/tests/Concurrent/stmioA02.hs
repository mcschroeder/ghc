module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import GHC.Conc.Sync

main = do
    trace ">>>t1 thread begin"

    lock <- newEmptyMVar
    x <- newTVarIO 0

    forkIO $ do trace ">>>t2 thread begin"
                atomicallyWithIO (do unsafeIOToSTM (trace ">>>t2 start tx")
                                     writeTVar x 1)
                                 (\_ -> do trace ">>>t2 IO wait "
                                           takeMVar lock
                                           trace ">>>t2 IO got signal"
                                           return ())
                trace ">>>t2 thread end"

    trace ">>>t1 thread delay"
    threadDelay 100000

    forkIO $ do trace ">>>t3 thread begin"
                atomically (do unsafeIOToSTM (trace ">>>t3 start tx")
                               writeTVar x 2)
                trace ">>>t3 thread end"

    trace ">>>t1 thread delay"
    threadDelay 100000

    trace ">>>t1 signal"
    putMVar lock 1

    atomically $ do unsafeIOToSTM (trace ">>>t1 start tx")
                    v <- readTVar x
                    if v == 2
                        then return ()
                        else retry

    trace ">>>t1 thread end"

--trace = putStrLn
trace _ = return ()
