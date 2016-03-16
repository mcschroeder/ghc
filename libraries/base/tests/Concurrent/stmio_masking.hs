module Main where

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
--import Control.Concurrent.STM
import Control.Exception
import GHC.Conc.Sync
import System.Timeout

delay = 1 * 10^5

printMaskingState = putStrLn =<< show <$> getMaskingState

-- normal execution
normal = do
    printMaskingState
    atomicallyWithIO
        (return ())
        (const $ printMaskingState)
    printMaskingState

-- asynchronous exception to interruptible finalizer
asyncex_interruptible = do
    tid <- forkIO $ do
        printMaskingState
        res <- try $ atomicallyWithIO
                        (return ())
                        (\_ -> do
                            printMaskingState
                            m <- newEmptyMVar
                            takeMVar m
                        )
        print (res :: Either AsyncException ())
        printMaskingState

    threadDelay delay
    timeout delay $ killThread tid
    threadDelay delay

-- asynchronous exception to uninterruptible finalizer
--asyncex_uninterruptible = do
--    tid <- forkIO $ do
--        printMaskingState
--        res <- try $ atomicallyWithIO
--                        (return ())
--                        (\_ -> do
--                            printMaskingState
--                            m <- newEmptyMVar
--                            let loop = tryTakeMVar m >> loop
--                            loop
--                        )
--        print (res :: Either AsyncException ())
--        printMaskingState

--    threadDelay delay
--    timeout delay $ killThread tid
--    threadDelay delay

main = do
    normal
    mask_ normal
    uninterruptibleMask_ normal
    asyncex_interruptible
    mask_ asyncex_interruptible
    --asyncex_uninterruptible
