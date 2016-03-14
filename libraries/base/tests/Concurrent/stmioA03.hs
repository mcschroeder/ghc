-- nested atomically

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
import GHC.Conc.Sync

cdo :: String -> IO () -> IO ()
cdo s x = do
    putStrLn $ "-- " ++ s
    catch x (\(e::SomeException) -> putStrLn $ "Caught: " ++ show e)

expect :: (Show a, Eq a) => String -> TVar a -> a -> IO ()
expect s x v = do
    v' <- readTVarIO x
    putStr $ s ++ " == " ++ show v'
    if v' == v
        then putStr $ "   OK\n"
        else putStr $ "   FAIL (expected " ++ show v ++ ")\n"

main = do
    x <- newTVarIO 0
    y <- newTVarIO 0

    let expect_x v = expect "x" x v
        expect_y v = expect "y" y v
        reset_vars = atomically $ writeTVar x 0 >> writeTVar y 0

    cdo "atomically (atomically), illegal" $
        atomically $ unsafeIOToSTM $ atomically $ writeTVar x 1
    expect_x 0

    cdo "atomically (atomicallyWithIO), illegal" $
        atomically $ unsafeIOToSTM $ atomicallyWithIO (writeTVar x 1) (return)
    expect_x 0

    cdo "atomicallyWithIO (atomically) (), illegal" $
        atomicallyWithIO (unsafeIOToSTM $ atomically $ writeTVar x 1)
                         (return)
    expect_x 0

    cdo "atomicallyWithIO () (atomically)" $
        atomicallyWithIO (return ())
                         (\_ -> atomically $ writeTVar x 1)
    expect_x 1

    reset_vars
    cdo "atomicallyWithIO () (atomically error)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomically $ errorWithoutStackTrace "rollback inner")
    expect_x 0

    reset_vars
    cdo "atomicallyWithIO () (catch atomically error)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> catch (atomically $ errorWithoutStackTrace "rollback inner")
                                      (\(e::SomeException) -> putStrLn $ "Caught: " ++ show e))
    expect_x 1

    reset_vars
    cdo "atomicallyWithIO () (atomically retry)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomically $ retry)
    expect_x 0

    reset_vars
    cdo "atomicallyWithIO () (atomically retry orElse)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomically $ retry `orElse` return ())
    expect_x 1

    reset_vars
    cdo "atomicallyWithIO () (atomically; atomically)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> do atomically $ writeTVar y 1
                                   atomically $ writeTVar y 2)
    expect_x 1
    expect_y 2

    reset_vars
    cdo "atomicallyWithIO () (atomically; atomically error)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> do atomically $ writeTVar y 1
                                   atomically $ errorWithoutStackTrace "rollback 2nd inner")
    expect_x 0
    expect_y 1

    reset_vars
    cdo "atomicallyWithIO () (atomicallyWithIO () ())" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomicallyWithIO (writeTVar y 1)
                                                 (return))
    expect_x 1
    expect_y 1

    reset_vars
    cdo "atomicallyWithIO () (atomicallyWithIO () (atomically error))" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomicallyWithIO (writeTVar y 1)
                                                 (\_ -> atomically $ errorWithoutStackTrace "rollback inner inner"))
    expect_x 0
    expect_y 0



    reset_vars
    cdo "atomicallyWithIO (read x) (atomically read x)" $
        atomicallyWithIO (readTVar x)
                         (\_ -> do atomically $ readTVar x
                                   return ())

    reset_vars
    cdo "atomicallyWithIO (write x) (atomically read x)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> do v <- atomically $ readTVar x
                                   putStrLn $ "inner read: x == " ++ show v) -- expected 0
    expect_x 1

    reset_vars
    cdo "atomicallyWithIO (write x) (readTVarIO x)" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> do v <- readTVarIO x
                                   putStrLn $ "inner read: x == " ++ show v) -- expected 0
    expect_x 1

    reset_vars
    cdo "atomicallyWithIO (read x) (atomically write x), illegal" $
        atomicallyWithIO (readTVar x)
                         (\_ -> atomically $ writeTVar x 1)
    expect_x 0


    reset_vars
    cdo "atomicallyWithIO (write x) (atomically write x), illegal" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomically $ writeTVar x 2)
    expect_x 0


    reset_vars
    cdo "atomicallyWithIO (read x) (atomicallyWithIO () (atomically write x)), illegal" $
        atomicallyWithIO (readTVar x)
                         (\_ -> atomicallyWithIO (return ())
                                                 (\_ -> atomically $ writeTVar x 2))
    expect_x 0

    reset_vars
    cdo "atomicallyWithIO (write x) (atomicallyWithIO () (atomically write x)), illegal" $
        atomicallyWithIO (writeTVar x 1)
                         (\_ -> atomicallyWithIO (return ())
                                                 (\_ -> atomically $ writeTVar x 2))
    expect_x 0
