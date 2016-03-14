{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.Conc
import Control.Exception
import GHC.Conc.Sync (atomicallyWithIO)


-- check rollback of invariants
main = do
  putStrLn "Starting"
  x <- atomically ( newTVar 42 )
  v <- readTVarIO x
  putStrLn $ "x = " ++ show v

  putStrLn "Adding false invariant & throw exception in IO"
  catch ( atomicallyWithIO ( alwaysSucceeds ( do v <- readTVar x
                                                 if (v == 42) then throw (ErrorCall "Exn in invariant")
                                                              else return ()) )
                           ( throw $ ErrorCall "Exn in attached IO") )
        (\(e::SomeException) -> putStrLn $ "Caught: " ++ show e)

  putStrLn "Check that invariant wasn't added"
  atomically ( writeTVar x 100 )
  v <- readTVarIO x
  putStrLn $ "x = " ++ show v

  putStrLn "Adding true invariant & throw exception in IO"
  catch ( atomicallyWithIO ( alwaysSucceeds ( do v <- readTVar x
                                                 if (v /= 100) then throw (ErrorCall "Exn in invariant")
                                                               else return ()) )
                           ( throw $ ErrorCall "Exn in attached IO") )
        (\(e::SomeException) -> putStrLn $ "Caught: " ++ show e)

  putStrLn "Check that invariant wasn't added"
  catch ( atomically ( writeTVar x 42 ) )
        (\(e::SomeException) -> putStrLn $ "Caught: " ++ show e)
  v <- readTVarIO x
  putStrLn $ "x = " ++ show v
