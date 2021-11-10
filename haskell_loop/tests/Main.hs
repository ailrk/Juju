module Main where

import Control.Monad.Loop
import Control.Monad.Trans.Class

main :: IO ()
main = do
  putStrLn "sad"

loopAcrossWith_IO :: IO ()
loopAcrossWith_IO = do
  loop `across` [0..] `with_` \i -> do
    if i == 10 then quit else do
      lift $ putStrLn $ "===> " ++ show i
      lift $ putStrLn "hi"

loopAcrossWithi_IO :: IO ()
loopAcrossWithi_IO = do
  loop `across` [0..] `withi_` \(idx, val) -> do
    if idx == 10 then quit else do
      lift $ putStrLn $ "===> idx: " ++ show idx ++ ", value: " ++ show val
      lift $ putStrLn "hi"

loopAcrossWithWhilei_IO :: IO ()
loopAcrossWithWhilei_IO = do
  loop `across` [0..] `while` (<5) `withWhile_` \val -> do
    lift $ putStrLn "hi"
