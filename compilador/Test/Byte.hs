module Byte where

import Spec

main :: IO ()
main = 
  putStrLn "BVM Haskell Test Suite" >>
  runTestWith "Test/byte_run.sh "