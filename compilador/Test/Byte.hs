module Byte where

import Spec

main :: IO ()
main = 
  putStrLn "Byte Test Suite" >>
  runTestWith "Test/byte_run.sh "