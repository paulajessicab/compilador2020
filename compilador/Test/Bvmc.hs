module Bvmc where

import Spec

main :: IO ()
main = 
  putStrLn "BVM C Test Suite" >>
  runTestWith "Test/byte_run_c.sh "