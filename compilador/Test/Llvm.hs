module Llvm where

import Spec

main :: IO ()
main = 
  putStrLn "Llvm Test Suite" >>
  runTestWith "Test/llvm_run.sh "