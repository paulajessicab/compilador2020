{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | The main test Program.
-- Original Source Code: https://github.com/skogsbaer/HTF/tree/master/sample

module Spec where

import Test.Framework
import Test.Framework.BlackBoxTest

runTestWith :: String -> IO ()
runTestWith script = do
  bbts <- blackBoxTests "Test/Cases" script ".pcf" bbTArg
  htfMain ([makeTestSuite "bbts" bbts])
  where
    bbTArg = defaultBBTArgs
      { bbtArgs_stdoutSuffix = ".pcf.out"
      , bbtArgs_stderrSuffix = ".pcf.err"
      , bbtArgs_verbose = True
      }