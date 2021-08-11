{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Spec where

import Test.Framework
import {-@ HTF_TESTS @-} Tests

main :: IO ()
main = htfMain htf_importedTests
