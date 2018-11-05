module Dev where

import           Text.Pretty.Simple

import qualified Data.Map.Strict               as Map

import qualified Module

main :: IO ()
main = do
  initial <- Module.initialize filepath
  let result = pipeline initial
  print result
  putTextLn "done"

pipeline = Module.process >=> Module.run


filepath = "examples/medium.lang"

