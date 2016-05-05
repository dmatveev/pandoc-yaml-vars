module Main where

import Text.Pandoc
import Text.Pandoc.JSON
import Text.YamlVars.Transform (transform)

main :: IO ()
main = toJSONFilter transform
