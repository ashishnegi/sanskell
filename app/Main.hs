module Main where

import qualified Sanskell.Api as A
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 8034 A.app
