import Data.List
import Servant.Elm

import Sanskell.Api as Api

main :: IO ()
main = do
  putStrLn "-- In generateElm.hs"
  let code = intercalate "\n\n" $
        "module Api exposing (..)" :
        defElmImports :
        generateElmForAPI Api.jobApi
  writeFile "ui/src/Api.elm" code
