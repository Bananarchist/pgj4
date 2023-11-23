module WasmWorker where

import Asterius.Types
import qualified Logic
import qualified WSResponse

theInt :: Int
theInt = 5


updateGameState : JSString -> JSString -> IO JSObject
updateGameState _ _ =
  pure ""


instance ToJSON

-- the key here is just to translate Logic.hs calls into asterious JS primitives
-- A JS worker will coordinate calls between the elm view and the desired source
