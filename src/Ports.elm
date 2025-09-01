port module Ports exposing (..)

import Json.Encode as E

-- Send a single envelope to JS for persistence
port saveEvent : E.Value -> Cmd msg

-- Optionally receive events back (read/query results)
port onEvents : (E.Value -> msg) -> Sub msg    
  