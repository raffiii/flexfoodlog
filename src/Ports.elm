port module Ports exposing (..)

import Json.Encode as E

-- Send a single envelope to JS for persistence
port saveEvent : E.Value -> Cmd msg

-- Optionally receive events back (read/query results)
port onEvents : (E.Value -> msg) -> Sub msg    

-- Query past events
port queryStreamEvents : String -> Cmd msg

-- Query all events (use with caution)
port queryAllEvents : () -> Cmd msg

-- Query events by type
port queryEventType : String -> Cmd msg