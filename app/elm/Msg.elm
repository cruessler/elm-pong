module Msg exposing (Msg(..))

import Mouse exposing (Position)
import Time exposing (Time)


type Msg
    = Restart Time
    | Tick Time
    | MouseMove Position
    | MouseClick
