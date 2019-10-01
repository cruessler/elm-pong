module Msg exposing (Msg(..))

import Model exposing (Position)


type Msg
    = Tick Float
    | MouseMove Position
    | MouseClick
