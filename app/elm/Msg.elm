module Msg exposing (Msg(..))

import Model exposing (Backend, Position)


type Msg
    = Tick Float
    | MouseMove Position
    | MouseClick
    | SetBackend Backend
