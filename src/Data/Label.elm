module Data.Label exposing (LabelInterfaceMsg(..))

{-| -}

import Api.InputObject exposing (LabelInput)


{-| I didn't know where to put it, so Data will do for now
-}
type LabelInterfaceMsg
    = NoOp
    | NextDatapoint
    | PreviousDatapoint
    | LabelDatapoint LabelInput
