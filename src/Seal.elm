module Seal exposing
    ( Seal(..)
    , empty
    , isEmpty
    , isRedacted
    , map
    , open
    , redact
    , toString
    , toStringWithPad
    , toStringWithPrefix
    , toggle
    , update
    )

{-| A Seal boxes a String that may be open or redacted.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Objecthash.Hash as Hash
import Objecthash.Value as OValue exposing (Value(..))


type alias Bytes =
    Hash.ByteList


type Seal
    = Open String
    | Redacted String Bytes


empty : Seal
empty =
    Open ""


isEmpty : Seal -> Bool
isEmpty seal =
    case seal of
        Open "" ->
            True

        Open _ ->
            False

        Redacted "" _ ->
            True

        Redacted _ _ ->
            False


isRedacted : Seal -> Bool
isRedacted seal =
    case seal of
        Open _ ->
            False

        Redacted _ _ ->
            True


toggle : Seal -> Seal
toggle seal =
    case seal of
        Open _ ->
            redact seal

        Redacted _ _ ->
            open seal


open : Seal -> Seal
open seal =
    case seal of
        Redacted str bytes ->
            Open str

        _ ->
            seal


redact : Seal -> Seal
redact seal =
    case seal of
        Open str ->
            Redacted str (Hash.unicode str)

        _ ->
            seal


map : (String -> String) -> Seal -> Seal
map fn seal =
    case seal of
        Open str ->
            Open (fn str)

        Redacted str bytes ->
            let
                str_ =
                    fn str
            in
            Redacted str_ (Hash.unicode str_)


update : String -> Seal -> Seal
update value seal =
    map (\_ -> value) seal


{-| Returns either the open string or the hex representation of the hashed
bytes
-}
toString : Seal -> String
toString seal =
    case seal of
        Open str ->
            str

        Redacted str bytes ->
            Hash.toHex bytes


{-| Prepends a byte 1 to tag the string as open or redacted

TODO: Is this better than the **REDACTED** version?

-}
toStringWithPad : Seal -> String
toStringWithPad seal =
    case seal of
        Open str ->
            str

        Redacted str bytes ->
            Hash.toHex ([ 0x01 ] ++ bytes)


toStringWithPrefix : Seal -> String
toStringWithPrefix seal =
    case seal of
        Open str ->
            str

        Redacted str bytes ->
            "**REDACTED**" ++ Hash.toHex bytes
