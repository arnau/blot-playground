module Seal exposing
    ( Seal(..)
    , empty
    , hash
    , isEmpty
    , isLocked
    , isOpen
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


isOpen : Seal -> Bool
isOpen seal =
    case seal of
        Open str ->
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


{-| The original string is redacted
-}
isLocked : Seal -> Bool
isLocked seal =
    case seal of
        Open _ ->
            False

        Redacted str _ ->
            isTokenRedacted str


toggle : Seal -> Seal
toggle seal =
    if isLocked seal then
        seal

    else
        case seal of
            Open _ ->
                redact seal

            Redacted str _ ->
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


isTokenRedacted : String -> Bool
isTokenRedacted token =
    String.startsWith "**REDACTED**" token
        && (String.length token == 76)
        && String.all Char.isHexDigit (String.dropLeft 12 token)


update : String -> Seal -> Seal
update value seal =
    if isTokenRedacted value then
        Redacted value (Hash.redacted value)

    else
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


hash : Seal -> Bytes
hash seal =
    case seal of
        Open str ->
            Hash.unicode str

        Redacted _ bytes ->
            bytes
