module Blot exposing (Blot(..), Bytes, Step, hash, steps, toHex)

import Array exposing (Array)
import Blob exposing (Blob)
import Dict exposing (Dict)
import Objecthash.Hash as Hash
import Objecthash.Tag as Tag exposing (Tag)
import Objecthash.Value as OValue exposing (Value(..))
import Seal exposing (Seal)
import Value exposing (Value(..))


type alias Name =
    String


type alias Bytes =
    Hash.ByteList


type Blot
    = Leaf Bytes
    | DictLeaves (List Bytes)
    | DictBlot (Dict String Blot)
    | SetBlot (List Bytes)


type alias Step =
    { blot : Blot
    }


hash : Blob -> String
hash blob =
    let
        toValue ( name, value ) =
            case value of
                String seal ->
                    if Seal.isEmpty seal then
                        Nothing

                    else
                        Just ( name, VString (Seal.toStringWithPrefix seal) )

                Set xs ->
                    let
                        xs_ =
                            Array.filter (\seal -> not (Seal.isEmpty seal)) xs
                    in
                    if Array.length xs_ == 0 then
                        Nothing

                    else
                        Just
                            ( name
                            , xs_
                                |> Array.toList
                                |> List.map
                                    (VString << Seal.toStringWithPrefix)
                                |> VSet
                            )
    in
    blob
        |> Blob.toList
        |> List.filterMap toValue
        |> Dict.fromList
        |> VDict
        |> Hash.bytes
        |> Hash.toHex


toHex : Bytes -> String
toHex bytes =
    Hash.toHex bytes


steps : Blob -> List Step
steps blob =
    let
        toBlot ( name, value ) =
            case value of
                String seal ->
                    if Seal.isEmpty seal then
                        Nothing

                    else
                        Just ( name, Leaf <| Seal.hash seal )

                Set xs ->
                    let
                        xs_ =
                            Array.filter (\seal -> not (Seal.isEmpty seal)) xs
                    in
                    if Array.length xs_ == 0 then
                        Nothing

                    else
                        Just <|
                            ( name
                            , SetBlot
                                (xs_
                                    |> Array.toList
                                    |> List.map Seal.hash
                                )
                            )

        blot =
            blob
                |> Blob.toList
                |> List.filterMap toBlot
                |> Dict.fromList
                |> DictBlot
    in
    reduce blot


reduce : Blot -> List Step
reduce blot =
    reduce_ blot []


reduce_ : Blot -> List Step -> List Step
reduce_ blot stepList =
    case blot of
        Leaf bytes ->
            { blot = blot } :: stepList

        SetBlot xs ->
            { blot = blot } :: reduce_ (Leaf (Hash.set xs)) stepList

        DictBlot d ->
            { blot = blot } :: reduce_ (dictLeaves d) stepList

        DictLeaves xs ->
            { blot = blot } :: reduce_ (Leaf (Hash.bag Tag.Dict xs)) stepList


dictLeaves : Dict String Blot -> Blot
dictLeaves input =
    input
        |> Dict.toList
        |> List.map hashPair
        |> List.sort
        |> DictLeaves


hashPair : ( String, Blot ) -> Bytes
hashPair ( name, blot ) =
    case blot of
        Leaf bytes ->
            Hash.pair ( name, bytes )

        SetBlot inner ->
            Hash.pair ( name, Hash.set inner )

        -- TODO: This is untested territory.
        DictBlot inner ->
            Hash.pair ( name, [] )

        DictLeaves leaves ->
            Hash.pair ( name, Hash.untagged leaves )
