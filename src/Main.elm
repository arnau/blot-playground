module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Blob exposing (Blob)
import Blot exposing (Blot, Step)
import Browser
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , button
        , code
        , div
        , h1
        , h2
        , h3
        , input
        , label
        , node
        , option
        , p
        , pre
        , select
        , span
        , text
        )
import Html.Attributes as At
import Html.Events exposing (onClick, onInput)
import Seal exposing (Seal)
import Value exposing (Cardinality(..), Value(..))


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { source : Blob
    , next : ( String, Cardinality )
    , steps : List Step
    }


init : Model
init =
    { source = Blob.empty
    , next = ( "", One )
    , steps = []
    }



-- UPDATE


type Msg
    = ChangeVal String Int String
    | ChangeAttr String String
    | ChangeNextAttr String
    | ChangeNextValue String
    | Remove String
    | AddNext
    | AddValue String
    | RemoveValue String
    | SealToggle Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddValue name ->
            { model | source = Blob.addValue name model.source }

        RemoveValue name ->
            { model | source = Blob.removeValue name model.source }

        ChangeNextAttr attr ->
            { model | next = ( attr, Tuple.second model.next ) }

        ChangeNextValue input ->
            { model | next = ( Tuple.first model.next, Blob.cardinalityFromString input ) }

        ChangeVal name idx token ->
            { model | source = Blob.changeValue name idx token model.source }

        ChangeAttr name newName ->
            let
                blob =
                    Blob.rename name newName model.source
                        |> Result.withDefault model.source
            in
            { model | source = blob }

        Remove name ->
            { model | source = Blob.remove name model.source }

        AddNext ->
            case model.next of
                ( "", _ ) ->
                    model

                _ ->
                    { model
                        | source = addPair model.next model.source
                        , next = ( "", One )
                    }

        SealToggle idx name ->
            { model | source = toggleSeal idx name model.source }


toggleSeal : Int -> String -> Blob -> Blob
toggleSeal idx name blob =
    case Blob.get name blob of
        Nothing ->
            blob

        Just value ->
            Blob.update name (Value.redact idx value) blob
                |> Result.withDefault blob


addPair : ( String, Cardinality ) -> Blob -> Blob
addPair ( name, card ) blob =
    Blob.insert name (Value.empty card) blob
        |> Result.withDefault blob



-- VIEW


style : List (Html.Attribute msg) -> List (Html msg) -> Html msg
style attributes children =
    node "style" attributes children


viewSeal : String -> ( Int, Seal ) -> Html Msg
viewSeal name ( idx, seal ) =
    div []
        [ input
            [ At.value (Seal.toStringWithPrefix seal)
            , onInput (ChangeVal name idx)
            , At.disabled (Seal.isRedacted seal)
            ]
            []
        , button
            [ onClick (SealToggle idx name)
            , At.disabled (Seal.isEmpty seal)
            ]
            [ text
                (if Seal.isRedacted seal then
                    "Open"

                 else
                    "Redact"
                )
            ]
        ]


viewValue : String -> Value.Value -> List (Html Msg)
viewValue name value =
    case value of
        String seal ->
            [ viewSeal name ( 0, seal )
            , button [ onClick (Remove name) ] [ text "Remove" ]
            ]

        Set xs ->
            (xs
                |> Array.toIndexedList
                |> List.map (viewSeal name)
            )
                ++ [ button [ onClick (AddValue name) ] [ text "+" ]
                   , button [ onClick (RemoveValue name) ] [ text "-" ]
                   , button [ onClick (Remove name) ] [ text "Remove" ]
                   ]


viewPair : ( String, Value.Value ) -> Html Msg
viewPair ( name, value ) =
    div []
        ([ span
            [ At.style "background-color" "antiquewhite"
            , At.style "display" "inline-block"
            , At.style "padding" "4px"
            ]
            [ input [ At.id (name ++ "-attr"), At.value name, onInput (ChangeAttr name) ] []
            , text ": "
            ]
         ]
            ++ viewValue name value
        )


viewBlob : Blob -> List (Html Msg)
viewBlob blob =
    blob
        |> Blob.toList
        |> List.reverse
        |> List.map viewPair


viewToolbar : ( String, Cardinality ) -> Html Msg
viewToolbar next =
    let
        card =
            Blob.cardinalityToString (Tuple.second next)

        isSelected test =
            card == test
    in
    div [ At.style "padding" "8px" ]
        [ input [ At.value (Tuple.first next), onInput ChangeNextAttr ] []
        , select [ onInput ChangeNextValue ]
            [ option [ At.selected (isSelected "1"), At.value "1" ] [ text "one" ]
            , option [ At.selected (isSelected "n"), At.value "n" ] [ text "many" ]
            ]
        , button [ onClick AddNext ] [ text "Add" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ At.style "display" "grid"
        , At.style "grid-template-columns" "1fr 1fr"
        , At.style "grid-template-rows" "auto"
        ]
        [ style [] [ text "body { margin: 0}" ]
        , div
            [ At.style "min-height" "100vh"
            , At.style "padding" "16px"
            , At.style "box-sizing" "border-box"
            ]
            (List.concat
                [ [ h1 [] [ text "Blot playground (objecthash)" ] ]
                , viewBlob model.source
                , [ viewToolbar model.next ]
                ]
            )
        , div
            [ At.style "background-color" "ivory"
            , At.style "padding" "16px"
            , At.style "box-sizing" "border-box"
            ]
            [ viewSteps model ]
        ]


viewSteps : Model -> Html Msg
viewSteps model =
    div []
        [ h1 [] [ text "Steps" ]
        , div [] (List.map viewStep (Blot.steps model.source))
        ]


viewStep : Step -> Html Msg
viewStep step =
    case step.blot of
        Blot.Leaf inner ->
            div []
                [ h2 [] [ text "Result" ]
                , viewBlot (Blot.toHex inner)
                ]

        Blot.DictLeaves inner ->
            div []
                [ h2 [] [ text "Combined pairs" ]
                , div []
                    (case inner of
                        [] ->
                            [ text "None" ]

                        _ ->
                            List.map viewDictLeave inner
                    )
                ]

        Blot.DictBlot inner ->
            div []
                [ h2 [] [ text "Pairs" ]
                , div []
                    (case Dict.toList inner of
                        [] ->
                            [ text "None" ]

                        pairs ->
                            List.map viewDictPair pairs
                    )
                ]

        Blot.SetBlot inner ->
            p [ At.style "border" "1px solid black", At.style "margin" "5px" ]
                [ text <| Debug.toString <| inner ]


viewDictLeave : Blot.Bytes -> Html Msg
viewDictLeave bytes =
    viewBlot (Blot.toHex bytes)


viewDictPair : ( String, Blot ) -> Html Msg
viewDictPair ( name, blot ) =
    case blot of
        Blot.Leaf bytes ->
            viewBlot (name ++ ": " ++ Blot.toHex bytes)

        Blot.SetBlot inner ->
            viewBlot
                (name
                    ++ ": [ "
                    ++ String.join ", " (List.map Blot.toHex inner)
                    ++ " ] "
                )

        _ ->
            text ""


viewBlot : String -> Html Msg
viewBlot hash =
    pre
        [ At.style "width" "50vw"
        , At.style "overflow-x" "auto"
        , At.style "background-color" "antiquewhite"
        , At.style "padding" "16px"
        , At.style "box-sizing" "border-box"
        ]
        [ code [] [ text hash ] ]
