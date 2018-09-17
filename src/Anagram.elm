module Anagram exposing (main)

import Browser
import Debug
import Html exposing (Attribute, Html, button, div, input, label, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)


type alias Model =
    { word : String
    , minCount : Int
    , suggestions : List String
    }


type Msg
    = NoOp
    | ChangeWord String
    | PerformSearch
    | ChangeCount String


init : Model
init =
    { word = ""
    , minCount = 3
    , suggestions = []
    }


view : Model -> Html Msg
view model =
    let
        m =
            Debug.log "Model" model

        wordCountStr =
            model.minCount
                |> String.fromInt

        maxLength =
            model.word
                |> String.length
                |> String.fromInt
    in
    div []
        [ div [] [ input [ A.placeholder "Word to find anagrams for", A.value model.word, onInput ChangeWord ] [] ]
        , div [] [ input [ A.type_ "number", A.value wordCountStr, A.min "2", A.max "6", onInput ChangeCount ] [] ]
        , div [] [ button [ onClick PerformSearch ] [ text "Find anagrams" ] ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangeWord newWord ->
            { model | word = newWord }

        ChangeCount toCount ->
            let
                newCount =
                    case String.toInt toCount of
                        Nothing ->
                            model.minCount

                        Just new ->
                            new
            in
            { model | minCount = newCount }

        PerformSearch ->
            model


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
