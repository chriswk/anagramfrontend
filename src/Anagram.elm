module Anagram exposing (main)

import Browser
import Debug
import Html exposing (Attribute, Html, button, div, input, label, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Http


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
    | NewAnagrams (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = ""
      , minCount = 3
      , suggestions = []
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeWord newWord ->
            ( { model | word = newWord }, Cmd.none )

        ChangeCount toCount ->
            let
                newCount =
                    case String.toInt toCount of
                        Nothing ->
                            model.minCount

                        Just new ->
                            new
            in
            ( { model | minCount = newCount }, Cmd.none )

        PerformSearch ->
            ( model, getAnagrams model.word )


getAnagrams : String -> Cmd Msg
getAnagrams word =
    Http.send NewAnagrams (Http.get (toAnagramUrl word) anagramDecoder)


toAnagramUrl : String -> String
toAnagramUrl word =
    Url.crossorigin "http://anagrambackend.chriswk.com"
        [ "anagram" ]
        [ Url.string "word" word
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
