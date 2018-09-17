module Anagram exposing (main)

import Browser
import Debug
import Html exposing (Attribute, Html, button, div, input, label, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Url.Builder as U


type alias Model =
    { word : String
    , minCount : Int
    , anagrams : List String
    }


type Msg
    = NoOp
    | ChangeWord String
    | PerformSearch
    | ChangeCount String
    | NewAnagrams (Result Http.Error (List String))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = ""
      , minCount = 3
      , anagrams = []
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
        [ div []
            [ div [] [ input [ A.placeholder "Find anagrams for?", A.value model.word, onInput ChangeWord ] [] ]
            , div [] [ input [ A.type_ "number", A.value wordCountStr, A.min "2", A.max "6", onInput ChangeCount ] [] ]
            , div [] [ button [ onClick PerformSearch ] [ text "Find anagrams" ] ]
            ]
        , div [] (anagramList model.anagrams)
        ]


anagramNode : String -> Html Msg
anagramNode anagram =
    div [] [ text anagram ]


anagramList : List String -> List (Html Msg)
anagramList anagrams =
    if List.isEmpty anagrams then
        [ div [] [ text "No anagrams found yet" ] ]

    else
        let
            sortByLengthAsc =
                List.sortBy String.length anagrams
        in
        sortByLengthAsc
            |> List.reverse
            |> List.map anagramNode


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

        NewAnagrams anagramsList ->
            let
                anagrams =
                    case anagramsList of
                        Ok words ->
                            words

                        Err _ ->
                            []
            in
            ( { model | anagrams = anagrams }, Cmd.none )


getAnagrams : String -> Cmd Msg
getAnagrams word =
    Http.send NewAnagrams (Http.get (toAnagramUrl word) anagramDecoder)


anagramDecoder : Decode.Decoder (List String)
anagramDecoder =
    Decode.field "anagrams" (Decode.list Decode.string)


toAnagramUrl : String -> String
toAnagramUrl word =
    U.crossOrigin "https://anagrambackend.herokuapp.com"
        [ "anagram" ]
        [ U.string "word" word
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
