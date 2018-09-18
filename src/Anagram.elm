module Anagram exposing (main)

import Browser
import Debug
import Dict as D
import Dict.Extra as E
import Html exposing (Attribute, Html, button, div, fieldset, form, h2, input, label, legend, section, table, tbody, td, text, th, thead, tr)
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
    div [ A.id "layout", A.class "pure-g" ]
        [ sidebar model
        , answer model
        ]


sidebar : Model -> Html Msg
sidebar model =
    let
        wordCountStr =
            String.fromInt model.minCount

        wordLength =
            model.word
                |> String.length
                |> String.fromInt
    in
    div [ A.class "sidebar pure-u-1 pure-u-md-1-4" ]
        [ div [ A.class "header" ]
            [ h2 [ A.class "brand-tagline" ] [ text "Find your anagrams" ]
            , fieldset []
                [ legend [] [ text "Find anagrams for?" ]
                , input [ A.placeholder "Baseword", A.value model.word, onInput ChangeWord ] []
                , input [ A.type_ "number", A.value wordCountStr, A.min "2", A.max wordLength, onInput ChangeCount ] []
                , button [ A.class "pure-button pure-button-primary", onClick PerformSearch ] [ text "Find anagrams" ]
                ]
            ]
        ]


answer : Model -> Html Msg
answer model =
    let
        anagrams =
            if List.isEmpty model.anagrams then
                []

            else
                anagramGroups model
    in
    div [ A.class "content pure-u-1 pure-u-md-3-4" ]
        [ div [] anagrams
        ]


anagramGroups : Model -> List (Html Msg)
anagramGroups model =
    let
        grouped =
            E.groupBy String.length model.anagrams

        wordLengths =
            List.range model.minCount (String.length model.word)
                |> List.reverse
                |> List.map (getData grouped)
    in
    wordLengths
        |> List.map wordSection


getData : D.Dict Int (List String) -> Int -> List String
getData d k =
    case D.get k d of
        Just h ->
            List.sort h

        Nothing ->
            []


wordSection : List String -> Html Msg
wordSection words =
    let
        letterCount =
            case List.head words of
                Just h ->
                    String.length h

                Nothing ->
                    0
    in
    section [ A.class "post" ]
        [ table [ A.class "pure-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Number of letters" ]
                    , th [] [ text (String.fromInt letterCount) ]
                    ]
                ]
            , tbody [] (wordRows words)
            ]
        ]


wordRows : List String -> List (Html Msg)
wordRows words =
    words
        |> List.map word


word : String -> Html Msg
word w =
    tr []
        [ td [ A.colspan 2 ] [ text w ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeWord newWord ->
            ( { model | word = newWord, anagrams = [] }, Cmd.none )

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
            ( model, getAnagrams model )

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


getAnagrams : Model -> Cmd Msg
getAnagrams model =
    Http.send NewAnagrams (Http.get (toAnagramUrl model) anagramDecoder)


anagramDecoder : Decode.Decoder (List String)
anagramDecoder =
    Decode.field "anagrams" (Decode.list Decode.string)


toAnagramUrl : Model -> String
toAnagramUrl model =
    U.crossOrigin "https://anagrambackend.chriswk.com"
        [ "anagram" ]
        [ U.string "word" model.word
        , U.string "minCount" (String.fromInt model.minCount)
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
