module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ElmEscapeHtml


-- import Json.Encode as Encode

import Http
import Decoder
import Jsonp
import Task
import Dom


main =
    Html.program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { title : String
    , searchText : String
    , errorText : String
    , loading : Bool
    , result : Bool
    , resultList : List Decoder.Result
    }


model : Model
model =
    { title = "វចនានុក្រមខ្មែរ"
    , searchText = ""
    , errorText = ""
    , loading = False
    , result = False
    , resultList = []
    }


type Msg
    = SearchInput String
    | ClearText
    | Search
    | OnFetchData (Result Http.Error Decoder.Data)
    | OnInputFocus (Result Dom.Error ())
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput text ->
            ( { model | searchText = text }, Cmd.none )

        ClearText ->
            ( { model | searchText = "" }
            , Task.attempt OnInputFocus (Dom.focus "input")
            )

        Search ->
            case model.searchText of
                "" ->
                    ( { model | errorText = "សូមសរសេរពាក្យ" }, Cmd.none )

                _ ->
                    ( { model
                        | loading = True
                        , result = False
                        , errorText = ""
                      }
                    , fetchData model.searchText
                    )

        OnInputFocus status ->
            case status of
                Ok result ->
                    ( model, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Focus Dom" error
                    in
                        ( model, Cmd.none )

        OnFetchData status ->
            case status of
                Ok result ->
                    let
                        _ =
                            Debug.log "Fetch Data" result
                    in
                        ( { model
                            | resultList = result.data
                            , result = True
                            , loading = False
                          }
                        , Cmd.none
                        )

                -- ( { model | promoList = newPromoList }, modal )
                Err error ->
                    let
                        _ =
                            Debug.log "Fetch Data" error
                    in
                        ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


lookupInfo : String -> Task.Task Http.Error Decoder.Data
lookupInfo phrase =
    Jsonp.get Decoder.dataDecoder
        ("https://glosbe.com/gapi_v0_1/translate?from=km&dest=ljp&format=json&phrase="
            ++ phrase
        )


fetchData : String -> Cmd Msg
fetchData phrase =
    Task.attempt OnFetchData (lookupInfo phrase)


view : Model -> Html Msg
view model =
    div [ class "w-100 w-60-ns center dark-gray pa2 " ]
        [ div [ class "f2 mb3 mt2 mid-gray title-kh tc" ] [ text model.title ]
        , Html.form
            [ class "mb3 dib ba bw2 br2 b--light-gray pa2 flex"
            , onSubmit Search
            ]
            [ input
                [ class "bn noselect outline-0 lh-kh flex-auto"
                , placeholder "ពាក្យ"
                , onInput SearchInput
                , value model.searchText
                , id "input"
                ]
                []
            , button
                [ class "pv2 ph3 bn bg-white pointer hover-bg-light-gray mr1"
                , type_ "button"
                , onClick ClearText
                ]
                [ text "X" ]
            , button
                [ class "pa2 bg-light-gray bn br1 pointer hover-bg-moon-gray"
                ]
                [ text "ស្វែងរក" ]
            ]
          -- Error Text
        , div [ class "my-mh" ]
            [ if model.errorText /= "" then
                p [ class "red" ] [ text model.errorText ]
              else
                div [] []
              -- Loading
            , if model.loading == True then
                loadingView model.searchText
              else
                div [] []
              -- Result List
            , if model.result == True then
                if model.resultList == [] then
                    p [ class "red" ] [ text "ស្វែងរកមិនមាន" ]
                else
                    div [] (List.map resultView model.resultList)
              else
                div [] []
            ]
        , footerView
        ]


loadingView searchText =
    div
        [ class "mb3 green"
        ]
        [ text ("កំពុងស្វែងរកពាក្យ \"" ++ searchText ++ "\" ...") ]


resultView result =
    div [ class "mb3" ]
        (List.map
            (\meaning ->
                p
                    [ class "lh-kh"
                    ]
                    [ meaning.text
                        |> ElmEscapeHtml.unescape
                        |> ElmEscapeHtml.unescape
                        |> String.append "- "
                        |> text
                    ]
            )
            result.meaningList
        )


footerView =
    div []
        [ text "បង្កើតដោយ"
        , a [ class "blue no-underline", href "https://github.com/chmar77/elm-khmer-dictionary", target "_blank" ]
            [ text " chmar77" ]
        ]



-- , p [ class "lh-copy" ] [ text "ព្យញ្ជនៈទី ១ ក្នុងវគ្គទី ១ ជាកណ្ឋជៈ មានសំឡេងក្នុងឋានបំពង់ក ជាសិថិល-អឃោសៈ, សំ. បា. មានសូរស័ព្ទថា កៈ ។" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
