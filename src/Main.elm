module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Decoder


main =
    Html.program
        { init = ( model, fetchData )
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
    , resultList : List String
    }


model : Model
model =
    { title = "វចនានុក្រមខ្មែរ"
    , searchText = ""
    , errorText = ""
    , loading = False
    , result = True
    , resultList = []
    }


type Msg
    = SearchInput String
    | ClearText
    | Search
    | OnFetchData (Result Http.Error Decoder.Data)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput text ->
            ( { model | searchText = text }, Cmd.none )

        ClearText ->
            ( { model | searchText = "" }, Cmd.none )

        Search ->
            case model.searchText of
                "" ->
                    ( { model | errorText = "សូមសរសេរពាក្យ" }, Cmd.none )

                _ ->
                    ( { model | loading = True, errorText = "" }, Cmd.none )

        OnFetchData status ->
            case status of
                Ok result ->
                    let
                        _ =
                            Debug.log "Fetch Data" result
                    in
                        ( model, Cmd.none )

                -- ( { model | promoList = newPromoList }, modal )
                Err error ->
                    let
                        _ =
                            Debug.log "Fetch Data" error
                    in
                        ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


fetchData =
    Http.request
        { method = "GET"
        , headers =
            -- []
            [ Http.header "Origin" "https://chmar77.github.io/elm-khmer-dictionary/index.html"
            , Http.header "Access-Control-Request-Method" "GET"
            , Http.header "Access-Control-Request-Headers" "X-Custom-Header"
            ]
        , url = "https://glosbe.com/gapi/translate?from=km&dest=ljp&format=json&phrase=%E1%9E%80&pretty=true"
        , body = Http.emptyBody
        , expect = Http.expectJson Decoder.dataDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send OnFetchData


view : Model -> Html Msg
view model =
    div [ class "w-100 w-60-ns center dark-gray pa2 " ]
        [ div [ class "f2 mb3 mt2 mid-gray title-kh tc" ] [ text model.title ]
        , div [ class "mb3 dib ba bw2 br2 b--light-gray pa2 flex" ]
            [ input
                [ class "bn noselect outline-0 lh-kh flex-auto"
                , placeholder "ពាក្យ"
                , onInput SearchInput
                , value model.searchText
                ]
                []
            , button
                [ class "pa2 bn bg-white pointer hover-bg-light-gray mr1"
                  -- dn-ns
                , onClick ClearText
                ]
                [ text "X" ]
            , button
                [ class "pa2 bg-light-gray bn br1 pointer hover-bg-moon-gray"
                , onClick Search
                ]
                [ text "ស្វែងរក" ]
            ]
          -- Error Text
        , if model.errorText /= "" then
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
                p [] [ text "ស្វែងរកមិនមាន" ]
            else
                resultView
          else
            div [] []
        ]


loadingView searchText =
    div [ class "mb3" ]
        [ text ("កំពុងស្វែងរកពាក្យ \"" ++ searchText ++ "\" ...")
        ]


resultView =
    div [ class "mb3 dn" ]
        [ p [ class "lh-copy" ] [ text "ព្យញ្ជនៈទី ១ ក្នុងវគ្គទី ១ ជាកណ្ឋជៈ មានសំឡេងក្នុងឋានបំពង់ក ជាសិថិល-អឃោសៈ, សំ. បា. មានសូរស័ព្ទថា កៈ ។" ]
        , p [ class "lh-copy" ] [ text "ព្យញ្ជនៈទី ១ ក្នុងវគ្គទី ១ ជាកណ្ឋជៈ មានសំឡេងក្នុងឋានបំពង់ក ជាសិថិល-អឃោសៈ, សំ. បា. មានសូរស័ព្ទថា កៈ ។" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
