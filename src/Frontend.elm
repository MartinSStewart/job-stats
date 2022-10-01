module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Dict exposing (Dict)
import Element
import Element.Font
import Element.Input
import Http
import Json.Decode as Decode exposing (Decoder)
import Lamdera
import List.Extra as List
import Time exposing (Month(..))
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , history = []
      }
    , Http.get { url = "/network-data.har", expect = Http.expectString LoadData }
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        LoadData result ->
            case result of
                Ok jsonText ->
                    let
                        bracketIndices =
                            String.indexes delimiter jsonText

                        history =
                            String.indexes "conversations.history" jsonText
                                |> List.concatMap
                                    (\urlPos ->
                                        getJsonObject urlPos 0 bracketIndices jsonText
                                            |> Decode.decodeString
                                                (Decode.field "response"
                                                    (Decode.field "content"
                                                        (Decode.field "text"
                                                            (Decode.andThen
                                                                (\content ->
                                                                    case Decode.decodeString decodeHistory content of
                                                                        Ok ok ->
                                                                            Decode.succeed ok

                                                                        Err error ->
                                                                            Decode.fail (Decode.errorToString error)
                                                                )
                                                                Decode.string
                                                            )
                                                        )
                                                    )
                                                )
                                            |> (\a ->
                                                    case a of
                                                        Ok list ->
                                                            list

                                                        Err error ->
                                                            let
                                                                _ =
                                                                    Debug.log "error" error
                                                            in
                                                            []
                                               )
                                            |> List.filterMap
                                                (\message ->
                                                    case message of
                                                        NormalMessage message_ ->
                                                            Just message_

                                                        DeletedMessage ->
                                                            Nothing

                                                        UserJoinedMessage ->
                                                            Nothing

                                                        UserLeftMessage ->
                                                            Nothing

                                                        PinnedMessage ->
                                                            Nothing
                                                )
                                            |> List.uniqueBy .time
                                    )
                    in
                    ( { model
                        | history =
                            List.map
                                (\message ->
                                    { isChecked =
                                        String.startsWith "|" message.text
                                            && String.contains "Elm" message.text
                                    , message = message
                                    }
                                )
                                history
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )

        PressedCheckbox time isChecked ->
            let
                newHistory =
                    List.updateIf (.message >> .time >> (==) time)
                        (\a -> { a | isChecked = isChecked })
                        model.history

                _ =
                    Debug.log "history"
                        (List.filterMap
                            (\a ->
                                if a.isChecked then
                                    Just a.message.time

                                else
                                    Nothing
                            )
                            newHistory
                        )
            in
            ( { model | history = newHistory }, Cmd.none )


decodeHistory : Decoder (List Message)
decodeHistory =
    Decode.field "messages" (Decode.list decodeMessage)


decodeMessage : Decoder Message
decodeMessage =
    Decode.field "text" Decode.string
        |> Decode.andThen
            (\messageText ->
                if messageText == "This message was deleted." then
                    Decode.succeed DeletedMessage

                else if String.contains "> has joined the channel" messageText then
                    Decode.succeed UserJoinedMessage

                else if String.contains " has left the channel" messageText then
                    Decode.succeed UserLeftMessage

                else if String.contains " pinned a message to this channel." messageText then
                    Decode.succeed PinnedMessage

                else
                    Decode.map
                        (\a -> Message_ a messageText |> NormalMessage)
                        (Decode.field "ts"
                            (Decode.andThen
                                (\text ->
                                    case String.toFloat text of
                                        Just float ->
                                            float * 1000 |> round |> Time.millisToPosix |> Decode.succeed

                                        Nothing ->
                                            Decode.fail "Invalid timestamp"
                                )
                                Decode.string
                            )
                        )
            )


delimiter =
    "      },\n      {"


getJsonObject urlPos previousIndex startEndIndices jsonText =
    case startEndIndices of
        first :: rest ->
            if first > urlPos then
                String.slice (previousIndex + String.length delimiter - 1) (first + 7) jsonText

            else
                getJsonObject urlPos first rest jsonText

        [] ->
            ""


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


intToMonth : Int -> Month
intToMonth month =
    case month of
        0 ->
            Jan

        1 ->
            Feb

        2 ->
            Mar

        3 ->
            Apr

        4 ->
            May

        5 ->
            Jun

        6 ->
            Jul

        7 ->
            Aug

        8 ->
            Sep

        9 ->
            Oct

        10 ->
            Nov

        11 ->
            Dec

        _ ->
            Debug.todo "error"


roundToMonths : Time.Posix -> Int
roundToMonths time =
    Time.toYear Time.utc time * 12 + (Time.toMonth Time.utc time |> monthToInt)


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        minTime : Time.Posix
        minTime =
            List.minimumBy (.message >> .time >> Time.posixToMillis) model.history
                |> Maybe.map (.message >> .time)
                |> Maybe.withDefault (Time.millisToPosix 0)

        maxTime : Time.Posix
        maxTime =
            List.maximumBy (.message >> .time >> Time.posixToMillis) model.history
                |> Maybe.map (.message >> .time)
                |> Maybe.withDefault (Time.millisToPosix 0)

        minValue =
            roundToMonths minTime

        maxValue =
            roundToMonths maxTime

        dataList : Dict Int Int
        dataList =
            List.range (minValue |> Debug.log "minInt") (maxValue |> Debug.log "maxInt")
                |> List.map (\index -> ( index, 0 ))
                |> Dict.fromList

        data : List Float
        data =
            List.foldl
                (\a state ->
                    if a.isChecked then
                        Dict.update (roundToMonths a.message.time) (Maybe.withDefault 0 >> (+) 1 >> Just) state

                    else
                        state
                )
                dataList
                model.history
                |> Dict.toList
                |> List.map (Tuple.second >> toFloat)
    in
    { title = ""
    , body =
        [ Element.layout
            [ Element.Font.size 16
            , Element.padding 16
            ]
            (Element.column
                []
                [ C.chart
                    [ CA.height 100
                    , CA.width 300
                    ]
                    [ C.xTicks [ CA.amount ((1 + maxValue - minValue) // 12) ]
                    , C.yTicks []
                    , C.xLabels
                        [ CA.format
                            (\value ->
                                let
                                    a =
                                        minValue + round value

                                    year =
                                        a // 12

                                    month =
                                        modBy 12 a
                                in
                                String.fromInt month ++ " " ++ String.fromInt year
                            )
                        , CA.fontSize 8
                        ]
                    , C.yLabels [ CA.fontSize 8 ]
                    , C.xAxis []
                    , C.yAxis []
                    , C.bars [] [ C.bar identity [] ] data
                    ]
                    |> Element.html
                    |> Element.el [ Element.width Element.fill, Element.padding 64 ]
                , Element.column
                    [ Element.spacing 32, Element.width Element.fill ]
                    (List.reverse model.history
                        |> List.map
                            (\{ isChecked, message } ->
                                Element.Input.checkbox
                                    [ Element.width Element.fill ]
                                    { onChange = PressedCheckbox message.time
                                    , icon = Element.Input.defaultCheckbox
                                    , checked = isChecked
                                    , label =
                                        String.split "Elm" message.text
                                            |> List.map Element.text
                                            |> List.intersperse (Element.el [ Element.Font.bold ] (Element.text "Elm"))
                                            |> Element.paragraph []
                                            |> Element.Input.labelRight [ Element.width Element.fill ]
                                    }
                            )
                    )
                ]
            )
        ]
    }
