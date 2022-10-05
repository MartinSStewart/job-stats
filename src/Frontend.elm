module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Bytes
import Bytes.Decode
import Chart as C
import Chart.Attributes as CA
import Date exposing (Date, Interval(..))
import Dict exposing (Dict)
import Element
import Element.Font
import Element.Input
import Html
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Lamdera
import List.Extra as List
import MarkedAsJobs
import Set
import Time exposing (Month(..))
import Types exposing (..)
import Url
import Zip exposing (Zip)
import Zip.Entry


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
    , Http.get
        { url = "/jobs.zip"
        , expect =
            Http.expectBytesResponse LoadData
                (\response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            Ok body

                        _ ->
                            Debug.todo ""
                )
        }
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
                Ok bytes ->
                    let
                        zip : Zip
                        zip =
                            case Zip.fromBytes bytes of
                                Just zip_ ->
                                    zip_

                                Nothing ->
                                    Debug.todo "Invalid zip"

                        files : List String
                        files =
                            Zip.entries zip
                                |> List.map
                                    (\entry ->
                                        case Zip.Entry.toString entry of
                                            Ok text ->
                                                text

                                            Err _ ->
                                                Debug.todo "zip decode error"
                                    )

                        history =
                            List.concatMap
                                (\file ->
                                    case
                                        Decode.decodeString
                                            (Decode.list decodeMessage)
                                            file
                                    of
                                        Ok messages ->
                                            messages

                                        Err _ ->
                                            []
                                )
                                files
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

                        history2 =
                            List.map
                                (\message ->
                                    { isChecked =
                                        Set.member (Time.posixToMillis message.time) MarkedAsJobs.data

                                    --String.startsWith "|" message.text
                                    --    && String.contains "Elm" message.text
                                    , message = message
                                    }
                                )
                                history

                        --_ =
                        --    Debug.log "match" ( List.count .isChecked history2, Set.size MarkedAsJobs.data )
                        --
                        --_ =
                        --    Debug.log "are unique"
                        --        ( List.length (List.uniqueBy (.message >> .time) history2)
                        --        , List.length history2
                        --        )
                    in
                    ( { model
                        | history = history2
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


dateToTime : Date -> Time.Posix
dateToTime date =
    Date.toIsoString date |> Iso8601.toTime |> Result.withDefault (Time.millisToPosix 0)


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        dataList : Dict Int { start : Date, y : Float }
        dataList =
            Dict.empty

        data : List { start : Time.Posix, end : Time.Posix, y : Float }
        data =
            List.foldl
                (\a state ->
                    if a.isChecked then
                        let
                            date =
                                Date.fromPosix Time.utc a.message.time |> Date.floor Month
                        in
                        Dict.update
                            (Date.toRataDie date)
                            (\value ->
                                (case value of
                                    Just b ->
                                        { b | y = b.y + 1 }

                                    Nothing ->
                                        { start = date, y = 1 }
                                )
                                    |> Just
                            )
                            state

                    else
                        state
                )
                dataList
                model.history
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map
                    (\a ->
                        { start = dateToTime a.start
                        , end = Date.add Date.Months 1 a.start |> dateToTime
                        , y = a.y
                        }
                    )
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
                    [ CA.height 300
                    , CA.width 900
                    ]
                    [ C.xTicks [ CA.times Time.utc, CA.amount 20, CA.withGrid ]
                    , C.xLabels [ CA.times Time.utc, CA.amount 20, CA.withGrid ]
                    , C.yLabels [ CA.ints, CA.withGrid ]
                    , C.labelAt .min
                        CA.middle
                        [ CA.moveLeft 35, CA.rotate 90 ]
                        [ Html.text "Job posts per month" ]
                    , C.bars
                        [ CA.x1 (.start >> Time.posixToMillis >> toFloat)
                        , CA.x2 (.end >> Time.posixToMillis >> toFloat)
                        , CA.margin 0.05
                        ]
                        [ C.bar .y [] ]
                        data
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
