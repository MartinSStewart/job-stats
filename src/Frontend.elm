module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Element.Font
import Html
import Html.Attributes as Attr
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Lamdera
import Time
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
                                    )
                    in
                    ( { model | history = history }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )


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


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            [ Element.Font.size 16, Element.padding 16 ]
            (Element.column
                [ Element.spacing 32 ]
                (List.map
                    (\message ->
                        case message of
                            NormalMessage { time, text } ->
                                Element.paragraph [] [ Element.text text ]

                            DeletedMessage ->
                                Element.none

                            UserJoinedMessage ->
                                Element.none

                            UserLeftMessage ->
                                Element.none
                    )
                    model.history
                )
            )
        ]
    }
