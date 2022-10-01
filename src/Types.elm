module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , history : List { isChecked : Bool, message : Message_ }
    }


type Message
    = NormalMessage Message_
    | DeletedMessage
    | UserJoinedMessage
    | UserLeftMessage
    | PinnedMessage


type alias Message_ =
    { time : Time.Posix, text : String }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | LoadData (Result Http.Error String)
    | PressedCheckbox Time.Posix Bool


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
