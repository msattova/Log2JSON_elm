module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import File exposing (File)
import File.Select as Select
import File.Download as Download
import Json.Decode as Decode
import Json.Encode as Encode
import Debug

import List exposing (..)
import Parser exposing (..)
import Set
import Task
import File.Download as Download

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { html : Maybe (List MessageData)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = HtmlRequested
  | HtmlSelected File
  | HtmlLoaded (List MessageData)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HtmlRequested ->
      ( model
      , Select.file ["text/html"] HtmlSelected)
    HtmlSelected file ->
      ( model
      , Task.perform HtmlLoaded
        <| Task.map getMessageDataList
        <| File.toString file
      )
    HtmlLoaded content ->
      ( { model | html = Just content }
      , save content)


-- VIEW

insertLi : a -> Html msg
insertLi some =
  li [] [ text (Debug.toString some) ]

view : Model -> Html Msg
view model =
  div [ style "margin" "3em" ] [
    case model.html of
      Nothing ->
        button [ onClick HtmlRequested ] [ text "Load HTML" ]
      Just content ->
        div []
        [ ul [ style "white-space" "pre" ] (List.map insertLi content)
        ]

  ]
--- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

--- download

save : List MessageData -> Cmd msg
save data =
  Download.string "sessiondata.json" "application/json"
  <| toJSON
  <| toSectionData data

toJSON : SectionData -> String
toJSON data =
  let
    encodeMessageData: MessageData -> Encode.Value
    encodeMessageData data2 =
      Encode.object
        [ ("type", Encode.string data2.messageType)
        , ("tabNum", Encode.string data2.tabNum)
        , ("name", Encode.string data2.name)
        , ("color", Encode.string data2.color)
        , ("timestamp", Encode.string data2.timestamp)
        , ("content", Encode.string data2.content)
        ]
  in
  Encode.object
    [ ("title", Encode.string data.title)
    , ("sectionNum", Encode.int data.sectionNum)
    , ("data", Encode.list encodeMessageData data.data)
    ]
  |> Encode.encode 2

--- others

getMessageDataList : String -> List MessageData
getMessageDataList html =
  html
  |> run chatlogParser
  |> Result.withDefault ""
  |> toChatlogs
  |> toMessageDataList

toChatlogs : String -> List String
toChatlogs html =
  let
    removeLines : String -> String
    removeLines str
      = String.replace "\n" "" str

    addLineAfter : String -> String -> String
    addLineAfter target str
      = String.replace target (target++"\n") str

    replaces : String -> String
    replaces str
      = str
        |> addLineAfter "<div class=\"main-logs\">"
        |> addLineAfter "</div>"

    isNotEmpty : String -> Bool
    isNotEmpty str
      = not (String.isEmpty str)

  in
  html
    |> removeLines
    |> replaces
    |> String.lines
    |> List.map String.trim
    |> List.filter isNotEmpty

toMessageDataList : List String ->  List MessageData
toMessageDataList data =
  List.filterMap divToObject data

type alias SectionData =
  { title: String
  , sectionNum: Int
  , data: List MessageData
  }

toSectionData : List MessageData -> SectionData
toSectionData data_ =
  { title = "章タイトル"
  , sectionNum = 0
  , data = data_
  }

type alias MessageData =
  { messageType: String
  , tabNum: String
  , name: String
  , color: String
  , timestamp: String
  , content: String
  }

----- Parser

untilChatolog : Parser String
untilChatolog =
  getChompedString <|
    succeed ()
    |. chompUntil "<div class=\"chatlog-wrap\">"

untilControlHidden : Parser String
untilControlHidden =
  getChompedString <|
    succeed ()
    |. chompUntil "<div class=\"controll hidden\">"

chatlogParser : Parser String
chatlogParser =
  succeed identity
  |. untilChatolog
  |. symbol "<div class=\"chatlog-wrap\">"
  |. spaces
  |= untilControlHidden


type alias TabColor =
  { tab: String
  , color: String
  }

type alias  ParsedMessage =
    { tabColor : TabColor
    , name : String
    , text : String
    , timestamp : String
    }

classAttrParser : Parser String
classAttrParser =
      succeed identity
      |. keyword "class"
      |. spaces
      |. symbol "="
      |. spaces
      |. symbol "\""
      |= variable
          {
            start = \c -> c == 't'
            , inner = \c -> Char.isAlphaNum c
            , reserved = Set.empty
          }
      |. symbol "\""


colorAttrParser : Parser String
colorAttrParser =
      succeed identity
      |. keyword "style"
      |. spaces
      |. symbol "="
      |. spaces
      |. symbol "\""
      |. keyword "color"
      |. spaces
      |. symbol ":"
      |. spaces
      |= variable
          {
            start = \c -> c == '#'
            , inner = \c -> Char.isAlphaNum c
            , reserved = Set.empty
          }
      |. symbol ";"
      |. symbol "\""


getTabColorParser : Parser TabColor
getTabColorParser =
      succeed TabColor
        |. symbol "<"
        |. keyword "div"
        |. spaces
        |= classAttrParser
        |. spaces
        |= colorAttrParser
        |. symbol ">"


getNameParser : Parser String
getNameParser =
    let
     untilNameEnd : Parser String
     untilNameEnd =
      getChompedString <|
        succeed ()
        |. chompUntil "：</b>"
    in
      succeed identity
      |. symbol "<b>"
      |= untilNameEnd

getTextParser : Parser String
getTextParser =
  getChompedString <|
    succeed ()
    |. chompUntil "<span>"

getTimestampParser : Parser String
getTimestampParser =
  getChompedString <|
  succeed ()
  |. chompUntil "</span>"

divLineParser : Parser ParsedMessage
divLineParser =
  succeed ParsedMessage
  |= getTabColorParser
  |= getNameParser
  |. symbol "：</b>"
  |= getTextParser
  |. symbol "<span>"
  |= getTimestampParser

toMessageData : ParsedMessage -> MessageData
toMessageData parsed =
        { messageType = "message"
        , tabNum = String.replace "tab" "" parsed.tabColor.tab
        , name = parsed.name
        , color = parsed.tabColor.color
        , content = parsed.text
        , timestamp = parsed.timestamp
        }

divToObject : String -> Maybe MessageData
divToObject line =
    case (run divLineParser line) of
      Ok parsed ->
        Just <|
          toMessageData parsed
      Err _ ->
        Nothing

