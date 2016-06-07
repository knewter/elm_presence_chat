module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))


-- Our model will track a list of messages and the text for our new message to
-- send.  We only support chatting in a single channel for now.


type alias ChatMessage =
    { user : String
    , body : String
    }


type alias Model =
    { newMessage : String
    , messages : List ChatMessage
    , phxSocket : Phoenix.Socket.Socket Msg
    }



-- We can either set our new message or join our channel


type Msg
    = SetNewMessage String
    | JoinChannel
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SendMessage
    | ReceiveChatMessage JE.Value



-- Basic initial model is straightforward


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , phxSocket = initPhxSocket
    }


socketServer : String
socketServer =
    "ws://localhost:4000/socket/websocket"


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "room:lobby" ReceiveChatMessage



-- We'll handle either setting the new message or joining the channel.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewMessage string ->
            { model | newMessage = string } ! []

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init "room:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        SendMessage ->
            let
                payload =
                    (JE.object [ ( "body", JE.string model.newMessage ) ])

                push' =
                    Phoenix.Push.init "new:msg" "room:lobby"
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push' model.phxSocket
            in
                ( { model
                    | newMessage = ""
                    , phxSocket = phxSocket
                  }
                , Cmd.map PhoenixMsg phxCmd
                )

        ReceiveChatMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    { model | messages = chatMessage :: model.messages } ! []

                Err error ->
                    model ! []


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.object2 ChatMessage
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)


viewMessage : ChatMessage -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]



-- Our view will consist of a button to join the lobby, a list of messages, and
-- our text input for crafting our message


view : Model -> Html Msg
view model =
    div []
        -- Clicking the button joins the lobby channel
        [ button [ onClick JoinChannel ] [ text "Join lobby" ]
        , div [ class "messages" ]
            (List.map viewMessage model.messages)
          -- On input, we'll SetNewMessage
        , form [ onSubmit SendMessage ]
            [ input [ placeholder "Message...", onInput SetNewMessage, value model.newMessage ] [] ]
        ]



-- Wire together the program


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- No subscriptions yet


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- And here's our init function


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )
