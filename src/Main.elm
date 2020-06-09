port module Main exposing (..)

--
--
--

import Audio exposing (..)
import Browser.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (..)
import Note exposing (..)
import SaveLoad exposing (..)
import Scales exposing (..)
import Time
import Track exposing (..)
import Update exposing (..)
import View exposing (..)
import WebAudio.Context as Cont
import WebAudio.Program



-- Send the JSON encoded audio graph to javascript


port updateAudio : Encode.Value -> Cmd msg



-- MAIN -----------------------------------------------------------------------


main : Program Cont.AudioContext Model Msg
main =
    WebAudio.Program.element
        { init = init
        , update = update
        , audio = audio
        , view = view
        , subscriptions = subscriptions
        , audioPort = updateAudio
        }



-- INIT -----------------------------------------------------------------------


init : Cont.AudioContext -> ( Model, Cmd msg )
init co =
    ( { notes = scaleInit pentatonic
      , piano = pianoKeys (scaleInit pentatonic)
      , tracks = trackInit pentatonic 8
      , context = co
      , beat = 0
      , len = 8
      , go = True
      , transpose = 0
      , bpm = 200
      , scale = pentatonic
      , seqType = "sawtooth"
      , pianoType = "sawtooth"
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS --------------------------------------------------------------
--


noteOnDecoder : List Note -> Decode.Decoder Msg
noteOnDecoder notes =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case List.any (\note -> note.key == key) notes of
                    True ->
                        Decode.succeed (NoteOn key)

                    False ->
                        Decode.fail ""
            )


noteOffDecoder : List Note -> Decode.Decoder Msg
noteOffDecoder notes =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case List.any (\note -> note.key == key) notes of
                    True ->
                        Decode.succeed (NoteOff key)

                    False ->
                        Decode.fail ""
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| noteOnDecoder model.piano
        , Browser.Events.onKeyUp <| noteOffDecoder model.piano
        , Time.every (60000 / model.bpm) NextStep
        ]
