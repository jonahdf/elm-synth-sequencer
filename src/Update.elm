module Update exposing (..)

import Array as A
import Dict
import File exposing (File)
import File.Download
import File.Select
import Model exposing (..)
import Note exposing (..)
import SaveLoad exposing (..)
import Scales exposing (..)
import Task
import Time
import Track exposing (..)


type Msg
    = NoOp
      --
    | NoteOn String
    | NoteOff String
      --
    | Transpose String
    | NextStep Time.Posix
    | ToggleNote Float Int
    | PauseToggle
    | Clear
    | Reset
    | Bpm String
    | ChangeBeats String
    | ChangeScale String
    | ChangePianoType String
    | ChangeSeqType String
    | Download
    | Load File
    | OpenFileClicked
    | FileRead String
    | InstructionsToggle
    | SeqVolume String
    | PianoVolume String


noteOn : String -> Model -> Model
noteOn key model =
    { model
        | piano =
            List.map
                (\note ->
                    if note.key == key then
                        { note | triggered = True }

                    else
                        note
                )
                model.piano
    }


noteOff : String -> Model -> Model
noteOff key model =
    { model
        | piano =
            List.map
                (\note ->
                    if note.key == key then
                        { note | triggered = False }

                    else
                        note
                )
                model.piano
    }


updateTrack : Model -> Model
updateTrack model =
    { model
        | beat =
            if model.beat >= (model.len - 1) then
                0

            else
                model.beat + 1
        , notes =
            A.toList
                (A.map
                    (\track ->
                        case A.get model.beat track.beats of
                            Just bool ->
                                Note track.key track.midi bool

                            Nothing ->
                                Note track.key track.midi False
                    )
                    model.tracks
                )
    }


updateTranspose : Model -> String -> Model
updateTranspose model t =
    { model
        | transpose = Maybe.withDefault 0 (String.toFloat t)
    }


updateToggle : Model -> Float -> Int -> Model
updateToggle model midi beat =
    { model
        | tracks = trackToggle model.tracks midi beat
    }


pauseToggle : Model -> Model
pauseToggle model =
    { model
        | go =
            not model.go
        , notes = []
    }


initTracks : Model -> A.Array Track
initTracks model =
    A.map
        (\track -> { track | beats = A.repeat (model.len + 1) False })
        model.tracks


clear : Model -> Model
clear model =
    { model
        | tracks = initTracks model
    }


updateBpm : Model -> String -> Model
updateBpm model string =
    { model
        | bpm =
            case String.toFloat string of
                Just i ->
                    i

                Nothing ->
                    model.bpm
    }


updateBeats : Model -> String -> Model
updateBeats model string =
    { model
        | len =
            case String.toInt string of
                Just i ->
                    i

                Nothing ->
                    model.len
        , tracks = initTracks model
    }


updateReset : Model -> Model
updateReset model =
    { model
        | len = 8
        , bpm = 200
        , transpose = 0
    }


updateScale : Model -> String -> Model
updateScale model string =
    let
        newScale =
            Maybe.withDefault pentatonic (Dict.get string scales)
    in
    { model
        | notes = scaleInit newScale
        , piano = pianoKeys (scaleInit newScale)
        , scale = newScale
        , tracks = trackInit newScale model.len
    }


updatePianoType : Model -> String -> Model
updatePianoType model string =
    { model
        | pianoType = String.toLower string
    }


updateSeqType : Model -> String -> Model
updateSeqType model string =
    { model
        | seqType = String.toLower string
    }


updateInstructions : Model -> Model
updateInstructions model =
    { model
        | instructions = not model.instructions
    }


updateSeqVolume : Model -> String -> Model
updateSeqVolume model string =
    { model
        | seqVolume =
            case String.toFloat string of
                Just i ->
                    i

                Nothing ->
                    model.seqVolume
    }


updatePianoVolume : Model -> String -> Model
updatePianoVolume model string =
    { model
        | pianoVolume =
            case String.toFloat string of
                Just i ->
                    i

                Nothing ->
                    model.pianoVolume
    }


download : Model -> Cmd msg
download model =
    File.Download.string "song.jonah" "text/rtf" (save model)


modelFromFile : Model -> String -> Model
modelFromFile model string =
    load model string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            Tuple.pair model Cmd.none

        NoteOn key ->
            ( noteOn key model
            , Cmd.none
            )

        NoteOff key ->
            ( noteOff key model
            , Cmd.none
            )

        PauseToggle ->
            ( pauseToggle model, Cmd.none )

        Transpose string ->
            ( updateTranspose model string
            , Cmd.none
            )

        Reset ->
            ( updateReset model, Cmd.none )

        NextStep _ ->
            if model.go then
                ( updateTrack model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ToggleNote midi beat ->
            ( updateToggle model midi beat, Cmd.none )

        Clear ->
            ( clear model, Cmd.none )

        Bpm string ->
            ( updateBpm model string, Cmd.none )

        ChangeBeats string ->
            ( updateBeats model string, Cmd.none )

        ChangeScale string ->
            ( updateScale model string, Cmd.none )

        ChangePianoType string ->
            ( updatePianoType model string, Cmd.none )

        ChangeSeqType string ->
            ( updateSeqType model string, Cmd.none )

        Download ->
            ( model, download model )

        OpenFileClicked ->
            ( model, File.Select.file [] Load )

        Load file ->
            ( model, Task.perform FileRead (File.toString file) )

        FileRead string ->
            ( modelFromFile model string, Cmd.none )

        InstructionsToggle ->
            ( updateInstructions model, Cmd.none )

        SeqVolume string ->
            ( updateSeqVolume model string, Cmd.none )

        PianoVolume string ->
            ( updatePianoVolume model string, Cmd.none )
