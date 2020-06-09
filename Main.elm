port module Main exposing (..)

--
--
--

import Array as A
import Browser
import Browser.Events
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , code
        , div
        , h1
        , main_
        , p
        , pre
        , text
        )
import Html.Attributes as H exposing (class, href, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (decodeString, field)
import Json.Encode as Encode
import Task
import Time
import WebAudio
import WebAudio.Context as Cont
import WebAudio.Program
import WebAudio.Property as Prop



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



-- MODEL ----------------------------------------------------------------------
--


type alias Note =
    { key : String
    , midi : Float
    , triggered : Bool
    }



--
--name: name of track, tone: note key, beats: Array of booleans


type alias Track =
    { key : String
    , midi : Float
    , beats : A.Array Bool
    }


type alias Model =
    { notes : List Note
    , piano : List Note
    , tracks : A.Array Track
    , beat : Int
    , len : Int
    , context : Cont.AudioContext
    , go : Bool
    , transpose : Float
    , bpm : Float
    , scale : Scale
    , seqType : String
    , pianoType : String
    }



-- SAVE


save : Model -> String
save model =
    Encode.encode
        0
        (Encode.object
            ([ ( "len", Encode.int model.len )
             , ( "bpm", Encode.float model.bpm )
             , ( "transpose", Encode.float model.transpose )
             , ( "scale-name", Encode.string model.scale.name )
             , ( "seqType", Encode.string model.seqType )
             , ( "pianoType", Encode.string model.pianoType )
             ]
                ++ List.map
                    (\t ->
                        ( t.key
                        , Encode.list Encode.bool
                            (A.toList
                                t.beats
                            )
                        )
                    )
                    (A.toList
                        model.tracks
                    )
            )
        )



-- LOAD --


load : Model -> String -> Model
load model s =
    let
        scaleString =
            case decodeString (field "scale-name" Decode.string) s of
                Ok sc ->
                    sc

                _ ->
                    "Pentatonic"

        newScale =
            Maybe.withDefault pentatonic (Dict.get scaleString scales)

        newLen =
            case decodeString (field "len" Decode.int) s of
                Ok l ->
                    l

                _ ->
                    model.len

        tracks =
            trackInit newScale newLen

        newTrack =
            A.map
                (\track ->
                    case
                        decodeString
                            (field track.key
                                (Decode.list
                                    Decode.bool
                                )
                            )
                            s
                    of
                        Ok bts ->
                            { track | beats = A.fromList bts }

                        _ ->
                            track
                )
                tracks
    in
    { model
        | tracks = newTrack
        , notes = scaleInit newScale
        , piano = pianoKeys (scaleInit newScale)
        , scale = newScale
        , len = newLen
        , bpm =
            case decodeString (field "bpm" Decode.float) s of
                Ok b ->
                    b

                _ ->
                    model.bpm
        , transpose =
            case decodeString (field "transpose" Decode.float) s of
                Ok t ->
                    t

                _ ->
                    model.transpose
        , seqType =
            case decodeString (field "seqType" Decode.string) s of
                Ok st ->
                    st

                _ ->
                    model.seqType
        , pianoType =
            case decodeString (field "pianoType" Decode.string) s of
                Ok pt ->
                    pt

                _ ->
                    model.pianoType
    }



{-
   decodeTracks : String -> A.Array Track
   decodeTracks string =
-}


scales : Dict String Scale
scales =
    Dict.fromList
        [ ( "Diatonic"
          , Scale "Diatonic"
                [ ( "C4", 60 )
                , ( "D4", 62 )
                , ( "E4", 64 )
                , ( "F4", 65 )
                , ( "G4", 67 )
                , ( "A4", 69 )
                , ( "B4", 71 )
                , ( "C5", 72 )
                , ( "D5", 74 )
                , ( "E5", 76 )
                ]
          )
        , ( "Pentatonic"
          , Scale "Pentatonic"
                [ ( "C4", 60 )
                , ( "D4", 62 )
                , ( "E4", 64 )
                , ( "G4", 67 )
                , ( "A4", 69 )
                , ( "C5", 72 )
                , ( "D5", 74 )
                , ( "E5", 76 )
                , ( "G5", 79 )
                , ( "A5", 81 )
                ]
          )
        , ( "Blues"
          , Scale "Blues"
                [ ( "C4", 60 )
                , ( "Eb4", 63 )
                , ( "F4", 65 )
                , ( "Gb4", 66 )
                , ( "G4", 67 )
                , ( "B4", 70 )
                , ( "C5", 72 )
                , ( "Eb5", 75 )
                , ( "F5", 77 )
                , ( "Gb5", 78 )
                ]
          )
        , ( "Major Arpeggio"
          , Scale "Major Arpeggio"
                [ ( "C3", 48 )
                , ( "E3", 52 )
                , ( "G3", 55 )
                , ( "C4", 60 )
                , ( "E4", 64 )
                , ( "G4", 67 )
                , ( "C5", 72 )
                , ( "E5", 76 )
                , ( "G5", 79 )
                , ( "C6", 84 )
                ]
          )
        , ( "Minor Arpeggio"
          , Scale "Minor Arpeggio"
                [ ( "A2", 45 )
                , ( "C3", 48 )
                , ( "E3", 52 )
                , ( "A3", 57 )
                , ( "C4", 60 )
                , ( "E4", 64 )
                , ( "A4", 69 )
                , ( "C5", 72 )
                , ( "E5", 76 )
                , ( "A5", 81 )
                ]
          )
        , ( "Chromatic"
          , Scale "Minor Arpeggio"
                [ ( "C4", 60 )
                , ( "C#4", 61 )
                , ( "D4", 62 )
                , ( "D#4", 63 )
                , ( "E4", 64 )
                , ( "F4", 65 )
                , ( "F#4", 66 )
                , ( "G4", 67 )
                , ( "G#4", 68 )
                , ( "A4", 69 )
                ]
          )
        ]


pentatonic =
    Scale "Pentatonic"
        [ ( "C4", 60 )
        , ( "D4", 62 )
        , ( "E4", 64 )
        , ( "G4", 67 )
        , ( "A4", 69 )
        , ( "C5", 72 )
        , ( "D5", 74 )
        , ( "E5", 76 )
        , ( "G5", 79 )
        , ( "A5", 81 )
        ]


type alias Scale =
    { name : String
    , notes : List ( String, Float )
    }


scaleInit : Scale -> List Note
scaleInit scale =
    List.map (\note -> Note (Tuple.first note) (Tuple.second note) False)
        scale.notes


pianoKeys : List Note -> List Note
pianoKeys notes =
    List.map2 (\note k -> { note | key = k })
        notes
        [ "a", "s", "d", "f", "g", "h", "j", "k", "l", ";" ]


trackToggle : A.Array Track -> Float -> Int -> A.Array Track
trackToggle tracks midi beat =
    A.map
        (\track ->
            if track.midi == midi then
                case A.get beat track.beats of
                    Just bool ->
                        Track track.key
                            track.midi
                            (A.set beat
                                (not bool)
                                track.beats
                            )

                    Nothing ->
                        Track track.key track.midi track.beats

            else
                Track track.key track.midi track.beats
        )
        tracks


trackSetList : A.Array Track -> List ( Float, Int ) -> A.Array Track
trackSetList tracks lst =
    case lst of
        [] ->
            tracks

        ( midi, beat ) :: tl ->
            trackSetList (trackToggle tracks midi beat) tl


getValIndex : Model -> Int -> Int -> Bool
getValIndex model index beat =
    case A.get index model.tracks of
        Just t ->
            case A.get beat t.beats of
                Just b ->
                    b

                Nothing ->
                    False

        Nothing ->
            False


getVal : Model -> Float -> Int -> Bool
getVal model midi beat =
    getValIndex model (midiToIndex model midi) beat


midiToIndex : Model -> Float -> Int
midiToIndex model midi =
    let
        check i lst =
            case lst of
                [] ->
                    Debug.todo "not in model"

                h :: tl ->
                    if h.midi == midi then
                        i

                    else
                        check (i + 1) tl
    in
    check 0 (A.toList model.tracks)


trackInit : Scale -> Int -> A.Array Track
trackInit scale beats =
    A.fromList
        (List.map
            (\note ->
                { key = Tuple.first note
                , midi = Tuple.second note
                , beats = A.repeat beats False
                }
            )
            scale.notes
        )


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



-- UPDATE ---------------------------------------------------------------------


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



--


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



--


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
            if model.go then
                False

            else
                True
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

        NextStep newTime ->
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



-- AUDIO ----------------------------------------------------------------------
-- Super simple utility function that takes a MIDI note number like 60 and
-- converts it to the corresponding frequency in Hertz. We use Float for the
-- MIDI number to allow for detuning, and we assume A4 is MIDI note number
-- 69.


mtof : Float -> Float
mtof midi =
    440 * 2 ^ ((midi - 69) / 12)



-- This takes a Note (as defined above) and converts that to a synth voice.


voice : Float -> String -> Note -> WebAudio.Node
voice transpose waveType note =
    WebAudio.oscillator
        [ Prop.frequency <| mtof (note.midi + transpose)
        , Prop.type_ waveType
        ]
        [ WebAudio.gain
            [ Prop.gain <|
                if note.triggered then
                    0.1

                else
                    0
            ]
            [ WebAudio.dac ]
        ]


voicePiano : Float -> String -> Note -> WebAudio.Node
voicePiano transpose waveType note =
    WebAudio.oscillator
        [ Prop.frequency <| mtof (note.midi + transpose)
        , Prop.type_ waveType
        ]
        [ WebAudio.gain
            [ Prop.gain <|
                if note.triggered then
                    0.1

                else
                    0
            ]
            [ WebAudio.dac ]
        ]



-- On the js side, the virtual audio graph is expecting an array of virtual
-- nodes. This plays nicely with our list of Notes, we can simply map the
-- Notes to synth voices and encode the new list.
-- Returns a Cmd Msg as we call the port from within this function (rather
-- than returning the encoded JSON).


audio : Model -> WebAudio.Graph
audio model =
    List.map (voice model.transpose model.seqType) model.notes
        ++ List.map (voicePiano model.transpose model.pianoType) model.piano



-- VIEW -----------------------------------------------------------------------
-- Use this to toggle the main styling on a note based on wheter it is currently
-- active or note. Basically just changes the background and font colour.


noteCSS : Bool -> String
noteCSS active =
    if active then
        "bg-indigo-500 text-white font-bold py-2 px-4 mr-4 rounded"

    else
        "bg-indigo-100 text-black font-bold py-2 px-4 mr-4 rounded"



-- This takes a Note (as defined above) and converts that to some  Notice
-- how we use the data for both the `voice` function and this `noteView` function.
-- Our audio graph should never become out of sync with our view!


noteView : Note -> Html Msg
noteView note =
    div [ class <| noteCSS note.triggered, class "flex-1 mx-2 text-center" ]
        [ text note.key ]



--


viewSquare : Model -> Float -> Int -> Html Msg
viewSquare model midi beat =
    button
        [ onClick (ToggleNote midi beat)
        , class
            (noteCSS
                (getVal model
                    midi
                    beat
                )
            )
        , style "border" "1px solid black"
        , style "padding" "20px"
        ]
        --[ text (Debug.toString (beat + 1)) ]
        []


viewTrack : Model -> Float -> Html Msg
viewTrack model midi =
    div [ style "padding-bottom" "10px" ]
        (List.map
            (\x -> viewSquare model midi x)
            (List.range
                0
                (model.len - 1)
            )
        )


viewMeter : Model -> Int -> Html Msg
viewMeter model beat =
    div
        [ style "padding" "20px"
        , class
            (noteCSS
                (if beat == model.len then
                    model.beat == 0

                 else
                    model.beat == beat
                )
            )
        ]
        [ text (Debug.toString beat) ]


viewScales : Model -> Html Msg
viewScales model =
    div
        [ class "mx-2" ]
        [ Html.select
            [ onInput ChangeScale ]
            (List.map
                (\name ->
                    Html.option
                        [ H.value name
                        , H.selected
                            (name
                                == model.scale.name
                            )
                        ]
                        [ text
                            name
                        ]
                )
                (Dict.keys scales)
            )
        ]


viewTypes : Model -> String -> Html Msg
viewTypes model version =
    div
        [ class "mx-2" ]
        [ Html.select
            [ onInput
                (if version == "piano" then
                    ChangePianoType

                 else
                    ChangeSeqType
                )
            ]
            (List.map
                (\name ->
                    Html.option
                        [ H.value name
                        , H.selected
                            (String.toLower name
                                == (if version == "piano" then
                                        model.pianoType

                                    else
                                        model.seqType
                                   )
                            )
                        ]
                        [ text
                            name
                        ]
                )
                [ "Triangle", "Square", "Sine", "Sawtooth" ]
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ class "m-10" ]
        [ h1 [ class "text-3xl my-4" ]
            [ text "Elm Synth Sequencer by Jonah Fleishhacker" ]
        , Html.a
            [ H.href
                "https://drive.google.com/drive/folders/1n7L9bH3w3hNBkmn4vi6Vu5Ez_Zy6_4lZ?usp=sharing"
            , style "color" "blue"
            , style "font-style" "italic"
            , style "text-decoration" "underline"
            , class "my-4"
            ]
            [ text "Share songs here" ]
        , Html.hr [ style "padding-top" "10px" ] []
        , Html.h2 [] [ text "Scale: " ]
        , viewScales model
        , Html.h2 [] [ text "Piano Wave Type: " ]
        , viewTypes model "piano"
        , div [ class "flex", style "padding" "10px" ] <|
            List.map noteView model.piano
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Sequencer\n        Settings" ] ]
        , div [ class "p-2 my-6" ]
            [ button [ onClick PauseToggle, class "bg-indigo-500 text-white\n\n            font-bold py-2 px-4 mr-4 rounded" ]
                [ if model.go then
                    text "Pause"

                  else
                    text "Play"
                ]
            , button [ onClick Clear, class "bg-indigo-500 text-white\n            font-bold\n            py-2 px-4 mr-4 rounded" ]
                [ text "Clear" ]
            , button [ onClick Reset, class "bg-indigo-500 text-white font-bold\n            py-2 px-4 mr-4 rounded" ] [ text "Reset Sliders" ]
            , button [ onClick Download, class "bg-indigo-900 text-white\n\n\n            font-bold rounded mr-4", style "padding" "20px" ] [ text "Download" ]
            , button [ onClick OpenFileClicked, class "bg-indigo-900\n            text-white\n            font-bold rounded", style "padding" "20px" ] [ text "Upload" ]
            ]
        , div []
            [ Html.input
                [ H.type_ "range"
                , class
                    "bg-indigo-500 mr-4"
                , H.min "-15.0"
                , H.max "15.0"
                , H.step "0.5"
                , H.value
                    (Debug.toString
                        model.transpose
                    )
                , onInput Transpose
                ]
                []
            , text ("Transpose Value: " ++ Debug.toString model.transpose)
            ]
        , div []
            [ Html.input
                [ H.type_ "range"
                , class "bg-indigo-500 mr-4"

                -- , style "padding" "10px"
                , H.min "50"
                , H.max "500"
                , H.value
                    (Debug.toString
                        model.bpm
                    )
                , onInput Bpm
                ]
                []
            , text ("BPM: " ++ Debug.toString model.bpm)
            ]
        , div [ style "padding-bottom" "10px" ]
            [ Html.input
                [ H.type_ "range"
                , class "bg-indigo-500 mr-4"

                --, style "padding" "10px"
                , H.min "2"
                , H.max "20"
                , H.value
                    (Debug.toString
                        model.len
                    )
                , onInput ChangeBeats
                ]
                []
            , text ("Beats: " ++ Debug.toString model.len)
            ]
        , Html.h2 [] [ text "Sequencer Wave type: " ]
        , viewTypes model "seq"
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Sequencer\n        Notes" ] ]
        , div [ class "flex", style "padding" "10px" ] <|
            List.map noteView model.notes
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Beat\n        Indicator" ] ]
        , div
            [ style "padding" "10px"
            , class "flex-container"
            , style "display"
                "flex"
            , style "justify-content" "center"
            ]
          <|
            List.map
                (\x -> viewMeter model x)
                (List.range 1 model.len)
        , Html.hr [ style "padding" "10px" ] []
        , div
            [ style "padding" "10px"
            , class "flex-container"
            , style "flex-direction" "column"
            , style "display"
                "flex"
            , style "justify-content" "left"
            ]
            (List.map
                (\note -> viewTrack model (Tuple.second note))
                model.scale.notes
            )
        ]



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

        --, Cont.every 3 model.time NoOp NextStep model.context
        ]
