port module Main exposing (..)

--
--
--

import Array as A
import Browser
import Browser.Events
import Html exposing (Attribute, Html, a, button, code, div, h1, main_, p, pre, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Time
import WebAudio
import WebAudio.Context as Cont
import WebAudio.Program
import WebAudio.Property as Prop



-- Send the JSON encoded audio graph to javascript


port updateAudio : Json.Encode.Value -> Cmd msg



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
    , tracks : A.Array Track
    , time : Float
    , beat : Int
    , len : Int
    , context : Cont.AudioContext
    }



--


initialModel : List Note
initialModel =
    [ { key = "a", midi = 60, triggered = False }
    , { key = "s", midi = 62, triggered = False }
    , { key = "d", midi = 64, triggered = False }
    , { key = "f", midi = 65, triggered = False }
    , { key = "g", midi = 67, triggered = False }
    , { key = "h", midi = 69, triggered = False }
    , { key = "j", midi = 71, triggered = False }
    , { key = "k", midi = 72, triggered = False }
    , { key = "l", midi = 74, triggered = False }
    ]


initialTrackHelp : A.Array Track
initialTrackHelp =
    A.fromList
        (List.map
            (\note ->
                { key = note.key
                , midi = note.midi
                , beats = A.repeat 9 False
                }
            )
            initialModel
        )


trackSet : Model -> String -> Int -> Model
trackSet model key beat =
    { model
        | tracks =
            A.map
                (\track ->
                    if track.key == key then
                        Track track.key track.midi (A.set beat True track.beats)

                    else
                        Track track.key track.midi track.beats
                )
                model.tracks
    }


trackSetList : Model -> List ( String, Int ) -> Model
trackSetList model lst =
    case lst of
        [] ->
            model

        ( key, beat ) :: tl ->
            trackSetList (trackSet model key beat) tl



--


s =
    [ ( "a", 2 ), ( "c", 2 ), ( "a", 4 ), ( "d", 3 ), ( "g", 5 ), ( "j", 8 ) ]


initialTrack : Model -> Model
initialTrack model =
    trackSetList model s


init co =
    ( { notes = initialModel
      , tracks = initialTrackHelp
      , time = 0
      , context = co
      , beat = 0
      , len = 9
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
    | TransposeUp
    | TransposeDown
    | NextStep Time.Posix



--


noteOn : String -> Model -> Model
noteOn key model =
    { model
        | notes =
            List.map
                (\note ->
                    if note.key == key then
                        { note | triggered = True }

                    else
                        note
                )
                model.notes
    }


notePulse : Float -> Model -> Model
notePulse time model =
    { model
        | notes =
            List.map
                (\note ->
                    if note.triggered == True then
                        { note | triggered = False }

                    else
                        { note | triggered = True }
                )
                model.notes
        , time = Cont.currentTime model.context
    }


updateTrack : Model -> Model
updateTrack model =
    { model
        | beat =
            if model.beat == model.len then
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
        | notes =
            List.map
                (\note ->
                    if note.key == key then
                        { note | triggered = False }

                    else
                        note
                )
                model.notes
    }


transposeUp : Model -> Model
transposeUp model =
    { model
        | notes = List.map (\note -> { note | midi = note.midi + 1 }) model.notes
    }


transposeDown : Model -> Model
transposeDown model =
    { model
        | notes = List.map (\note -> { note | midi = note.midi - 1 }) model.notes
    }



--


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

        TransposeUp ->
            ( transposeUp model
            , Cmd.none
            )

        TransposeDown ->
            ( transposeDown model
            , Cmd.none
            )

        NextStep newTime ->
            ( updateTrack (initialTrack model)
            , Cmd.none
            )



-- AUDIO ----------------------------------------------------------------------
-- Super simple utility function that takes a MIDI note number like 60 and
-- converts it to the corresponding frequency in Hertz. We use Float for the
-- MIDI number to allow for detuning, and we assume A4 is MIDI note number
-- 69.


mtof : Float -> Float
mtof midi =
    440 * 2 ^ ((midi - 69) / 12)



-- This takes a Note (as defined above) and converts that to a synth voice.


voice : Note -> WebAudio.Node
voice note =
    WebAudio.oscillator [ Prop.frequency <| mtof note.midi ]
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
    List.map voice model.notes



-- VIEW -----------------------------------------------------------------------
-- Use this to toggle the main styling on a note based on wheter it is currently
-- active or note. Basically just changes the background and font colour.


noteCSS : Bool -> String
noteCSS active =
    if active then
        "bg-indigo-500 text-white font-bold py-2 px-4 rounded"

    else
        "bg-indigo-100 text-black font-bold py-2 px-4 rounded"



-- This takes a Note (as defined above) and converts that to some  Notice
-- how we use the data for both the `voice` function and this `noteView` function.
-- Our audio graph should never become out of sync with our view!


noteView : Note -> Html Msg
noteView note =
    div [ class <| noteCSS note.triggered, class "flex-1 mx-2 text-center" ]
        [ text note.key ]


audioView : List Note -> List (Html Msg)
audioView =
    List.map
        (\note ->
            voice note
                |> WebAudio.encode
                |> Json.Encode.encode 2
                |> (\json ->
                        pre
                            [ class "text-xs"
                            , class <|
                                if note.triggered then
                                    "text-gray-800"

                                else
                                    "text-gray-500"
                            ]
                            [ code [ class "my-2" ]
                                [ text json ]
                            ]
                   )
        )



--


view : Model -> Html Msg
view model =
    main_ [ class "m-10" ]
        [ h1 [ class "text-3xl my-10" ]
            [ text "elm-web-audio" ]
        , p [ class "p-2 my-6" ]
            [ text """This package provides an elm/html-like API for declaring Web 
          Audio graphs in Elm. The intention being that these `virtual` audio 
          graphs are then sent via a port to be constructed by a javascript. 
          There is a reference implementation of this found in the repository 
          that you are free to copy until I or someone else releases a package 
          formally.""" ]
        , p [ class "p-2 my-6" ]
            [ text """This site primarily serves as a demonstration that the library
          actually works. If you'd like some more in depth documentation on the
          Elm library itself you should check out the package """
            , a
                [ href "https://package.elm-lang.org/packages/pd-andy/elm-web-audio/1.0.0/"
                , class "text-indigo-500 hover:text-indigo-700"
                ]
                [ text "here." ]
            ]
        , p [ class "p-2 my-6" ]
            [ text (Debug.toString model.time ++ """ A Web Audio context typically starts in a suspended state. 
          If you can't hear any sound, click anywhere to resume the audio 
          context.""") ]
        , div [ class "p-2 my-6" ]
            [ button [ onClick TransposeUp, class "bg-indigo-500 text-white font-bold py-2 px-4 mr-4 rounded" ]
                [ text "Transpose up" ]
            , button [ onClick TransposeDown, class "bg-indigo-500 text-white font-bold py-2 px-4 rounded" ]
                [ text "Transpose down" ]
            ]
        , div [ class "flex" ] <|
            List.map noteView model.notes
        , div [ class "p-2 my-10" ]
            [ text """Below is the json send via ports to javascript. Active notes 
          are highlighted.""" ]
        , div [ class "bg-gray-200 p-2 my-10 rounded h-64 overflow-scroll" ] <|
            audioView model.notes
        ]



-- SUBSCRIPTIONS --------------------------------------------------------------
--


noteOnDecoder : List Note -> Json.Decode.Decoder Msg
noteOnDecoder notes =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case List.any (\note -> note.key == key) notes of
                    True ->
                        Json.Decode.succeed (NoteOn key)

                    False ->
                        Json.Decode.fail ""
            )



--


noteOffDecoder : List Note -> Json.Decode.Decoder Msg
noteOffDecoder notes =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case List.any (\note -> note.key == key) notes of
                    True ->
                        Json.Decode.succeed (NoteOff key)

                    False ->
                        Json.Decode.fail ""
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| noteOnDecoder model.notes
        , Browser.Events.onKeyUp <| noteOffDecoder model.notes
        , Time.every 300 NextStep

        --, Cont.every 3 model.time NoOp NextStep model.context
        ]
