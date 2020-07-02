module Model exposing (..)

import Array as A exposing (Array)
import Note exposing (..)
import Scales exposing (..)
import Track exposing (..)
import WebAudio.Context exposing (AudioContext)


type alias Model =
    { notes : List Note
    , piano : List Note
    , tracks : Array Track
    , beat : Int
    , len : Int
    , context : AudioContext
    , go : Bool
    , transpose : Float
    , bpm : Float
    , scale : Scale
    , seqType : String
    , pianoType : String
    , instructions : Bool
    , seqVolume : Float
    , pianoVolume : Float
    }


getVal : Model -> Float -> Int -> Bool
getVal model midi beat =
    getValIndex model (midiToIndex model midi) beat


midiToIndex : Model -> Float -> Int
midiToIndex model midi =
    let
        check i lst =
            case lst of
                [] ->
                    0

                h :: tl ->
                    if h.midi == midi then
                        i

                    else
                        check (i + 1) tl
    in
    check 0 (A.toList model.tracks)


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
