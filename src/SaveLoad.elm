module SaveLoad exposing (..)

import Array as A
import Dict
import Json.Decode as Decode exposing (decodeString, field)
import Json.Encode as Encode
import Model exposing (..)
import Scales exposing (..)
import Track exposing (..)


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
