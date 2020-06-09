module Track exposing (..)

import Array exposing (Array, fromList, get, map, repeat, set)
import Scales exposing (..)


type alias Track =
    { key : String
    , midi : Float
    , beats : Array Bool
    }


trackInit : Scale -> Int -> Array Track
trackInit scale beats =
    fromList
        (List.map
            (\note ->
                { key = Tuple.first note
                , midi = Tuple.second note
                , beats = repeat beats False
                }
            )
            scale.notes
        )


trackToggle : Array Track -> Float -> Int -> Array Track
trackToggle tracks midi beat =
    map
        (\track ->
            if track.midi == midi then
                case get beat track.beats of
                    Just bool ->
                        Track track.key
                            track.midi
                            (set beat
                                (not bool)
                                track.beats
                            )

                    Nothing ->
                        Track track.key track.midi track.beats

            else
                Track track.key track.midi track.beats
        )
        tracks


trackSetList : Array Track -> List ( Float, Int ) -> Array Track
trackSetList tracks lst =
    case lst of
        [] ->
            tracks

        ( midi, beat ) :: tl ->
            trackSetList (trackToggle tracks midi beat) tl
