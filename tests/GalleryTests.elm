module GalleryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Gallery
import Test exposing (..)


state : Int -> Gallery.State
state index =
    Gallery.init 10
        |> Gallery.index index


suite : Test
suite =
    describe "The Gallery module"
        [ describe "GalleryGallery.current"
            [ fuzz int "Stay in list boundries" <|
                \randomInt ->
                    Gallery.index randomInt (state 0)
                        |> Gallery.current
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.atMost 10
                            ]
            ]
        , describe "Gallery.next"
            [ test "Select next slide" <|
                \_ ->
                    Expect.equal 1 <| Gallery.current <| Gallery.next (state 0)
            , test "Select next slide on last" <|
                \_ ->
                    Expect.equal 9 <| Gallery.current <| Gallery.next (state 9)
            ]
        , describe "Gallery.previous"
            [ test "Select previous slide" <|
                \_ ->
                    Expect.equal 0 <| Gallery.current <| Gallery.previous (state 1)
            , test "Select previous slide on first" <|
                \_ ->
                    Expect.equal 0 <| Gallery.current <| Gallery.previous (state 0)
            ]
        ]
