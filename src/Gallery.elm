module Gallery
    exposing
        ( Config
        , Controls(..)
        , Length
        , Msg
        , SlideCount
        , State
        , config
        , current
        , em
        , index
        , init
        , next
        , pct
        , previous
        , px
        , rem
        , update
        , vh
        , view
        , vw
        )

{-|


# View

@docs view


# Configuration

@docs Config
@docs config


## Dimensions

@docs Length
@docs px
@docs pct
@docs rem
@docs em
@docs vw
@docs vh


# State

@docs init
@docs update


## Definitions

@docs State
@docs Msg
@docs SlideCount


# Controls

@docs Controls


# Navigation

@docs index
@docs next
@docs previous
@docs current

-}

import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Json.Decode as Decode
import Time exposing (Time)


-- STATE


{-| Tracks index, mouse and touch movements
-}
type State
    = State Index (Maybe Drag) SlideCount


type Drag
    = Drag Position Position


{-| Total number of slides
-}
type alias SlideCount =
    Int


type alias Index =
    Int


type alias Position =
    { x : Int
    , y : Int
    }


{-| Initialize a gallery state

    { gallery = Gallery.init (List.length someSlides) }

-}
init : SlideCount -> State
init slideCount =
    State 0 Nothing slideCount



-- CONFIG


{-| -}
type Config
    = Config
        { id : String
        , transition : Time
        , width : Length
        , height : Length
        }


{-|

     Gallery.config
        { id = "image-gallery"
        , transition = Time.second * 0.5
        , width = Gallery.px 800
        , height = Gallery.px 400
        }

-}
config :
    { id : String
    , transition : Time
    , width : Length
    , height : Length
    }
    -> Config
config { id, transition, width, height } =
    Config
        { id = id
        , transition = transition
        , width = width
        , height = height
        }



-- UPDATE


{-| A message type for the gallery to update.
-}
type Msg
    = Next
    | Previous
    | DragStart Position
    | DragAt Position
    | DragEnd


{-|

    type Msg
        = GalleryMsg Gallery.Msg

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            GalleryMsg msg ->
                { model | gallery = Gallery.update msg model.gallery }

-}
update : Msg -> State -> State
update msg ((State index drag slideCount) as state) =
    case msg of
        Next ->
            next state

        Previous ->
            previous state

        DragStart position ->
            State index (Just (Drag position position)) slideCount

        DragAt position ->
            let
                updateDrag current (Drag start _) =
                    Drag start current
            in
            State index (Maybe.map (updateDrag position) drag) slideCount

        DragEnd ->
            case drag of
                Just (Drag start current) ->
                    if start.x - current.x > 100 then
                        next state
                    else if start.x - current.x < -100 then
                        previous state
                    else
                        State index Nothing slideCount

                Nothing ->
                    state



-- CONTROL


{-| Go to next slide

    { model | gallery = Gallery.next model.gallery }

-}
next : State -> State
next (State index _ slideCount) =
    State (clamp 0 (slideCount - 1) (index + 1)) Nothing slideCount


{-| Go to previous slide

    { model | gallery = Gallery.previous model.gallery }

-}
previous : State -> State
previous (State index _ slideCount) =
    State (clamp 0 (slideCount - 1) (index - 1)) Nothing slideCount


{-| Go to slide at index

    { model | gallery = Gallery.index 10 model.gallery }

-}
index : Int -> State -> State
index index (State _ _ slideCount) =
    State (clamp 0 (slideCount - 1) index) Nothing slideCount


{-| Get current index
-}
current : State -> Int
current (State index _ _) =
    index



-- VIEW


{-|

    Gallery.view config model.gallery [] slides

-}
view : Config -> State -> List Controls -> List ( String, Html Msg ) -> Html Msg
view ((Config config_) as config) ((State _ drag _) as state) navigation slides =
    div [ id config_.id ] <|
        [ Lazy.lazy3 viewSlides state config slides
        , Lazy.lazy2 styleSheet config drag
        ]
            ++ List.map (viewControls state) navigation



-- SLIDES


viewSlides : State -> Config -> List ( String, Html Msg ) -> Html Msg
viewSlides ((State index _ _) as state) config slides =
    Keyed.ul (viewSlidesAttributes state) <|
        List.indexedMap (viewSlide index) slides


viewSlidesAttributes : State -> List (Attribute Msg)
viewSlidesAttributes ((State index drag slideCount) as state) =
    events drag
        ++ [ classList
                [ ( "elm-gallery-transition", drag == Nothing ) ]
           , style
                [ ( "transform", toTranslate state )
                , ( "width", toString (100 * slideCount) ++ "%" )
                ]
           , id "elm-gallery-focusable"
           , tabindex 0
           ]


toTranslate : State -> String
toTranslate (State index drag slideCount) =
    case drag of
        Nothing ->
            toCssTranslate slideCount index 0 0

        Just (Drag start current) ->
            toCssTranslate slideCount index (current.x - start.x) 0



-- ITEMS


viewSlide : Index -> Index -> ( String, Html Msg ) -> ( String, Html Msg )
viewSlide currentIndex slideIndex ( id, html ) =
    ( id
    , div
        [ classList
            [ ( "elm-gallery-itemcontainer", True )
            , ( "elm-gallery-current", currentIndex == slideIndex )
            ]
        ]
        [ html ]
    )



-- NAVIGATION


{-| -}
type Controls
    = Arrows


viewControls : State -> Controls -> Html Msg
viewControls state navigation =
    case navigation of
        Arrows ->
            viewControlsArrows state


viewControlsArrows : State -> Html Msg
viewControlsArrows (State index _ slideCount) =
    div
        []
        [ button
            [ onClick Next
            , class "elm-gallery-next"
            , title "next"
            , disabled (index == slideCount - 1)
            ]
            []
        , button
            [ onClick Previous
            , class "elm-gallery-previous"
            , title "previous"
            , disabled (index == 0)
            ]
            []
        ]



-- EVENTS


events : Maybe Drag -> List (Attribute Msg)
events drag =
    moveEvent drag
        ++ [ on "mousedown" (Decode.map DragStart decodePosition)
           , on "touchstart" (Decode.map DragStart decodePosition)
           , on "keydown" (Decode.andThen keycodeToMsg decodeKeyboard)
           ]


moveEvent : Maybe a -> List (Attribute Msg)
moveEvent drag =
    case drag of
        Just _ ->
            [ onPreventDefault "mousemove" (Decode.map DragAt decodePosition)
            , onPreventDefault "touchmove" (Decode.map DragAt decodePosition)
            , on "mouseup" (Decode.succeed DragEnd)
            , on "mouseleave" (Decode.succeed DragEnd)
            , on "touchend" (Decode.succeed DragEnd)
            , on "touchcancel" (Decode.succeed DragEnd)
            ]

        Nothing ->
            []


decodeKeyboard : Decode.Decoder Int
decodeKeyboard =
    Decode.field "keyCode" Decode.int


decodePosition : Decode.Decoder Position
decodePosition =
    let
        decoder =
            Decode.map2 Position
                (Decode.field "pageX" (Decode.map floor Decode.float))
                (Decode.field "pageY" (Decode.map floor Decode.float))
    in
    Decode.oneOf [ decoder, Decode.at [ "touches", "0" ] decoder ]


keycodeToMsg : Int -> Decode.Decoder Msg
keycodeToMsg keyCode =
    case keyCode of
        37 ->
            Decode.succeed Previous

        39 ->
            Decode.succeed Next

        _ ->
            Decode.fail "Unknown key"


onPreventDefault : String -> Decode.Decoder Msg -> Attribute Msg
onPreventDefault event =
    onWithOptions event
        { defaultOptions | preventDefault = True }



-- CSS


{-| -}
type Length
    = Px Float
    | Pct Float
    | Rem Float
    | Em Float
    | Vh Float
    | Vw Float
    | Unset


{-| -}
unset : Length
unset =
    Unset


{-| -}
px : Float -> Length
px x =
    Px x


{-| -}
pct : Float -> Length
pct x =
    Pct x


{-| -}
rem : Float -> Length
rem x =
    Rem x


{-| -}
em : Float -> Length
em x =
    Em x


{-| -}
vh : Float -> Length
vh x =
    Vh x


{-| -}
vw : Float -> Length
vw x =
    Vw x


lengthToString : Length -> String
lengthToString length =
    case length of
        Px x ->
            toString x ++ "px"

        Pct x ->
            toString x ++ "%"

        Rem x ->
            toString x ++ "rem"

        Em x ->
            toString x ++ "em"

        Vh x ->
            toString x ++ "vh"

        Vw x ->
            toString x ++ "vw"

        Unset ->
            ""


toCssColor : Color -> String
toCssColor color =
    Color.toRgb color
        |> (\{ red, green, blue, alpha } ->
                "rgba("
                    ++ toString red
                    ++ ","
                    ++ toString green
                    ++ ","
                    ++ toString blue
                    ++ ","
                    ++ toString alpha
                    ++ ")"
           )


toCssTranslate : SlideCount -> Int -> a -> b -> String
toCssTranslate slideCount index x y =
    "translate3d("
        ++ toString (negate <| toFloat index * (100 / toFloat slideCount))
        ++ "%, "
        ++ toString y
        ++ "px, 0) "
        ++ "translateX("
        ++ toString x
        ++ "px)"


toCssButtonColor : Maybe Color -> String
toCssButtonColor buttonColor =
    case buttonColor of
        Just color ->
            toCssColor color

        Nothing ->
            toCssColor Color.white


styleSheet : Config -> Maybe Drag -> Html msg
styleSheet (Config config) drag =
    node "style"
        []
        [ text <|
            """
            #"""
                ++ config.id
                ++ """ {
                position: relative;
                width: """
                ++ lengthToString config.width
                ++ """;
                height:
                    """
                ++ lengthToString config.height
                ++ """;
                overflow: hidden;
            }

            #"""
                ++ config.id
                ++ """ ul {
                position: absolute;
                display: flex;
                height: 100%;
                left: 0;
                top: 0;
                padding: 0;
                margin: 0;
                z-index: 1;
                outline: none;
            }

            #"""
                ++ config.id
                ++ """ ul:hover {
                cursor: move;
                cursor: grab;
                cursor: -moz-grab;
                cursor: -webkit-grab;
            }

            #"""
                ++ config.id
                ++ """ ul:active {
                cursor: grabbing;
                cursor: -moz-grabbing;
                cursor: -webkit-grabbing;
            }

            #"""
                ++ config.id
                ++ """ button {
                background-color: transparent;
                border-radius: 0;
                border: 0;
                cursor: pointer;
                z-index: 2;
                transform: translate3d(0, 0, 0);
            }

            #"""
                ++ config.id
                ++ """ button:hover {
                opacity: .85;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-previous,
            #"""
                ++ config.id
                ++ """ .elm-gallery-next {
                position: absolute;
                top: 24%;
                width: 4rem;
                height: 50%;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-next {
                right: 0rem;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-previous {
                left: 0rem;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-previous:disabled,
            #"""
                ++ config.id
                ++ """ .elm-gallery-next:disabled {
                opacity: 0.2;
                cursor: not-allowed;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-close:after,
            #"""
                ++ config.id
                ++ """ .elm-gallery-close:before,
            #"""
                ++ config.id
                ++ """ .elm-gallery-previous:after,
            #"""
                ++ config.id
                ++ """ .elm-gallery-previous:before,
            #"""
                ++ config.id
                ++ """ .elm-gallery-next:after,
            #"""
                ++ config.id
                ++ """ .elm-gallery-next:before {
                content: '';
                position: absolute;
                width: 2rem;
                height: 4px;
                background-color: white;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-next:before {
                top: calc(50% - 1rem);
                right: 1rem;
                transform: rotate(45deg);
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-next:after {
                top: calc(50% + .3rem);
                right: 1rem;
                transform: rotate(-45deg);
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-previous:before {
                top: calc(50% - 1rem);
                left: 1rem;
                transform: rotate(-45deg);
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-previous:after {
                top: calc(50% + .3rem);
                left: 1rem;
                transform: rotate(45deg);
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-itemcontainer {
                position: relative;
                width: 100%;
                height: 100%;
            }

            #"""
                ++ config.id
                ++ """ .elm-gallery-image {
                position: absolute;
                background-position: center center;
                background-repeat: no-repeat;
                width: 100%;
                height: 100%;
            }


            #"""
                ++ config.id
                ++ """ .elm-gallery-transition {
                transition: transform """
                ++ toString config.transition
                ++ """ms ease;
            }
            """
        ]
