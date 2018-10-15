module Gallery.Image exposing
    ( slide
    , Size(..)
    , Url
    )

{-|


# Image slide

@docs slide


## Definitions

@docs Size
@docs Url

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| How the image is displayed within it's container, same as CSS background-size
-}
type Size
    = Cover
    | Contain


{-| Image url
-}
type alias Url =
    String


{-| Create an image slide that either fits or covers the gallery container
-}
slide : Url -> Size -> Html msg
slide url size =
    div
        [ style "background-image" ("url(" ++ url ++ ")")
        , style "background-size" (toBackgroundSize size)
        , class "elm-gallery-image"
        ]
        []


toBackgroundSize : Size -> String
toBackgroundSize size =
    case size of
        Cover ->
            "cover"

        Contain ->
            "contain"
