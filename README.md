# Elm-gallery

A simple content and/or image gallery with touch and keyboard navigation.


[example live](https://rl-king.github.io/elm-gallery-example/) |
[example code](https://github.com/rl-king/elm-gallery/tree/master/example)

## Example code
```elm
type alias Model =
    { gallery : Gallery.State }


init : Model
init =
    { gallery = Gallery.init (List.length someSlides) }


type Msg
    = GalleryMsg Gallery.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        GalleryMsg msg ->
            { model | gallery = Gallery.update msg model.gallery }


view : Model -> Html Msg
view model =
    Html.map GalleryMsg <|
        Gallery.view config model.gallery [ Gallery.Arrows ] someSlides


config : Gallery.Config
config =
    Gallery.config
        { id = "image-gallery"
        , transition = 500
        , width = Gallery.vw 60
        , height = Gallery.px 400
        }
```

## Images by
[esa](http://www.esa.int/spaceinimages/Images)
