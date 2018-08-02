module Example exposing (..)

import Gallery exposing (..)
import Gallery.Image as Image
import Html exposing (..)
import Html.Attributes exposing (..)
import Time


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { textGallery : Gallery.State
    , imageGallery : Gallery.State
    }


init : Model
init =
    { textGallery = Gallery.init (List.length textSlides)
    , imageGallery = Gallery.init (List.length imageSlides)
    }


type Msg
    = TextGalleryMsg Gallery.Msg
    | ImageGalleryMsg Gallery.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextGalleryMsg msg ->
            { model | textGallery = Gallery.update msg model.textGallery }

        ImageGalleryMsg msg ->
            { model | imageGallery = Gallery.update msg model.imageGallery }


view : Model -> Html Msg
view model =
    main_ []
        [ styling
        , Html.map ImageGalleryMsg <|
            Gallery.view imageConfig model.imageGallery [ Gallery.Arrows ] imageSlides
        , Html.map TextGalleryMsg <|
            Gallery.view textConfig model.textGallery [] textSlides
        ]


textSlides : List ( String, Html msg )
textSlides =
    List.map (\x -> ( x, textSlide x )) texts


imageSlides : List ( String, Html msg )
imageSlides =
    List.map (\x -> ( x, Image.slide x Image.Cover )) images


textSlide : String -> Html msg
textSlide slide =
    article [] [ h3 [] [ text "Title" ], p [] [ text slide ] ]


imageConfig : Gallery.Config
imageConfig =
    Gallery.config
        { id = "image-gallery"
        , transition = Time.second * 0.5
        , width = Gallery.vw 60
        , height = Gallery.px 400
        }


textConfig : Gallery.Config
textConfig =
    Gallery.config
        { id = "text-gallery"
        , transition = Time.second * 0.5
        , width = Gallery.px 600
        , height = Gallery.px 400
        }


images : List String
images =
    [ "images/Ambrym_South_Pacific_Ocean.jpg"
    , "images/Irrawaddy_Delta_Myanmar.jpg"
    , "images/Northwest_England.jpg"
    , "images/Snowbound_Italy.jpg"
    , "images/Uyuni_salt_flat_Bolivia.jpg"
    ]


texts : List String
texts =
    [ """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec et quam nec eros pellentesque ultrices at et mauris. Sed sed ultricies lacus. Vestibulum porta elit et odio bibendum tempor. Nullam scelerisque quam felis, vitae pulvinar tortor scelerisque at. Mauris efficitur sagittis elit, pretium dapibus justo volutpat ac. Phasellus maximus lorem sit amet felis vestibulum accumsan. Mauris porta commodo massa placerat facilisis. Nunc et metus felis. Nulla scelerisque pretium elementum. Mauris pharetra eleifend erat et fermentum. Nulla eget sem consectetur, posuere felis sagittis, dictum metus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nunc feugiat et lorem feugiat gravida. Morbi elementum, eros at imperdiet eleifend, enim leo vestibulum nisi, et convallis lectus leo eu ipsum. Nam faucibus est sit amet accumsan luctus."""
    , """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec et quam nec eros pellentesque ultrices at et mauris. Sed sed ultricies lacus. Vestibulum porta elit et odio bibendum tempor. Nullam scelerisque quam felis, vitae pulvinar tortor scelerisque at. Mauris efficitur sagittis elit, pretium dapibus justo volutpat ac. Phasellus maximus lorem sit amet felis vestibulum accumsan. Mauris porta commodo massa placerat facilisis. Nunc et metus felis. Nulla scelerisque pretium elementum. Mauris pharetra eleifend erat et fermentum. Nulla eget sem consectetur, posuere felis sagittis, dictum metus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nunc feugiat et lorem feugiat gravida. Morbi elementum, eros at imperdiet eleifend, enim leo vestibulum nisi, et convallis lectus leo eu ipsum. Nam faucibus est sit amet accumsan luctus."""
    , """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec et quam nec eros pellentesque ultrices at et mauris. Sed sed ultricies lacus. Vestibulum porta elit et odio bibendum tempor. Nullam scelerisque quam felis, vitae pulvinar tortor scelerisque at. Mauris efficitur sagittis elit, pretium dapibus justo volutpat ac. Phasellus maximus lorem sit amet felis vestibulum accumsan. Mauris porta commodo massa placerat facilisis. Nunc et metus felis. Nulla scelerisque pretium elementum. Mauris pharetra eleifend erat et fermentum. Nulla eget sem consectetur, posuere felis sagittis, dictum metus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nunc feugiat et lorem feugiat gravida. Morbi elementum, eros at imperdiet eleifend, enim leo vestibulum nisi, et convallis lectus leo eu ipsum. Nam faucibus est sit amet accumsan luctus."""
    ]


styling : Html msg
styling =
    node "style"
        []
        [ text
            """
                main {
                    font-family: Helvetica, arial, sans-serif;
                }

                a {
                    color: white;
                }

                #image-gallery {
                    margin: 5rem auto;
                    background-color: #eee;
                }

                #text-gallery {
                    margin: 5rem auto;
                    background-color: #eee;
                }

                article {
                    padding: 2rem;
                }

                h4 {
                    color: grey;
                    margin: 1rem 0 0;
                    font-weight: 500;
                }
            """
        ]
