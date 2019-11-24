module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Alert as Alert
import Bootstrap.Form.Radio as Radio
import Array



type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , currentQuestion: Int
    , lastSubmittedAnswer: Int
   
    , question1 : Int
    , question2 : Int
    , question3 : Int
    , question4 : Int
    , question5 : Int
    , enableNextQuestion: Bool
    }

type Page
    = Home
    | IntegrationByParts
    | USubstitution
    | PartialFractionDecomp
    | NotFound


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Home, modalVisibility= Modal.hidden, question1 = -1, question2 = -1 , question3 = -1 , question4 = -1 ,question5 = -1, currentQuestion = 1, lastSubmittedAnswer =0, enableNextQuestion=False  }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )




type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal Int
    | UpdateLastSubmittedAnswer Int
    | UpdateQuestion Int
   
    | Question1 Int
    | Question2 Int
    | Question3 Int
    | Question4 Int
    | Question5 Int
    | EnableNextQuestion Bool
    | LaodNextQuestion 


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg

questionLabels = ["Question 1", "Question 2", "Question 3", "Question 4", "Question 5"]
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                 Browser.External href ->
                     ( model, Navigation.load href )


        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal qNum ->
            ( { model | modalVisibility = Modal.shown}
            , Cmd.none
            )

        UpdateLastSubmittedAnswer answer -> ({model | lastSubmittedAnswer = answer}
            , Cmd.none)
        Question1 value ->
            ( { model | question1 = value, enableNextQuestion = (checkIfCorrectAnswer 1 value)}
            , Cmd.none
            )

        UpdateQuestion qNum -> ({model | currentQuestion = qNum}
            , Cmd.none)
   
        Question2 value ->
            ( { model | question2 = value }
            , Cmd.none
            )

        Question3 value ->
            ( { model | question3 = value }
            , Cmd.none
            )

        Question4 value ->
            ( { model | question4 = value }
            , Cmd.none
            )

        Question5 value ->
            ( { model | question5 = value }
            , Cmd.none
            )
        EnableNextQuestion value ->
            ( { model | enableNextQuestion = value }
            , Cmd.none
            )
        LaodNextQuestion ->
            ( { model | enableNextQuestion = False, currentQuestion = model.currentQuestion+1 }
            , Cmd.none
            )




checkIfCorrectAnswer quNumber value = if value == 1 then True else False
urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map IntegrationByParts (s "IntegrationByParts")
        , UrlParser.map USubstitution (s "USubstitution")
        , UrlParser.map PartialFractionDecomp (s "PartialFractionDecomp")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome"
    , body =
        [ div []
            [ menu model
            , mainContent model
            , modal model
            ]
        ]
    }



menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Welcome!" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#IntegrationByParts" ] [ text "Integration By Parts" ]
            , Navbar.itemLink [ href "#USubstitution" ] [ text "U Subsitution" ]
              , Navbar.itemLink [ href "#PartialFractionDecomp" ] [ text "Partial Fraction Decomposition" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            IntegrationByParts ->
                integrationByPartsPage model

            USubstitution ->
                uSubstitutionPage model

            PartialFractionDecomp -> pageNotFound
            NotFound ->
                pageNotFound

questionFeedback1 : Model -> Html Msg
questionFeedback1 model = 
    if model.question1 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question1 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []
get n xs = List.head (List.drop n xs)
questionFeedback2 : Model -> Html Msg
questionFeedback2 model = 
    if model.question2 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question2 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

questionFeedback3 : Model -> Html Msg
questionFeedback3 model = 
    if model.question3 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question3 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

questionFeedback4 : Model -> Html Msg
questionFeedback4 model = 
    if model.question4 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question4 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

questionFeedback5 : Model -> Html Msg
questionFeedback5 model = 
    if model.question5 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question5 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []        

pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Click on an integration technique you want to learn today." ]]
fromJust : Maybe String -> String
fromJust x = case x of
    Just y -> y
    Nothing -> "NotFound"

integrationByPartsPage : Model -> List (Html Msg)
integrationByPartsPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questionLabels)))) ]
                |> Card.block []
                    [ Block.text [] [ text "Add some question here" ]
                    ,
                    Block.text [] [ node "font" [ attribute "size" "6" ] [text "∫"], text " (x * x+1)"]

                    , Block.text [] [ text "a) option1" ]
                    , Block.text [] [ text "b) option2" ]
                    , Block.text [] [ text "c) option3" ]
                    , Block.text [] [ text "d) option4" ]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick (Question1 1 ) ] ] [text "A"]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick (Question1 0 ) ] ] [text "B"]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick (Question1 0 ) ] ] [text "C"]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick (Question1 0 ) ] ] [text "D"]
                    , Block.custom <| questionFeedback1 model 
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enableNextQuestion)), Button.attrs [onClick (LaodNextQuestion)] ] [text "Next question"]
                    ]
                |> Card.view
            ]
        ]
    , br [] []
    
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick (ShowModal 1) ]
        ]
        [ text "Click me" ]
    ]



pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]

uSubstitutionPage : Model -> List (Html Msg)
uSubstitutionPage model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]

pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]



modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "You got that one wrong. Click next to try again." ]

           
        |> Modal.view model.modalVisibility