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
questions = ["What values would you choose for u and dv?", "What is the value for v?", "What is the value of du?", "Given the general form of integration by parts, what is the subsituted formula?", "What is the final answer?"]
questOneOptions = ["u = x and dv = sqrt(x+1)", "u = sin x and dv = x^2", "u = v = x^2 + 1"]
questTwoOptions = ["2/3 (x+1)^3/2", "sin x", "sqrt(x+1) + 1"]
questThreeOptions = ["dx", "x^2 dx", "x^10 dx"]
questFourOptions = ["x * 2/3(x+1)^3/2 - integral(2/3(x+1)^3/2) dx", "x^2 * 4/3(x+1) - integral(4/3) dx", "x * (x+1) - integral(x^2) dx"]
questFiveOptions = ["2/3 * x * (x+1)^3/2 - 4/15(x+1)^5/2) + C", "3/2 * x * (x+1)^3/2 - 4/15(x+1)^5/2) + C", "2/3 * x * (x+1)^3/2 - 4/15(x+1)^5/2)"]
questionOptions = [questOneOptions, questTwoOptions, questThreeOptions, questFourOptions, questFiveOptions]

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
            ( { model | question2 = value, enableNextQuestion = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question3 value ->
            ( { model | question3 = value, enableNextQuestion = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question4 value ->
            ( { model | question4 = value, enableNextQuestion = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question5 value ->
            ( { model | question5 = value, enableNextQuestion = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )
        EnableNextQuestion value ->
            ( { model | enableNextQuestion = value }
            , Cmd.none
            )
        LaodNextQuestion ->
            ( { model | enableNextQuestion = False, currentQuestion = model.currentQuestion+1, question1=-1, question2=-1, question3=-1, question4=-1, question5=-1 }
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

questionFeedback : Model -> Html Msg
questionFeedback model = 
    if model.question1 == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if model.question1 == 0 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []
     
questionNotifications = [Question1, Question2, Question3, Question4, Question5]
pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text """Click on an integration technique you want to learn today.
    	Learn the pattern and then try on your own!""" ]]

fromJustMsg: Maybe (Int -> Msg) ->(Int -> Msg)

fromJustMsg x = case x of
    Just y -> y
    Nothing -> Question1

fromJust : Maybe String -> String
fromJust x = case x of
    Just y -> y
    Nothing -> "No"

fromJustList x = case x of
    Just [y] -> [y]
    Just [] -> ["h","h"]
    Nothing -> ["y","y"]
    Just (xs :: ys :: zz) -> [xs]++[ys]++zz


integrationByPartsPage : Model -> List (Html Msg)
integrationByPartsPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , h2 [] [ text "f(x) = ∫ (x) √(x+1)"]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questionLabels)))) ]
                |> Card.block []
                    [ Block.text [] [ text (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questions))) ]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 1) ] ] [text (fromJust(Array.get (0) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]
                     , Block.text [] [ text "" ]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick ((fromJustMsg(Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 0 ) ] ] [text (fromJust(Array.get (1) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]
                     , Block.text [] [ text "" ]
                    , Block.custom <| Button.button [Button.outlinePrimary, Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 0) ] ] [text (fromJust(Array.get (2) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]
                     , Block.text [] [ text "" ]
                    , Block.custom <| questionFeedback model 
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