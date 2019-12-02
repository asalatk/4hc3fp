{--

A common problem that we had noticed when we interviewed the first year university students, was learning about integration.
Integration is a complex process and not all functions can be integrated the same way. There are many ways to integrate 
functions such as integration by parts, partial fraction, and u substitution. Deciding on how to integrate a function can 
be the hardest part of the process, even harder than integration itself. There are a number of techniques for identifying
which process to apply. Students would find that if they had a tool to help them understand the techniques they need to use, 
they are more likely to skip using Youtube and Khan Academy. Students taking 1ZA3 in first year engineering need a tool to help 
them decide the correct integration methods to use for each question they face. 

--}

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
import Bootstrap.Utilities.Spacing as Spacing
import Markdown.Elm exposing (..)
import Markdown.Option exposing (..)
import Array
import Bootstrap.Text as Text

type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , currentQuestion: Int
    , lastSubmittedAnswer: Int
    , question1 : Int
    , question2 : Int
    , question3 : Int
    , question4 : Int
    , question5 : Int
    , enableNextQuestion: Bool
    , enablePreviousQuestion: Bool
    , showHintEnabled : Bool
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
            urlUpdate url { 
                navKey = key,
                navState = navState,
                page = Home,
                question1 = -1,
                question2 = -1,
                question3 = -1,
                question4 = -1,
                question5 = -1,
                currentQuestion = 1,
                lastSubmittedAnswer = 0,
                enableNextQuestion = False,
                enablePreviousQuestion = False,
                showHintEnabled = False
            }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )

type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | Question1 Int
    | Question2 Int
    | Question3 Int
    | Question4 Int
    | Question5 Int
    | UpdateLastSubmittedAnswer Int
    | UpdateQuestion Int
    | EnableNextQuestion Bool
    | EnablePreviousQuestion Bool
    | LoadPreviousQuestion
    | LoadNextQuestion 
    | ShowHintForQuestion


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg

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

        Question1 value ->
            ( { model | question1 = value, enableNextQuestion = (checkIfCorrectAnswer 1 value) }
            , Cmd.none
            )

        Question2 value ->
            ( { model | question2 = value, enableNextQuestion = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question3 value ->
            ( { model | question3 = value, enableNextQuestion = (checkIfCorrectAnswer 3 value) }
            , Cmd.none
            )

        Question4 value ->
            ( { model | question4 = value, enableNextQuestion = (checkIfCorrectAnswer 4 value) }
            , Cmd.none
            )

        Question5 value ->
            ( { model | question5 = value, enableNextQuestion = (checkIfCorrectAnswer 5 value) }
            , Cmd.none
            )

        UpdateLastSubmittedAnswer answer -> 
            ( { model | lastSubmittedAnswer = answer}
            , Cmd.none
            )

        UpdateQuestion qNum -> 
            ( { model | currentQuestion = qNum}
            , Cmd.none
            )
   
        EnableNextQuestion value ->
            ( { model | enableNextQuestion = value }
            , Cmd.none
            )

        EnablePreviousQuestion value ->
            ( { model | enablePreviousQuestion = value }
            , Cmd.none
            )

        LoadNextQuestion ->
            ( { model | showHintEnabled = False, 
                        enableNextQuestion = (fromJustInt (Array.get (model.currentQuestion) (Array.fromList (questionsAnswers model))) == 1),
                        currentQuestion = model.currentQuestion + 1,
                        enablePreviousQuestion = (if model.currentQuestion > 0 then True else False)
              }
            , Cmd.none
            )

        LoadPreviousQuestion ->
            ( { model | showHintEnabled = False, 
                        enableNextQuestion = True, 
                        currentQuestion = model.currentQuestion - 1,
                        enablePreviousQuestion = (if model.currentQuestion > 2 then True else False)
              }
            , Cmd.none
            )

        ShowHintForQuestion -> 
            ({ model | showHintEnabled = not (model.showHintEnabled) }
            , Cmd.none
            )

view : Model -> Browser.Document Msg
view model =
    { title = "Welcome"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }

-- Helper Function that allows for pages to be rendered based on URL Route
urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )

-- Helper Function that decodes the URL Routes
decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser

-- Function that Parses URL Routes. 
routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map IntegrationByParts (s "IntegrationByParts")
        , UrlParser.map USubstitution (s "USubstitution")
        , UrlParser.map PartialFractionDecomp (s "PartialFractionDecomp")
        ]

-- Navigation Bar that is rendered across all Pages/Modules.
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

-- Helper Function that renders the different Pages/Modules based on URL Routes
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

            PartialFractionDecomp -> 
                partialFractionDecompPage model

            NotFound ->
                pageNotFound

questionsAnswers model = [model.question1, model.question2, model.question3, model.question4, model.question5]

questionNotifications = [Question1, Question2, Question3, Question4, Question5]

-- List Containing Questions Labels/Numbers in the Integration by Parts Module
questionLabels = ["Question 1", "Question 2", "Question 3", "Question 4", "Question 5", "Congratulations!"]

-- List Containing the Simple Questions in the Integration by Parts Module
questions = ["What values would you choose for u and dv?", "What is the value for v?", "What is the value of du?", "Given the general form of integration by parts, what is the substituted formula?", "What is the final answer?", "Good job on working through this complex integration problem. Here is another one that you can try out: f(x) = $ \\int (cos(7x+5) dx $. To learn more, click on the button below."]

-- List Containing Options for Question 1 in the Integration By Parts Module
question1_Options = ["$u = x$ and $dv = \\sqrt{x+1}$", "$u = sin(x)$ and $dv = x^{2}$", "$u = x^{2} + 1$ and $dv = x^{2} + 1$"]

-- List Containing Options for Question 2 in the Integration By Parts Module
question2_Options = ["$\\frac{2}{3} (x+1)^{3/2}$", "$sin x$", "$\\sqrt{x+1} + 1$"]

-- List Containing Options for Question 3 in the Integration By Parts Module
question3_Options = ["dx", "$x^{2}$ dx", "$x^{10}$ dx"]

-- List Containing Options for Question 4 in the Integration By Parts Module
question4_Options = ["$ x * \\frac{2}{3} (x+1)^{3/2} - \\int \\frac{2}{3} (x+1)^{3/2} dx$", "$ x^{2} * \\frac{4}{3} (x+1) - \\int \\frac{4}{3} dx$", "$x (x+1) - \\int x^{2} dx$"]

-- List Containing Options for Question 5 in the Integration By Parts Module
question5_Options = ["$ \\frac{2}{3} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$ + C", "$\\frac{3}{2} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$ + C", "$\\frac{2}{3} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$"]

-- List Containing all the QuestionOptions. This List can be seen as a 2D List
questionOptions = [question1_Options, question2_Options, question3_Options, question4_Options, question5_Options]

-- List Containing Hints for Integration By Parts Module
hints = ["Choose $u$ as the first of the following: Inverse Trignometric, Logarithmic, Algebraic, Trignometric, Exponential. If both $u$ and $dv$ are the same type, the order does not matter.", "Integrate $dv$ to obtain $v$. Integral formula for polynomials is : $\\int x^{n} = \\frac{x^{n+1}}{n+1}$", "Differentiate $u$ to get $du$. Derivative Formula for polynomials is : $\\frac{d u^{n}}{dx} = u^{n-1} * n $", "Substitute $v$ and $u$ into the formula. Formula: $\\int u * dv = u * v - \\int v * du $", "Integrate then simplify it to get the answer. Don't forget to add C"]

-- Helper Function that Converts "Just" into a value
fromJustMsg : Maybe (Int -> Msg) -> (Int -> Msg)
fromJustMsg x = 
    case x of
        Just y -> y
        Nothing -> Question1

-- Helper Function that Converts "Just" into a value
fromJustInt : Maybe Int -> Int
fromJustInt x = 
    case x of 
        Just y -> y
        Nothing -> 0

-- Helper Function that Converts "Just" into a value
fromJust : Maybe String -> String
fromJust x = 
    case x of
        Just y -> y
        Nothing -> ""

-- Helper Function that Converts "Just" into a value
fromJustList x = 
    case x of
        Just [y] -> [y]
        Just [] -> ["h","h"]
        Nothing -> []
        Just (xs :: ys :: zz) -> [xs]++[ys]++zz

-- Function that checks the Correct answer for the given question
checkIfCorrectAnswer quNumber value = 
    if value == 1 then 
        True
    else
        False

-- Function that generates an Alert which provides feedback to the user based on the chosen answer
questionFeedbackAlert : Model -> Html Msg
questionFeedbackAlert model = 
    if (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) ==2  ||  
            (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) ==3 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

-- Function that checks if the last question has been reached
isLastQuestion model = 
    if (model.currentQuestion == 6) then 
        True 
    else 
        False 

-- Function that renders an alert which provides hints for the given question
showHintAlert model = 
    if model.showHintEnabled == True then 
        Alert.simpleWarning [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (model.currentQuestion-1) (Array.fromList(hints)))) ]
    else
        div [] []

-- Function that checks if the Button has been selected 
checkIfSelected model number = 
    if fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model))) == number then 
        Button.primary 
    else 
        Button.outlinePrimary

-- Function that renders a Button that links to the U Substitution Page/Module
renderUSubstitutionLinkButton: Int -> Html Msg
renderUSubstitutionLinkButton value = 
    if value == 5 then
        Button.linkButton [ Button.primary, Button.attrs [ href "#USubstitution"] ] [ text "Go to USubstitution" ]
    else 
        div [] [] 

-- Function that renders the Option 1 Button in the MCQ Section
renderOption1Button: Model -> Html Msg
renderOption1Button model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else
        Button.button [(checkIfSelected model 1), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 1) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (0) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions))))))) ]
        
-- Function that renders the Option 2 Button in the MCQ Section
renderOption2Button: Model -> Html Msg
renderOption2Button model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected model 2), Button.attrs [ onClick ((fromJustMsg(Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 2 ) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (1) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]

-- Function that renders the Option 3 Button in the MCQ Section
renderOption3Button: Model -> Html Msg
renderOption3Button model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected model 3), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 3) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (2) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]

-- Function that renders the Home / Landing Page
pageHome : Model -> List (Html Msg)
pageHome model =
    [   h1 [] 
        [ text """Click on an integration technique you want to learn today.
        Learn the pattern and then try on your own!""" 
        ],
        h5 [attribute "style" "position: fixed;left: 0;bottom: 0;width: 100%;color: black;text-align: center"] [text "Made with ♥ by Asalat, Razan, Hamid, and Prithvi"]
    ]

-- Function that renders the Integration By Parts Page/Module
integrationByPartsPage : Model -> List (Html Msg)
integrationByPartsPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , h2 [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath "$f(x) = \\int x \\sqrt{x+1}$" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questionLabels)))) ] --Renders the QuestionLabels
                |> Card.block []
                    [ Block.text [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questions))) ] --Renders the Simple Questions
                    , Block.custom <| (renderUSubstitutionLinkButton (model.currentQuestion-1))     --Renders the U Substitution Link Button
                    , Block.custom <| (renderOption1Button model)                                   --Renders the Option 1 Button
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption2Button model)                                   --Renders the Option 2 Button
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption3Button model)                                   --Renders the Option 3 Button
                    , Block.text [] [ text "" ]
                    , Block.custom <| (questionFeedbackAlert model)                                 --Renders the Alert for Question Feedback
                    , Block.text [] [ text "" ]
                    , Block.custom <| (showHintAlert model)]                                        --Renders the Alert for Hints
                |> Card.block [ Block.align Text.alignXsCenter ] 
                    [
                     Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enablePreviousQuestion)), Button.attrs [Spacing.mr5, onClick (LoadPreviousQuestion)] ] [text "Previous question"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (isLastQuestion model), Button.attrs [Spacing.ml5,Spacing.mr5, onClick (ShowHintForQuestion)] ] [text "Hint"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enableNextQuestion)), Button.attrs [ Spacing.ml5, onClick (LoadNextQuestion)] ] [text "Next question"]
                    ]
                |> Card.view
            ]
        ]
    , br [] []
    ]

-- Function that renders the U Substitution Page/Module
uSubstitutionPage : Model -> List (Html Msg)
uSubstitutionPage model =
    [ h1 [] [ text "uSubstitution" ]
    ]

-- Function that renders the Partial Fraction Decomposition Page/Module
partialFractionDecompPage : Model -> List (Html Msg)
partialFractionDecompPage model =
    [ h1 [] [ text "partialFractionDecomp" ]
    ]

-- Function that renders Page Not Found Error
pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "404 Page Not found" ]
    ]
