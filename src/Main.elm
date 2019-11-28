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
    , modalVisibility : Modal.Visibility
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
    , question11 : Int
    , question21 : Int
    , question31 : Int
    , question41 : Int
    , question51 : Int
    , currentQuestion1: Int
    , enableNextQuestion1: Bool
    , enablePreviousQuestion1: Bool
    , showHintEnabled1 : Bool
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
            urlUpdate url { navKey = key, navState = navState, page = Home, modalVisibility= Modal.hidden, showHintEnabled=False, question1 = -1, question2 = -1 , question3 = -1 , question4 = -1 ,question5 = -1, currentQuestion = 1, lastSubmittedAnswer =0, enableNextQuestion=False, enablePreviousQuestion=False, currentQuestion1 = 1, question11 = -1, question21 = -1 , question31 = -1 , question41 = -1 ,question51 = -1, enableNextQuestion1=False, enablePreviousQuestion1=False, showHintEnabled1=False}
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
    | EnablePreviousQuestion Bool
    | LoadPreviousQuestion
    | LoadNextQuestion 
    | ShowHintForQuestion
    | Question11 Int
    | Question21 Int
    | Question31 Int
    | Question41 Int
    | Question51 Int
    | LoadPreviousQuestion1
    | LoadNextQuestion1
    | ShowHintForQuestion1

questionsAnswers model = [model.question1, model.question2, model.question3, model.question4, model.question5]
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

        EnableNextQuestion value ->
            ( { model | enableNextQuestion = value }
            , Cmd.none
            )
        EnablePreviousQuestion value ->
            ( { model | enablePreviousQuestion = value }
            , Cmd.none
            )
        LoadNextQuestion ->
            ( { model | showHintEnabled=False, enableNextQuestion = (fromJustInt (Array.get (model.currentQuestion) (Array.fromList (questionsAnswers model))) == 1), currentQuestion = model.currentQuestion+1, enablePreviousQuestion = (if model.currentQuestion > 0 then True else False) }
            , Cmd.none
            )
        LoadPreviousQuestion ->
            ( { model |currentQuestion = model.currentQuestion-1, showHintEnabled = False, enablePreviousQuestion= (if model.currentQuestion > 2 then True else False), enableNextQuestion = True }
            , Cmd.none
            )

        ShowHintForQuestion -> 
            ({ model | showHintEnabled = not (model.showHintEnabled) }
            , Cmd.none
            )

        LoadNextQuestion1 ->
            ( { model | showHintEnabled1=False, enableNextQuestion1 = (fromJustInt (Array.get (model.currentQuestion1) (Array.fromList (questionsAnswers1 model))) == 1), currentQuestion1 = (model.currentQuestion1 + 1), enablePreviousQuestion1 = (if model.currentQuestion1 > 0 then True else False) }
            , Cmd.none
            )
        LoadPreviousQuestion1 ->
            ( { model |currentQuestion1 = model.currentQuestion1-1, showHintEnabled1 = False, enablePreviousQuestion1= (if model.currentQuestion1 > 2 then True else False), enableNextQuestion1 = True }
            , Cmd.none
            )

        Question11 value ->
            ( { model | question11 = value, enableNextQuestion1 = (checkIfCorrectAnswer 1 value)}
            , Cmd.none
            )

        Question21 value ->
            ( { model | question21 = value, enableNextQuestion1 = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question31 value ->
            ( { model | question31 = value, enableNextQuestion1 = (checkIfCorrectAnswer 3 value) }
            , Cmd.none
            )

        Question41 value ->
            ( { model | question41 = value, enableNextQuestion1 = (checkIfCorrectAnswer 4 value) }
            , Cmd.none
            )

        Question51 value ->
            ( { model | question51 = value, enableNextQuestion1 = (checkIfCorrectAnswer 5 value) }
            , Cmd.none
            )    

        ShowHintForQuestion1 -> 
            ({ model | showHintEnabled1 = not (model.showHintEnabled1) }
            , Cmd.none
            )

checkIfCorrectAnswer quNumber value = 
    if value == 1 then 
        True 
    else 
        False


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

            PartialFractionDecomp -> 
                partialFractionDecompPage model

            NotFound ->
                pageNotFound


questionLabels = ["Question 1", "Question 2", "Question 3", "Question 4", "Question 5", "Congratulations!"]
questions = ["What values would you choose for u and dv?", "What is the value for v?", "What is the value of du?", "Given the general form of integration by parts, what is the substituted formula?", "What is the final answer?", "Good job on working through this complex integration problem. Here is another one that you can try out: f(x) = $ \\int (cos(7x+5) dx $. To learn more, click on the button below."]
questOneOptions = ["$u = x$ and $dv = \\sqrt{x+1}$", "$u = sin(x)$ and $dv = x^{2}$", "$u = x^{2} + 1$ and $dv = x^{2} + 1$"]
questTwoOptions = ["$\\frac{2}{3} (x+1)^{3/2}$", "$sin x$", "$\\sqrt{x+1} + 1$"]
questThreeOptions = ["dx", "$x^{2}$ dx", "$x^{10}$ dx"]
questFourOptions = ["$ x * \\frac{2}{3} (x+1)^{3/2} - \\int \\frac{2}{3} (x+1)^{3/2} dx$", "$ x^{2} * \\frac{4}{3} (x+1) - \\int \\frac{4}{3} dx$", "$x (x+1) - \\int x^{2} dx$"]
questFiveOptions = ["$ \\frac{2}{3} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$ + C", "$\\frac{3}{2} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$ + C", "$\\frac{2}{3} * x * (x+1)^{3/2} - \\frac{4}{15} (x+1)^{5/2}$"]
questionOptions = [questOneOptions, questTwoOptions, questThreeOptions, questFourOptions, questFiveOptions]

hints = ["Choose $u$ as the first of the following: Inverse Trignometric, Logarithmic, Algebraic, Trignometric, Exponential. If both $u$ and $dv$ are the same type, the order does not matter.", "Integrate $dv$ to obtain $v$. Integral formula for polynomials is : $\\int x^{n} = \\frac{x^{n+1}}{n+1}$", "Differentiate $u$ to get $du$. Derivative Formula for polynomials is : $\\frac{d u^{n}}{dx} = u^{n-1} * n $", "Substitute $v$ and $u$ into the formula. Formula: $\\int u * dv = u * v - \\int v * du $", "Integrate then simplify it to get the answer. Don't forget to add C"]

questionFeedback : Model -> Html Msg
questionFeedback model = 
    if (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) ==2  ||  
            (fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model)))) ==3 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

questionNotifications = [Question1, Question2, Question3, Question4, Question5]

isLastQuestion model = 
    if (model.currentQuestion == 6) then 
        True 
    else 
        False 

pageHome : Model -> List (Html Msg)
pageHome model =
    [   h1 [] [ text """Click on an Integration Technique you want to learn today.
        Learn the pattern and then try on your own!"""],
        br [] [],
        h2 [attribute "style" "color: red;text-align: center"] [Markdown.Elm.toHtml Markdown.Option.ExtendedMath "Impress your friends by learning integration techniques from our modules"],
        h4 [attribute "style" "position: fixed;left: 0;bottom: 0;width: 100%;color: black;text-align: center"] [text "Made with ♥ by Asalat, Razan, Hamid, and Prithvi"]
    ]

showHint model = 
    if model.showHintEnabled == True then 
        Alert.simpleWarning [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (model.currentQuestion-1) (Array.fromList(hints)))) ]
    else
        div [] []

checkIfSelected model number = 
    if fromJustInt (Array.get (model.currentQuestion-1) (Array.fromList (questionsAnswers model))) == number then 
        Button.primary 
    else 
        Button.outlinePrimary

fromJustMsg: Maybe (Int -> Msg) ->(Int -> Msg)
fromJustMsg x = case x of
    Just y -> y
    Nothing -> Question1

fromJustInt x = case x of 
    Just y -> y
    Nothing -> 0

fromJust : Maybe String -> String
fromJust x = case x of
    Just y -> y
    Nothing -> ""

fromJustList x = case x of
    Just [y] -> [y]
    Just [] -> ["h","h"]
    Nothing -> []
    Just (xs :: ys :: zz) -> [xs]++[ys]++zz


renderLinkButton: Int -> Html Msg
renderLinkButton value = 
    if value == 5 then
        Button.linkButton [ Button.primary, Button.attrs [ href "#USubstitution"] ] [ text "Go to USubstitution" ]
    else 
        div [] [] 

renderOption1: Model -> Html Msg
renderOption1 model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else
        Button.button [(checkIfSelected model 1), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 1) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (0) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions))))))) ]
        

renderOption2: Model -> Html Msg
renderOption2 model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected model 2), Button.attrs [ onClick ((fromJustMsg(Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 2 ) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (1) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]

renderOption3: Model -> Html Msg
renderOption3 model = 
    if (model.currentQuestion-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected model 3), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion-1) (Array.fromList (questionNotifications)))) 3) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (2) (Array.fromList(fromJustList(Array.get (model.currentQuestion-1) (Array.fromList(questionOptions)))))))]


integrationByPartsPage : Model -> List (Html Msg)
integrationByPartsPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , h2 [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath "$f(x) = \\int x \\sqrt{x+1}$" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questionLabels)))) ]
                |> Card.block []
                    [ Block.text [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust (Array.get (model.currentQuestion-1) (Array.fromList questions))) ]
                    , Block.custom <| (renderLinkButton (model.currentQuestion-1))
                    , Block.custom <| (renderOption1 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption2 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption3 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (questionFeedback model) 
                    , Block.text [] [ text "" ]
                    , Block.custom <| (showHint model)]
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

uSubstitutionPage : Model -> List (Html Msg)
uSubstitutionPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , h2 [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath "$f(x) =  \\int cos(7x+5) dx $" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion1-1) (Array.fromList questionLabels)))) ]
                |> Card.block []
                    [ Block.text [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust (Array.get (model.currentQuestion1-1) (Array.fromList questions1))) ]
                    , Block.custom <| (renderOption11 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption21 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption31 model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (questionFeedback1 model) 
                    , Block.text [] [ text "" ]
                    , Block.custom <| (showHint1 model)]
                |> Card.block [ Block.align Text.alignXsCenter ] 
                    [
                     Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enablePreviousQuestion1)), Button.attrs [Spacing.mr5, onClick (LoadPreviousQuestion1)] ] [text "Previous question"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (isLastQuestion1 model), Button.attrs [Spacing.ml5,Spacing.mr5, onClick (ShowHintForQuestion1)] ] [text "Hint"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enableNextQuestion1)), Button.attrs [ Spacing.ml5, onClick (LoadNextQuestion1)] ] [text "Next question"]
                    ]
                |> Card.view
            ]
        ]
    , br [] []
    ]

questionsAnswers1 model = [model.question11, model.question21, model.question31, model.question41, model.question51]
questions1 = ["What values can you choose for $u$","what is the value of dx","Upon substituting $u$ and $du$, what is the integral that you get","What is the integrated value in terms of $u$","What is the integrated value in terms of $x$"]
questOneOptions1 = ["u $= 7x + 5$","u $= cos(7x+5)$","u $= 7x$"]
questTwoOptions1 = ["$\\frac{1}{7} du $","$sin(x)$","$\\frac{1}{5} du$"]
questThreeOptions1 = ["$\\int cos(u) * \\frac{1}{7}$ du","$\\int sin(u) * \\frac{1}{7}$ du","$\\int cos(u) * \\frac{1}{5}$ du"]
questFourOptions1 = ["1/7 sin(u) + C","1/7 cos(u) + C","1/5 tan(u) + C"]
questFiveOptions1 =[" $\\frac{1}{7} sin(7*x+5) + C$","$\\frac{1}{7} cos(7*x+5) + C$","$\\frac{1}{5} tan(7*x+5) + C$"]
questionOptions1 = [questOneOptions1, questTwoOptions1, questThreeOptions1, questFourOptions1, questFiveOptions1]
hints1 = ["Hint 1","Hint 2","Hint 3","Hint 4","Hint 5"]
questionNotifications1 = [Question11, Question21, Question31, Question41, Question51]

questionFeedback1 : Model -> Html Msg
questionFeedback1 model = 
    if (fromJustInt (Array.get (model.currentQuestion1-1) (Array.fromList (questionsAnswers1 model)))) == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if (fromJustInt (Array.get (model.currentQuestion1-1) (Array.fromList (questionsAnswers1 model)))) ==2  ||  
            (fromJustInt (Array.get (model.currentQuestion1-1) (Array.fromList (questionsAnswers1 model)))) ==3 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

isLastQuestion1 model = 
    if (model.currentQuestion1 == 6) then 
        True 
    else 
        False 

showHint1 model = 
    if model.showHintEnabled1 == True then 
        Alert.simpleWarning [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (model.currentQuestion1-1) (Array.fromList(hints1)))) ]
    else
        div [] []

checkIfSelected1 model number = 
    if fromJustInt (Array.get (model.currentQuestion1-1) (Array.fromList (questionsAnswers1 model))) == number then 
        Button.primary 
    else 
        Button.outlinePrimary

renderOption11: Model -> Html Msg
renderOption11 model = 
    if (model.currentQuestion1-1) == 5 then
        div [] []
    else
        Button.button [(checkIfSelected1 model 1), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion1-1) (Array.fromList (questionNotifications1)))) 1) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (0) (Array.fromList(fromJustList(Array.get (model.currentQuestion1-1) (Array.fromList(questionOptions1))))))) ]
        

renderOption21: Model -> Html Msg
renderOption21 model = 
    if (model.currentQuestion1-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected1 model 2), Button.attrs [ onClick ((fromJustMsg(Array.get (model.currentQuestion1-1) (Array.fromList (questionNotifications1)))) 2 ) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (1) (Array.fromList(fromJustList(Array.get (model.currentQuestion1-1) (Array.fromList(questionOptions1)))))))]

renderOption31: Model -> Html Msg
renderOption31 model = 
    if (model.currentQuestion1-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected1 model 3), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion1-1) (Array.fromList (questionNotifications1)))) 3) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (2) (Array.fromList(fromJustList(Array.get (model.currentQuestion1-1) (Array.fromList(questionOptions1)))))))]




-- ##### Step 1 : Copy paste the below comment, and find "&" and replace it with a number #####
{--

-- ##### Paste this in "type alias Model{}"

    , question1& : Int
    , question2& : Int
    , question3& : Int
    , question4& : Int
    , question5& : Int
    , currentQuestion&: Int
    , enableNextQuestion&: Bool
    , enablePreviousQuestion&: Bool
    , showHintEnabled& : Bool

-- ##### Paste this in "init flags url key, (model, urlCmd)"

currentQuestion& = 1, question1& = -1, question2& = -1 , question3& = -1 , question4& = -1 ,question5& = -1, enableNextQuestion&=False, enablePreviousQuestion&=False, showHintEnabled&=False

-- ##### Paste this in "type Msg"

    | Question1& Int
    | Question2& Int
    | Question3& Int
    | Question4& Int
    | Question5& Int
    | LoadPreviousQuestion&
    | LoadNextQuestion&
    | ShowHintForQuestion&


-- ##### Paste this in "update msg model"

        LoadNextQuestion& ->
            ( { model | showHintEnabled&=False, enableNextQuestion& = (fromJustInt (Array.get (model.currentQuestion&) (Array.fromList (questionsAnswers& model))) == 1), currentQuestion& = (model.currentQuestion& + 1), enablePreviousQuestion& = (if model.currentQuestion& > 0 then True else False) }
            , Cmd.none
            )
        LoadPreviousQuestion& ->
            ( { model |currentQuestion& = model.currentQuestion&-1, showHintEnabled& = False, enablePreviousQuestion&= (if model.currentQuestion& > 2 then True else False), enableNextQuestion& = True }
            , Cmd.none
            )

        Question1& value ->
            ( { model | question1& = value, enableNextQuestion& = (checkIfCorrectAnswer 1 value)}
            , Cmd.none
            )

        Question2& value ->
            ( { model | question2& = value, enableNextQuestion& = (checkIfCorrectAnswer 2 value) }
            , Cmd.none
            )

        Question3& value ->
            ( { model | question3& = value, enableNextQuestion& = (checkIfCorrectAnswer 3 value) }
            , Cmd.none
            )

        Question4& value ->
            ( { model | question4& = value, enableNextQuestion& = (checkIfCorrectAnswer 4 value) }
            , Cmd.none
            )

        Question5& value ->
            ( { model | question5& = value, enableNextQuestion& = (checkIfCorrectAnswer 5 value) }
            , Cmd.none
            )    

        ShowHintForQuestion& -> 
            ({ model | showHintEnabled& = not (model.showHintEnabled&) }
            , Cmd.none
            )


    questionsAnswers& model = [model.question1&, model.question2&, model.question3&, model.question4&, model.question5&]


--}

{-- 
-- ##### Step 2 : To create more questions,replace the "&" like before this and copy paste this code #####

questionsAnswers& model = [model.question1&, model.question2&, model.question3&, model.question4&, model.question5&]
questions& = ["q1","q2","q3","q4","q5"]
questOneOptions& = ["op1","op1","op1"]
questTwoOptions& = ["op2","op2","op2"]
questThreeOptions& = ["op3","op3","op3"]
questFourOptions& = ["op4","op4","op4"]
questFiveOptions& =["op5","op5","op5"]
questionOptions& = [questOneOptions&, questTwoOptions&, questThreeOptions&, questFourOptions&, questFiveOptions&]
hints& = ["h1","h2","h3","h4","h5"]
questionNotifications& = [Question1&, Question2&, Question3&, Question4&, Question5&]

questionFeedback& : Model -> Html Msg
questionFeedback& model = 
    if (fromJustInt (Array.get (model.currentQuestion&-1) (Array.fromList (questionsAnswers& model)))) == 1 then
        Alert.simpleSuccess [] [ text "Correct!" ]
    else if (fromJustInt (Array.get (model.currentQuestion&-1) (Array.fromList (questionsAnswers& model)))) ==2  ||  
            (fromJustInt (Array.get (model.currentQuestion&-1) (Array.fromList (questionsAnswers& model)))) ==3 then
        Alert.simpleDanger  [] [ text "Incorrect"]
    else 
        div [] []

isLastQuestion& model = 
    if (model.currentQuestion& == 6) then 
        True 
    else 
        False 

showHint& model = 
    if model.showHintEnabled& == True then 
        Alert.simpleWarning [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (model.currentQuestion&-1) (Array.fromList(hints&)))) ]
    else
        div [] []

checkIfSelected& model number = 
    if fromJustInt (Array.get (model.currentQuestion&-1) (Array.fromList (questionsAnswers& model))) == number then 
        Button.primary 
    else 
        Button.outlinePrimary

renderOption1&: Model -> Html Msg
renderOption1& model = 
    if (model.currentQuestion&-1) == 5 then
        div [] []
    else
        Button.button [(checkIfSelected& model 1), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion&-1) (Array.fromList (questionNotifications&)))) 1) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (0) (Array.fromList(fromJustList(Array.get (model.currentQuestion&-1) (Array.fromList(questionOptions&))))))) ]
        

renderOption2&: Model -> Html Msg
renderOption2& model = 
    if (model.currentQuestion&-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected& model 2), Button.attrs [ onClick ((fromJustMsg(Array.get (model.currentQuestion&-1) (Array.fromList (questionNotifications&)))) 2 ) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (1) (Array.fromList(fromJustList(Array.get (model.currentQuestion&-1) (Array.fromList(questionOptions&)))))))]

renderOption3&: Model -> Html Msg
renderOption3& model = 
    if (model.currentQuestion&-1) == 5 then
        div [] []
    else 
        Button.button [(checkIfSelected& model 3), Button.attrs [ onClick ((fromJustMsg (Array.get (model.currentQuestion&-1) (Array.fromList (questionNotifications&)))) 3) ] ] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust(Array.get (2) (Array.fromList(fromJustList(Array.get (model.currentQuestion&-1) (Array.fromList(questionOptions&)))))))]

uSubstitutionPage : Model -> List (Html Msg)
uSubstitutionPage model =
    [ h1 [] [ text "Let's work through an example..." ]
    , h2 [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath "$f(x) =  \\int cos(7x+5) dx $" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ (text (fromJust (Array.get (model.currentQuestion&-1) (Array.fromList questionLabels)))) ]
                |> Card.block []
                    [ Block.text [] [ Markdown.Elm.toHtml Markdown.Option.ExtendedMath (fromJust (Array.get (model.currentQuestion&-1) (Array.fromList questions&))) ]
                    , Block.custom <| (renderOption1& model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption2& model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (renderOption3& model)
                    , Block.text [] [ text "" ]
                    , Block.custom <| (questionFeedback& model) 
                    , Block.text [] [ text "" ]
                    , Block.custom <| (showHint& model)]
                |> Card.block [ Block.align Text.alignXsCenter ] 
                    [
                     Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enablePreviousQuestion&)), Button.attrs [Spacing.mr5, onClick (LoadPreviousQuestion&)] ] [text "Previous question"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (isLastQuestion& model), Button.attrs [Spacing.ml5,Spacing.mr5, onClick (ShowHintForQuestion&)] ] [text "Hint"]
                     , Block.custom <| Button.button [Button.outlinePrimary, Button.disabled (not(model.enableNextQuestion&)), Button.attrs [ Spacing.ml5, onClick (LoadNextQuestion&)] ] [text "Next question"]
                    ]
                |> Card.view
            ]
        ]
    , br [] []
    ]

--}



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


{--
pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]
--}

partialFractionDecompPage : Model -> List (Html Msg)
partialFractionDecompPage model =
    [ h1 [] [ text "partialFractionDecomp" ]
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