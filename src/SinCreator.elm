{-  

Name: Asalat Kamal and Razan Abujarad
Group 29
Wave Creator Assignment 

User:  Our imagined user is a student/child 
Activity: Our user is involved in simulating waves and applying waves to shapes in terms of transformations
(for animations) and colours. 


Emotion: The user will feel excitement because they have a shape and want to apply animations/transformations to it.

Tasks:  A user will simulate a wave by changing parameters, select a shape, apply transformation, apply colours, copy code  

Typical Interaction: 
1) A user will simulate a wave by adjusting the amplitude, frequency and phase parameters of the sin wave. They can do this
by clicking the triangles to increase or decrease the values.
2) A user can select a shape to apply this wave to.
3) The user can apply a transformation to this shape; the transformation will follow the sine wave created
4) The user will apply a colour to the shape; the value of the rgb can be computed as a result of the sine function or its equivalent cosine 
function
5) The user will see the resulting code of their work and can copy it onto their own elm projects.

Principle 1:  
Our first Norman principles is signifiers. 
A signifier can be anything used to indicate what affordances things have.
Signifiers guided our design in two ways, which are:

1) We signified to our users the purpose and what they can do at each step (affordances) with titles like Pick a Transform, Pick a colour,
much like the other tabs in the ShapeCreator website do. 

2) We signified constraints by graying out buttons that could not apply any changes to the wave. For example, in the wave creator
(prior to our changes) the max amplitude is 40 but once you
got to 40 when clicking the up arrow, nothing signifies that this is the max. What we did instead is that we turned the button to gray
to signify that 40 is the max. We did this for the min amplitude, max frequency and min frequency as well. 

Principle 2: 

Our second design principle is conceptual model.
This principle heavily influenced our design.
We imagined our user to be someone who had already gone through the other tabs and created shapes and now, came to the wave creator
tab to apply an animation of varying colours to their elm creation (i.e. game).
The other tabs have the conceptual model of an editor and we wanted to keep that model to this tab. The original code did not have any
conceptual model of any sort and that is why it was so hard to understand what was going on.
With our changes, a user can clearly see its another editor where they apply transformations to the large central shape. Because the
steps are right after each other, like a list, it makes it very simple to see what to do next. 
The use of signifiers at each step also enforce this conceptual model.
We also added a reset button to allow the user to reset their function.
If a user changes the RGB values, the colours of the shapes change accordingly so feedback is instant. 
-}



module SinCreator exposing (..)

{-
Copyright 2017-2019 Christopher Kumar Anand,  Adele Olejarz, Chinmay Sheth, Yaminah Qureshi, Graeme Crawley and students of McMaster University.  Based on the Shape Creator by Levin Noronha.

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution, and cite the paper

   @article{d_Alves_2018,
   title={Using Elm to Introduce Algebraic Thinking to K-8 Students},
   volume={270},
   ISSN={2075-2180},
   url={http://dx.doi.org/10.4204/EPTCS.270.2},
   DOI={10.4204/eptcs.270.2},
   journal={Electronic Proceedings in Theoretical Computer Science},
   publisher={Open Publishing Association},
   author={d’ Alves, Curtis and Bouman, Tanya and Schankula, Christopher and Hogg, Jenell and Noronha, Levin and Horsman, Emily and Siddiqui, Rumsha and Anand, Christopher Kumar},
   year={2018},
   month={May},
   pages={18–36}
   }

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR AN, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)


init =
    { time = Nothing
    , currentTime = 0
    , notify = NotifyTap
    , uArg = 0
    , vArg = 0
    , editableArg = 0
    , uDilation = 1
    , vDilation = 1
    , editableDilation = 0
    , editableShift = 0
    , uScale = 5
    , vScale = 5
    , editableScale = 0
    , uShift = 0
    , uShiftScale = 1
    , u = 1
    , shape=circle
    , v = 1
    , rScale = 1
    , gScale = 1
    , bScale = 1
    , rFun = OneFun
    , bFun = UFun
    , gFun = VFun
    , sinGraph = []
    , cosGraph = []
    , vTransparency = 0.5
    , trigCycleU = Sin
    , trigCycleV = Cos
    , latestPointV = ( 0, 0, rgb 160 128 96 )
    , uTransform = ScaleU
    , moveX = ZeroFun
    , moveY = UFunZero
    , moveX1 = UFunZero
    , moveY1 = ZeroFun
    , transformFun = ZeroFun
    , uCosGraph = 0
    , uSinGraph = 0
    , editableYSinForTransforms = 0
    , r = 0
    , g = 0
    , b = 0
    , currentButton = None
    , buttonDownTime = 0
    , transformsRightArrowTransp = 0.25
    , transformsLeftArrowTransp = 0.25

    --, transformsNumTransp = 0.25
    , moveTextX = 0.25
    , moveTextY = 0.25
    , moveTextX1 = 0.25
    , moveTextY1 = 0.25
    , rTransp = 0.25
    , gTransp = 0.25
    , bTransp = 0.25
    , addAnotherFuncTransp = 0.25
    , uTextTransp = 0.5
    , vTextTransp = 0.5
    , maxAmplitude = 40
    , maxFrequency = 10
    , maxShift = 2 * Basics.pi
    , cosWaveLength = 200
    , sinWaveLength = 200
    }


type Msg m
    = Tick Float GetKeyState
    | TransM (m -> m)
    | Notif Notifications
    | R
    | G
    | B
    | UScalePlus
    | UDilationPlus
    | UShiftPlus
    | UScaleMinus
    | UDilationMinus
    | UShiftMinus
    | EditableScalePlus
    | EditableDilationPlus
    | EditableScaleMinus
    | EditableDilationMinus
    | VScalePlus
    | VScaleMinus
    | VDilationPlus
    | VDilationMinus
    | TrigCycleU
    | TrigCycleV
    | UTransforms
    | UTransformsReverse
    | ChangeToSquare
    | ChangeToTriangle
    | ChangeToCircle
    | ResetWave
      --| MoveX
      --| MoveY
      --| MoveX1
      --| MoveY1
      --| TransformsFunctionChange
    | RScalePlus
    | RScaleMinus
    | GScalePlus
    | GScaleMinus
    | BScalePlus
    | BScaleMinus
    | ButtonDown ButtonDir
    | MouseUp


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type FunType
    = OneFun
    | UFun
    | VFun


type Trig
    = Sin
    | Cos


type ZeroFunType
    = ZeroFun
    | UFunZero
    | NegUFun
    | VFunZero
    | NegVFun


type Transforms
    = ScaleU
    | MoveX
    | MoveY
    | MoveCircle
    | URotate
    | ScaleX
    | ScaleY
    | MakeTransparent
    | EditableXSin

type Shapes = Triangle | Square| Circle

type ButtonDir
    = AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | ShiftUp
    | ShiftDown
    | EditableAmplitudeUp
    | EditableAmplitudeDown
    | EditableFrequencyUp
    | EditableFrequencyDown
    | RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None
    | VUP
    | VDown


update msg model =
    case msg of
        Tick t _ ->
            let
                uArg =
                    model.uArg + model.uDilation * (t - (t - 0.05))

                vArg =
                    model.vArg + model.vDilation * (t - (t - 0.05))

                editableArg =
                    model.editableArg + model.editableDilation * (t - (t - 0.05))

                currentTime =
                    case model.time of
                        Nothing ->
                            0

                        Just ct ->
                            ct

                u =
                    model.uScale * evalTrig model.trigCycleU uArg

                v =
                    model.vScale * evalTrig model.trigCycleV vArg

                r =
                    clamp 0 255 (abs (model.rScale * eval model.rFun u v))

                g =
                    clamp 0 255 (abs (model.gScale * eval model.gFun u v))

                b =
                    clamp 0 255 (abs (model.bScale * eval model.bFun u v))

                uSinGraph =
                    model.uScale * sin uArg

                sinGraphPoint =
                    ( 0, uSinGraph, rgb r g b )

                cosGraphPoint =
                    ( uCosGraph, 0, rgb r g b )

                uCosGraph =
                    model.uScale * cos uArg

                editableYSinForTransforms =
                    model.editableScale * cos editableArg
            in
            { model
                | time = Just t
                , uArg = uArg
                , vArg = vArg
                , currentTime = currentTime
                , u = u
                , v = v
                , sinGraph =
                    List.take 2470
                        ([ sinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.sinWaveLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.sinGraph
                        )
                , cosGraph =
                     List.take 2470
                        ([ cosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -model.cosWaveLength then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.cosGraph
                        )
                , r = r
                , g = g
                , b = b
                , uCosGraph = uCosGraph
                , uSinGraph = uSinGraph

                --, editableYSinForTransforms = editableYSinForTransforms
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , uScale =
                    case model.currentButton of
                        AmplitudeUp ->
                            if model.uScale < model.maxAmplitude then
                                model.uScale + curveX model.buttonDownTime

                            else if model.uScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.uScale

                        AmplitudeDown ->
                            if model.uScale > -model.maxAmplitude then
                                model.uScale - curveX model.buttonDownTime

                            else if model.uScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.uScale

                        _ ->
                            model.uScale
                , uDilation =
                    case model.currentButton of
                        FrequencyUp ->
                            if model.uDilation < model.maxFrequency then
                                model.uDilation + curveX model.buttonDownTime

                            else if model.uDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.uDilation

                        FrequencyDown ->
                            if model.uDilation > -model.maxFrequency then
                                model.uDilation - curveX model.buttonDownTime

                            else if model.uDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.uDilation

                        _ ->
                            model.uDilation
                , uShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.uShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.uShift - curveX model.buttonDownTime

                        _ ->
                            model.uShift
                , editableScale =
                    case model.currentButton of
                        EditableAmplitudeUp ->
                            if model.editableScale < model.maxAmplitude then
                                model.editableScale + curveX model.buttonDownTime

                            else if model.editableScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.editableScale

                        EditableAmplitudeDown ->
                            if model.editableScale > -model.maxAmplitude then
                                model.editableScale - curveX model.buttonDownTime

                            else if model.editableScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.editableScale

                        _ ->
                            model.editableScale
                , editableDilation =
                    case model.currentButton of
                        EditableFrequencyUp ->
                            if model.editableDilation < model.maxFrequency then
                                model.editableDilation + curveX model.buttonDownTime

                            else if model.editableDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.editableDilation

                        EditableFrequencyDown ->
                            if model.editableDilation > -model.maxFrequency then
                                model.editableDilation - curveX model.buttonDownTime

                            else if model.editableDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.editableDilation

                        _ ->
                            model.editableDilation
                , editableShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.editableShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.editableShift - curveX model.buttonDownTime

                        _ ->
                            model.editableShift
                , rScale =
                    case model.currentButton of
                        RedUp ->
                            if model.rScale < 253 then
                                model.rScale + curveX model.buttonDownTime

                            else
                                model.rScale

                        RedDown ->
                            if model.rScale > 2 then
                                model.rScale - curveX model.buttonDownTime

                            else
                                model.rScale

                        _ ->
                            model.rScale
                , bScale =
                    case model.currentButton of
                        BlueUp ->
                            if model.bScale < 253 then
                                model.bScale + curveX model.buttonDownTime

                            else
                                model.bScale

                        BlueDown ->
                            if model.bScale > 2 then
                                model.bScale - curveX model.buttonDownTime

                            else
                                model.bScale

                        _ ->
                            model.bScale
                , gScale =
                    case model.currentButton of
                        GreenUp ->
                            if model.gScale < 252 then
                                model.gScale + curveX model.buttonDownTime

                            else
                                model.gScale

                        GreenDown ->
                            if model.gScale > 2 then
                                model.gScale - curveX model.buttonDownTime

                            else
                                model.gScale

                        _ ->
                            model.gScale
                , vScale =
                    case model.currentButton of
                        VUP ->
                            if model.vScale < 48 then
                                model.vScale + curveX model.buttonDownTime

                            else
                                model.vScale

                        VDown ->
                            if model.vScale > -48 then
                                model.vScale - curveX model.buttonDownTime

                            else
                                model.vScale

                        _ ->
                            model.vScale
            }

        TransM t ->
            t model

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

        R ->
            { model | rFun = cycleFun model.rFun }

        G ->
            { model | gFun = cycleFun model.gFun }

        B ->
            { model | bFun = cycleFun model.bFun }

        RScalePlus ->
            { model
                | rScale =
                    if model.rScale < 255 then
                        model.rScale + 1

                    else
                        model.rScale
            }

        RScaleMinus ->
            { model
                | rScale =
                    if model.rScale > 0 then
                        model.rScale - 1

                    else
                        model.rScale
            }

        GScalePlus ->
            { model
                | gScale =
                    if model.gScale < 255 then
                        model.gScale + 1

                    else
                        model.gScale
            }

        GScaleMinus ->
            { model
                | gScale =
                    if model.gScale > 0 then
                        model.gScale - 1

                    else
                        model.gScale
            }

        BScalePlus ->
            { model
                | bScale =
                    if model.bScale < 255 then
                        model.bScale + 1

                    else
                        model.bScale
            }

        BScaleMinus ->
            { model
                | bScale =
                    if model.bScale > 0 then
                        model.bScale - 1

                    else
                        model.bScale
            }

        UScalePlus ->
            { model
                | uScale =
                    if model.uScale < model.maxAmplitude then
                        model.uScale + 1

                    else
                        model.uScale
            }

        UScaleMinus ->
            { model
                | uScale =
                    if model.uScale > -model.maxAmplitude then
                        model.uScale - 1

                    else
                        model.uScale
            }

        UDilationPlus ->
            { model
                | uDilation =
                    if model.uDilation < model.maxFrequency then
                        model.uDilation + 1

                    else
                        model.uDilation
            }

        UDilationMinus ->
            { model
                | uDilation =
                    if model.uDilation > 0 then
                        model.uDilation - 1

                    else
                        model.uDilation
            }

        UShiftPlus ->
            { model
                | uArg =
                    model.uArg + model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift + model.uShiftScale
            }

        UShiftMinus ->
            { model
                | uArg =
                    model.uArg - model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift - model.uShiftScale
            }

        EditableScalePlus ->
            { model
                | editableScale =
                    if model.editableScale < model.maxAmplitude then
                        model.editableScale + 1

                    else
                        model.editableScale
            }

        EditableScaleMinus ->
            { model
                | editableScale =
                    if model.editableScale > -model.maxAmplitude then
                        model.editableScale - 1

                    else
                        model.editableScale
            }

        EditableDilationPlus ->
            { model
                | editableDilation =
                    if model.editableDilation < model.maxFrequency then
                        model.editableDilation + 1

                    else
                        model.editableDilation
            }

        EditableDilationMinus ->
            { model
                | editableDilation =
                    if model.editableDilation > -model.maxFrequency then
                        model.editableDilation - 1

                    else
                        model.editableDilation
            }

        VScalePlus ->
            { model
                | vScale =
                    if model.vScale < model.maxAmplitude then
                        model.vScale + 1

                    else
                        model.vScale
            }

        VScaleMinus ->
            { model
                | vScale =
                    if model.vScale > -model.maxAmplitude then
                        model.vScale - 1

                    else
                        model.vScale
            }

        VDilationPlus ->
            { model
                | vDilation =
                    if model.vDilation < model.maxFrequency then
                        model.vDilation + 1

                    else
                        model.vDilation
            }

        VDilationMinus ->
            { model | vDilation = model.vDilation - 1 }

        TrigCycleU ->
            { model | trigCycleU = cycleTrig model.trigCycleU }

        TrigCycleV ->
            { model | trigCycleV = cycleTrig model.trigCycleV }

        UTransforms ->
            { model | uTransform = cycleTransforms model.uTransform }


        ChangeToSquare -> 
            {model | shape  = square
            }
        
        ResetWave -> 
            {model | uScale = 5, uDilation =1, uShift =  0}
            
        ChangeToTriangle ->
            {model | shape =  triangle
            }

        ChangeToCircle ->
            {model | shape = circle
            }


        UTransformsReverse ->
            { model | uTransform = cycleTransformsReverse model.uTransform }

        {-
           MoveX ->
               { model | moveX = cycleFunZero model.moveX }

           MoveY ->
               { model | moveY = cycleFunZero model.moveY }

           MoveX1 ->
               { model | moveX1 = cycleFunZero model.moveX1 }

           MoveY1 ->
               { model | moveY1 = cycleFunZero model.moveY1 }
           TransformsFunctionChange ->
                  { model | transformFun = cycleFunZero model.transformFun }
        -}
        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads


eval f u v =
    case f of
        OneFun ->
            u

        UFun ->
            u

        VFun ->
            v


showFun f u v =
    case f of
        OneFun ->
            "u"

        UFun ->
            "u"

        VFun ->
            "v"


cycleFun f =
    case f of
        OneFun ->
            UFun

        UFun ->
            VFun

        VFun ->
            OneFun


cycleTrig f =
    case f of
        Sin ->
            Cos

        Cos ->
            Sin


textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"


evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u


cycleFunZero f =
    case f of
        ZeroFun ->
            UFunZero

        UFunZero ->
            NegUFun

        NegUFun ->
            VFunZero

        VFunZero ->
            NegVFun

        NegVFun ->
            ZeroFun


moveText mv =
    case mv of
        ZeroFun ->
            "u"

        UFunZero ->
            "u"

        NegUFun ->
            "-u"

        VFunZero ->
            "v"

        NegVFun ->
            "-v"



{-
   moveFun mv model =
       let
           u =
               model.u

           v =
               model.v
       in
       case mv of
           ZeroFun ->
               u

           UFunZero ->
               u

           NegUFun ->
               -u

           VFunZero ->
               v

           NegVFun ->
               -v
-}



cycleTransforms tr =
    case tr of
        ScaleU ->
            URotate

        URotate ->
            ScaleX

        ScaleX ->
            ScaleY

        ScaleY ->
            MakeTransparent

        MakeTransparent ->
            MoveX

        MoveX ->
            MoveY

        MoveY ->
            MoveCircle

        MoveCircle ->
            EditableXSin

        EditableXSin ->
            ScaleU

            


cycleTransformsReverse tr =
    case tr of
        URotate ->
            ScaleU

        ScaleX ->
            URotate

        ScaleY ->
            ScaleX

        MakeTransparent ->
            ScaleY

        MoveX ->
            MakeTransparent

        MoveY ->
            MoveX

        MoveCircle ->
            MoveY

        EditableXSin ->
            MoveCircle

        ScaleU ->
            EditableXSin


applyTransforms tr model =
    let
        u =
            model.u
    in
    case tr of
        ScaleU ->
            scale ((model.uSinGraph + model.uScale) / 10)

        MoveX ->
            move ( model.uCosGraph, 0 )

        MoveY ->
            move ( 0, model.uSinGraph )

        MoveCircle ->
            move ( model.uCosGraph, model.uSinGraph )

        URotate ->
            rotate (u / 10)

        ScaleX ->
            scaleX ((model.uCosGraph + model.uScale) / 10)

        ScaleY ->
            scaleY ((model.uSinGraph + model.uScale) / 10)

        MakeTransparent ->
            makeTransparent u

        EditableXSin ->
            move ( model.uCosGraph, 0 )


         


mapthis model tr =
    case tr of
        MoveX ->
            "Move X"

        MoveY ->
            "Move Y"

        MoveCircle ->
            "Move in a Circle"

        ScaleU ->
            "Scale"

        URotate ->
            "Rotate"

        ScaleX ->
            "Scale X"

        ScaleY ->
            "Scale Y"

        MakeTransparent ->
            "Make it Transparent"

        EditableXSin ->
            "Edit X Sin"


applyTransformsText tr =
    case tr of
        MoveX ->
            " move x "

        MoveY ->
            " move y "

        MoveCircle ->
            " move in a circle "

        ScaleU ->
            " scale "

        URotate ->
            " rotate "

        ScaleX ->
            " scaleX "

        ScaleY ->
            " scaleY "

        MakeTransparent ->
            " makeTransparent "

        EditableXSin ->
            " editable X Sin "


applyTransformsYourCode model tr =
    case tr of
        MoveX ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*cos(model.time) , 0)"

        MoveY ->
            "|> move (0 , " ++ String.fromFloat model.uScale ++ "*sin(model.time))"

        MoveCircle ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model ++ ", " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleU ->
            "|> scale " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        URotate ->
            "|> rotate " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        ScaleX ->
            "|> scaleX " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleY ->
            "|> scaleY " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        MakeTransparent ->
            "|> makeTransparent " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        EditableXSin ->
            "|> move (" ++ String.fromFloat model.editableScale ++ "*cos(model.time) , " ++ "0" ++ ")"



-- change you app's state based on your new messages


numGraphPoints model =
    round 2505


curveX x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


sinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.cosGraph (List.drop 1 model.cosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (dotted 1) col1) points)


cosinString model =
    let
        fraction =
            if (model.uShift / 8 * 2) < 0 then
                showDigits 5 (model.uShift / 8 * 2)

            else
                "+" ++ showDigits 4 (model.uShift / 8 * 2)
    in
    showDigits 2 model.uDilation ++ "*model.time" ++ fraction ++ "*Pi)"


view model =
    
    [ 

    
    ]

--(rgba 255 0 0 0.6)
upArrowUDilution model= polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (if model.uDilation < model.maxFrequency then rgb 255 0 0 else darkGray)
upArrowUScale model= polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (if model.uScale < model.maxAmplitude then rgb 255 0 0 else darkGray)
upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (rgba 255 0 0 0.6)

downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (rgba 255 0 0 0.6)

downArrowUScale model =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (if model.uScale > -(model.maxAmplitude) then rgba 255 0 0 0.6 else darkGray)

downArrowUDilution model =
     polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (if model.uDilation > -(model.maxFrequency) then rgba 255 0 0 0.6 else darkGray)

trigGraphAxis model =
    group
        [
         rect model.sinWaveLength 0.5 |> filled red |> move ( 135 + model.sinWaveLength / 2, -20 )

        -- Subtract 130 to account for the ratio of the screen and remove excess
    
        
        ]



showDigits width x =
    "      " ++ String.fromFloat x |> String.right width


titleColour =
    rgba 200 0 0 0.95


copiable str =
    str |> text |> selectable |> fixedwidth |> size 6 |> filled black


copiable2 str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black
