module LxPlot where

import Diagrams as D
import Graphics.Collage as GC
import Graphics.Element as GE
import Graphics.Input as GI
import Graphics.Input.Field as GF
import Color
import List ((::))
import List as L
import Keyboard
import Signal as S
import Window
import Mouse
import Dict
import Text as T
import String

import Debug

------------ MODEL ------------

-- All positions are in inches. Use the "ft" utility function to get inches from
-- feet and inches representation.

-- Symbol is used as a Tag for the Diagram rendering
type Symbol = HP String
            | F Int
            | P -- Whole plot

-- type for hanging position (pipes)
type alias HangingPosition = { position : Int    -- position of the center of the pipe
                             , length   : Int    -- total length of the pipe
                             -- could add height, left-right
                             }

-- type for fixture (lights)
type FixtureType = ERS | PAR | Fresnel
type alias Fixture = { position   : String
                     , distance   : Int              -- distance from center of hanging position (+right, -left)
                     , instrument : FixtureType
                     , channel    : Int
                     , dimmer     : Int
                     -- could add unit number, gel, etc.
                     }

fixtureTypes = [ERS, PAR, Fresnel]

-- type for plot
type alias Plot = {architecture : Dict.Dict String HangingPosition, fixtures : Dict.Dict Int Fixture}
initPlot = { architecture = theaterEast, fixtures = Dict.empty}
--initPlot = {architecture = theaterEast, fixtures = tops}

type alias Options = { plasterLine : Bool
                     , centerLine  : Bool
                     , zoom        : Float
                     , offset      : D.Point
                     , nextLight   : Int
                     , nextChannel : Int
                     , nextDimmer  : Int
                     }

initOptions = { plasterLine = True, centerLine = True, zoom = 1, offset = (0,0), nextLight = 1, nextChannel = 1, nextDimmer = 1}
--initOptions = { plasterLine = True, centerLine = True, zoom = 1, offset=(0,0), nextLight = 9, nextChannel = 19, nextDimmer = 109}
 

type alias Model = { options : Options, plot : Plot }
initModel = { options = initOptions, plot = initPlot }

type Tool = Insert FixtureType
          | Delete
          | MovePan

type alias ModelAndDrag = { model : Model, dragState : Maybe DraggingState, tool : Tool}
initModelAndDrag = { model = initModel, dragState = Nothing, tool = MovePan}

------------ UPDATE ------------

--Note: All the code to do with dragging is heavily based on Pete Vilter's Graph
--Editor example, which can be found here:
--https://github.com/vilterp/elm-diagrams/blob/master/examples/GraphEditor.elm

type alias State = { modelAndDrag : ModelAndDrag, diagram : Diagram }
initState = { modelAndDrag = initModelAndDrag, diagram = view initModelAndDrag }

type Event = MouseMoveEvt
           | MouseUpEvt
           | MouseDownEvt
           | ZoomIn
           | ZoomOut
           | ZoomNone
           | PL
           | CL
           | CreateEvt FixtureType
           | RemoveEvt
           | PanEvt
           | ChangeChannel String
           | ChangeDimmer String


events : S.Signal (Event, D.Point)
events = let
           keys {y} = if | y ==  1 -> ZoomIn
                         | y == -1 -> ZoomOut
                         | otherwise -> ZoomNone
           inOut = S.map keys (S.merge Keyboard.arrows Keyboard.wasd)
           upDown = S.map (\id -> if id then MouseDownEvt else MouseUpEvt) Mouse.isDown
           moves = S.map (\pos -> MouseMoveEvt) collageMousePos'
           cl = S.map (\b -> CL) <| S.subscribe centerLineCheck
           pl = S.map (\b -> PL) <| S.subscribe plasterLineCheck
           tool t = case t of
                      Insert ft -> CreateEvt ft
                      Delete -> RemoveEvt
                      MovePan -> PanEvt
           toolChange = S.map tool <| S.subscribe toolSelect
           chan = S.map (\s -> ChangeChannel s.string) <| S.subscribe channelNum
           dim = S.map (\s -> ChangeDimmer s.string) <| S.subscribe dimmerNum
           merged = S.mergeMany [ chan, dim, toolChange, cl, pl, inOut, upDown, moves ]
         in
           S.map2 (,) merged collageMousePos'

type DraggingState = DraggingPosition { label : String, offset : D.Point}
                   | DraggingFixture { label : Int, offset : D.Point }
                   | DraggingPlot D.Point

getDragState : D.PickPath Symbol -> Maybe DraggingState
getDragState pp = case pp of
  (HP lbl,ofs)::_ -> Just <| DraggingPosition {label = lbl, offset = ofs}
  (F lbl,ofs)::_   -> Just <| DraggingFixture {label = lbl, offset = ofs}
  (P, ofs)::_    -> Just <| DraggingPlot ofs
  _ -> Nothing

getFixture : D.PickPath Symbol -> Maybe {label : Int}
getFixture pp = case pp of
  (F lbl,_)::_ -> Just {label = lbl}
  _ -> Nothing

movePosition : Model -> {label : String, offset : D.Point } -> D.Point -> Model
movePosition model ds mousePos = 
  let toInches p = round <| p / (model.options.zoom)
      updateFn value = case value of
                         Just hp -> let (_, my) = mousePos
                                        (_, oy) = ds.offset
                                        (_, cy) = model.options.offset
                                        newPos = toInches (my - oy - cy)
                                    in Just { hp | position <- newPos }
                         Nothing -> Nothing
      plot = model.plot
      newPlot = { plot | architecture <- 
                           Dict.update ds.label updateFn plot.architecture }
  in { model | plot <- newPlot }


--moveFixture is special, because the units will snap to hanging positions.
moveFixture : Model -> {label : Int, offset : D.Point } -> D.Point -> Model
moveFixture model ds mousePos = 
  let toInches p = round (p / (model.options.zoom))
      plot = model.plot
      getNearestHP y = 
        let yi = toInches y
            dist (l,hp) = abs ((hp.position)-yi)
            compare hp1 hp2 = if | (dist hp1) < (dist hp2) -> hp1
                                 | otherwise -> hp2 
        in 
          case (Dict.toList plot.architecture) of
            hp::hps -> fst (L.foldr compare hp hps)
            _ -> "" -- No Hanging Positions, return dummy
      updateFn value = case value of
                         Just f  -> let (mx, my) = mousePos
                                        (ox, _ ) = ds.offset
                                        (cx, cy) = model.options.offset
                                        newDist = toInches (mx - ox - cx)
                                        newPos = getNearestHP (my-cy)
                                    in Just { f | distance <- newDist 
                                                , position <- newPos}
                         Nothing -> Nothing
      newPlot = { plot | fixtures <- 
                           Dict.update ds.label updateFn plot.fixtures }
  in { model | plot <- newPlot }

makeFixture : Model -> {label : Int, ft : FixtureType } -> D.Point -> Model
makeFixture model ds mousePos =
  let toInches p = round (p / (model.options.zoom))
      plot = model.plot
      opt = model.options
      getNearestHP y = 
        let yi = toInches y
            dist (l,hp) = abs ((hp.position)-yi)
            compare hp1 hp2 = if | (dist hp1) < (dist hp2) -> hp1
                                 | otherwise -> hp2 
        in 
          case (Dict.toList plot.architecture) of
            hp::hps -> (L.foldr compare hp hps)
      (mx, my) = mousePos
      (cx, cy) = model.options.offset
      pos = getNearestHP (my-cy)
      dist = toInches (mx - cx)
      mkFixture = { position = (fst pos), distance = dist, instrument = ds.ft,
                  channel = opt.nextChannel, dimmer = opt.nextDimmer }
      newPlot = { plot | fixtures <- Dict.insert ds.label mkFixture plot.fixtures }
      newOpt = {opt | nextLight <- ds.label + 1
                    , nextDimmer <- opt.nextDimmer + 1
                    , nextChannel <- opt.nextChannel + 1}
  in if | (abs dist) > ((snd pos).length)//2 -> model
        | otherwise -> { model | plot <- newPlot 
                       , options <- newOpt }

deleteFixture : Model -> Maybe {label : Int} -> Model
deleteFixture model ds =
  case ds of
    Just ds -> let plot = model.plot
                   newPlot = {plot | fixtures <- Dict.remove ds.label plot.fixtures}
               in { model | plot <- newPlot }
    Nothing -> model

movePlot : Model -> D.Point -> D.Point -> Model
movePlot model (ox,oy) (mx,my) =
  let opt = model.options
      newOpt = {opt | offset <- (mx - ox, my - oy)}
  in {model | options <- newOpt}

updateZoom : Float -> State -> State
updateZoom z s =
  let 
    oldModelAndDrag = s.modelAndDrag
    oldModel = oldModelAndDrag.model
    oldOptions = oldModel.options

    nZoom = if | (oldOptions.zoom + z) < 0 -> 0
               | otherwise -> oldOptions.zoom + z

    nOptions = {oldOptions | zoom <- nZoom}
    nModel = {oldModel | options <- nOptions}
    nModelAndDrag = {oldModelAndDrag | model <- nModel } 

    nDiagram = viewPlot nModel
  in
    {s | modelAndDrag <- nModelAndDrag
       , diagram <- nDiagram}

updateDragState : State -> Maybe DraggingState -> State
updateDragState state ds =
  let maD = state.modelAndDrag
      newMaD = { maD | dragState <- ds }
  in {state | modelAndDrag <- newMaD }

updateModel : State -> Model -> State
updateModel state newModel = 
  let maD = state.modelAndDrag
      newMaD = { maD | model <- newModel }
  in {state | modelAndDrag <- newMaD 
            , diagram <- view newMaD }

updateTool : State -> Tool -> State
updateTool s t =
  let maD = s.modelAndDrag
      newMaD = { maD | tool <- t }
  in {s | modelAndDrag <- newMaD}


upstate : (Event, D.Point) -> State -> State
upstate (evt,p) s = 
  let opt = s.modelAndDrag.model.options in
  case evt of
    ChangeChannel str -> updateChannel s str
    ChangeDimmer str -> updateDimmer s str
    CL -> updateCL s
    PL -> updatePL s
    ZoomIn  -> updateZoom  0.25 s
    ZoomOut -> updateZoom -0.25 s
    ZoomNone -> s
    CreateEvt ft -> updateTool s (Insert ft)
    RemoveEvt -> updateTool s Delete
    PanEvt -> updateTool s MovePan
    MouseDownEvt -> case s.modelAndDrag.tool of
                      MovePan -> let overPath = D.pick s.diagram p
                                 in updateDragState s <| getDragState overPath
                      Insert ft -> 
                        let newModel = makeFixture s.modelAndDrag.model 
                                                   {label = opt.nextLight, ft = ft}
                                                   p
                        in updateModel s newModel 
                      Delete ->
                        let overPath = D.pick s.diagram p
                            newModel = deleteFixture s.modelAndDrag.model
                                                     (getFixture overPath)
                        in updateModel s newModel
    MouseUpEvt -> case s.modelAndDrag.tool of
                    MovePan -> updateDragState s Nothing  
                    _ -> s
    MouseMoveEvt -> case s.modelAndDrag.tool of
                      MovePan -> case s.modelAndDrag.dragState of
                                   Nothing -> s
                                   Just (DraggingPosition dsp) ->
                                     let newModel = movePosition s.modelAndDrag.model dsp p
                                     in updateModel s newModel
                                   Just (DraggingFixture dsf) ->
                                     let newModel = moveFixture s.modelAndDrag.model dsf p
                                     in updateModel s newModel
                                   Just (DraggingPlot ofs) ->
                                     let newModel = movePlot s.modelAndDrag.model ofs p
                                     in updateModel s newModel
                      _ -> s

getOptions : State -> Options
getOptions s = s.modelAndDrag.model.options

updateOptions : State -> Options -> State
updateOptions s newOpt =
  let maD = s.modelAndDrag
      model = maD.model
      newModel = {model | options <- newOpt}
      newMaD = { maD | model <- newModel}
  in {s | modelAndDrag <- newMaD
        , diagram <- view newMaD }

updateCL : State -> State
updateCL s = 
  let opt = getOptions s
      newOpt = {opt | centerLine <- (not opt.centerLine)}
  in  updateOptions s newOpt
    

updatePL : State -> State
updatePL s = 
  let opt = getOptions s
      newOpt = {opt | centerLine <- (not opt.plasterLine)}
  in  updateOptions s newOpt

updateChannel : State -> String -> State
updateChannel s str = case (String.toInt str) of
  Err _ -> s
  Ok i -> let opt = getOptions s
              newOpt = {opt | nextChannel <- i}
          in  updateOptions s newOpt

updateDimmer : State -> String -> State
updateDimmer s str = case (String.toInt str) of
  Err _ -> s
  Ok i -> let opt = getOptions s
              newOpt = {opt | nextDimmer <- i}
          in  updateOptions s newOpt

state : Signal State
state = S.foldp upstate initState events


------------ VIEW ------------

type alias Diagram = D.Diagram Symbol

view : ModelAndDrag -> Diagram
view maD = viewPlot maD.model

toPixels zoom i = (toFloat i) * zoom

viewPlot : Model -> Diagram
viewPlot {options, plot} =
  let 
    architecture = let ps = Dict.toList plot.architecture
                   in case ps of
                   [] -> D.empty
                   _  -> D.group <| L.map (viewPosition options) ps
    fixtures = let fs = Dict.toList plot.fixtures
               in case fs of
                  [] -> D.empty
                  _  -> D.group <| L.map (viewFixture options architecture False) fs
    plasterLine = if (options.plasterLine) 
                  then D.hline (1.25 * D.width architecture) (GC.dashed Color.black)
                  else D.empty
    centerLine = if (options.centerLine) 
                 then D.vline (1.25 * D.height architecture) (GC.dashed Color.black)
                 else D.empty
  in
    D.move options.offset
    <| D.tag P 
    <| D.background D.invisible
    <| D.zcat [fixtures, architecture, plasterLine, centerLine]
 
viewLight : Options -> Bool -> (Int, {instrument : FixtureType}) -> Diagram
viewLight opt new (label, {instrument}) =
  let
    zoom = opt.zoom
    toPix = toPixels zoom
    color = if new then Color.lightGreen else Color.white
  in
    D.tag (F label) <| case instrument of
                         PAR -> D.rect (toPix 8) (toPix 8) 
                                     (D.fillAndStroke (GC.Solid color) GC.defaultLine)
                         ERS -> D.rect (toPix 6) (toPix 16)
                                     (D.fillAndStroke (GC.Solid color) GC.defaultLine)
                         _ -> D.circle (toPix 4) 
                                     (D.fillAndStroke (GC.Solid color) GC.defaultLine)
                                 

viewFixture : Options -> Diagram -> Bool -> (Int, Fixture) -> Diagram
viewFixture opt architecture new (label, {position, distance, instrument, channel, dimmer}) = 
  let
    zoom = opt.zoom
    toPix = toPixels zoom
    center = D.empty
    defStyle = T.defaultStyle
    scaledStyle = {defStyle | height <- Just (max (toPix 4) 12) }
    light = viewLight opt new (label, {instrument=instrument})
    channelLbl = D.text (toString channel) scaledStyle `D.atop`
      D.circle (toPix 4) (D.fillAndStroke (GC.Solid Color.white) GC.defaultLine)
    dimmerLbl =  D.text (toString dimmer) scaledStyle `D.atop`
      D.ngon 6 (toPix 4) (D.fillAndStroke (GC.Solid Color.white) GC.defaultLine)
    fixture = vcat'' <| L.intersperse (D.vspace (toPix 1)) [channelLbl, dimmerLbl, light]
    positionLight = 
      if | distance > 0 -> D.hcat [ center
                                  , D.hspace (toPix distance)
                                  , fixture ]
         | otherwise    -> hcat'  [ fixture
                                  , D.hspace (toPix (abs distance))
                                  , center ] 
  in
    case (D.getCoords architecture [HP position]) of
      Just p -> D.move p positionLight
      Nothing -> D.empty -- if position does not exist, don't render the light.


viewPosition : Options -> (String, HangingPosition) -> Diagram
viewPosition opt (label,{position, length}) = 
  let 
    zoom = opt.zoom
    toPix = toPixels zoom
    pipe = D.tag (HP label)
           <| D.rect (toPix length) (toPix 2) (D.justStroke GC.defaultLine)

    pipeWithLbl = D.hcat [pipe, D.hspace 20, D.text (" " ++ label) T.defaultStyle]
    center = D.empty
  in 
    if | position > 0 -> vcat'  [ pipeWithLbl
                                , D.vspace (toPix position)
                                , center]
       | otherwise    -> D.vcat [ center
                                , D.vspace (toPix (abs position))
                                , pipeWithLbl]

centerLineCheck : S.Channel Bool
centerLineCheck = S.channel True

plasterLineCheck : S.Channel Bool
plasterLineCheck = S.channel True

toolSelect : S.Channel Tool
toolSelect = S.channel MovePan

channelNum : S.Channel GF.Content
channelNum = S.channel {string= "1", selection = GF.Selection 1 1 GF.Forward}

dimmerNum : S.Channel GF.Content
dimmerNum = S.channel {string= "1", selection = GF.Selection 1 1 GF.Forward}

viewToolbar : State -> (Int, Int) -> GF.Content -> GF.Content -> GE.Element
viewToolbar s (w,h) nC nD = 
  let tw = (w//5)
      opt = s.modelAndDrag.model.options
      bg = GE.layers [ GE.color Color.black     <| GE.spacer (tw  ) h
                     , GE.color Color.lightBlue 
                       <| GE.container (tw-1) h GE.bottomLeft footer ]
      clCheckbox = GE.flow GE.right <| L.intersperse (GE.spacer 5 5)
                   [ GE.empty 
                   , GE.above 
                      (GE.spacer 0 1)
                      (GI.checkbox (S.send centerLineCheck) opt.centerLine)
                   , T.plainText "Center Line visible?"
                   ]
      plCheckbox = GE.flow GE.right <| L.intersperse (GE.spacer 5 5)
                   [ GE.empty
                   , GE.above 
                      (GE.spacer 0 1)
                      (GI.checkbox (S.send plasterLineCheck) opt.plasterLine)
                   , T.plainText "Plaster Line visible?"
                   ]

      but sym col = (GE.container 48 48 GE.middle sym |>
                 GE.color col |>
                 GE.container 50 50 GE.middle |>
                 GE.color Color.black)

      createFixture ft = let sym = GC.collage 45 45 
                                   [D.render 
                                   <| viewLight {opt | zoom <- 2} False (opt.nextLight,{instrument=ft})]
                         in GE.flow GE.down 
                            [ (GI.customButton (S.send toolSelect (Insert ft)) 
                                            (but sym Color.lightGray)
                                            (but sym Color.lightGreen)
                                            (but sym Color.gray))
                            , T.plainText (toString ft)]

      deleteFixture = let sym = GE.image 45 45 "assets/Cancel.png"
                      in GI.customButton (S.send toolSelect Delete) 
                                         (but sym Color.lightGray)
                                         (but sym Color.lightRed)
                                         (but sym Color.gray)

      pan = let sym = GE.image 45 45 "assets/Hand_Cursor.png"
            in GI.customButton (S.send toolSelect MovePan) 
                               (but sym Color.lightGray)
                               (but sym Color.gray)
                               (but sym Color.gray)

      currentTool = case s.modelAndDrag.tool of
                      Insert ft -> createFixture ft
                      Delete -> deleteFixture
                      MovePan -> pan
      
      defTextStyle = T.defaultStyle
      scaledTextStyle = {defTextStyle | height <- Just 12 }
      defFieldStyle = GF.defaultStyle
      scaledFieldStyle = {defFieldStyle | style <- scaledTextStyle }
      channelField = GE.beside 
                       (T.fromString "Channel #: " |> T.leftAligned 
                        |> GE.container 80 28 GE.midLeft)
                       (GF.field scaledFieldStyle (S.send channelNum) "Channel #:" nC)
      dimmerField = GE.beside 
                       (T.fromString "Dimmer #: " |> T.leftAligned
                        |> GE.container 80 28 GE.midLeft)
                       (GF.field scaledFieldStyle (S.send dimmerNum) "Dimmer #:" nD)

      appName = T.fromString "lx.plot" |> T.height 24 
                                       |> T.bold 
                                       |> T.centered
                                       |> GE.width tw
                                       |> outline

      footer = T.concat [ T.fromString "By "
                        , T.link "http://kedki.me"
                                 (T.fromString "Kevin Freese")
                        , T.fromString "\nWritten in "
                        , T.link "http://elm-lang.org" 
                                 (T.fromString "Elm")
                        , T.fromString " and is "
                        , T.link "https://github.com/Ked-Ki/lx.plot" 
                                 (T.fromString "open source")
                        , T.fromString "."
                        ]
                        |> T.leftAligned
                        |> outline
                             
      zoomInstructions = "To zoom, use up and down arrow keys (or WASD)." 

      outline e = GE.color Color.black 
                  <| GE.container tw ((GE.heightOf e)+2) GE.middle 
                  <| GE.color Color.lightGray 
                  <| GE.container (tw-2) (GE.heightOf e) GE.midLeft e

      title str = [GE.spacer 0 20
                  , outline <| tab 5 <| GE.width tw 
                            <| T.leftAligned <| T.fromString str
                  , GE.spacer 0 10]

      tab i e = GE.flow GE.right [GE.spacer i i , e]

  in
     GE.layers [ bg
               , GE.flow GE.down 
                  <| L.concat
                      [ [GE.spacer 0 20]
                      , [appName]
                      , [GE.spacer 0 20]
                      , L.map outline [clCheckbox, plCheckbox]
                      , title "Create Fixtures:"
                      , [GE.flow GE.right 
                           <| L.intersperse (GE.spacer 10 10)
                           <| (::) GE.empty
                           <| L.map createFixture fixtureTypes]
                      , title "Delete Fixtures:"
                      , [tab 10 deleteFixture]
                      , title "Move Fixtures/Pan Plot:"
                      , [tab 10 pan]
                      , title "Current Tool:"
                      , [tab 10 currentTool]
                      , title "Next Fixture:"
                      , [tab 5 channelField]
                      , [tab 5 dimmerField]
                      , title zoomInstructions
                      ]
               ]



render : State -> (Int, Int) -> GF.Content -> GF.Content -> GE.Element
render s (w,h) nC nD = viewToolbar s (w,h) nC nD `GE.beside` (GC.collage ((4*w)//5) h [D.render s.diagram])

------------ MAIN ------------

main = S.map4 render state Window.dimensions (S.subscribe channelNum) (S.subscribe dimmerNum)
             

------------ UTILITIES ------------

-- ft converts from feet and inches to inches
ft : Int -> Int -> Int
ft f i = (f * 12) + i

-- Rewritten versions of D.vcat and D.above so that the origin is on the bottom
-- element.
vcat' = L.foldr above' D.empty
vcat'' = L.foldr1 above'
above' a b = let yTrans = (D.envelope D.Down a) + (D.envelope D.Up b)
               in D.group [D.moveY yTrans a, b]

hcat' = L.foldr beside' D.empty
beside' a b = let xTrans = (D.envelope D.Right a) + (D.envelope D.Left b)
                in D.group [D.moveX -xTrans a, b]

toPoint (x,y) = (toFloat x, toFloat y)

floatWindowDims = S.map toPoint Window.dimensions
floatMousePos = S.map toPoint Mouse.position
toCollageCoords (w,h) (x,y) = let midw = ((3*w)/5)
                                  x' =  clamp (w/5) w x
                              in (x' - midw, h/2 - y)

collageMousePos' = S.map2 toCollageCoords floatWindowDims floatMousePos


-- hardcoded examples
foh4 =  ("FOH 4" , {position = -(24 `ft` 2), length = 32 `ft` 0} )
foh3 =  ("FOH 3" , {position = -(21 `ft` 6), length = 32 `ft` 0} )
foh2 =  ("FOH 2" , {position = -(6  `ft` 6), length = 32 `ft` 0} )
foh1 =  ("FOH 1" , {position = -(2  `ft` 0), length = 32 `ft` 0} )
elec1 = ("1 Elec", {position =  (4  `ft` 0), length = 36 `ft` 0} )
elec2 = ("2 Elec", {position =  (11 `ft` 4), length = 36 `ft` 0} )
elec3 = ("3 Elec", {position =  (21 `ft` 5), length = 36 `ft` 0} )

theaterEast = Dict.fromList [foh4, foh3, foh2, foh1, elec1, elec2, elec3]


tops = Dict.fromList <|
       [ (1,{position = "1 Elec", distance =  (9 `ft` 0), instrument = PAR,
       channel = 11, dimmer = 101})
       , (2,{position = "1 Elec", distance =  (3 `ft` 0), instrument = PAR,
       channel = 12, dimmer = 102})
       , (3,{position = "1 Elec", distance = -(3 `ft` 0), instrument = PAR,
       channel = 13, dimmer = 103})
       , (4,{position = "1 Elec", distance = -(9 `ft` 0), instrument = PAR,
       channel = 14, dimmer = 104})
       , (5,{position = "2 Elec", distance =  (9 `ft` 0), instrument = PAR,
       channel = 15, dimmer = 105})
       , (6,{position = "2 Elec", distance =  (3 `ft` 0), instrument = PAR,
       channel = 16, dimmer = 106})
       , (7,{position = "2 Elec", distance = -(3 `ft` 0), instrument = PAR,
       channel = 17, dimmer = 107})
       , (8,{position = "2 Elec", distance = -(9 `ft` 0), instrument = PAR,
       channel = 18, dimmer = 108})
       ]
