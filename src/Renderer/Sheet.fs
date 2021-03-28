module Sheet

open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React


open Helpers

type KeyboardMsg =
    | CtrlN | AltC | AltV | AltZ | AltShiftZ | DEL| Ctrl | AltUp |AltDown | CtrlS | Alt | CtrlPlus | AltU | AltR |CtrlNPlus |CtrlT| AltO | CtrlNPlusWidth

type Undo =
    | MoveMultiSelect of ((Symbol.Symbol * int) * (XYPos * XYPos)) list * ((BusWire.Wire * int) * (XYPos*XYPos) list) list 
    | DeleteMultiSelect of ((Symbol.Symbol * int) * (XYPos * XYPos)) list * ((BusWire.Wire * int) * (XYPos*XYPos) list) list 
    | Nothing 

type Model = {
    Wire: BusWire.Model
    IsWiring: (Option<CommonTypes.Port> * Option<CommonTypes.Port>)   //Input/Output * (portId * portId)       //do we need null for the first one - so does it need to be an option
    IsSelecting: CommonTypes.ComponentId list * (BusWire.Wire * int) list        //Symbols * Wires
    MultiSelectBox: bool * XYPos * XYPos  //boxOrWire,startPos, endPos multi-select box
    ZoomSpaceBox: XYPos * XYPos 
    Restore: Undo
    UndoTemp: Undo 
    LastOp: Helpers.MouseOp;
    LastKey: KeyboardMsg
    Zoom : float * XYPos
    ZoomSpace: bool * bool 
    LastDragPos : XYPos
    Undo : Undo list
    Comp: string
    }
    

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg


type SelectingBox={
    TopCorner: XYPos
    BottomCorner: XYPos
}

//helper functions
//let zoom = 1.0

//let inputno = match comp with 
//              |"not" -> 2
//              |"and" -> 2
//              |"or" -> 2
//              |"xor" -> 2
//              |"nand" -> 2
//              |"nor" -> 2
//              |"xnor" -> 2
//              |"mux2" -> 3
//              |"demux2" -> 2
//              |"nbitsadder" -> 3
//              |"mergewires" -> 2
//              |"splitwire" -> 1
//              |"dff" -> 1
//              |"dffe" -> 2
//              |"register" -> 1
//              |"registere" -> 2
//              |"rom" -> 1
//              |"asyncrom" -> 1
//              |"ram" -> 3

    



let stringToComponent comp width=
    match comp with
    |"Not" -> CommonTypes.Not
    |"And" -> CommonTypes.And
    |"Or" -> CommonTypes.Or
    |"Xor" -> CommonTypes.Xor
    |"Nand" -> CommonTypes.Nand
    |"Nor" -> CommonTypes.Nor
    |"Xnor" -> CommonTypes.Xnor
    |"Mux2" -> CommonTypes.Mux2
    |"Demux2" -> CommonTypes.Demux2
    //|"NbitsAdder" -> CommonTypes.NbitsAdder bits
    |"MergeWires" -> CommonTypes.MergeWires
    //|"SplitWire" -> CommonTypes.SplitWire
    |"DFF" -> CommonTypes.DFF
    |"DFFE" -> CommonTypes.DFFE
    |"Register" -> CommonTypes.Register width
    //|"RegisterE" -> CommonTypes.RegisterE bits
    //|"ROM" -> CommonTypes.ROM Memory
    //|"AsyncROM" -> CommonTypes.AsyncROM Memory
    //|"RAM" -> CommonTypes.RAM Memory
    //|"Custom" -> CommonTypes.Custom 

let renderBusWidth dispatch width model=
    Dropdown.Item.a 
        [ 
            Dropdown.Item.Props
                [
                    OnClick (fun _ ->
                        dispatch (Wire (BusWire.NewComponent (model.Comp, width)))
                         
                    )
                ]
        ]
        [ 
            str (string (width))
        ]

let renderDropdownInput model dispatch =
    Dropdown.dropdown [ Dropdown.IsHoverable ]
        [
            div [ ]
                [ 
                    Button.button [ ]
                        [ str "choose your input buswidth" ]
                ]
            Dropdown.menu [ ]
                [
                    Dropdown.content [ ]
                        [ 
                            renderBusWidth dispatch 1 model
                            renderBusWidth dispatch 2 model
                            renderBusWidth dispatch 3 model
                            renderBusWidth dispatch 4 model
                            renderBusWidth dispatch 5 model
                            renderBusWidth dispatch 6 model
                            renderBusWidth dispatch 7 model
                            renderBusWidth dispatch 8 model
                            renderBusWidth dispatch 9 model
                            renderBusWidth dispatch 10 model
                        ]
                ]
        ]

let renderItem component dispatch width model=
        Dropdown.Item.a 
            [ 
                Dropdown.Item.Props
                    [
                        OnClick (fun _ ->
                            dispatch (Wire (BusWire.NewComponent (component, width)))
                             
                        )
                    ]
            ]
            [ 
                str (component)
            ]


let renderDropdown model dispatch = 

    Dropdown.dropdown [ Dropdown.IsHoverable ]
        [
            div [ ]
                [ 
                    Button.button [ ]
                        [ str "Choose your Symbol" ]
                ]
            Dropdown.menu [ ]
                [
                    Dropdown.content [ ]
                        [ 
                            renderItem "Not" dispatch 0 model
                            renderItem "And" dispatch 0 model
                            renderItem "Or" dispatch 0 model
                            renderItem "Xor" dispatch 0 model
                            renderItem "Nand" dispatch 0 model
                            renderItem "Nor" dispatch 0 model
                            renderItem "Xnor" dispatch 0 model
                            renderItem "Mux2" dispatch 0 model
                            renderItem "Demux2" dispatch 0 model
                            renderItem "Register" dispatch 0 model
                        ]
                ]

        ]


//let renderDropdownOutput model dispatch comp =
//    //let outputno = match comp with 
//    //              |"Not" -> 1
//    //              |"And" -> 1
//    //              |"Or" -> 1
//    //              |"Xor" -> 1
//    //              |"Nand" -> 1
//    //              |"Nor" -> 1
//    //              |"Xnor" -> 1
//    //              |"Mux2" -> 1
//    //              |"Demux2" -> 2
//    //              |"NbitsAdder" -> 2
//    //              |"MergeWires" -> 1
//    //              |"SplitWire" -> 2
//    //              |"DFF" -> 1
//    //              |"DFFE" -> 1
//    //              |"Register" -> 1
//    //              |"RegisterE" -> 1
//    //              |"ROM" -> 1
//    //              |"AsyncROM" -> 1
//    //              |"RAM" -> 1
                  
//        Dropdown.dropdown [ Dropdown.IsHoverable ]
//        [
//            div [ ]
//                [ 
//                    Button.button [ ]
//                        [ str "Choose your Input BusWidth" ]
//                ]
//            Dropdown.menu [ ]
//                [
//                    Dropdown.content [ ]
//                        [ 
//                            renderItem "Not" dispatch
//                            renderItem "And" dispatch
//                            renderItem "Or" dispatch
//                            renderItem "Xor" dispatch 
//                            renderItem "Nand" dispatch 
//                            renderItem "Nor" dispatch 
//                            renderItem "Xnor" dispatch 
//                            renderItem "Mux2" dispatch 
//                            renderItem "Demux2" dispatch
//                        ]
//                ]
//        ]

//let renderPreview model =
//    match model.FavLang with
//    |comp ->
//        p [ ]
//            [ str ("You selected: " + comp) ]


let dimensions startPos endPos = sprintf "%f,%f %f,%f %f,%f %f,%f" startPos.X startPos.Y startPos.X endPos.Y endPos.X endPos.Y endPos.X startPos.Y


//display

let displaySvgWithZoom (zoom:float*XYPos) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model)=
    printfn "multi %A" model.Zoom

    let sizeInPixels = sprintf "%.2fpx" ((1000. * (fst model.Zoom)))


    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = (ev.clientX/(fst model.Zoom) - (snd model.Zoom).X/(fst model.Zoom)); Y = (ev.clientY/(fst model.Zoom) - (snd model.Zoom).Y/(fst model.Zoom))}})
    let keyDown (ev:Types.KeyboardEvent) =
        dispatch <|KeyPress (Alt)

    let (boxOrWire, startPos, endPos) = model.MultiSelectBox
    let (boxPos1, boxPos2) = model.ZoomSpaceBox
    let backgroundSize = sprintf "%fpx %fpx" (30.*(fst model.Zoom)) (30.*(fst model.Zoom))
    let background = "linear-gradient(to right, LightGrey 1px, transparent 1px), linear-gradient(to bottom, LightGrey 1px, transparent 1px)"
    let boxColour = match model.ZoomSpace with 
                   |(true,_) -> "green" 
                   |_ -> "blue"

    
    let box = [ 
                match boxOrWire with 
                | true -> polygon [
                                    SVGAttr.Points (dimensions startPos endPos)
                                    SVGAttr.StrokeWidth "1px"
                                    SVGAttr.Stroke "Black"
                                    SVGAttr.FillOpacity 0.1
                                    SVGAttr.Fill boxColour
                                ][]
                          svgReact
                     // the application code
                | _ -> match model.ZoomSpace with 
                       |(true, false) -> polygon [
                                                    SVGAttr.Points (dimensions boxPos1 boxPos2)
                                                    SVGAttr.StrokeWidth "1px"
                                                    SVGAttr.Stroke "Black"
                                                    SVGAttr.FillOpacity 0.1
                                                    SVGAttr.Fill boxColour
                                                ][]
                                         svgReact
                       |_ -> svgReact
            ]

    match model.LastKey with 
    |CtrlN -> 
        div [ Style 
                [ 
                    Height "100vh" 
                    MaxWidth "100vw"
                    CSSProp.OverflowX OverflowOptions.Auto 
                    CSSProp.OverflowY OverflowOptions.Auto
                ] 
              OnMouseDown (fun ev -> (mouseOp Down ev))
              OnMouseUp (fun ev -> (mouseOp Up ev))
              OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
              ]
              [
                  renderDropdown model dispatch
              ]    
    |CtrlNPlusWidth -> 
        div [ Style 
                [ 
                    Height "100vh" 
                    MaxWidth "100vw"
                    CSSProp.OverflowX OverflowOptions.Auto 
                    CSSProp.OverflowY OverflowOptions.Auto
                ] 
              OnMouseDown (fun ev -> (mouseOp Down ev))
              OnMouseUp (fun ev -> (mouseOp Up ev))
              OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
              ]
              [
                  renderDropdownInput model dispatch
              ]   
    |_ -> 
        div [ Style 
                [ 
                    Height "100vh" 
                    MaxWidth "100vw"
                    CSSProp.OverflowX OverflowOptions.Auto 
                    CSSProp.OverflowY OverflowOptions.Auto
                ] 
              OnMouseDown (fun ev -> (mouseOp Down ev))
              OnMouseUp (fun ev -> (mouseOp Up ev))
              OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
            ]

            [  svg
                [ Style 
                    [
                  
                        match model.IsWiring with 
                        |(None, Some Port) -> Cursor "grabbing"
                        |(Some Port, None) -> Cursor "grabbing"
                        |(None, None) -> match model.ZoomSpace with 
                                         |true,_ -> Cursor "zoom-in"
                                         |false,false -> match model.LastKey with 
                                                         |CtrlS -> Cursor "alias"
                                                         |_ -> Cursor "default"

                        Border "3px solid green"
                        Height sizeInPixels
                        Width sizeInPixels  
                        BackgroundSize backgroundSize
                        BackgroundImage background        
                    ]
                ]
                [ g // group list of elements with list of attributes
                    [ Style [
                            Transform (sprintf "translate(%fpx,%fpx) scale(%f)" (snd model.Zoom).X (snd model.Zoom).Y (fst model.Zoom))
                            //Transform (sprintf "scale(%f)" zoomChanged)
                        
                           ]
                    ] box  // top-level transform style attribute for zoom
                
              ]

            ]
      

let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model.Zoom wireSvg dispatch model

let inSelBox (model:Model) (sc:XYPos) (ec:XYPos): (CommonTypes.ComponentId list * (BusWire.Wire*int) list) =
    let corners = if sc.X < ec.X     //dragging left to right
                      then if sc.Y < ec.Y
                           then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
                           else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
                      else if sc.Y > ec.Y    //dragging right to left
                          then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
                          else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down
    let overlap index (pos1,pos2) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
                                        ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
                                    then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
                                              ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
                                           then Some index         //use index to get the symbol id
                                           else None
                                    else None
    let symbolscontained =
        List.mapi overlap model.Wire.Symbol.SymBBoxes
        |> List.map (fun indexOption -> match indexOption with
                                        | Some index -> Some (model.Wire.Symbol.Symbols.[index].Id)
                                        | None ->  None)
        |> List.choose (fun x-> x )
    // FINDING WIRES IN THE MULTI-SELECT BOX - NOT CURRENTLY NEEDED
    let wirescontained = 
         List.mapi (fun index segmentsList -> 
                             List.tryPick (overlap index) segmentsList
                             ) model.Wire.wBB
         |> List.map (fun val1 -> match val1 with
                                  | Some index -> Some (model.Wire.Wires.[index])
                                  | None ->  None)
         |> List.choose (fun x->x)
         |> List.map (fun wire -> (wire, 0))
    (symbolscontained, wirescontained)


let wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : (BusWire.Wire * int) list = //checks if point is in wire bounding box
    let isInside bBoxList wire= //gives you the wire bb list 
        let inSeg indexSeg segment = //list of bounding boxes 
            let (box1, box2) = match segment,(indexSeg%2) with 
                               |(a,b),1 -> (b,a)
                               |(a,b),0 -> (a,b)
                               | _ -> failwithf "Not Implemented"
            if ((pos.X <= box1.X && pos.X >= box2.X) && (pos.Y <= box2.Y && pos.Y >= box1.Y)) ||  ((pos.X >= box1.X && pos.X <= box2.X) && (pos.Y >= box2.Y && pos.Y <= box1.Y))
            then (true, wire, indexSeg) 
            else (false, wire, indexSeg)            
        bBoxList 
        |> List.mapi (fun indexSeg y -> inSeg indexSeg y)
    
    let vertices = List.map (fun (i:BusWire.Wire) -> i.Vertices) wModel.Wires

    let mapToBB = 
        wModel.wBB 
        |> List.mapi (fun index wireBBoxes -> isInside wireBBoxes wModel.Wires.[index])
        |> List.collect id
        |> List.filter (fun (x,y,indexSeg) -> x=true) 

    match mapToBB with 
    | [(true, wire,indexSeg)] ->[(wire,indexSeg)]
    | _ -> []

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    
    match msg with
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}}) ->

        //helper functions
        let mousePos = {X=mX;Y=mY}
        let boundingBoxSearchS = match List.tryFindIndex (fun (co1,co2) -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Wire.Symbol.SymBBoxes with
                                 | Some index -> [model.Wire.Symbol.Symbols.[index]]
                                 | None -> []

        let boundingBoxSearchW = wireToSelectOpt model.Wire mousePos

        let boundingBoxSearchP (symbol: Symbol.Symbol): CommonTypes.Port list=
            let dist (pos1:XYPos) (pos2:XYPos) = sqrt((pos1.X-pos2.X)**2. + (pos1.Y-pos2.Y)**2.)
            let portCalculator portlist =
                    match List.tryFind (fun (port:CommonTypes.Port) -> (dist port.PortPos mousePos)<5.) portlist with
                    | Some port -> [port]
                    | None -> []
            if mousePos.X <= (symbol.Pos.X+(symbol.W/2.))
            then portCalculator symbol.InputPorts
            else portCalculator symbol.OutputPorts

        let addWire (ports: string * string): Msg = (Wire <| BusWire.AddWire ports)

        match mouseState with
        
        | Down -> 
                //   if model.IsDropping = true 
                //   then {model with IsDropping=false;LastOp=Down}, Cmd.none
                //   else 
                  match boundingBoxSearchS with
                       |[sym] -> match boundingBoxSearchP sym with
                                 | [port] -> match model.IsWiring with
                                              | (None, None) -> match port.PortType with
                                                                | CommonTypes.Input -> {model with IsWiring=(Some port, None);LastOp=Down;LastDragPos=mousePos},Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypes.ShowOutputsOnly, port.Id, mousePos)) )
                                                                | CommonTypes.Output -> {model with IsWiring=(None, Some port);LastOp=Down;LastDragPos=mousePos},Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypes.ShowInputsOnly, port.Id, mousePos)) )
                                              | (None, Some outputPort)-> match port.PortType with
                                                                          | CommonTypes.Input -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos}, Cmd.ofMsg (addWire (string port.Id,string outputPort.Id))
                                                                          | CommonTypes.Output -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos},Cmd.none
                                              | (Some inputPort, None) -> match port.PortType with
                                                                          | CommonTypes.Output -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos}, Cmd.ofMsg (addWire (string inputPort.Id,string port.Id))
                                                                          | CommonTypes.Input -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos},Cmd.none
                                              | _ -> failwithf "Not implemented - Down Sheet Update function ~ 219"          
                                 | _ -> let (syms,wires) = model.IsSelecting
                                        let selectSyms = if List.contains sym.Id syms then List.filter (fun symbId -> symbId <> sym.Id) syms else List.append syms [sym.Id] 
                                        let undoTemp = if selectSyms = []
                                                       then Nothing 
                                                       else 
                                                            let undoIndexInfo = List.map (fun sym -> ((List.findIndex (fun (oGSym:Symbol.Symbol) -> oGSym.Id = sym) model.Wire.Symbol.Symbols))) selectSyms 
                                                            let syms = List.map (fun index -> (List.item index model.Wire.Symbol.Symbols, index)) undoIndexInfo 
                                                            let BB = List.map (fun index -> model.Wire.Symbol.SymBBoxes.[index]) undoIndexInfo 
                                                            let finalSyms = List.zip syms BB
                                                            MoveMultiSelect (finalSyms, [])
                                        {model with IsSelecting = (selectSyms,[]); LastOp=Down;LastDragPos=mousePos; UndoTemp = undoTemp}, Cmd.none
                                 
                       | _ -> match boundingBoxSearchW with 
                               |[wireAndSeg] -> let (syms,wires) = model.IsSelecting
                                                let selectWires = if List.contains wireAndSeg wires then wires else List.append wires [wireAndSeg] 
                                                let undoTemp = if selectWires = []
                                                               then Nothing 
                                                               else 
                                                                    let justWires = List.map (fun (wire,seg) -> wire) selectWires 
                                                                    let undoIndexInfo = List.map (fun wire -> ((List.findIndex (fun (oGWire:BusWire.Wire) -> oGWire = wire) model.Wire.Wires))) justWires
                                                                    let wires = List.map (fun index -> (List.item index model.Wire.Wires, index)) undoIndexInfo 
                                                                    let BB = List.map (fun index -> model.Wire.wBB.[index]) undoIndexInfo 
                                                                    let finalWires = List.zip wires BB
                                                                    MoveMultiSelect ([], finalWires)
                                                {model with IsSelecting = ([],selectWires); LastOp=Down;LastDragPos=mousePos; UndoTemp = undoTemp}, Cmd.none         //reset wiring to none
                               |_ -> match model.LastKey with 
                                     |CtrlPlus -> {model with IsSelecting = ([],[]); LastKey = CtrlPlus; LastOp=Down;MultiSelectBox=(true,mousePos,mousePos);LastDragPos=mousePos; IsWiring = (None,None)}, Cmd.none
                                     |AltZ ->   {model with ZoomSpaceBox = (mousePos, mousePos)}, Cmd.none
                                     |_ -> {model with IsSelecting = ([],[]); LastOp=Down;MultiSelectBox=(true,mousePos,mousePos);LastDragPos=mousePos; IsWiring = (None,None)}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect ([],[]))


        | Up -> match model.LastOp with
                | Drag -> match model.MultiSelectBox with
                          |(true,p1,p2) ->  let (symbolIds, wireIdSeg) = inSelBox model p1 p2
                                            let indicesToFind Lst = snd (List.unzip Lst)
                                            let wireIds = fst (List.unzip wireIdSeg)
                                            let symUndo = List.mapi (fun realIndex (OGSym:Symbol.Symbol) -> (List.tryFindIndex (fun symId -> OGSym.Id = symId ) symbolIds ) |> function |Some index -> [(OGSym,realIndex)] |None -> [] ) model.Wire.Symbol.Symbols
                                                          |> List.collect id 
                                            let wireUndo = List.mapi (fun index (OGWire:BusWire.Wire) -> (List.tryFind (fun wire -> OGWire = wire) wireIds) |> function |Some found -> [(OGWire,index)] |None -> [] ) model.Wire.Wires   
                                                           |> List.collect id
                                            let symBBUndo = List.mapi (fun index (bb:XYPos * XYPos) -> (List.tryFind  (fun ind -> ind = index) (indicesToFind symUndo)) |> function |Some ind -> [bb] |None -> []) model.Wire.Symbol.SymBBoxes
                                                            |> List.collect id 
                                            let wireBBUndo = List.mapi (fun index (bb:(XYPos * XYPos) list) -> (List.tryFind  (fun ind -> ind = index) (indicesToFind wireUndo))|> function |Some ind -> [bb] |None -> []) model.Wire.wBB
                                                             |> List.collect id
                                            let finalSymUndo = List.map2 (fun sym bb -> (sym,bb)) symUndo symBBUndo
                                            let finalWireUndo = List.zip wireUndo wireBBUndo 
                                            {model with MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.});LastOp=Up;LastDragPos=mousePos; UndoTemp = MoveMultiSelect(finalSymUndo, finalWireUndo)}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect (inSelBox model p1 p2))// (symbInSelBox model p1 p2 , wireInSelBox model.Wire p1 p2) )//check if in bounding boxes
                          | _ ->  match model.ZoomSpace with
                                  |true,false ->    let (posa,posb) = model.ZoomSpaceBox
                                                    let (zoomChanged, coord) =  let (pos1, pos2) = (posa, mousePos) 
                                                                                if model.ZoomSpace = (true,false)
                                                                                then let (top, bottom) = if pos1.Y > pos2.Y then (pos2, pos1) else (pos1, pos2)
                                                                                     let newZoomStart = int (10000./(float (max (bottom.X - top.X) (bottom.Y - top.Y))))
                                                                                     let newZoom = (float newZoomStart)/10.
                                                                                     (newZoom, {X=(-(top.X)*newZoom); Y = (-(top.Y)*newZoom)})
                                                                                else model.Zoom
                                                    {model with ZoomSpace = (true,true); Zoom = (zoomChanged, {X=coord.X; Y=coord.Y}); ZoomSpaceBox = (posa,mousePos); LastOp=Up;LastDragPos=mousePos}, Cmd.none
                                  |_ -> let newUndo = model.UndoTemp :: model.Undo
                                        {model with LastOp=Up;LastDragPos=mousePos;IsSelecting=([],[]); Undo = newUndo; UndoTemp = Nothing} , Cmd.ofMsg (Wire <| BusWire.SnaptoGrid model.IsSelecting) //Cmd.ofMsg (Wire <| BusWire.UpdateBoundingBoxes model.IsSelecting) //   Cmd.ofMsg (updateBBoxes model.IsSelecting) //interface required

                | Down -> match model.LastKey with
                          |CtrlS -> {model with LastOp=Up;LastDragPos=mousePos;MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.}); ZoomSpace = (false,false)}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.HighlightSymbol (fst model.IsSelecting)))
                          |AltZ -> {model with LastKey = AltZ; IsSelecting = ([],[]);LastOp=Up;LastDragPos=mousePos;MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.}); ZoomSpace = (false,false)}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect model.IsSelecting)
                          |CtrlNPlus ->  {model with LastOp=Up;LastDragPos=mousePos; LastKey = Alt; IsSelecting=([],[])}, Cmd.ofMsg (Wire <| BusWire.SnaptoGrid model.IsSelecting)
                          |CtrlPlus -> {model with LastOp=Up;LastDragPos=mousePos; LastKey = Alt; IsSelecting=([],[])}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.SnapSymbolToGrid [(List.last model.Wire.Symbol.Symbols).Id]))
                          |_ -> {model with IsSelecting = ([],[]);LastOp=Up;LastDragPos=mousePos;MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.}); ZoomSpace = (false,false)}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect model.IsSelecting)
                | _   -> {model with LastOp=Up;LastDragPos=mousePos}, Cmd.none


        | Drag -> match model.MultiSelectBox with 
                  |(true, p1, p2) -> {model with IsSelecting=([],[]);LastOp=Drag;MultiSelectBox=(true,p1,mousePos);LastDragPos=mousePos}, Cmd.none
                  | _ -> match model.ZoomSpace with 
                         |(true,false) ->   let (pos1,pos2) = model.ZoomSpaceBox
                                            {model with IsSelecting=([],[]);LastOp=Drag; ZoomSpaceBox  = (pos1, mousePos); LastDragPos=mousePos}, Cmd.none
                         |_ -> {model with LastOp=Drag;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos))//BusWire.Symbol (Symbol.Dragging ((fst model.IsSelecting),mousePos, prevPos))) //send to symbol to move symbols lol
                  //   |(false, p1, prevPos) -> match model.LastOp with 
                //                            |Down -> {model with LastOp=Drag; MultiSelectBox=(false, {X=0.;Y=0.}, mousePos);LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos) ) 
                //                            |Drag -> {model with LastOp=Drag; MultiSelectBox=(false, {X=0.;Y=0.}, mousePos);LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos))//BusWire.Symbol (Symbol.Dragging ((fst model.IsSelecting),mousePos, prevPos))) //send to symbol to move symbols lol
                //                         //    | _ -> model, Cmd.none
        | Move -> match model.IsWiring with 
                  |(None,None) -> match boundingBoxSearchS with
                                  | [symbol] -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.Hovering [symbol.Id]))
                                  | _ -> match model.LastKey with 
                                         |CtrlNPlus ->  {model with LastOp = Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.DroppingNewSymbol (mousePos))) //hovering
                                         |CtrlPlus -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.Dragging ([(List.last model.Wire.Symbol.Symbols).Id], mousePos, model.LastDragPos)))
                                         |_ -> {model with LastOp = Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.Hovering [])) //hovering
                  |(None,Some port) -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypes.ShowInputsOnly, port.Id, mousePos)) )
                  |(Some port,None) -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypes.ShowOutputsOnly, port.Id, mousePos)) )
                  | _ ->  failwithf "Not implemented - Move Sheet Update function ~ 253" 
    
    | Wire (BusWire.NewComponent (comp, width)) ->  match (comp, width) with 
                                                    |("Register", 0) -> {model with LastKey = CtrlNPlusWidth; Comp = "Register"}, Cmd.none
                                                    |("Register", _) -> let wModel, wCmd = BusWire.update (BusWire.Msg.Symbol (Symbol.AddSymbol (stringToComponent comp width, width))) model.Wire    // [1], [1] - this needs to be different for different types        Custom {Name="Kurt";InputLabels=[("Udai",1);("Simi",1);("Gabs",1)];OutputLabels=[("Karl",1)]})
                                                                        {model with Wire = wModel; LastDragPos = {X=10.;Y=10.}; LastKey = CtrlNPlus; Comp = "nothing"}, Cmd.map Wire wCmd
                                                    |_ -> let wModel, wCmd = BusWire.update (BusWire.Msg.Symbol (Symbol.AddSymbol (stringToComponent comp 0, width))) model.Wire    // [1], [1] - this needs to be different for different types        Custom {Name="Kurt";InputLabels=[("Udai",1);("Simi",1);("Gabs",1)];OutputLabels=[("Karl",1)]})
                                                          {model with Wire = wModel; LastDragPos = {X=10.;Y=10.}; LastKey = CtrlNPlus; Comp = "nothing"}, Cmd.map Wire wCmd

    |Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire //send message
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none
    
    //| KeyPress CtrlT -> model,  Cmd.ofMsg (Wire <| BusWire.Msg.Symbol (Symbol.RotateSymbols)) 

    | KeyPress CtrlN -> {model with LastKey = CtrlN}, Cmd.none
    
    |KeyPress DEL ->
        let newTemp = match model.UndoTemp with 
                      |MoveMultiSelect(a,b) -> DeleteMultiSelect(a,b)
        let newUndo = newTemp :: model.Undo
        {model with IsSelecting=([],[]); Undo = newUndo; UndoTemp = Nothing}, Cmd.ofMsg (Wire <| BusWire.DeleteWire) 

    | KeyPress AltZ -> match model.LastKey with 
                       |AltZ -> {model with ZoomSpace = (false,false); Zoom = (1.0, {X=0.;Y=0.}); ZoomSpaceBox = ({X=0.;Y=0.},{X=0.;Y=0.}); LastKey = Alt}, Cmd.none
                       |_ -> match model.ZoomSpaceBox with 
                             |({X=0.;Y=0.},{X=0.;Y=0.}) -> {model with ZoomSpace = (true,false); LastKey = AltZ}, Cmd.none
                             |_ ->  {model with ZoomSpace = (false,false); Zoom = (1.0, {X=0.;Y=0.}); ZoomSpaceBox = ({X=0.;Y=0.},{X=0.;Y=0.}); LastKey = Alt}, Cmd.none 
    
    //{model with Wire = model.Restore; IsSelecting=([],[]);IsDraggingList=(0, {X=0.;Y=0.}); IsDropping=false; IsWiring=(None,None); R vxdbfnmestore=model.Wire}, Cmd.none //undo and reset everything
                // IsDragSelecting = (0, {X=0.;Y=0.}, {X=0.;Y=0.});
    | KeyPress AltUp ->
        printfn "Zoom In"
        {model with Zoom=((fst model.Zoom+0.1), {X=0.;Y=0.}); LastKey = AltUp}, Cmd.none

    | KeyPress AltDown ->
        printfn "Zoom Out"
        {model with Zoom=((fst model.Zoom-0.1), {X=0.;Y=0.}); LastKey = AltDown}, Cmd.none

    | KeyPress AltO -> model, Cmd.ofMsg (Wire <| BusWire.RunBusWidthInference )

    | KeyPress AltU -> 
        let multiFunctionWire acc (Lst: (BusWire.Wire * int) * (XYPos*XYPos) list ) =
            let ((wire, index), BB) = Lst
            let newWires = List.map (fun (changeWire:BusWire.Wire) -> if changeWire.Id = wire.Id then wire else changeWire) (fst acc)
            let newBB = List.mapi (fun indexBB changeBB -> if indexBB = index then BB else changeBB) (snd acc)
            (newWires,newBB)

        let multiFunctionSym acc (Lst: (Symbol.Symbol * int) * (XYPos*XYPos))= 
            let ((sym, index), BB) = Lst 
            let newSymbols = List.map (fun (changeSym: Symbol.Symbol) -> if changeSym.Id = sym.Id then sym else changeSym) (fst acc)
            let newBB = List.mapi (fun indexBB changeBB -> if indexBB = index then BB else changeBB) (snd acc)
            (newSymbols, newBB)

        let multiFunctionSymD acc (Lst: (Symbol.Symbol * int) * (XYPos*XYPos)) = 
            let ((sym, index), BB) = Lst 
            let symSplit = if index > 0 then List.splitAt (index-1) (fst acc) else List.splitAt (index) (fst acc)
            let bbSplit = if index > 0 then List.splitAt (index-1) (snd acc) else List.splitAt (index) (snd acc)
            let newSymbols = List.append [sym] (snd symSplit) 
                             |> List.append (fst symSplit)
            let newBB = List.append [BB] (snd bbSplit) 
                             |> List.append (fst bbSplit)
            (newSymbols, newBB)

        let multiFunctionWireD acc (Lst: (BusWire.Wire * int) * (XYPos*XYPos) list ) =
            let ((wire, index), BB) = Lst
            let wireSplit = if index > 0 then List.splitAt (index-1) (fst acc) else List.splitAt (index) (fst acc)
            let bbSplit = if index > 0 then List.splitAt (index-1) (snd acc) else List.splitAt (index) (snd acc)
            let newWires = List.append [wire] (snd wireSplit) 
                           |> List.append (fst wireSplit) 
            let newBB = List.append [BB] (snd bbSplit) 
                           |> List.append (fst bbSplit)
            (newWires,newBB)

        match List.head (model.Undo) with                                             
        | MoveMultiSelect (symIndexBB, wireIndexBB) -> let (newSymbols, newSBB) = List.fold (fun acc symIndexBB -> multiFunctionSym acc symIndexBB) (model.Wire.Symbol.Symbols, model.Wire.Symbol.SymBBoxes) symIndexBB
                                                       let (newWires, newWBB) = List.fold (fun acc wireIndexBB -> multiFunctionWire acc wireIndexBB) (model.Wire.Wires, model.Wire.wBB) wireIndexBB
                                                       {model with Undo = List.tail model.Undo;  Restore = List.head model.Undo}, Cmd.ofMsg (Wire <| BusWire.UpdateWires (newWires, newWBB, newSymbols, newSBB))
        | DeleteMultiSelect (symIndexBB, wireIndexBB) -> let sortIndexSymbols = List.sortBy (fun ((sym,index),BB) -> index) symIndexBB
                                                         let sortIndexWires = List.sortBy (fun ((wire,index),BB) -> index) wireIndexBB
                                                         let (newSymbols, newSBB) = List.fold (fun acc symIndexBB -> multiFunctionSymD acc symIndexBB) (model.Wire.Symbol.Symbols, model.Wire.Symbol.SymBBoxes) sortIndexSymbols
                                                         let (newWires, newWBB) = List.fold (fun acc wireIndexBB -> multiFunctionWireD acc wireIndexBB) (model.Wire.Wires, model.Wire.wBB) sortIndexWires
                                                         {model with Undo = List.tail model.Undo;  Restore = List.head model.Undo}, Cmd.ofMsg (Wire <| BusWire.UpdateWires (newWires, newWBB, newSymbols, newSBB))
        | Nothing  -> model, Cmd.none

    //|KeyPress Alt -> printfn "hey"
    //                 model, Cmd.none
    //|KeyPress AltR -> 

    |KeyPress CtrlS -> if model.LastKey = CtrlS then {model with LastKey = Alt}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect model.IsSelecting) else {model with LastKey = CtrlS}, Cmd.none

    |KeyPress CtrlPlus -> match model.LastKey with 
                          |CtrlPlus -> {model with LastKey = Alt}, Cmd.none
                          |_ -> {model with LastKey = CtrlPlus}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.DuplicateSymbol))
    
    // | KeyPress PrintSelected ->
    //     let nothing = 
    //         List.map (fun symbol -> printfn symbol.IsSelected;symbol) model.Wire.Symbol.Symbols
    //     {model with Wire with Symbol with Symbols=nothing} , Cmd.none

    | KeyPress s -> 
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | _ -> CommonTypes.Grey
        model, Cmd.none




let init() = 
    let model,cmds = (BusWire.init)() //initial model state
    {
        Wire = model
        IsWiring = (None, None)
        IsSelecting= ([], [])
        MultiSelectBox = (false, {X=0.;Y=0.}, {X=0.;Y=0.})
        Restore = Nothing 
        LastOp = Move
        ZoomSpaceBox = ({X=0.;Y=0.},{X=0.;Y=0.})
        LastKey = Alt
        Zoom = (1.0, {X=0.;Y=0.})
        LastDragPos={X=0.;Y=0.}
        ZoomSpace = (false, false)
        Undo = []
        UndoTemp = Nothing
        Comp = "nothing"
    }, Cmd.map Wire cmds





    

// let wireInSelBox (wModel:BusWire.Model) startPos finalPos =  //checks if wire bounding box within box 
//     let innerLayer start fn bblst = 
//         let innerSeg lst = 
//             let (box1, box2) = lst
//             {X = fn box1.X box2.X; Y = (fn box1.Y box2.Y)}            
//         bblst 
//         |> List.fold (fun acc y -> {X = (fn (innerSeg y).X acc.X); Y = (fn (innerSeg y).Y acc.Y)}) start
//     let maxCoord = 
//         wModel.wBB 
//         |> List.map (innerLayer {X=0.;Y=0.} max)
//     let minCoord =
//         wModel.wBB 
//         |> List.map (innerLayer {X=1000.;Y=1000.} min)
//     List.filter (fun a -> maxCoord.[a].X <= finalPos.X &&  maxCoord.[a].Y <= finalPos.Y) [0..(wModel.wBB.Length-1)]
//     |> List.filter (fun b -> minCoord.[b].X >= startPos.X && minCoord.[b].Y >= startPos.Y)
//     |> List.map (fun c -> wModel.Wires.[c].Id)

// let symbInSelBox (model:Model) (sc:XYPos) (ec:XYPos): (CommonTypes.ComponentId) list=     //sc : start corner, ec: end corner
//     let corners = if sc.X < ec.X     //dragging left to right
//                       then if sc.Y < ec.Y
//                            then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
//                            else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
//                       else if sc.Y > ec.Y    //dragging right to left
//                           then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
//                           else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down

//     let overlap index (pos1,pos2) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
//                                         ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
//                                     then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
//                                               ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
//                                            then Some (model.Wire.Symbol.Symbols.[index].Id)          //use index to get the symbol id
//                                            else None
//                                     else None

//     List.mapi overlap model.Wire.Symbol.SymBBoxes
//     |> List.choose (fun x->x)

        // let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.DeleteSymbol)) model.Wire
        // let newWSelectList = 
        //     if symList <> [] //if symbols have been selected then check whether wires connected also have to be deleted
        //     then
        //         let srcPorts = List.map (fun (x:BusWire.Wire) -> x.SrcPort) model.Wire.Wires //take all the inputports ids from wires
        //         let tgtPorts = List.map (fun (x:BusWire.Wire) -> x.TargetPort) model.Wire.Wires //take all the outputports ids from wires
        //         let symbolsList = 
        //             List.map (fun x -> List.item x model.Wire.Symbol.Symbols) symList //collect all the selected symbols
        //         let selectedSymbolInputs = //check if portids in the wires and selected symbols match for input ports -> if so then take the index of the wire 
        //             symbolsList
        //             |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypes.Port) -> [y.Id]) x.InputPorts)
        //             |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) srcPorts |> function |Some a -> [a] |None -> []) lst) 
        //         let selectedSymbolOutputs= //check if portids in the wires and selected symbols match for output ports -> if so then take the index of the wire 
        //             symbolsList
        //             |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypes.Port) -> [y.Id]) x.OutputPorts) 
        //             |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) tgtPorts |> function |Some a -> [a] |None -> []) lst) 
        //         let inPlusOut = List.fold (fun acc w -> selectAlready 0 acc w) selectedSymbolInputs selectedSymbolOutputs //check whether the wires have already been selected 
        //         List.fold (fun acc w -> selectAlready 0 acc w) inPlusOut wireList
        //     else 
        //         wireList
        // let wModel, wCmd = BusWire.update (BusWire.DeleteWire newWSelectList) sModel
        // {model with Wire = wModel; IsSelecting = ([],[]); Restore = model.Wire}, Cmd.map Wire wCmd //update model and reset selecting 
        // model, Cmd.none
        //need to 
