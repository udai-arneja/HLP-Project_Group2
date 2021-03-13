module Sheet

open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    IsWiring: string * (string*string)   //Input/Output * (portId * portId)
    IsSelecting: string list * string list        //Symbols * Wires
    IsDropping: bool
    IsDraggingList: int * XYPos
    MultiSelectBox: bool * XYPos * XYPos  //boxOrWire,startPos, endPos multi-select box
    Restore: BusWire.Model
    LastOp: Helpers.MouseOp;
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL| Ctrl 

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg

//helper functions
let zoom = 1.0

let boundingBoxWithinSearchW startPos finalPos (wModel:BusWire.Model)=  //checks if wire bounding box within box 
    let innerLayer start fn bblst = 
        let innerSeg lst = 
            let (box1, box2) = lst
            {X = fn box1.X box2.X; Y = (fn box1.Y box2.Y)}            
        bblst 
        |> List.fold (fun acc y -> {X = (fn (innerSeg y).X acc.X); Y = (fn (innerSeg y).Y acc.Y)}) start

    let maxCoord = 
        wModel.wBB 
        |> List.map (innerLayer {X=0.;Y=0.} max)
    let minCoord =
        wModel.wBB 
        |> List.map (innerLayer {X=1000.;Y=1000.} min)
    List.filter (fun a -> maxCoord.[a].X <= finalPos.X &&  maxCoord.[a].Y <= finalPos.Y) [0..(wModel.wBB.Length-1)]
    |> List.filter (fun b -> minCoord.[b].X >= startPos.X && minCoord.[b].Y >= startPos.Y)
    |> List.map (fun c -> model.Wire.wBB.[c].Id)

// let inSelBox (model:BusWire.Model) (sc:XYPos) (ec:XYPos): (CommonTypes.ComponentId) list=     //sc : start corner, ec: end corner
//     let corners = if sc.X < ec.X     //dragging left to right
//                       then if sc.Y < ec.Y
//                            then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
//                            else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
//                       else if sc.Y > ec.Y    //dragging right to left
//                           then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
//                           else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down

//     let overlap (pos1,pos2,id) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
//                                     ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
//                                   then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
//                                           ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
//                                        then Some id
//                                        else None
//                                   else None
//     List.choose overlap model.BusWire.Symbol.Boxes

let wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : CommonTypes.ConnectionId list = //checks if point is in wire bounding box
    let isInside bblst wireId= //gives you the wire bb list 
        let inSeg ind lst = //list of bounding boxes 
            let (box1, box2) = 
                match lst,(ind%2) with 
                |(a,b),1 -> (b,a)
                |(a,b),0 -> (a,b)
            if (pos.X <= box1.X && pos.X >= box2.X) && (pos.Y <= box2.Y && pos.Y >= box1.Y) then
                (true, wireId) 
            else 
                (false, wireId)            
        bblst 
        |> List.mapi (fun i y -> inSeg i y) //
    
    let vertices = List.map (fun (i:BusWire.Wire) -> i.Vertices) wModel.WX
    //need to pattern match every wire
    let mapToBB = 
        wModel.wBB 
        |> List.mapi (fun i w -> isInside w wModel.WX.[i].Id)
        |> List.collect id
        |> List.filter (fun (x,y) -> x=true) 

    match mapToBB with 
    | [(true, wireId)] -> [wireId]
    | _ -> []

//display

let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / zoom ; Y = ev.clientY / zoom}})
    let (boxOrWire, startPos, endPos) = model.MultiSelectBox
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
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                [
                match boxOrWire with 
                | true ->  rect // to apear for selecting
                              [ 
                                  X startPos.X
                                  Y startPos.Y
                                  SVGAttr.Width (abs (endPos.X - startPos.X))
                                  SVGAttr.Height (abs (endPos.Y - startPos.Y))
                                  SVGAttr.FillOpacity "0.1"
                                  SVGAttr.Fill "blue"
                                  SVGAttr.Stroke "blue"
                                  SVGAttr.StrokeWidth 1
                              ][]   
                           svgReact
                     // the application code
                |_ -> svgReact

                ]
            ]
        ]

let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch model

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =

    match msg with
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}; Zoom = zoom}) ->
        let boundingBoxSearchS = match List.tryFind (fun (co1,co2,symId) -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Wire.Symbol.Boxes with
                                 | Some (pos1,pos2,iD) -> [iD]
                                 | None -> []

        let boundingBoxSearchW = wireToSelectOpt model Pos
      
        //mouseposition - used when dragging to make the multi-select box
        let mousePos = {X=mX;Y=mY}
        
        let boundingBoxSearchP symbolId = 
            let currentSymbol = List.Find (fun sym -> [sym.Id] = symbolId) model.Wire.Symbol.Symbols //I CANT READ THIS
            let dist (p1:Helpers.XYPos) (p2:XYPos) = sqrt((p1.X-p2.X)**2. + (p1.Y-p2.Y)**2.)
            let portCalculator portlist iOut = match List.tryFind (fun (prt:CommonTypes.Port) -> (dist prt.PortPos mousePos)<40.) portlist with
                                               | Some x -> ([x], iOut)
                                               | None -> ([], "null")
            if (mX <= symbolX + currentSymbol.W+20.)  && (mX >= symbolX + currentSymbol.W-10.) = true then //checks whether its an input or output 
                    portCalculator currentSymbol.OutputPorts "output"
            else (mX <= symbolX+10.)  && (mX >= symbolX-20.) = true then
                    portCalculator currentSymbol.InputPorts "input"

        match mouseState with
        | Down -> if model.IsDropping = true then {model with IsDropping=false}, Cmd.none
                  else match boundingBoxSearchS with
                       |[symId] -> match boundingBoxSearchP [symId] with
                                   | ([portId], iOut) -> match model.IsWiring with
                                                         |("null", (_,_) )-> if iOut="input" then {model with IsWiring = (iOut, (portId,"null"))},Cmd.none
                                                                             else {model with IsWiring = (iOut,("null", portId))},Cmd.none
                                                         | (IOlabel, (iPortId,oPortId)) -> match boundingBoxSearchS with
                                                                                           | [] -> {model with IsWiring=("null",("null","null"));LastOp=Down}, Cmd.none
                                                                                           | [symbolId] -> match boundingBoxSearchP [symbolId] with 
                                                                                                           |(portId,label) when IOlabel <> label && label="input" -> {model with IsWiring=("null",("null","null"));LastOp=Down}, Cmd.OfMsg (AddWire portId, oPortId)
                                                                                                           |(portId,label) when IOlabel <> label && label="output" -> {model with IsWiring=("null",("null","null"));LastOp=Down}, Cmd.OfMsg (AddWire iPortId, portId)
                                                                                                           | _ -> {model with IsWiring=("null",("null","null"));LastOp=Down}, Cmd.none
                                   | ([], "null") -> {model with IsSelecting = ([symId],[]); LastOp=Down}, Cmd.none
                       | [] -> match boundingBoxSearchW with 
                               |[wireId] -> {model with IsSelecting = ([],[wireId]); LastOp=Down}, Cmd.none
                               |[] -> {model with IsSelecting = ([],[]);LastOp=Down;MultiSelectBox=(true,mousePos,mousePos)}, Cmd.ofMsg (ToggleSelect([],[]))
                               
        | Up -> match model.LastOp with
                | Drag -> match model.MultiSelectBox with
                          |(true,p1,p2) -> {model with MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.});LastOp=Drag}, Cmd.ofMsg (ToggleSelect (inSelBox model p1 p2 , boundingBoxWithinSearchW model p1 p2) )//check if in bounding boxes
                          | _ -> {model with LastOp=Up}, Cmd.ofMsg (updateBBoxes model.IsSelecting)
                          // drag group/single 
                | Down -> {model with IsSelected = ([],[])}, Cmd.ofMsg (ToggleSelect model.IsSelecting)
                | _ -> {model with LastOp=Up}, Cmd.none

        | Drag -> match model.MultiSelectBox with 
                  |(true, p1, p2) -> {model with IsSelecting = ([],[]);LastOp=Drag;MultiSelectBox=(true,p1,mousePos)}, Cmd.none
                  | _ -> {model with LastOp = Drag}, Cmd.ofMsg (Drag model.IsSelecting)//send to symbol to move symbols lol

        | Move -> match model.IsWiring with 
                  |("null", _) -> match boundingBoxSearchS with
                                  | [iD] -> {model with LastOp = Move}, Cmd.ofMsg (hovering [iD])
                                  | [] -> {model with LastOp = Move}, Cmd.none
                  |(IOLabel,("null",portId)) -> {model with LastOp=Move}, Cmd.ofMsg (ShowValidPorts (IOlabel, portId, Pos ))
                  |(IOLabel, (portId, "null")) -> {model with LastOp=Move}, Cmd.ofMsg (ShowValidPorts (IOLabel, portId, Pos))

    |Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire //send message
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged

    | KeyPress CtrlS -> // add symbol and create a restore point
        let wModel, wCmd = BusWire.update (BusWire.Msg.Symbol (Symbol.AddSymbol({X=10.; Y=10.}, 1, 1))) model.Wire 
        {model with Wire = wModel; IsDropping = 1; Restore = model.Wire}, Cmd.map Wire wCmd
    
    |KeyPress DEL -> 
        let sModel, sCmd = BusWire.update (BusWire.Msg.Symbol (Symbol.DeleteSymbol symList)) model.Wire
        let newWSelectList = 
            if symList <> [] //if symbols have been selected then check whether wires connected also have to be deleted
            then
                let srcPorts = List.map (fun (x:BusWire.Wire) -> x.SrcPort) model.Wire.WX //take all the inputports ids from wires
                let tgtPorts = List.map (fun (x:BusWire.Wire) -> x.TargetPort) model.Wire.WX //take all the outputports ids from wires
                let symbolsList = 
                    List.map (fun x -> List.item x model.Wire.Symbol.Symbols) symList //collect all the selected symbols
                let selectedSymbolInputs = //check if portids in the wires and selected symbols match for input ports -> if so then take the index of the wire 
                    symbolsList
                    |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypes.Port) -> [y.Id]) x.InputPorts)
                    |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) srcPorts |> function |Some a -> [a] |None -> []) lst) 
                let selectedSymbolOutputs= //check if portids in the wires and selected symbols match for output ports -> if so then take the index of the wire 
                    symbolsList
                    |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypes.Port) -> [y.Id]) x.OutputPorts) 
                    |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) tgtPorts |> function |Some a -> [a] |None -> []) lst) 
                let inPlusOut = List.fold (fun acc w -> selectAlready 0 acc w) selectedSymbolInputs selectedSymbolOutputs //check whether the wires have already been selected 
                List.fold (fun acc w -> selectAlready 0 acc w) inPlusOut wireList
            else 
                wireList
        let wModel, wCmd = BusWire.update (BusWire.DeleteWire newWSelectList) sModel
        {model with Wire = wModel; IsSelecting = ((0,0),[],[]); Restore = model.Wire}, Cmd.map Wire wCmd //update model and reset selecting 
    
    | KeyPress AltZ -> {model with Wire = model.Restore; IsSelecting = ((0,0),[],[]); IsDragSelecting = (0, {X=0.;Y=0.}, {X=0.;Y=0.});IsDraggingList = (0, {X=0.;Y=0.}); IsDropping= (0); IsWiring = (0,"null", ("null","null")); Restore = model.Wire}, Cmd.none //undo and reset everything

    | KeyPress s -> 
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | _ -> CommonTypes.Grey
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)


let init() = 
    let model,cmds = (BusWire.init)() //initial model state
    {
        Wire = model
        IsWiring = ("null", ("null","null"))
        IsSelecting= ([], [])
        IsDropping= false
        IsDraggingList = (0, {X=0.;Y=0.})
        MultiSelectBox = (false, {X=0.;Y=0.}, {X=0.;Y=0.})
        Restore = model 
        LastOp = Move
    }, Cmd.map Wire cmds