module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes


//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

/// Model to generate one symbol (skeleton). Id is a unique Id
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.


type Symbol =
    {
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
        InputPorts : CommonTypes.Port list
        OutputPorts : CommonTypes.Port list
        Pos: XYPos
        H : float
        W : float
        IsSelected: bool
        PortStatus: string
        IsSliding: bool * string * int * XYPos
    }

type Model = {
    Symbols: Symbol list
    SymBBoxes: (XYPos*XYPos)  List
    DragMultipleOrSingle: bool * XYPos
    // MouseInfo : MouseT
    }


//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    //| StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    //| Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId list * pagePos: XYPos * prevPagePos: XYPos
    //| DraggingList of sId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    //| EndDragging of sId : CommonTypes.ComponentId
    //| EndDraggingList of sId : CommonTypes.ComponentId list *pagePos:XYPos
    | AddSymbol of inputs: int list * outputs: int list * comp: CommonTypes.ComponentType
    | DeleteSymbol
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | ToggleSymbol of selectedSymbol:CommonTypes.ComponentId list // usually one thing
    | Hovering of portSymbol:CommonTypes.ComponentId list
    | ShowValidPorts of inOut:string* portId:string * mousePos:XYPos
    | UpdateBBoxes of CommonTypes.ComponentId list
    // | SelectSymbol of Symbol list
    
//---------------------------------helper types and functions----------------//



let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let gateWidth = 60.

let gateHeight = 80.

let circleRadius = 5.

let inOutLines = 10.

let rectum xPos yPos width height colour props  = // cheeky bit of kareem abstraction
        rect
            [
                X xPos
                Y yPos
                SVGAttr.Width width
                SVGAttr.Height height
                SVGAttr.Fill colour
                SVGAttr.FillOpacity 0.4
                SVGAttr.Stroke "Black"
                SVGAttr.StrokeWidth 2
            ]
            [ ]
            

let circus xPos yPos rad = // cheeky bit of kareem abstraction
    circle
        [
            SVGAttr.Cx xPos
            SVGAttr.Cy yPos
            SVGAttr.R rad
            SVGAttr.Fill "Transparent"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeWidth 2
        ] [ ]
        
let homotextual xPos yPos textAnchor domBaseline fontSize displayText = // cheeky bit of kareem abstraction
    text
        [ X xPos
          Y yPos 
          Style
                [
                    TextAnchor textAnchor
                    DominantBaseline domBaseline
                    FontSize fontSize
                    FontWeight "Bold"
                    Fill "Black"
                ]
        ]
        [ str <| sprintf displayText ]

let creditLines x1Pos y1Pos x2Pos y2Pos width = // cheeky bit of kareem abstraction
    line
        [   X1 x1Pos
            Y1 y1Pos
            X2 x2Pos
            Y2 y2Pos
            Style
                [
                    CSSProp.Stroke "Black"
                    CSSProp.StrokeWidth width
                    
                ]
        ]
        []

//-----------------------------Skeleton Model Type for symbols----------------//


let createPortList (comp:Symbol)(portType:CommonTypes.PortType)(portNumber:int)(width:int)(numPorts): CommonTypes.Port =
    let portPos =
        match comp.Type with 
        // |RAM -> if portType = CommonTypes.Input 
        //            then {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float(portNumber + 1))*(comp.H/6.))}
        //            else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/2.))}
        // |NbitAdder -> if portType = CommonTypes.Input 
        //               then {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
        //               else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/3.))}
        |_ -> match (portType, numPorts) with 
              | (CommonTypes.Input, 1) -> {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.H/2.))}
              | (CommonTypes.Input, 2) -> {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float (portNumber))*2. + 1.)*(comp.H/4.)}
              | (CommonTypes.Input, 3) -> {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
              | (_, 1) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
              | (_, 2) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+ (float (portNumber))*2. + 1.)*(comp.H/4.)}  
              |_ -> failwithf "Error on portlist"
    {
        CommonTypes.Port.Id = Helpers.uuid()
        CommonTypes.Port.PortNumber = Some portNumber
        CommonTypes.Port.PortType = portType
        CommonTypes.Port.HostId = string(comp.Id)
        CommonTypes.Port.PortPos = portPos
        CommonTypes.Port.BusWidth = width
        // CommonTypes.Port.PortInUse = false
    }

//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (inputs: int list) (outputs: int list) (comp:CommonTypes.ComponentType) = //could match comp for symbols of different heights and widths
    let mainSymbol = {
                LastDragPos = {X=0.;Y=0.}
                IsDragging = false
                Id = CommonTypes.ComponentId (Helpers.uuid())
                Type = comp
                InputPorts = []
                OutputPorts = []
                Pos = {X=10.;Y=10.}
                H = 80.
                W = 60.
                IsSelected = false
                PortStatus = "invisible"
                IsSliding = (false, "input" , 0, {X=0.; Y=0.})
              }
    
    let InputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypes.PortType.Input index width (List.length inputs)) inputs
    let OutputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypes.PortType.Output index width (List.length outputs)) outputs
    {mainSymbol with InputPorts=InputPortsList; OutputPorts=OutputPortsList}


let createNewBoundingBox (inputs: int list) (outputs: int list)=
    ({X=0.;Y=0.},{X=80.;Y=60.})
    
    // +float(max (List.length inputs) (List.length outputs))*40.;Y=75.+float (max (List.length inputs) (List.length outputs))*40.})
    // [start.X-10., start.Y-10.; 110., start.Y-10.; 110., 75.+float (max inputno outputno)*40.; 75.+float (max inputno outputno)*40., 75.+float (max inputno outputno)*40.]

let portmove portId inputYes model =
    let findPort i (acc: CommonTypes.Port list) (x:Symbol)  =  match i with
                                                               |1 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.InputPorts |> function |Some a -> [a] |None -> []) acc
                                                               |0 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.OutputPorts |> function |Some a -> [a] |None -> []) acc
                                                               | _ -> failwithf "not implemented - findPort Function, Symbol line 152"
    let portReturn = match inputYes with //haaate this
                     | "input" -> List.fold (findPort 1) [] model |> List.head // potentially global for symbol
                     | "output" -> List.fold (findPort 0) [] model |> List.head
                     | _ -> failwithf "not implemented - portReturn Function, Symbol line 155"
    let symbolReturn = List.find (fun x -> x.Id = CommonTypes.ComponentId portReturn.HostId) model
    let portNumber = match portReturn.PortNumber with
                     |Some a -> a
                     | _ -> failwithf "not implemented - portNumber Function, Symbol line 159"
    (symbolReturn, portReturn, portNumber)

let init() =
    {Symbols=[]; SymBBoxes =[]; DragMultipleOrSingle = (false, {X=0.; Y=0.})}, Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol(inputno, outputno, compType) ->
        let newSymbols = List.rev (createNewSymbol inputno outputno compType :: model.Symbols)
        let newSymbolsBoundingBoxes = List.rev (createNewBoundingBox inputno outputno :: model.SymBBoxes)
        {model with Symbols=newSymbols; SymBBoxes=newSymbolsBoundingBoxes} , Cmd.none

    | Dragging (sId, pagePos, prevPagePos) ->
        //let updatePorts pType xy mainS no=
        //    if pType = "Input" then {X=fst xy;Y=(snd xy+65.+(float no)*40.)}
        //    else {X=fst xy+mainS.W - 10.;Y=(snd xy+65.+(float no)*40.)}
        let singleOrMultipleDragBool =
            model.Symbols 
            |> List.exists (fun sym -> sId = [sym.Id] && sym.IsSelected = false)

        let diff = posDiff pagePos prevPagePos

        let dSymbols=
            let symFunction (newSym: Symbol) = if singleOrMultipleDragBool=true 
                                               then newSym.IsSelected = false 
                                               else sId <> [newSym.Id]
            model.Symbols
            |> List.map (fun sym ->
                if symFunction sym = false then
                    sym
                else //check whether symbol is selected
                    { sym with
                        Pos = posAdd sym.Pos diff
                        IsDragging = true
                        LastDragPos = pagePos
                        InputPorts = List.map (fun port -> {port with PortPos = posAdd port.PortPos diff}) sym.InputPorts
                        OutputPorts = List.map (fun port -> {port with PortPos = posAdd port.PortPos diff}) sym.OutputPorts
                    }
            )  
        
        {model with Symbols=dSymbols; DragMultipleOrSingle = (singleOrMultipleDragBool, diff) }, Cmd.none  

             //; SymBBoxes = updateSymBBoxesox
    
    | UpdateBBoxes (sId) ->
        let (singleDragBool , diff) = model.DragMultipleOrSingle
        let newSymbols, newBox =
            if singleDragBool = true then  
                List.map2 (fun sym box -> if sId <> [sym.Id] then (sym, box) else ({sym with IsDragging = false} , (posAdd (fst box) diff, posAdd (snd box) diff))) model.Symbols model.SymBBoxes
                |> List.unzip
            else
                List.map2 (fun sym box -> if sym.IsSelected = false then (sym, box) else ({sym with IsDragging = false}, (posAdd (fst box) diff, posAdd (snd box) diff))) model.Symbols model.SymBBoxes
                |> List.unzip
                        
        {model with SymBBoxes = newBox; Symbols = newSymbols}, Cmd.none 
    
    | DeleteSymbol ->
        let (remainingSymbols, remainingBBox) =
             List.fold2 (fun remainingValues sym box -> if sym.IsSelected = false 
                                                        then remainingValues @ [(sym, box)] 
                                                        else remainingValues ) [] model.Symbols model.SymBBoxes
             |> List.unzip
        {model with Symbols=remainingSymbols; SymBBoxes = remainingBBox}, Cmd.none

        // let selectedList = 
        //     let checkSymbol (wiresList, bBoxesList) (wireTest) (boundingBox:XYPos*XYPos)= 
        //         if wireTest.IsSelected = true
        //         then (wireTest::wiresList, bBoxesList@[boundingBox])
        //         else (wiresList, bBoxesList)
        //     List.fold2 checkSymbol ([],[]) model.Symbols model.SymBBoxes
        // let remainingSymbols = fst selectedList
        // let remainingBbox = snd selectedList
        // {model with Symbols=remainingSymbols ; SymBBoxes=remainingBbox}, Cmd.none

        //need to do the delete properly
        // let symbolsToKeepIndex (lst:int) = List.filter (fun x -> List.tryFind (fun y -> y = x) sIdList |> function |Some a -> false |None -> true) [0..lst]
        // let dSymbols =
        //      symbolsToKeepIndex ((model.Symbols.Length)- 1)
        //      |> List.map (fun i -> model.Symbols.[i]) // (fun index value ->  List.tryFind (fun x -> x = index) sIdList |> function |Some a -> [] |None -> [value])
        // let dBbox =
        //     symbolsToKeepIndex ((model.SymBBoxes.Length)- 1)
        //     |> List.map (fun i -> model.SymBBoxes.[i])
        // {model with Symbols = dSymbols; SymBBoxes = dBbox}, Cmd.none

    | ToggleSymbol (sId) ->
        let selectedSymbolList =
            List.map (fun sym -> if sId = [sym.Id] 
                                 then {sym with IsSelected = not sym.IsSelected} 
                                 else sym ) model.Symbols
        {model with Symbols = selectedSymbolList}, Cmd.none

    |Hovering (sId) ->
        let showPortSymbols =
            model.Symbols
            |> List.map (fun sym -> if [sym.Id] = sId then { sym with PortStatus = "visible"}  else { sym with PortStatus = "invisible"})
        {model with Symbols = showPortSymbols}, Cmd.none

    |ShowValidPorts (iO, portId, posi) ->
        let validPortSymbols =
            match portmove portId iO model.Symbols with
            | (symb, port, portNum) -> List.map (fun x -> if x.Id = symb.Id then { x with IsSliding = (true, iO, portNum, posi); PortStatus = iO}  else { x with PortStatus = iO; IsSliding = (false, iO, portNum, posi)}) model.Symbols
        {model with Symbols =  validPortSymbols}, Cmd.none

    | MouseMsg {Pos = {X=posX; Y=posY}; Op = Down} ->
        let showPorts =
            model.Symbols
            |> List.map (fun x -> { x with PortStatus = "invisible"; IsSliding = (false, "input" , 0, {X=0.; Y=0.})})
        {model with Symbols = showPorts}, Cmd.none
    | _ -> failwith "Not Implemented, Symbol Update Function, Symbol line 299" // allow unused mouse messags


type private RenderSymbolProps =
    {
        Symb : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
        Comp : CommonTypes.ComponentType
    }

/// View for one symbol with caching for efficient execution when input does not change


let private RenderSymbol (comp: CommonTypes.ComponentType)=
    let renderPorts (portVisibility:CommonTypes.PortVisibility) num sym =
        match portVisibility with
        | Visible -> 
                       circus sym.OutputPorts.[num].PortPos.X  sym.OutputPorts.[num].PortPos.Y 1.
                    //    ;
                    //    circus sym.InputPorts.[int num].PortPos.X  sym.InputPorts.[int num].PortPos.Y 5.
        | Invisible -> circus sym.OutputPorts.[int num].PortPos.X  sym.OutputPorts.[int num].PortPos.Y 5.
        | OutputPorts -> circus sym.OutputPorts.[int num].PortPos.X  sym.OutputPorts.[int num].PortPos.Y 5.
        | InputPorts -> circus sym.InputPorts.[int num].PortPos.X  sym.InputPorts.[int num].PortPos.Y 5. 
        // let individiualPorts = 
        //     let (slide, IO, slidePortNum, {X=xSlide; Y = ySlide}) = sym.IsSliding
        //     let slideCirc =
        //         let portList =
        //             if IO = "input" then sym.InputPorts.[(int num)].PortPos
        //             else sym.OutputPorts.[(int num)].PortPos
        //         [
        //                  circus xSlide ySlide 5. 
        //                  line [
        //                      X1 portList.X   //fst portList)
        //                      Y1 portList.Y   //(snd portList)
        //                      X2 xSlide
        //                      Y2 ySlide
        //                      SVGAttr.StrokeDasharray "4"
        //                      // Qualify these props to avoid name collision with CSSProp
        //                      SVGAttr.Stroke "black"
        //                      SVGAttr.StrokeWidth 5 ] []
        //         ]

        //     let inPorts =
        //         [
        //            circus sym.InputPorts.[int num].PortPos.X  sym.InputPorts.[int num].PortPos.Y 5.  
                
        //         ]
        //     let outPorts=
        //         [
        //             circus sym.OutputPorts.[int num].PortPos.X  sym.OutputPorts.[int num].PortPos.Y 5.
        //             //rect [
        //             //    X props.Square.OutputPorts.[int num].PortPos.X      //(fst props.Square.OutputPorts.[int num].PortPos)
        //             //    Y props.Square.OutputPorts.[int num].PortPos.Y      //(snd props.Square.OutputPorts.[int num].PortPos)
        //             //    SVGAttr.Width 10.
        //             //    SVGAttr.Height 10.
        //             //    SVGAttr.Fill "black"
        //             //    SVGAttr.Stroke "black"
        //             //    SVGAttr.StrokeWidth 1
        //             //][]
        //         ]

        //     let portSection =
        //         match (portVisibility, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
        //         |("visible", _, _, _ ) -> inPorts @ outPorts 
        //         |(_, true, slidePortNum, _) -> slideCirc // for valid ports but the port that slides for a sliding output
        //         |(_, true, slidePortNum, "output") -> inPorts
        //         |(_, true, slidePortNum, "input") -> outPorts//for normal showing ports when nearby
        //         |("input",false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs
        //         |("output", false,_,_ ) -> inPorts
        //         |_ -> []

        //     match portVisibility with
        //     |"invisible" -> []
        //     |_ -> [portSection]

        // List.map (outputPorts sym.PortStatus) [(0.)..(float (sym.OutputPorts.Length-1))]
        // |> List.append (List.map (outputPorts sym.PortStatus) [(0.)..(float (sym.InputPorts.Length-1))])
        // |> List.collect (fun x -> x)
        // |> List.collect (fun x->x)


    match comp with
    // | Input bits | Output bits ->
    //     FunctionComponent.Of(
    //         fun (props : RenderSymbolProps) ->
    //             let color =
    //                 if props.Symb.IsDragging then
    //                     "green"
    //                 else
    //                     "grey"
    //             g   []
    //                 [
    //                     rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth/3.) (gateHeight/4.) color

    //                 ]
    //     )
    | Not | And | Or | Xor | Nand | Nor | Xnor->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                // let handleMouseMove =
                //     Hooks.useRef(fun (ev : Types.Event) ->
                //         let ev = ev :?> Types.MouseEvent
                //         // x,y coordinates here do not compensate for transform in Sheet
                //         // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                //         Dragging([props.Symb.Id], (posOf ev.pageX ev.pageY))
                //         |> props.Dispatch
                //     )
                let color =
                    if props.Symb.IsSelected then
                        "green"
                    else
                        "grey"
                g   [ 
                        // OnMouseUp (fun ev ->
                        //     document.removeEventListener("mousemove", handleMouseMove.current)
                        //     // EndDragging props.Square.Id
                        //     // |> props.Dispatch
                        // )
                        // OnMouseDown (fun ev ->
                        //     // See note above re coords wrong if zoom <> 1.0
                        //     // StartDragging (props.Square.Id, posOf ev.pageX ev.pageY)
                        //     // |> props.Dispatch
                        //     document.addEventListener("mousemove", handleMouseMove.current)
                        // )
                    ]
                    [   rectum props.Symb.Pos.X props.Symb.Pos.Y gateWidth gateHeight color props

                        match props.Comp with
                            | Nor | Not | Nand | Xnor ->
                                circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius
                            | _ ->
                                circus 0 0 0
                                
                        match props.Comp with
                            | Not -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "1"
                            | And -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&"
                            | Or -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1"
                            | Xor -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1"
                            | Nand -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&"
                            | Nor -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1"
                            | Xnor -> homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1"
                            | _ -> homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"
                        
                        match props.Comp with
                        | Not ->
                            homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X0"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) props.Symb.Pos.X (props.Symb.Pos.Y + gateHeight/2.) 2
                            renderPorts Visible 1 props.Symb
                        | _ ->
                            homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2
                            renderPorts Visible 1 props.Symb
                            homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2
                            
                        
                        match props.Comp with
                        | And | Or | Xor ->
                            creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2
                        | _ ->
                            creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2
                            
                    ]

        )
    | Mux2 | Demux2 ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        match props.Comp with
                        | Mux2 ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "MUX"
                            
                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X1"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2//Mux output
                            

                            
                        | Demux2 ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DEMUX"

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "Middle" "10px" "Y0"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output
                            
                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "end" "Middle" "10px" "Y1"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2//Mux output

                        | _ ->
                            homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "X0"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "EN"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2

          
                    ]

        )
    | NbitsAdder bits ->
            FunctionComponent.Of(
                fun (props : RenderSymbolProps) ->

                    let color =
                        if props.Symb.IsDragging then
                            "green"
                        else
                            "grey"
                    g   [ 
                        ]
                        [
                            rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                            text
                                [   X (props.Symb.Pos.X + gateWidth) 
                                    Y (props.Symb.Pos.Y + gateHeight/8.)
                                    Style
                                        [
                                                TextAnchor "middle"
                                                DominantBaseline "middle"
                                                FontSize "14px"
                                                FontWeight "Bold"
                                                Fill "Black"
                                            ]
                                ]
                                [ str <| sprintf "Adder(%A:0)" bits ]

                            //inputs
                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Cin"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "A"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "B"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                            //outputa
                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "end" "Middle" "10px" "Sum"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/3.) 2//Mux output

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(3./2.)) "end" "Middle" "10px" "Cout"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(3./2.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(3./2.)) 2//Mux output
                            

                            ]
                                   
        )
    | MergeWires ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [   // first pure vertical line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 
                        // Second horizontal line top of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y) 2 
                        // Third horizontal line bottom of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y + gateHeight/3.) 2 
                        // 4th line in the middle; output line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y + gateHeight/6.) 2 
                                              

                        ]           
        )
    | SplitWire bits ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                   
                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                     
                    [   //let formatted = String.Format ("(1:{0})", bits) wtf

                    //    homotextual (props.Symb.Pos.X) (props.Symb.Pos.Y - gateHeight/8.) "middle" "middle" "14px" formatted
                        text
                            [ X (props.Symb.Pos.X ) 
                              Y (props.Symb.Pos.Y - gateHeight/8.)
                              Style
                                    [
                                         TextAnchor "middle"
                                         DominantBaseline "middle"
                                         FontSize "14px"
                                         FontWeight "Bold"
                                         Fill "Black"
                                     ]
                            ] 
                            [str <| sprintf "(1:%A)" bits]


                        // first pure vertical line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 
                        // Second horizontal line top of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y) 2 
                        // Third horizontal line bottom of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y + gateHeight/3.) 2 
                        // 4th line in the middle; output line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y + gateHeight/6.) 2 

                       

                        ]
                        

                
        )
    | DFF | DFFE ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props


                        match props.Comp with
                        | DFF ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFF"
                          
                            
                        | DFFE ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFFE"

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2
               
                        | _ ->
                            homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "D"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Q"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output


                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK
                        

                ]

        )
    | Register bits | RegisterE bits->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props
                        
                        
                        match props.Comp with
                        | Register bits ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "Register"
                                                  
                        
                                                    
                        | RegisterE bits ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RegisterE"
                        
                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2
                                       
                        | _ ->
                            homotextual 0 0 "" "" "" ""
                        
                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2
                        
                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output
                        
                        
                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK
                                                
                        
                       
                ]

        )
    | ROM memorySize | AsyncROM memorySize ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        match props.Comp with
                        | ROM memorySize ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "ROM"

                            homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                            line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
                            line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting

                                                  
                        
                                                    
                        | AsyncROM memorySize ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "AsyncROM"
                        
                                       
                        | _ ->
                            homotextual 0 0 "" "" "" ""
                        
   
                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Addr"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


                       
                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK

                        
                       
                ]

        )
    | RAM memorySize ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ 
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RAM"
                       
   
                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/6.) "start" "middle" "10px" "Addr"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) 2 // CLK


                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "start" "middle" "10px" "Data-in"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 // CLK

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Write"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK

       
                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting

                ]
        )
    |_-> failwithf"not yet implemented"







//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Circle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
/// View function for symbol layer of SVG

// let view (model : Model) (dispatch : Msg -> unit) =
//     //model
//     //|> List.map (fun ({Id = CommonTypes.ComponentId id} as circle) ->
//     //    renderCircle
//     //        {
//     //            Circle = circle
//     //            Dispatch = dispatch
//     //            key = id
//     //        }
//     //)
//     //|> ofList
//     model.Symbols
//     |> List.map (fun ({Id = CommonTypes.ComponentId ii} as custom) -> //match each symbol with its t
//         renderCustom
//             {
//                 Custom = custom
//                 Dispatch = dispatch
//                 key = ii // to make it string by type matching
//             }
//     )
//     |> ofList

let view (model : Model) (dispatch : Msg -> unit) = 
    model.Symbols
    |> List.map (fun rect ->
        RenderSymbol rect.Type
            {
            Symb = rect // name works for the demo!
            Dispatch = dispatch
            key= (string) rect.Id // special field used by react to detect whether lists have changed, set to symbol Id
            Comp = rect.Type
            }
    )
    |> ofList

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.Pos)

let inputPortList (symModel: Model) (sId: CommonTypes.ComponentId) : CommonTypes.Port list =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.InputPorts)

let outputPortList (symModel: Model) (sId: CommonTypes.ComponentId) : CommonTypes.Port list =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.OutputPorts)

let inputPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (pId: CommonTypes.InputPortId) : XYPos =
    let portList = inputPortList symModel sId

    List.find (fun (por:CommonTypes.Port) -> por.Id = string pId) portList
    |> (fun por -> por.PortPos)

let outputPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (pId: CommonTypes.OutputPortId) : XYPos =
    let portList = outputPortList symModel sId

    List.find (fun (por:CommonTypes.Port) -> por.Id = string pId) portList
    |> (fun por -> por.PortPos)

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth
        (wId: CommonTypes.ConnectionId)
        (outputPortNumber: int)
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent
        (symModel: Model)
        (sId:CommonTypes.ComponentId) : CommonTypes.Component =
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list =
    failwithf "Not implemented"



//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
    /// these are all the different types of symbols we can have here 





    // type RenderSquareProps =
    //     {
    //         Square: Symbol
    //         Dispatch: Dispatch<Msg>
    //         key:string
    //     }
    
    // let renderSquare =
    //     FunctionComponent.Of(
    //         fun (props : RenderSquareProps) ->
    //             let handleMouseMove =
    //                 Hooks.useRef(fun (ev : Types.Event) ->
    //                     let ev = ev :?> Types.MouseEvent
    //                     // x,y coordinates here do not compensate for transform in Sheet
    //                     // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
    //                     Dragging(props.Square.Id, posOf ev.pageX ev.pageY)
    //                     |> props.Dispatch
    //                 )
    //             let color =
    //                 if props.Square.IsDragging && not props.Square.IsSelected then
    //                     "lightblue"
    //                 else if props.Square.IsSelected then
    //                     "red"
    //                 else
    //                     "grey"
    
    //             let outputText inOrOutText portStat num =
    //                 let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Square.IsSliding
    //                 let textSection =
    //                     [
    //                         text [
    //                             X (props.Square.Pos.X + inOrOutText)
    //                             Y (props.Square.Pos.Y + 65. + num*40.);
    //                             Style [
    //                                 TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
    //                                 DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
    //                                 FontSize "20px"
    //                                 FontWeight "Bold"
    //                                 Fill "Black" // demo font color
    
    //                             ]
    //                         ] [str <| string num]
    //                     ]
    //                 let slideRect =
    //                     let portList =
    //                         if IO = "input" then props.Square.InputPorts.[(int num)].PortPos
    //                         else props.Square.OutputPorts.[(int num)].PortPos
    //                     [
    //                         rect [
    //                               X (xSlide)
    //                               Y (ySlide)
    //                               SVGAttr.Width 10.
    //                               SVGAttr.Height 10.
    //                               SVGAttr.Fill "black"
    //                               SVGAttr.Stroke "black"
    //                               SVGAttr.StrokeWidth 1
    //                           ][]
    //                         line [
    //                             X1 portList.X
    //                             Y1 portList.Y
    //                             X2 xSlide
    //                             Y2 ySlide
    //                             SVGAttr.StrokeDasharray "4"
    //                             // Qualify these props to avoid name collision with CSSProp
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 5 ] []
    //                     ]
    
    //                 let inPorts =
    //                     [
    //                       rect [
    //                             X props.Square.InputPorts.[int num].PortPos.X   //(fst props.Square.InputPorts.[int num].PortPos)
    //                             Y props.Square.InputPorts.[int num].PortPos.Y   //(snd props.Square.InputPorts.[int num].PortPos)
    //                             SVGAttr.Width 10.
    //                             SVGAttr.Height 10.
    //                             SVGAttr.Fill "black"
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    //                     ]
    //                 let outPorts=
    //                     [
    //                         rect [
    //                             X props.Square.OutputPorts.[int num].PortPos.X      //(fst props.Square.OutputPorts.[int num].PortPos)
    //                             Y props.Square.OutputPorts.[int num].PortPos.Y      //(snd props.Square.OutputPorts.[int num].PortPos)
    //                             SVGAttr.Width 10.
    //                             SVGAttr.Height 10.
    //                             SVGAttr.Fill "black"
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    //                     ]
    
    //                 let portSection =
    //                     match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
    //                     |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
    //                     |("visible", 70., _, _, _ ) -> outPorts
    //                     | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs
    //                     |("output", 20., false,_,_ ) -> inPorts
    //                     |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output
    //                     |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides
    //                     |(_, 20., true, slidePortNum, "output") -> inPorts
    //                     |(_, 70., true, slidePortNum, "input") -> outPorts
    //                     |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides
    //                     |("output", 20., true,_,_ ) -> inPorts
    //                     |_ -> []
    
    //                 match portStat with
    //                 |"invisible" -> [textSection]
    //                 |_ -> [textSection; portSection]
    
    //             let initialoutline =
    //                 [
    //                     rect
    //                         [
    //                             OnMouseUp (fun ev ->
    //                                 document.removeEventListener("mousemove", handleMouseMove.current)
    //                                 EndDragging props.Square.Id
    //                                 |> props.Dispatch
    //                             )
    //                             OnMouseDown (fun ev ->
    //                                 // See note above re coords wrong if zoom <> 1.0
    //                                 StartDragging (props.Square.Id, posOf ev.pageX ev.pageY)
    //                                 |> props.Dispatch
    //                                 document.addEventListener("mousemove", handleMouseMove.current)
    //                             )
    //                             X props.Square.Pos.X
    //                             Y props.Square.Pos.Y
    //                             SVGAttr.Width props.Square.W
    //                             SVGAttr.Height props.Square.H
    //                             SVGAttr.Fill color
    //                             SVGAttr.Stroke color
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    
    //                     text [ // a demo text svg element
    //                             X (props.Square.Pos.X + 30.);
    //                             Y (props.Square.Pos.Y + 10.);
    //                             Style [
    //                                 TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
    //                                 DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
    //                                 FontSize "30px"
    //                                 FontWeight "Bold"
    //                                 Fill "Black" // demo font color
    //                             ]
    //                          ] [str <| sprintf "Not"]
    //                 ]
    //             let reactElementList =
    //                 List.map (outputText 70. props.Square.PortStatus) [(0.)..(float (props.Square.OutputPorts.Length-1))]
    //                 |> List.append (List.map (outputText 20. props.Square.PortStatus) [(0.)..(float (props.Square.InputPorts.Length-1))])
    //                 |> List.collect (fun x -> x)
    //                 |> List.collect (fun x->x)
    //                 |>List.append initialoutline
    //             g   [ Style [
    //                     ]
    //                 ] (reactElementList)
    //     , "Square"
    //     , equalsButFunctions
    //     )

    // type RenderCustomProps =
    //     {
    //         Custom: Symbol
    //         Dispatch: Dispatch<Msg>
    //         key:string
    //     }


    // let renderCustom =
    //     FunctionComponent.Of(
    //         fun (props : RenderCustomProps) ->
              
    //             let color =
    //                 if props.Custom.IsDragging && not props.Custom.IsSelected then
    //                     "lightblue"
    //                 else if props.Custom.IsSelected then
    //                     "red"
    //                 else
    //                     "grey"
    
    //             let outputText inOrOutText portStat num =
    //                 let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Custom.IsSliding
    //                 let textSection =
    //                     [
    //                         text [
    //                             X (props.Custom.Pos.X + inOrOutText)
    //                             Y (props.Custom.Pos.Y + 65. + num*40.);
    //                             Style [
    //                                 TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
    //                                 DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
    //                                 FontSize "20px"
    //                                 FontWeight "Bold"
    //                                 Fill "Black" // demo font color
    
    //                             ]
    //                         ] [if inOrOutText = 20. then str <| (string (match (props.Custom.Type) with
    //                                                                      | CommonTypes.Custom customSymbol -> fst customSymbol.InputLabels.[int num]
    //                                                                      | _ -> failwithf "Not Implemented - Custom Component, Symbol line 678"))
    //                            else if inOrOutText = 70. then str <| (string (match (props.Custom.Type) with
    //                                                                           | CommonTypes.Custom customSymbol -> fst customSymbol.OutputLabels.[int num]
    //                                                                           | _ -> failwithf "Not Implemented - Custom Component, Symbol line 678"))]
    //                     ]
    //                 let slideRect =
    //                     let portList =
    //                         if IO = "input" then props.Custom.InputPorts.[(int num)].PortPos else props.Custom.OutputPorts.[(int num)].PortPos
    //                     [
    //                         circus xSlide ySlide 5. 
    //                         line [
    //                             X1 portList.X   //fst portList)
    //                             Y1 portList.Y   //(snd portList)
    //                             X2 xSlide
    //                             Y2 ySlide
    //                             SVGAttr.StrokeDasharray "4"
    //                             // Qualify these props to avoid name collision with CSSProp
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 5 ] []
    //                     ]
    
    //                 let inPorts =
    //                     [
    //                       rect [
    //                             X props.Custom.InputPorts.[int num].PortPos.X   //(fst props.Custom.InputPorts.[int num].PortPos)
    //                             Y props.Custom.InputPorts.[int num].PortPos.Y   //(snd props.Custom.InputPorts.[int num].PortPos)
    //                             SVGAttr.Width 10.
    //                             SVGAttr.Height 10.
    //                             SVGAttr.Fill "black"
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    //                     ]
    //                 let outPorts=
    //                     [
    //                         rect [
    //                             X props.Custom.OutputPorts.[int num].PortPos.X  //(fst props.Custom.OutputPorts.[int num].PortPos)
    //                             Y props.Custom.OutputPorts.[int num].PortPos.Y  //(snd props.Custom.OutputPorts.[int num].PortPos)
    //                             SVGAttr.Width 10.
    //                             SVGAttr.Height 10.
    //                             SVGAttr.Fill "black"
    //                             SVGAttr.Stroke "black"
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    //                     ]
    
    //                 let portSection =
    //                     match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
    //                     |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
    //                     |("visible", 70., _, _, _ ) -> outPorts
    //                     | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs
    //                     |("output", 20., false,_,_ ) -> inPorts
    //                     |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output
    //                     |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides
    //                     |(_, 20., true, slidePortNum, "output") -> inPorts
    //                     |(_, 70., true, slidePortNum, "input") -> outPorts
    //                     |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides
    //                     |("output", 20., true,_,_ ) -> inPorts
    //                     |_ -> []
    
    //                 match portStat with
    //                 |"invisible" -> [textSection]
    //                 |_ -> [textSection; portSection]
    
    //             let initialoutline =
    //                 [
    //                     rect
    //                         [
    //                             OnMouseUp (fun ev ->
    //                                 document.removeEventListener("mousemove", handleMouseMove.current)
    //                                 EndDragging props.Custom.Id
    //                                 |> props.Dispatch
    //                             )
    //                             OnMouseDown (fun ev ->
    //                                 // See note above re coords wrong if zoom <> 1.0
    //                                 StartDragging (props.Custom.Id, posOf ev.pageX ev.pageY)
    //                                 |> props.Dispatch
    //                                 document.addEventListener("mousemove", handleMouseMove.current)
    //                             )
    //                             X props.Custom.Pos.X
    //                             Y props.Custom.Pos.Y
    //                             SVGAttr.Width props.Custom.W
    //                             SVGAttr.Height props.Custom.H
    //                             SVGAttr.Fill color
    //                             SVGAttr.Stroke color
    //                             SVGAttr.StrokeWidth 1
    //                         ][]
    
    //                     text [ // a demo text svg element
    //                             X (props.Custom.Pos.X + 30.);
    //                             Y (props.Custom.Pos.Y + 10.);
    //                             Style [
    //                                 TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
    //                                 DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
    //                                 FontSize "30px"
    //                                 FontWeight "Bold"
    //                                 Fill "Black" // demo font color
    //                             ]
    //                          ] [str <| sprintf "Custom"]
    //                 ]
    //             let reactElementList =
    //                 List.map (outputText 70. props.Custom.PortStatus) [(0.)..(float (props.Custom.OutputPorts.Length-1))]
    //                 |> List.append (List.map (outputText 20. props.Custom.PortStatus) [(0.)..(float (props.Custom.InputPorts.Length-1))])
    //                 |> List.collect (fun x -> x)
    //                 |> List.collect (fun x->x)
    //                 |>List.append initialoutline
    //             g   [ Style [
    //                     ]
    //                 ] (reactElementList)
    //     , "Custom"
    //     , equalsButFunctions
    //     )
    
    



        //| DraggingList (rank, pagePos, prevPagePos) ->
    //    let updatePorts pType xy mainS no=
    //        if pType = "Input" then {X=fst xy;Y=(snd xy+65.+(float no)*40.)}
    //        else {X=fst xy+mainS.W - 10.;Y=(snd xy+65.+(float no)*40.)}
    //    let newSym sym =
    //        let diff = posDiff pagePos prevPagePos
    //        { sym with
    //            Pos = posAdd sym.Pos diff
    //            LastDragPos = pagePos
    //            InputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Input" ((posAdd sym.Pos diff).X, (posAdd sym.Pos diff).Y) sym num}) sym.InputPorts
    //            OutputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Output" ((posAdd sym.Pos diff).X, (posAdd sym.Pos diff).Y) sym num}) sym.OutputPorts
    //        }
    //    let dSymbols =
    //        model.Symbols
    //        |> List.map (fun sym -> (List.tryFind (fun k -> k = sym.Id) rank) |> function |Some a -> newSym sym |None -> sym)


        //this is going to be a separate function that will be called by Sheet:

        // let updateSymBBoxesox =
        //     let indexforBbox = List.map (fun k -> List.findIndex (fun w -> w.Id = k) model.Symbols) rank
        //     let updateBBox index boxList =
        //         let diff2 = posDiff pagePos model.Symbols.[index].LastDragPos
        //         let {X = correctX; Y= correctY} =  posAdd (model.Symbols.[index].Pos) diff2
        //         List.tryFind (fun k -> k = index) indexforBbox
        //         |> function |Some a -> [correctX-10.,correctY-10.;correctX+10.+model.Symbols.[index].W, correctY-10.; correctX+10.+model.Symbols.[index].W, correctY+10. + model.Symbols.[index].H; correctX-10.,correctY+10.+ model.Symbols.[index].H] |None -> boxList
        //     List.mapi (fun i p -> updateBBox i p) model.SymBBoxes

        //{model with Symbols = dSymbols}, Cmd.none           //; SymBBoxes = updateSymBBoxesox

    //| EndDragging sId ->
    //    let edSymbols =
    //        model.Symbols
    //        |> List.map (fun sym ->
    //            if sId <> sym.Id then
    //                sym
    //            else
    //                { sym with
    //                    IsDragging = false
    //                }
    //        )
    //    {model with Symbols = edSymbols}, Cmd.none

    //|EndDraggingList (sId, pagePos) ->
    //    let edSymbols =
    //        model.Symbols
    //        |> List.map (fun sym -> (List.tryFind (fun k -> k = sym.Id) sId) |> function |Some a -> {sym with IsDragging = false; LastDragPos = pagePos} |None -> sym)
    //    {model with Symbols = edSymbols}, Cmd.none