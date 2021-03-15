module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
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
        //Type2: CommonTypes.CustomComponentType
        InputPorts : CommonTypes.Port list
        OutputPorts : CommonTypes.Port list
        Pos: XYPos
        H : float
        W : float
        IsSelected: bool
        PortStatus: string
        IsSliding: bool* string*int*XYPos
    }

type Model = {
    Symbols: Symbol list
    sBB: (float*float) list List
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
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    //| Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId  * pagePos: XYPos
    | DraggingList of sId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | EndDraggingList of sId : CommonTypes.ComponentId list *pagePos:XYPos
    | AddSymbol of pos: XYPos *inputs: int * outputs: int * comp: CommonTypes.ComponentType// used by demo code to add a circle
    | DeleteSymbol of int list
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | SelectSymbol of int list
    | ShowPorts of int list
    | ShowValidPorts of string*string*XYPos


//---------------------------------helper types and functions----------------//



let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let tupleToXYPos (a,b) : Helpers.XYPos = 
    let XY = {
        X = a
        Y = b
    }  
    {XY with X = a ; Y = b}


//-----------------------------Skeleton Model Type for symbols----------------//


let createportlist (mainS)(t)(compId)(x:int) =
    let portPos no pType =
        if pType = "Input" then
            {X=mainS.Pos.X; Y=(mainS.Pos.Y + 65. + (float no)*40.)}
        else
            {X=mainS.Pos.X+mainS.W  - 10.; Y=(mainS.Pos.Y + 65. + (float no)*40.)}
    [{
        CommonTypes.Port.Id = Helpers.uuid() 
        CommonTypes.Port.PortNumber = Some x 
        CommonTypes.Port. PortType = t 
        CommonTypes. Port. HostId = compId
        CommonTypes.Port.PortPos = portPos x (string t)
    }]

//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (start:XYPos) (inputno: int) (outputno: int) (comp:CommonTypes.ComponentType)= //could match comp for symbols of different heights and widths
    //match comp with 
    //| Not | And | Or | Xor | Nand | Nor | Xnor ->    
    let mainSymbol = {
                LastDragPos = {X=0.;Y=0.}
                IsDragging = false
                Id = CommonTypes.ComponentId (Helpers.uuid())
                Type = comp
                InputPorts = []
                OutputPorts = []
                Pos = start
                H = 65. + float (max inputno outputno)*40.
                W = 100.   
                IsSelected = false
                PortStatus = "invisible"
                IsSliding = (false, "input" , 0, {X=0.; Y=0.})
              }
    //| Mux2 | DeMux2 ->
      //   let mainSymbol = {
        //        LastDragPos = {X=0.;Y=0.}
          //      IsDragging = false
            //    Id = CommonTypes.ComponentId (Helpers.uuid())
              //  Type = CommonTypes.ComponentType.Not
                //InputPorts = []
                //OutputPorts = []
                //Pos = start
               // H = 65. + float (max inputno outputno)*40.
                //W = 100.   
                //IsSelected = false
                //PortStatus = "invisible"
                //IsSliding = (false, "input" , 0, {X=0.; Y=0.})
              //}          
    let InputPortsList = List.collect (fun x -> createportlist mainSymbol CommonTypes.PortType.Input (string mainSymbol.Id) x) [0..(inputno-1)]
    let OutputPortsList = List.collect (fun x -> createportlist mainSymbol CommonTypes.PortType.Output (string mainSymbol.Id) x) [0..(outputno-1)] 
    {mainSymbol with InputPorts = InputPortsList; OutputPorts = OutputPortsList}
 

let createNewBoundingBox (start:XYPos) (inputno: int) (outputno: int)=
    [start.X-10., start.Y-10.; 110., start.Y-10.; 110., 75.+float (max inputno outputno)*40.; 75.+float (max inputno outputno)*40., 75.+float (max inputno outputno)*40.]

let portmove portId inputYes model =
    let findPort i (acc: CommonTypes.Port list) (x:Symbol)  =  match i with 
                                                                      |1 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.InputPorts |> function |Some a -> [a] |None -> []) acc
                                                                      |0 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.OutputPorts |> function |Some a -> [a] |None -> []) acc
    let portReturn = match inputYes with //haaate this 
                    | "input" -> List.fold (findPort 1) [] model |> List.head // potentially global for symbol 
                    | "output" -> List.fold (findPort 0) [] model |> List.head
    let symbolReturn = List.find (fun x -> x.Id = CommonTypes.ComponentId portReturn.HostId) model    
    let portNumber = match portReturn.PortNumber with 
                     |Some a -> a
    (symbolReturn, portReturn, portNumber)
    

/// Dummy function for test. The real init would probably have no symbols.
//let init () =
//    List.allPairs [1;5] [3]
//    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
//    |> List.map createNewSymbol
//    , Cmd.none

let init() =
    {Symbols=[]; sBB =[]}, Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol(pos, inputno, outputno, comp) -> 
       {model with Symbols = List.rev (createNewSymbol pos inputno outputno comp:: model.Symbols); sBB = List.rev (createNewBoundingBox pos inputno outputno :: model.sBB)} , Cmd.none
    | StartDragging (sId, pagePos) ->
        let sdSymbols = 
            model.Symbols
            |> List.map (fun sym ->
                if sId <> sym.Id then
                    sym
                else
                    {sym with
                        LastDragPos = pagePos
                        IsDragging = true
                    }
            )
        {model with Symbols = sdSymbols}, Cmd.none

    | Dragging (rank, pagePos) ->
        let updatePorts pType (xy:XYPos) mainS no= 
            if pType = "Input" then
                {xy with Y = xy.Y + 65. + (float no)*40.}
            else
                {xy with X = xy.X+mainS.W - 10.; Y = xy.Y + 65. + (float no)*40.}
                
        let dSymbols = 
            model.Symbols
            |> List.map (fun sym ->
                if rank <> sym.Id then
                    sym
                else
                    let diff = posDiff pagePos sym.LastDragPos
                    { sym with
                        Pos = posAdd sym.Pos diff
                        LastDragPos = pagePos
                        InputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Input" {X=(posAdd sym.Pos diff).X; Y = (posAdd sym.Pos diff).Y} sym num}) sym.InputPorts
                        OutputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Output" {X=(posAdd sym.Pos diff).X; Y =(posAdd sym.Pos diff).Y} sym num}) sym.OutputPorts
                    }
            )
        let updatesBbox =
            let indexforBbox = List.findIndex (fun w -> w.Id = rank) model.Symbols
            let updateBBox index boxList =
                let diff2 = posDiff pagePos model.Symbols.[index].LastDragPos
                let {X = correctX; Y= correctY} =  posAdd (model.Symbols.[index].Pos) diff2 
                if index = indexforBbox then [correctX-10.,correctY-10.;correctX+10.+model.Symbols.[index].W, correctY-10.; correctX+10.+model.Symbols.[index].W, correctY+10. + model.Symbols.[index].H; correctX-10.,correctY+10.+ model.Symbols.[index].H] else boxList
            List.mapi (fun i p -> updateBBox i p) model.sBB
        {model with Symbols = dSymbols; sBB = updatesBbox}, Cmd.none

    | DraggingList (rank, pagePos, prevPagePos) ->
        let updatePorts pType (xy:XYPos) mainS no= 
            if pType = "Input" then
                {xy with Y = xy.Y + 65. + (float no)*40.}
            else
                {xy with X = xy.X+mainS.W - 10.; Y = xy.Y + 65. + (float no)*40.}
        let newSym sym =
            let diff = posDiff pagePos prevPagePos
            { sym with
                Pos = posAdd sym.Pos diff
                LastDragPos = pagePos
                InputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Input" {X=(posAdd sym.Pos diff).X; Y = (posAdd sym.Pos diff).Y} sym num}) sym.InputPorts
                OutputPorts = List.mapi (fun num port -> {port with PortPos = updatePorts "Output" {X=(posAdd sym.Pos diff).X; Y =(posAdd sym.Pos diff).Y} sym num}) sym.OutputPorts
            }
        let dSymbols = 
            model.Symbols
            |> List.map (fun sym -> (List.tryFind (fun k -> k = sym.Id) rank) |> function |Some a -> newSym sym |None -> sym)

            
        let updatesBbox =
            let indexforBbox = List.map (fun k -> List.findIndex (fun w -> w.Id = k) model.Symbols) rank
            let updateBBox index boxList =
                let diff2 = posDiff pagePos model.Symbols.[index].LastDragPos
                let {X = correctX; Y= correctY} =  posAdd (model.Symbols.[index].Pos) diff2
                List.tryFind (fun k -> k = index) indexforBbox 
                |> function |Some a -> [correctX-10.,correctY-10.;correctX+10.+model.Symbols.[index].W, correctY-10.; correctX+10.+model.Symbols.[index].W, correctY+10. + model.Symbols.[index].H; correctX-10.,correctY+10.+ model.Symbols.[index].H] |None -> boxList
            List.mapi (fun i p -> updateBBox i p) model.sBB
            
        {model with Symbols = dSymbols; sBB = updatesBbox}, Cmd.none

    | EndDragging sId ->
        let edSymbols = 
            model.Symbols
            |> List.map (fun sym ->
                if sId <> sym.Id then 
                    sym
                else
                    { sym with
                        IsDragging = false 
                    }
            )
        {model with Symbols = edSymbols}, Cmd.none

    |EndDraggingList (sId, pagePos) ->
        let edSymbols = 
            model.Symbols
            |> List.map (fun sym -> (List.tryFind (fun k -> k = sym.Id) sId) |> function |Some a -> {sym with IsDragging = false; LastDragPos = pagePos} |None -> sym)
        {model with Symbols = edSymbols}, Cmd.none 

    | DeleteSymbol (sIdList) ->
        let symbolsToKeepIndex (lst:int) = List.filter (fun x -> List.tryFind (fun y -> y = x) sIdList |> function |Some a -> false |None -> true) [0..lst]
        let dSymbols = 
             symbolsToKeepIndex ((model.Symbols.Length)- 1)
             |> List.map (fun i -> model.Symbols.[i]) // (fun index value ->  List.tryFind (fun x -> x = index) sIdList |> function |Some a -> [] |None -> [value]) 
        let dBbox =
            symbolsToKeepIndex ((model.sBB.Length)- 1)
            |> List.map (fun i -> model.sBB.[i])
        {model with Symbols = dSymbols; sBB = dBbox}, Cmd.none
    | SelectSymbol (sId) -> 
        let selectedSymbolList =
            let defaultList = List.map (fun x -> {x with IsSelected = false; IsDragging = false}) model.Symbols
            let checker x =
                let outcome = 
                    List.tryFind (fun w -> w = x) sId
                match outcome with 
                    |Some a -> {defaultList.[x] with IsSelected = true; IsDragging = false}
                    |None -> {defaultList.[x] with IsSelected = false; IsDragging = false}
            [0..(defaultList.Length-1)]
            |> List.map checker    
        {model with Symbols = selectedSymbolList}, Cmd.none
    |ShowPorts (sId) -> 
        let showPortSymbols = 
            model.Symbols
            |> List.mapi (fun ind x -> if [ind] = sId then { x with PortStatus = "visible"}  else { x with PortStatus = "invisible"})
        {model with Symbols = showPortSymbols}, Cmd.none
    |ShowValidPorts (iO, portId, posi) ->
        let validPortSymbols =
            match portmove portId iO model.Symbols with 
            | (symb, port, portNum) -> List.map (fun x -> if x.Id = symb.Id then { x with IsSliding = (true, iO, portNum, posi); PortStatus = iO}  else { x with PortStatus = iO; IsSliding = (false, iO, portNum, posi)}) model.Symbols
        {model with Symbols =  validPortSymbols}, Cmd.none
    | MouseMsg {Pos = {X = posX; Y=posY}; Op = Down} -> 
        let showPorts = 
            model.Symbols
            |> List.map (fun x -> { x with PortStatus = "invisible"; IsSliding = (false, "input" , 0, {X=0.; Y=0.})})
        {model with Symbols = showPorts}, Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

type Gates = //one for each unique shape
    | Not 
    | And 
    | Or 
    | Xor 
    | Nand 
    | Nor 
    | Xnor

type Mux = 
    | Mux2 
    | Demux2    

type RenderGateProps= 
    {
        Gate:Symbol
        Dispatch: Dispatch<Msg>
        key: string
    }  

let renderGate = 
    FunctionComponent.Of(
        fun (props : RenderGateProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Gate.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            let color =
                if props.Gate.IsDragging && not props.Gate.IsSelected then
                    "lightblue"
                else if props.Gate.IsSelected then
                    "red"
                else
                    "grey"

            let outputText inOrOutText portStat num = 
                let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Gate.IsSliding
                let textSection =
                    [
                        text [ 
                            X (props.Gate.Pos.X + inOrOutText)
                            Y (props.Gate.Pos.Y + props.Gate.H);
                            Style [
                                TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "20px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color

                            ]
                        ] [str <| string num]
                    ]
                let slideRect =
                    let portList =
                        if IO = "input" then props.Gate.InputPorts.[(int num)].PortPos else props.Gate.OutputPorts.[(int num)].PortPos
                    [
                        rect [
                              X (xSlide)
                              Y (ySlide)
                              SVGAttr.Width 10.
                              SVGAttr.Height 10.
                              SVGAttr.Fill "black"
                              SVGAttr.Stroke "black"
                              SVGAttr.StrokeWidth 1
                          ][]
                        line [
                            X1 (portList.X)
                            Y1 (portList.Y)
                            X2 xSlide
                            Y2 ySlide
                            SVGAttr.StrokeDasharray "4"
                            // Qualify these props to avoid name collision with CSSProp
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 5 ] []
                    ]

                let inPorts = 
                    [
                      circle [
                            Cx (props.Gate.InputPorts.[int num].PortPos.X)
                            Cy (props.Gate.InputPorts.[int num].PortPos.Y)
                            SVGAttr.R 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]
                let outPorts=
                    [
                        circle [
                            Cx (props.Gate.OutputPorts.[int num].PortPos.X)
                            Cy (props.Gate.OutputPorts.[int num].PortPos.Y)
                            SVGAttr.R 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]

                let portSection =
                    match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
                    |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
                    |("visible", 70., _, _, _ ) -> outPorts
                    | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs  
                    |("output", 20., false,_,_ ) -> inPorts
                    |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output 
                    |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides 
                    |(_, 20., true, slidePortNum, "output") -> inPorts
                    |(_, 70., true, slidePortNum, "input") -> outPorts
                    |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides 
                    |("output", 20., true,_,_ ) -> inPorts
                    |_ -> []
                   
                match portStat with 
                |"invisible" -> [textSection]
                |_ -> [textSection; portSection]

            let initialoutline = 
                [
                    rect
                        [ 
                            OnMouseUp (fun ev -> 
                                document.removeEventListener("mousemove", handleMouseMove.current)
                                EndDragging props.Gate.Id
                                |> props.Dispatch
                            )
                            OnMouseDown (fun ev -> 
                                // See note above re coords wrong if zoom <> 1.0
                                StartDragging (props.Gate.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )
                            X props.Gate.Pos.X
                            Y props.Gate.Pos.Y
                            SVGAttr.Width props.Gate.W
                            SVGAttr.Height props.Gate.H
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ][]

                    text [ // a demo text svg element
                            X (props.Gate.Pos.X + 30.); 
                            Y (props.Gate.Pos.Y + 10.); 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "30px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                         ] [str <| sprintf "Not"]   
                ]
            let reactElementList = 
                List.map (outputText 70. props.Gate.PortStatus) [(0.)..(float (props.Gate.OutputPorts.Length-1))]
                |> List.append (List.map (outputText 20. props.Gate.PortStatus) [(0.)..(float (props.Gate.InputPorts.Length-1))])
                |> List.collect (fun x -> x)
                |> List.collect (fun x->x)
                |>List.append initialoutline
            g   [ Style [ 
                    ]
                ] (reactElementList)
    , "Gate"
    , equalsButFunctions
    )    

type RenderMuxProps = 
    {
        Mux: Symbol
        Dispatch: Dispatch<Msg>
        key: string
    }

let renderMux = 
    FunctionComponent.Of(
        fun (props : RenderMuxProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Mux.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            let color =
                if props.Mux.IsDragging && not props.Mux.IsSelected then
                    "lightblue"
                else if props.Mux.IsSelected then
                    "red"
                else
                    "grey"

            let outputText inOrOutText portStat num = 
                let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Mux.IsSliding
                let textSection =
                    [
                        text [ 
                            X (props.Mux.Pos.X + inOrOutText)
                            Y (props.Mux.Pos.Y + 65. + num*40.);
                            Style [
                                TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "20px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color

                            ]
                        ] [str <| string num]

                        text [ 
                            X (props.Mux.Pos.X + 20.)
                            Y (props.Mux.Pos.Y + 65. + num*40.);
                            Style [
                                TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "20px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color

                            ]
                        ] [str <| string num]
                    ]
                let slideRect =
                    let portList =
                        if IO = "input" then props.Mux.InputPorts.[(int num)].PortPos else props.Mux.OutputPorts.[(int num)].PortPos
                    [
                        rect [
                              X (xSlide)
                              Y (ySlide)
                              SVGAttr.Width 10.
                              SVGAttr.Height 10.
                              SVGAttr.Fill "black"
                              SVGAttr.Stroke "black"
                              SVGAttr.StrokeWidth 1
                          ][]
                        line [
                            X1 (portList.X)
                            Y1 (portList.Y)
                            X2 xSlide
                            Y2 ySlide
                            SVGAttr.StrokeDasharray "4"
                            // Qualify these props to avoid name collision with CSSProp
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 5 ] []
                    ]

                let inPorts = 
                    [
                      rect [
                            X (props.Mux.InputPorts.[int num].PortPos.X)
                            Y (props.Mux.InputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]
                let outPorts=
                    [
                        rect [
                            X (props.Mux.OutputPorts.[int num].PortPos.X)
                            Y (props.Mux.OutputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]

                let portSection =
                    match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
                    |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
                    |("visible", 70., _, _, _ ) -> outPorts
                    | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs  
                    |("output", 20., false,_,_ ) -> inPorts
                    |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output 
                    |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides 
                    |(_, 20., true, slidePortNum, "output") -> inPorts
                    |(_, 70., true, slidePortNum, "input") -> outPorts
                    |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides 
                    |("output", 20., true,_,_ ) -> inPorts
                    |_ -> []
                   
                match portStat with 
                |"invisible" -> [textSection]
                |_ -> [textSection; portSection]

            let initialoutline = 
                [
                    rect
                        [ 
                            OnMouseUp (fun ev -> 
                                document.removeEventListener("mousemove", handleMouseMove.current)
                                EndDragging props.Mux.Id
                                |> props.Dispatch
                            )
                            OnMouseDown (fun ev -> 
                                // See note above re coords wrong if zoom <> 1.0
                                StartDragging (props.Mux.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )
                            X props.Mux.Pos.X
                            Y props.Mux.Pos.Y
                            SVGAttr.Width props.Mux.W
                            SVGAttr.Height props.Mux.H
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ][]

                    text [ // a demo text svg element
                            X (props.Mux.Pos.X + 30.); 
                            Y (props.Mux.Pos.Y + 10.); 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "30px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                         ] [str <| sprintf "Not"]   
                ]
            let reactElementList = 
                List.map (outputText 70. props.Mux.PortStatus) [(0.)..(float (props.Mux.OutputPorts.Length-1))]
                |> List.append (List.map (outputText 20. props.Mux.PortStatus) [(0.)..(float (props.Mux.InputPorts.Length-1))])
                |> List.collect (fun x -> x)
                |> List.collect (fun x->x)
                |>List.append initialoutline
            g   [ Style [ 
                    ]
                ] (reactElementList)
    , "Mux"
    , equalsButFunctions
    )    
    
type RenderCustomProps = 
    {
        Custom: Symbol
        Dispatch: Dispatch<Msg>
        key:string
    }

let renderCustom = 
    FunctionComponent.Of(
        fun (props : RenderCustomProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Custom.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            let color =
                if props.Custom.IsDragging && not props.Custom.IsSelected then
                    "lightblue"
                else if props.Custom.IsSelected then
                    "red"
                else
                    "grey"

            let outputText inOrOutText portStat num = 
                let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Custom.IsSliding
                let textSection =
                    [
                        text [ 
                            X (props.Custom.Pos.X + inOrOutText)
                            Y (props.Custom.Pos.Y + 65. + num*40.);
                            Style [
                                TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "20px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color

                            ]
                        ] 
                           [if inOrOutText = 20. then str <| (string (match (props.Custom.Type) with
                                                                     | CommonTypes.Custom customSymbol -> fst customSymbol.InputLabels.[int num]
                                                                     | _ -> failwithf "Not Implemented - Custom Component, Symbol line 678"))
                           else if inOrOutText = 70. then str <| (string (match (props.Custom.Type) with
                                                                     | CommonTypes.Custom customSymbol -> fst customSymbol.OutputLabels.[int num]
                                                                     | _ -> failwithf "Not Implemented - Custom Component, Symbol line 678"))]                                          
                    ]
                let slideRect =
                    let portList =
                        if IO = "input" then props.Custom.InputPorts.[(int num)].PortPos else props.Custom.OutputPorts.[(int num)].PortPos
                    [
                        rect [
                              X (xSlide)
                              Y (ySlide)
                              SVGAttr.Width 10.
                              SVGAttr.Height 10.
                              SVGAttr.Fill "black"
                              SVGAttr.Stroke "black"
                              SVGAttr.StrokeWidth 1
                          ][]
                        line [
                            X1 (portList.X)
                            Y1 (portList.Y)
                            X2 xSlide
                            Y2 ySlide
                            SVGAttr.StrokeDasharray "4"
                            // Qualify these props to avoid name collision with CSSProp
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 5 ] []
                    ]

                let inPorts = 
                    [
                      rect [
                            X (props.Custom.InputPorts.[int num].PortPos.X)
                            Y (props.Custom.InputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]
                let outPorts=
                    [
                        rect [
                            X (props.Custom.OutputPorts.[int num].PortPos.X)
                            Y (props.Custom.OutputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]

                let portSection =
                    match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
                    |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
                    |("visible", 70., _, _, _ ) -> outPorts
                    | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs  
                    |("output", 20., false,_,_ ) -> inPorts
                    |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output 
                    |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides 
                    |(_, 20., true, slidePortNum, "output") -> inPorts
                    |(_, 70., true, slidePortNum, "input") -> outPorts
                    |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides 
                    |("output", 20., true,_,_ ) -> inPorts
                    |_ -> []
                   
                match portStat with 
                |"invisible" -> [textSection]
                |_ -> [textSection; portSection]

            let initialoutline = 
                [
                    rect
                        [ 
                            OnMouseUp (fun ev -> 
                                document.removeEventListener("mousemove", handleMouseMove.current)
                                EndDragging props.Custom.Id
                                |> props.Dispatch
                            )
                            OnMouseDown (fun ev -> 
                                // See note above re coords wrong if zoom <> 1.0
                                StartDragging (props.Custom.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )
                            X props.Custom.Pos.X
                            Y props.Custom.Pos.Y
                            SVGAttr.Width props.Custom.W
                            SVGAttr.Height props.Custom.H
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ][]

                    text [ // a demo text svg element
                            X (props.Custom.Pos.X + 30.); 
                            Y (props.Custom.Pos.Y + 10.); 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "30px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                         ] [str <| sprintf "Custom"]   
                ]
            let reactElementList = 
                List.map (outputText 70. props.Custom.PortStatus) [(0.)..(float (props.Custom.OutputPorts.Length-1))]
                |> List.append (List.map (outputText 20. props.Custom.PortStatus) [(0.)..(float (props.Custom.InputPorts.Length-1))])
                |> List.collect (fun x -> x)
                |> List.collect (fun x->x)
                |>List.append initialoutline
            g   [ Style [ 
                    ]
                ] (reactElementList)
    , "Custom"
    , equalsButFunctions
    )  
    

type RenderSquareProps = 
    {
        Square: Symbol
        Dispatch: Dispatch<Msg>
        key:string
    }

let renderSquare = 
    FunctionComponent.Of(
        fun (props : RenderSquareProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Square.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            let color =
                if props.Square.IsDragging && not props.Square.IsSelected then
                    "lightblue"
                else if props.Square.IsSelected then
                    "red"
                else
                    "grey"

            let outputText inOrOutText portStat num = 
                let (slide, IO, slidePortNum, {X = xSlide; Y = ySlide}) = props.Square.IsSliding
                let textSection =
                    [
                        text [ 
                            X (props.Square.Pos.X + inOrOutText)
                            Y (props.Square.Pos.Y + 65. + num*40.);
                            Style [
                                TextAnchor "right" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "20px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color

                            ]
                        ] [str <| string num]
                    ]
                let slideRect =
                    let portList =
                        if IO = "input" then props.Square.InputPorts.[(int num)].PortPos else props.Square.OutputPorts.[(int num)].PortPos
                    [
                        rect [
                              X (xSlide)
                              Y (ySlide)
                              SVGAttr.Width 10.
                              SVGAttr.Height 10.
                              SVGAttr.Fill "black"
                              SVGAttr.Stroke "black"
                              SVGAttr.StrokeWidth 1
                          ][]
                        line [
                            X1 (portList.X)
                            Y1 (portList.Y)
                            X2 xSlide
                            Y2 ySlide
                            SVGAttr.StrokeDasharray "4"
                            // Qualify these props to avoid name collision with CSSProp
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 5 ] []
                    ]

                let inPorts = 
                    [
                      rect [
                            X (props.Square.InputPorts.[int num].PortPos.X)
                            Y (props.Square.InputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]
                let outPorts=
                    [
                        rect [
                            X (props.Square.OutputPorts.[int num].PortPos.X)
                            Y (props.Square.OutputPorts.[int num].PortPos.Y)
                            SVGAttr.Width 10.
                            SVGAttr.Height 10.
                            SVGAttr.Fill "black"
                            SVGAttr.Stroke "black"
                            SVGAttr.StrokeWidth 1
                        ][]
                    ]

                let portSection =
                    match (portStat, inOrOutText, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
                    |("visible", 20., _, _, _ ) -> inPorts //for normal showing ports when nearby
                    |("visible", 70., _, _, _ ) -> outPorts
                    | ("input", 70.,false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs  
                    |("output", 20., false,_,_ ) -> inPorts
                    |(_, 70., true, slidePortNum, "output") -> slideRect // for valid ports but the port that slides for a sliding output 
                    |(_, 20., true, slidePortNum, "input") -> slideRect // for valid ports but the port that slides 
                    |(_, 20., true, slidePortNum, "output") -> inPorts
                    |(_, 70., true, slidePortNum, "input") -> outPorts
                    |("input", 70., true, _, _) -> outPorts //for valid ports but not the port that slides 
                    |("output", 20., true,_,_ ) -> inPorts
                    |_ -> []
                   
                match portStat with 
                |"invisible" -> [textSection]
                |_ -> [textSection; portSection]

            let initialoutline = 
                [
                    rect
                        [ 
                            OnMouseUp (fun ev -> 
                                document.removeEventListener("mousemove", handleMouseMove.current)
                                EndDragging props.Square.Id
                                |> props.Dispatch
                            )
                            OnMouseDown (fun ev -> 
                                // See note above re coords wrong if zoom <> 1.0
                                StartDragging (props.Square.Id, posOf ev.pageX ev.pageY)
                                |> props.Dispatch
                                document.addEventListener("mousemove", handleMouseMove.current)
                            )
                            X props.Square.Pos.X
                            Y props.Square.Pos.Y
                            SVGAttr.Width props.Square.W
                            SVGAttr.Height props.Square.H
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ][]

                    text [ // a demo text svg element
                            X (props.Square.Pos.X + 30.); 
                            Y (props.Square.Pos.Y + 10.); 
                            Style [
                                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "30px"
                                FontWeight "Bold"
                                Fill "Black" // demo font color
                            ]
                         ] [str <| sprintf "Not"]   
                ]
            let reactElementList = 
                List.map (outputText 70. props.Square.PortStatus) [(0.)..(float (props.Square.OutputPorts.Length-1))]
                |> List.append (List.map (outputText 20. props.Square.PortStatus) [(0.)..(float (props.Square.InputPorts.Length-1))])
                |> List.collect (fun x -> x)
                |> List.collect (fun x->x)
                |>List.append initialoutline
            g   [ Style [ 
                    ]
                ] (reactElementList)
    , "Square"
    , equalsButFunctions
    )    


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
let view (model : Model) (dispatch : Msg -> unit) = 
    model.Symbols
    |> List.map (fun ({Id = CommonTypes.ComponentId ii} as custom) -> //match each symbol with its t
        renderCustom 
            {
                Custom = custom
                Dispatch = dispatch
                key = ii // to make it string by type matching 
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
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"