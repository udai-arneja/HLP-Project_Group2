﻿module Symbol
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
        IsHighlighted: bool 
        PortStatus: PortVisibility
        IsSliding: PortVisibility * string * int * XYPos 
        Rotate: bool
    }

type Model = {
    Symbols: Symbol list
    SymBBoxes: (XYPos*XYPos)  List
    SingleOrMultiple: bool          //true - single
    }


//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    //| Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId list * pagePos: XYPos * prevPagePos: XYPos
    //| DraggingList of sId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    //| EndDragging of sId : CommonTypes.ComponentId
    //| EndDraggingList of sId : CommonTypes.ComponentId list *pagePos:XYPos
    | AddSymbol of comp: CommonTypes.ComponentType * width: int
    | DeleteSymbol
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | ToggleSymbol of selectedSymbol:CommonTypes.ComponentId list // usually one thing
    | Hovering of portSymbol:CommonTypes.ComponentId list
    | ShowValidPorts of inOut:PortVisibility  * portId:string * mousePos:XYPos
    //| UpdateBBoxes of CommonTypes.ComponentId list
    | SnapSymbolToGrid of CommonTypes.ComponentId list
    | HighlightSymbol of CommonTypes.ComponentId list
    | DuplicateSymbol
    | DroppingNewSymbol of XYPos
    | UpdateSymbols of Symbol list * (XYPos *XYPos) list
    | RotateSymbols
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

let homotextual xPos yPos textAnchor domBaseline fontSize displayText rotate= // cheeky bit of kareem abstraction
    text
        [    X xPos
             Y yPos

             match rotate with  
             | true -> SVGAttr.Transform (sprintf "rotate(%A %A %A)" 270 xPos yPos)
             | false -> SVGAttr.Transform (sprintf "rotate(%A)" 0)
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


let createPortList (comp:Symbol)(portType:CommonTypes.PortType)(portNumber:int)(width:int)(portno:int): CommonTypes.Port =
    let portPos =
        match comp.Type with
         |Mux2 -> if portType = CommonTypes.Input
                  then {X=comp.Pos.X;Y=(comp.Pos.Y+(float(portNumber + 1))*(comp.H/4.))}
                  else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float(portNumber + 1))*(comp.H/2.))}
         |Demux2 -> if portType = CommonTypes.Input
                    then {X=comp.Pos.X;Y=(comp.Pos.Y+((float(portNumber))*(comp.H*(1./2.)))+((comp.H)*(1./4.)))}
                    else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+((float(portNumber))*(comp.H*(1./2.)))+((comp.H)*(1./4.)))}         
         |RAM mem -> if portType = CommonTypes.Input
                     then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float(portNumber + 1))*(comp.H/6.))}
                     else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/2.))}
         | NbitsAdder width ->if portType = CommonTypes.Input
                                 then {X=comp.Pos.X;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
                                 else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/3.))}
         | DFF ->  if portType = CommonTypes.Input
                   then {X=comp.Pos.X;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
                   else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
         | DFFE -> if portType = CommonTypes.Input
                   then {X=comp.Pos.X;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
                   else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
         | Register width ->    if portType = CommonTypes.Input
                                then {X=comp.Pos.X;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
                                else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
         | RegisterE width -> if portType = CommonTypes.Input
                                 then {X=comp.Pos.X;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
                                 else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y + ((float (portNumber)) + 1.)*(comp.H/4.))}
         |_ -> match comp.Rotate with 
              | false ->  match (portType, portno) with
                          | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.H/2.))}
                          | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.H)/2.) + comp.H/4.)}
                          | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
                          | (_, 1) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ( comp.H/2.) )}
                          | (_, 2) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.H/4.))}  // this one
                          |_ -> failwithf "Error on portlist"
              |true ->  match (portType, portno) with
                          | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.W/2.))}
                          | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.W)/2.) + comp.W/4.)}
                          | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
                          | (_, 1) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ( comp.W/2.) )}
                          | (_, 2) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.W/4.))}  // this one
                          |_ -> failwithf "Error on portlist"
    printfn "portpos %A" portPos
    {
        CommonTypes.Port.Id = Helpers.uuid()
        CommonTypes.Port.PortNumber = Some portNumber
        CommonTypes.Port.PortType = portType
        CommonTypes.Port.HostId = string(comp.Id)
        CommonTypes.Port.PortPos = portPos
        CommonTypes.Port.BusWidth = width
        // CommonTypes.Port.PortInUse = false
    }

let createCustomPortList (comp:Symbol)(portType:CommonTypes.PortType)(portNumber:int)(width:int): CommonTypes.Port =
    let portPos =
        match comp.Type with
         |RAM mem -> if portType = CommonTypes.Input
                     then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float(portNumber + 1))*(comp.H/6.))}
                     else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/2.))}
         | NbitsAdder width ->if portType = CommonTypes.Input
                                 then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
                                 else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/3.))}
         | DFF ->  if portType = CommonTypes.Input
                   then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
                   else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
         | DFFE -> if portType = CommonTypes.Input
                   then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
                   else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
         | Register width -> if portType = CommonTypes.Input
                                then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
                                else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
         | RegisterE width -> if portType = CommonTypes.Input
                                 then {X=comp.Pos.X;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
                                 else {X=comp.Pos.X+comp.W;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
         |_ -> match comp.Rotate with 
               | false ->  match (portType, portNumber) with
                          | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.H/2.))}
                          | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.H)/2.) + comp.H/4.)}
                          | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
                          | (_, 1) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ( comp.H/2.) )}
                          | (_, 2) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.H/4.))}  // this one
                          |_ -> failwithf "Error on portlist"
               |true ->  match (portType, portNumber) with
                          | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.W/2.))}
                          | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.W)/2.) + comp.W/4.)}
                          | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
                          | (_, 1) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ( comp.W/2.) )}
                          | (_, 2) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.W/4.))}  // this one
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

//let updatePorts rotate port portno length= 
//        let portPos =
//            match rotate with 
//            | false ->  match (port.portType, length ) with
//                        | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.H/2.))}
//                        | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.H)/2.) + comp.H/4.)}
//                        | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
//                        | (_, 1) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ( comp.H/2.) )}
//                        | (_, 2) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.H/4.))}  // this one
//                        |_ -> failwithf "Error on portlist"
//            |true ->  match (portType, length) with
//                        | (CommonTypes.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.W/2.))}
//                        | (CommonTypes.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.W)/2.) + comp.W/4.)}
//                        | (CommonTypes.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
//                        | (_, 1) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ( comp.W/2.) )}
//                        | (_, 2) -> {X=(comp.Pos.X+comp.H);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.W/4.))}  // this one
//                        |_ -> failwithf "Error on portlist"

//        {
//            CommonTypes.Port.Id = port.Id
//            CommonTypes.Port.PortNumber = Some portno
//            CommonTypes.Port.PortType = port.portType
//            CommonTypes.Port.HostId = post.HostId
//            CommonTypes.Port.PortPos = portPos
//            CommonTypes.Port.BusWidth = port.Width
//            // CommonTypes.Port.PortInUse = false
//        }

//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (inputno: int) (outputno: int) (width:int) (comp:CommonTypes.ComponentType) = //could match comp for symbols of different heights and widths
    let symWidth = 
        match comp with 
        |Register bits -> 60.*2.
        |RegisterE bits -> 60.*2.
        |Mux2 -> 60.*2.
        |Demux2 -> 60.*2.
        |DFF -> 60.*2.
        |DFFE -> 60.*2.
        |_ -> 60.
    let mainSymbol = {
                LastDragPos = {X=10.;Y=10.}
                IsDragging = false
                Id = CommonTypes.ComponentId (Helpers.uuid())
                Type = comp
                InputPorts = []
                OutputPorts = []
                Pos = {X=10.;Y=10.}
                H = 80.
                W = symWidth
                IsSelected = false
                IsHighlighted = false
                PortStatus = Invisible
                IsSliding = (ShowInputsOnly, "null", 0, {X=0.; Y=0.})
                Rotate = false
              }
    let InputPortsList = List.map (fun input -> createPortList mainSymbol CommonTypes.PortType.Input input width inputno) [0..(inputno-1)]
    let OutputPortsList = List.map (fun output -> createPortList mainSymbol CommonTypes.PortType.Output output width outputno) [0..(outputno-1)]
    {mainSymbol with InputPorts=InputPortsList; OutputPorts=OutputPortsList}

let createCustomSymbol (comp:CommonTypes.CustomComponentType) = 
    let mainSymbol = {
                LastDragPos = {X=10.;Y=10.}
                IsDragging = false
                Id = CommonTypes.ComponentId (Helpers.uuid())
                Type = Custom comp
                InputPorts =  []
                OutputPorts =  []
                Pos = {X=10.;Y=10.}
                H = max (80.) (float (max (List.length comp.InputLabels) (List.length comp.OutputLabels))*40.)
                W = 60.
                IsSelected = false
                IsHighlighted = false
                PortStatus = Invisible
                IsSliding = (ShowInputsOnly, "null", 0, {X=0.; Y=0.})
                Rotate = false
              }  
    let _, inputBusWidths = List.unzip comp.InputLabels
    let _, outputBusWidths = List.unzip comp.OutputLabels
    let InputPortsList = List.mapi (fun index width -> createCustomPortList mainSymbol CommonTypes.PortType.Input index width ) inputBusWidths //comp.InputLabels?
    let OutputPortsList = List.mapi (fun index width -> createCustomPortList mainSymbol CommonTypes.PortType.Output index width ) outputBusWidths
    {mainSymbol with InputPorts=InputPortsList; OutputPorts=OutputPortsList}            


let createNewBoundingBox (inputs: int) (outputs: int) (sym: Symbol)=
    ({X=0.;Y=0.},{X=sym.W+20.;Y=sym.H+20.})

    // +float(max (List.length inputs) (List.length outputs))*40.;Y=75.+float (max (List.length inputs) (List.length outputs))*40.})
    // [start.X-10., start.Y-10.; 110., start.Y-10.; 110., 75.+float (max inputno outputno)*40.; 75.+float (max inputno outputno)*40., 75.+float (max inputno outputno)*40.]

let portmove portId inputYes model =
    let findPort i (acc: CommonTypes.Port list) (x:Symbol)  =  match i with
                                                               |1 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.InputPorts |> function |Some a -> [a] |None -> []) acc
                                                               |0 -> List.append (List.tryFind (fun (y:CommonTypes.Port) -> string y.Id = portId ) x.OutputPorts |> function |Some a -> [a] |None -> []) acc
                                                               | _ -> failwithf "not implemented - findPort Function, Symbol line 152"
    let portReturn = match inputYes with
                     | ShowOutputsOnly -> List.fold (findPort 1) [] model |> List.head // potentially global for symbol
                     | ShowInputsOnly -> List.fold (findPort 0) [] model |> List.head
                     | _ -> failwithf "not implemented - portReturn Function, Symbol line 155"
    let symbolReturn = List.find (fun x -> x.Id = CommonTypes.ComponentId portReturn.HostId) model
    let portNumber = match portReturn.PortNumber with
                     |Some a -> a
                     | _ -> failwithf "not implemented - portNumber Function, Symbol line 159"
    (symbolReturn, portReturn, portNumber)


let init() =
    {Symbols=[]; SymBBoxes =[]; SingleOrMultiple=false}, Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol(compType, width) ->
        //need to have anther sheet input parameter for when custom - this could be an option
        // let customInformation: CustomComponentType= 
        //     {Name="Kurtangle";InputLabels=[("Udai",1);("Simi",1)];OutputLabels=[("Karl",1)]}
        let (inputno,outputno) = match compType with 
                                 |CommonTypes.Not -> (1,1)
                                 |CommonTypes.And -> (2,1)
                                 |CommonTypes.Or -> (2,1)
                                 |CommonTypes.Xor -> (2,1)
                                 |CommonTypes.Nand -> (2,1)
                                 |CommonTypes.Nor -> (2,1)
                                 |CommonTypes.Xnor -> (2,1)
                                 |CommonTypes.Mux2 -> (3,1)
                                 |CommonTypes.Demux2 -> (2,2)
                                 |CommonTypes.MergeWires -> (2,1)
                                 |CommonTypes.SplitWire bits -> (2,1)
                                 |CommonTypes.DFF -> (1,1)
                                 |CommonTypes.DFFE -> (2,1)
                                 |CommonTypes.Register bits -> (1,1)
                                 |CommonTypes.AsyncROM mem -> (1,1)
                                 |CommonTypes.ROM mem -> (1,1)
                                 |CommonTypes.RAM mem -> (3,1)
                                 |Input bits -> (1,0)
                                 |Output bits -> (0,1)
                                 |CommonTypes.BusSelection (outWid,lsb) -> (1,1)
                                 |CommonTypes.NbitsXor bits -> (1,1)
                                 |CommonTypes.BusCompare (bits,bits1) -> (1,1)
                                 |CommonTypes.Constant (bits,_) -> (1,1)

        match compType with
        | CommonTypes.Custom customInformation -> let newSymbol = createCustomSymbol customInformation
                                                  let newBoundingBox = createNewBoundingBox inputno outputno newSymbol
                                                  let newSymbolList = List.rev (newSymbol::model.Symbols)
                                                  let newSymbolsBoundingBoxes = List.rev (newBoundingBox::model.SymBBoxes)
                                                  {model with Symbols=newSymbolList; SymBBoxes=newSymbolsBoundingBoxes} , Cmd.none
        | _ -> let newSymbol = createNewSymbol inputno outputno width compType
               let newBoundingBox = createNewBoundingBox inputno outputno newSymbol
               let newSymbolList = List.rev (newSymbol::model.Symbols)
               let newSymbolsBoundingBoxes = List.rev (newBoundingBox::model.SymBBoxes)
               {model with Symbols=newSymbolList; SymBBoxes=newSymbolsBoundingBoxes} , Cmd.none

    | DuplicateSymbol -> 
        let newPorts symlist input =
            if input = true 
            then List.map (fun port -> {port with HostId = string (symlist.Id); Id = Helpers.uuid(); PortPos = posAdd port.PortPos {X=10.;Y=10.}}) symlist.InputPorts 
            else List.map (fun port -> {port with HostId = string (symlist.Id); Id = Helpers.uuid(); PortPos = posAdd port.PortPos {X=10.;Y=10.}}) symlist.OutputPorts 
        let oneOrMany = List.fold (fun acc sym -> if sym.IsSelected = true then acc + 1 else acc+ 0 ) 0 model.Symbols
        let newSymbols = if oneOrMany > 1 
                         then List.collect (fun sym -> if sym.IsSelected = true
                                                       then [{sym with Id = CommonTypes.ComponentId (Helpers.uuid()); IsSelected = true; IsHighlighted = false; Pos = posAdd sym.Pos {X=10.;Y=10.}}]
                                                       else []) model.Symbols 
                         else List.collect (fun sym -> if sym.IsSelected = true
                                                       then [{sym with Id = CommonTypes.ComponentId (Helpers.uuid()); IsSelected = false; IsHighlighted = false; Pos = posAdd sym.Pos {X=10.;Y=10.}}]
                                                       else []) model.Symbols 

        let newBBoxes = List.map (fun newSym -> (posAdd newSym.Pos {X= -10.; Y= -10.}, posAdd newSym.Pos {X = (newSym.W + 10.); Y=  (newSym.H + 10.)} )) newSymbols
        let originalSymbols = List.map (fun sym -> {sym with IsSelected = false; IsHighlighted = false}) model.Symbols
        let newPortSymbols = List.map (fun sym -> {sym with InputPorts = newPorts sym true}) newSymbols
                             |> List.map (fun sym -> {sym with OutputPorts = newPorts sym false})

        {model with Symbols = originalSymbols @ newPortSymbols; SymBBoxes = model.SymBBoxes @ newBBoxes}, Cmd.none  

    | DroppingNewSymbol (mousePos) ->
        let newSym = model.Symbols.[(List.length model.Symbols)-1]
        let (newSymbols, newBox) = List.map2 (fun sym box -> if newSym <> sym
                                                             then (sym, box)
                                                             else let diff = posDiff mousePos sym.LastDragPos
                                                                  let newPortPos (port:XYPos) = 
                                                                        posAdd port diff 
                                                                  ({sym with 
                                                                        Pos = mousePos
                                                                        LastDragPos = mousePos
                                                                        InputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.InputPorts
                                                                        OutputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.OutputPorts
                                                                    } , ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                                   |> List.unzip

        {model with Symbols = newSymbols; SymBBoxes = newBox}, Cmd.none
        
    | SnapSymbolToGrid (sId) ->

        let isSingleSelected = printfn "%A" model.Symbols
                               List.exists (fun sym -> sId=[sym.Id] && sym.IsSelected = false) model.Symbols

        let snapDifference coord= if coord % 30. < 15.
                                  then coord % 30.
                                  else -(30. - (coord % 30.))

        let roundCoord coord : float= coord - snapDifference coord

        let roundSymbolPos (symPos:XYPos) = {X=(roundCoord symPos.X);Y=(roundCoord symPos.Y)}

        let updatePortPos (currentPortPos:XYPos) (oldSymbolPos:XYPos) = {X=currentPortPos.X - (snapDifference oldSymbolPos.X);Y=currentPortPos.Y - (snapDifference oldSymbolPos.Y)}

        

        let snappedSymbols=
            match isSingleSelected with
            |true ->List.map (fun sym -> if [sym.Id] = sId
                                         then { sym with 
                                                    InputPorts = List.map (fun port -> {port with PortPos=updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                    OutputPorts = List.map (fun port -> {port with PortPos=updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                    Pos = roundSymbolPos sym.Pos
                                                    LastDragPos = sym.Pos
                                                }
                                         else {sym with IsSelected=false} ) model.Symbols
            |false -> List.map (fun sym -> if [sym.Id] <> sId
                                                then if sym.IsSelected = true
                                                     then { sym with
                                                                InputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                                OutputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                                Pos = roundSymbolPos sym.Pos
                                                                IsDragging = true
                                                           }
                                                     else sym
                                                else //check whether symbol is selected
                                                    { sym with
                                                        InputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                        OutputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                        Pos = roundSymbolPos sym.Pos
                                                        IsDragging = true
                                                        // LastDragPos = pagePos
                                                    }
                        ) model.Symbols

        let newSymbols, newBox =
            if model.SingleOrMultiple = true
            then  List.map2 (fun sym box -> if sId <> [sym.Id]
                                            then (sym, box)
                                            else ({sym with IsDragging=false} , ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) snappedSymbols model.SymBBoxes
                  |> List.unzip
            else
                List.map2 (fun sym box -> if sym.IsSelected = false
                                          then (sym, box)
                                          else ({sym with IsDragging = false}, ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) snappedSymbols model.SymBBoxes
                |> List.unzip

        {model with Symbols=snappedSymbols; SingleOrMultiple=isSingleSelected; SymBBoxes=newBox}, Cmd.none //bbouning boxes


    | Dragging (sId, pagePos, prevPagePos) ->
         

        let isSingleSelected =
            List.exists (fun sym -> sId=[sym.Id] && sym.IsSelected = false) model.Symbols
        //if symbol being dragged is not selected - then you are dragging one component

        let diff = posDiff pagePos prevPagePos

        let newPortPos (port:XYPos) = 
            posAdd port diff 

        let dSymbols=
            // let diff = posDiff pagePos prevPagePos//sym.LastDragPos
            match isSingleSelected with
            |true ->printfn "hola %A" model.Symbols
                    List.map (fun sym -> if [sym.Id] = sId
                                         then { sym with 
                                                    Pos = posAdd sym.Pos diff
                                                    IsDragging = true
                                                    LastDragPos = sym.Pos
                                                    InputPorts = List.map (fun port -> {port with PortPos= newPortPos port.PortPos}) sym.InputPorts
                                                    OutputPorts = List.map (fun port -> {port with PortPos= newPortPos port.PortPos}) sym.OutputPorts
                                                }
                                         else {sym with IsSelected=false} ) model.Symbols
            |false -> List.map (fun sym -> if [sym.Id] <> sId
                                                then if sym.IsSelected = true
                                                     then { sym with
                                                                Pos = posAdd sym.Pos diff
                                                                IsDragging = true
                                                                LastDragPos = pagePos
                                                                InputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.InputPorts
                                                                OutputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.OutputPorts
                                                           }
                                                     else sym
                                                else //check whether symbol is selected
                                                    { sym with
                                                        Pos = posAdd sym.Pos diff
                                                        IsDragging = true
                                                        LastDragPos = pagePos
                                                        InputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.InputPorts
                                                        OutputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.OutputPorts
                                                    }
                        ) model.Symbols
        let newSymbols, newBox =
            if isSingleSelected = true
            then  List.map2 (fun sym box -> if sId <> [sym.Id]
                                            then (sym, box)
                                            else ({sym with IsDragging=false} , ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                  |> List.unzip
            else
                List.map2 (fun sym box -> if sym.IsSelected = false
                                          then (sym, box)
                                          else ({sym with IsDragging = false}, ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                |> List.unzip
        printfn "Ports %A diff %A" dSymbols diff 
        {model with Symbols=dSymbols; SingleOrMultiple=isSingleSelected; SymBBoxes = newBox}, Cmd.none


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

    |HighlightSymbol (sId) ->
        printfn "highlight"
        let highlightedSymbolList = 
            List.map (fun sym -> if List.exists (fun idsInList -> idsInList=sym.Id) sId
                                 then {sym with IsHighlighted = true}
                                 else {sym with IsHighlighted = false}) model.Symbols
        {model with Symbols = highlightedSymbolList}, Cmd.none


    | ToggleSymbol (sId) ->
        let selectedSymbolList =
            match sId with 
            |multipleSelect :: [lst] -> List.map (fun sym -> if List.exists (fun idsInList -> idsInList=sym.Id) sId
                                                             then {sym with IsSelected=true; IsHighlighted = false}
                                                             else {sym with IsSelected=false; IsHighlighted = false}) model.Symbols
            |_ -> List.map (fun sym -> if List.exists (fun idsInList -> idsInList=sym.Id) sId
                                       then {sym with IsSelected=not sym.IsSelected; IsHighlighted = false}
                                       else {sym with IsSelected=false; IsHighlighted = false}) model.Symbols
            
            
        printfn "syms %A" selectedSymbolList
        {model with Symbols = selectedSymbolList}, Cmd.none

    |Hovering (sId) ->
        let showPortSymbols =
            model.Symbols
            |> List.map (fun sym -> if [sym.Id] = sId then { sym with PortStatus = Visible}  else { sym with PortStatus = Invisible})
        {model with Symbols = showPortSymbols}, Cmd.none

    |ShowValidPorts (portVis, portId, mousePos) ->
        let validPortSymbols =
            printfn "validPortSymbols %A" portVis
            match (portmove portId portVis model.Symbols) with
            | (symb, port, portNum) -> List.map (fun x -> if x.Id = symb.Id 
                                                          then { x with IsSliding=(portVis, string symb.Id, portNum, mousePos); PortStatus=portVis}  
                                                          else { x with PortStatus=portVis; IsSliding = (portVis, string symb.Id, portNum, mousePos)}) model.Symbols
        {model with Symbols =  validPortSymbols}, Cmd.none

    | UpdateSymbols (sym, bb) ->
        printfn "hey"
        if sym <> [] then 
            {model with Symbols = sym; SymBBoxes = bb}, Cmd.none
        else 
            model, Cmd.none
    | RotateSymbols ->
        let newSymbols = List.map(fun sym -> if sym.IsSelected = true then {sym with Rotate = true} else sym) model.Symbols
        //let newPorts = List.map (fun sym -> if sym.IsSelected = true then {sym with InputPorts = List.mapi (fun i w -> (true i portno length) sym.InputPorts; OuputPorts = }
        {model with Symbols = newSymbols}, Cmd.none

    | MouseMsg sMsg ->
        printfn "symbol %A" sMsg
        let showPorts =
            model.Symbols
            |> List.map (fun x -> { x with PortStatus=Invisible; IsSliding=(ShowInputsOnly , "null", 0, {X=0.; Y=0.})})
        {model with Symbols = showPorts}, Cmd.none

    //| Rotate -> 
    //    List.map (fun x -> x with 
        


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

    match comp with
    | Input bits | Output bits ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [ match props.Symb.Rotate with
                        | true ->  SVGAttr.Transform (sprintf "rotate(%A %A %A)" 90 (props.Symb.Pos.X + (props.Symb.W)/2.) (props.Symb.Pos.Y + (props.Symb.H)/2.))
                        | false -> SVGAttr.Transform (sprintf "rotate(%A)" 0)]
                    [
                       // rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth/3.) (gateHeight/4.) color

                    ]
        )
    | Not | And | Or | Xor | Nand | Nor | Xnor | Mux2 | Demux2 |DFF | DFFE -> //| Demux2 | DFF | DFFE | Register bits | RegisterE bits | ROM memorySize | AsyncROM memorySize | RAM memorySize ->
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
                let (portVis, symId, portNum, mousePosition) = props.Symb.IsSliding
                let displayPort = 
                    let outputPorts = List.map (fun (ports:CommonTypes.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.OutputPorts
                    let inputPorts = List.map (fun (ports:CommonTypes.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.InputPorts
                    let slideCirc IO portNum (mousePos:XYPos)=
                        let portList =
                            if IO = "input" then props.Symb.InputPorts.[portNum].PortPos else props.Symb.OutputPorts.[portNum].PortPos
                        [
                            circus mousePos.X  mousePos.Y 5.
                            line [
                                X1 portList.X
                                Y1 portList.Y
                                X2 mousePos.X
                                Y2 mousePos.Y
                                SVGAttr.StrokeDasharray "4"
                                // Qualify these props to avoid name collision with CSSProp
                                SVGAttr.Stroke "black"
                                SVGAttr.StrokeWidth 5 ] []
                        ]
                
                    match props.Symb.PortStatus with
                    | Visible ->    outputPorts @ inputPorts
                
                    | Invisible ->  []
                
                    | ShowInputsOnly -> if string props.Symb.Id = symId 
                                        then slideCirc "output" portNum mousePosition 
                                        else inputPorts
                
                    | ShowOutputsOnly -> if string props.Symb.Id = symId 
                                         then slideCirc "input" portNum mousePosition 
                                         else outputPorts

                let color =
                    if props.Symb.IsSelected then
                        "green"
                    else if props.Symb.IsHighlighted then 
                        "yellow"
                    else
                        "grey"

                let mainOutline = 
                        let textSection =   
                                        match props.Comp with
                                                                   | Not -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "1" (props.Symb.Rotate)]
                                                                            |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) props.Symb.Pos.X (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]

                                                                   | And -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate) ]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)] 
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2 ]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Or -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1" (props.Symb.Rotate)]
                                                                           |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                           |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                           |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                           |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                           |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                           |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Xor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2 ]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Nand -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&" (props.Symb.Rotate)]
                                                                             |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                             |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Nor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1" (props.Symb.Rotate)]
                                                                            |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Xnor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1" (props.Symb.Rotate)]
                                                                             |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                             |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                   | Mux2 -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "MUX" (props.Symb.Rotate)]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X1" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "X0" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                             |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "EN" (props.Symb.Rotate)]
                                                                             |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                   | Demux2 -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DEMUX" (props.Symb.Rotate)]
                                                                               |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "Middle" "10px" "Y0" (props.Symb.Rotate)]
                                                                               |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                               |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "end" "Middle" "10px" "Y1" (props.Symb.Rotate)]
                                                                               |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                               |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "X0" (props.Symb.Rotate)]
                                                                               |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                               |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "EN" (props.Symb.Rotate)]
                                                                               |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                                   | DFF -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFF" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "D" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Q" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                            |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1] 
                                                                   | DFFE -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFFE" (props.Symb.Rotate)]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "D" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Q" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                            |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                   //         |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1] 
                                                                   //| Register bits -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "Register" (props.Symb.Rotate)] 
                                                                   //                   |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in" (props.Symb.Rotate)]
                                                                   //                   |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                   //                   |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]
                                                                   //                   |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]//Mux output
                                                                   //                   |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                   //                   |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                   //                   |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1] // CLK   
                                                                   //| RegisterE bits -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RegisterE" (props.Symb.Rotate)]
                                                                   //                    |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN" (props.Symb.Rotate)]
                                                                   //                    |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2] 
                                                                   //                    |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in" (props.Symb.Rotate)]
                                                                   //                    |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                   //                    |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]
                                                                   //                    |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]//Mux output
                                                                   //                    |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                   //                    |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                   //                    |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1]
                                                                   | ROM memorySize -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "ROM" (props.Symb.Rotate)]
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                                       |> List.append [line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []]  //clk ting
                                                                                       |> List.append [line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []]  //clk ting 
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Addr" (props.Symb.Rotate)]
                                                                                       |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2] // homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]                                                                
                                                                                       |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2]                                                  
                                                                   | AsyncROM memorySize -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "AsyncROM" (props.Symb.Rotate)]
                                                                                            |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Addr" (props.Symb.Rotate)]
                                                                                            |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2] // homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)
                                                                                            |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]                                                                
                                                                                            |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2] 
                                                                   | RAM memorySize -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RAM" (props.Symb.Rotate)]
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/6.) "start" "middle" "10px" "Addr" (props.Symb.Rotate)]
                                                                                       |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) 2] // CLK
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "start" "middle" "10px" "Data-in" (props.Symb.Rotate)]
                                                                                       |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 ]// CLK
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Write" (props.Symb.Rotate)]
                                                                                       |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2] // CLK
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out" props.Symb.Rotate]
                                                                                       |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2] // CLK
                                                                                       |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" props.Symb.Rotate]
                                                                                       |> List.append [line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] [] ] //clk ting
                                                                                       |> List.append [line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] [] ] //clk ting
                                                                   | _ -> [homotextual 0 0 "" "" "" "" (props.Symb.Rotate)]
                        textSection @ [rectum props.Symb.Pos.X props.Symb.Pos.Y props.Symb.W gateHeight color props]
            

                let finalPortsSymbol = mainOutline @ displayPort 

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
                        match props.Symb.Rotate with
                        | true ->  SVGAttr.Transform (sprintf "rotate(%A %A %A)" 90 (props.Symb.Pos.X + (props.Symb.W)/2.) (props.Symb.Pos.Y + (props.Symb.H)/2.))
                        | false -> SVGAttr.Transform (sprintf "rotate(%A)" 0)
                    ] (finalPortsSymbol)


        )

        | Register bits | RegisterE bits -> // | ROM memorySize | AsyncROM memorySize | RAM memorySize ->
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
                    let (portVis, symId, portNum, mousePosition) = props.Symb.IsSliding
                    let displayPort = 
                        let outputPorts = List.map (fun (ports:CommonTypes.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.OutputPorts
                        let inputPorts = List.map (fun (ports:CommonTypes.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.InputPorts
                        let slideCirc IO portNum (mousePos:XYPos)=
                            let portList =
                                if IO = "input" then props.Symb.InputPorts.[portNum].PortPos else props.Symb.OutputPorts.[portNum].PortPos
                            [
                                circus mousePos.X  mousePos.Y 5.
                                line [
                                    X1 portList.X
                                    Y1 portList.Y
                                    X2 mousePos.X
                                    Y2 mousePos.Y
                                    SVGAttr.StrokeDasharray "4"
                                    // Qualify these props to avoid name collision with CSSProp
                                    SVGAttr.Stroke "black"
                                    SVGAttr.StrokeWidth 5 ] []
                            ]
                    
                        match props.Symb.PortStatus with
                        | Visible ->    outputPorts @ inputPorts
                    
                        | Invisible ->  []
                    
                        | ShowInputsOnly -> if string props.Symb.Id = symId 
                                            then slideCirc "output" portNum mousePosition 
                                            else inputPorts
                    
                        | ShowOutputsOnly -> if string props.Symb.Id = symId 
                                             then slideCirc "input" portNum mousePosition 
                                             else outputPorts

                    let color =
                        if props.Symb.IsSelected then
                            "green"
                        else if props.Symb.IsHighlighted then 
                            "yellow"
                        else
                            "grey"

                    let mainOutline = 
                            let textSection =   
                                            match props.Comp with

                                                                       | Register bits -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "Register" (props.Symb.Rotate)] 
                                                                                          |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in" (props.Symb.Rotate)]
                                                                                          |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                                          |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]
                                                                                          |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]//Mux output
                                                                                          |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                                          |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                                          |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1] // CLK   

                                                                       | RegisterE bits -> [homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RegisterE" (props.Symb.Rotate)]
                                                                                           |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN" (props.Symb.Rotate)]
                                                                                           |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2] 
                                                                                           |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in" (props.Symb.Rotate)]
                                                                                           |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                                                           |> List.append [homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out" (props.Symb.Rotate)]
                                                                                           |> List.append [creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2]//Mux output
                                                                                           |> List.append [homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk" (props.Symb.Rotate)]
                                                                                           |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1] // CLK
                                                                                           |> List.append [creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1]

                                                                       | _ -> [homotextual 0 0 "" "" "" "" (props.Symb.Rotate)]
                            textSection @ [rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props]
                

                    let finalPortsSymbol = mainOutline @ displayPort 

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
                            match props.Symb.Rotate with
                            | true ->  SVGAttr.Transform (sprintf "rotate(%A %A %A)" 90 (props.Symb.Pos.X + (props.Symb.W)/2.) (props.Symb.Pos.Y + (props.Symb.H)/2.))
                            | false -> SVGAttr.Transform (sprintf "rotate(%A)" 0)
                        ] (finalPortsSymbol)


            )
    //| Mux2 | Demux2 ->
    //    FunctionComponent.Of(
    //        fun (props : RenderSymbolProps) ->

    //            let color =
    //                if props.Symb.IsDragging then
    //                    "green"
    //                else
    //                    "grey"
    //            g   [
    //                ]
    //                [
    //                    rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

    //                    match props.Comp with
    //                    | Mux2 ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "MUX"

    //                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X1"
    //                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

    //                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"
    //                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2//Mux output



    //                    | Demux2 ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DEMUX"

    //                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "Middle" "10px" "Y0"
    //                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output

    //                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "end" "Middle" "10px" "Y1"
    //                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2//Mux output

    //                    | _ ->
    //                        homotextual 0 0 "" "" "" ""

    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "X0"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "EN"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2


    //                ]

    //    )
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
                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Cin" (props.Symb.Rotate)
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "A" (props.Symb.Rotate)
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "B" (props.Symb.Rotate)
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                            //outputa
                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "end" "Middle" "10px" "Sum" (props.Symb.Rotate)
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/3.) 2//Mux output

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(3./2.)) "end" "Middle" "10px" "Cout" (props.Symb.Rotate)
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
    //| DFF | DFFE ->
    //    FunctionComponent.Of(
    //        fun (props : RenderSymbolProps) ->

    //            let color =
    //                if props.Symb.IsDragging then
    //                    "green"
    //                else
    //                    "grey"
    //            g   [
    //                ]
    //                [
    //                    rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props


    //                    match props.Comp with
    //                    | DFF ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFF"


    //                    | DFFE ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFFE"

    //                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
    //                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

    //                    | _ ->
    //                        homotextual 0 0 "" "" "" ""

    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "D"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

    //                    homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Q"
    //                    creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output


    //                    homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
    //                    creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
    //                    creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK


    //            ]

    //    )
    //| Register bits | RegisterE bits->
    //    FunctionComponent.Of(
    //        fun (props : RenderSymbolProps) ->

    //            let color =
    //                if props.Symb.IsDragging then
    //                    "green"
    //                else
    //                    "grey"
    //            g   [
    //                ]
    //                [
    //                    rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props


    //                    match props.Comp with
    //                    | Register bits ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "Register"



    //                    | RegisterE bits ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RegisterE"

    //                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
    //                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

    //                    | _ ->
    //                        homotextual 0 0 "" "" "" ""

    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

    //                    homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out"
    //                    creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output


    //                    homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
    //                    creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
    //                    creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK



    //            ]

    //    )
    //| ROM memorySize | AsyncROM memorySize ->
    //    FunctionComponent.Of(
    //        fun (props : RenderSymbolProps) ->

    //            let color =
    //                if props.Symb.IsDragging then
    //                    "green"
    //                else
    //                    "grey"
    //            g   [
    //                ]
    //                [
    //                    rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

    //                    match props.Comp with
    //                    | ROM memorySize ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "ROM"

    //                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
    //                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
    //                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting




    //                    | AsyncROM memorySize ->
    //                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "AsyncROM"


    //                    | _ ->
    //                        homotextual 0 0 "" "" "" ""


    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Addr"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK



    //                    homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
    //                    creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK



    //            ]

    //    )
    //| RAM memorySize ->
    //    FunctionComponent.Of(
    //        fun (props : RenderSymbolProps) ->

    //            let color =
    //                if props.Symb.IsDragging then
    //                    "green"
    //                else
    //                    "grey"
    //            g   [
    //                ]
    //                [
    //                    rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

    //                    homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RAM"


    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/6.) "start" "middle" "10px" "Addr"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) 2 // CLK


    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "start" "middle" "10px" "Data-in"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 // CLK

    //                    homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Write"
    //                    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


    //                    homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
    //                    creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


    //                    homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
    //                    line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
    //                    line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting

    //            ]
    //    )
    | Custom customSymbol ->    //custom symbol contains - name of component; list of input and output ports with name & buswidth
            //generate ports
            
         FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                let color =
                    if props.Symb.IsSelected then
                        "green"
                    else
                        "grey"
                let generatePortsList index portName inoutput =
                    let inputPortNum = List.length props.Symb.InputPorts
                    let outputPortNum = List.length props.Symb.OutputPorts
                    let inputPortName = Printf.StringFormat<string> (fst customSymbol.InputLabels.[index]) 
                    // let outputPortName = Printf.StringFormat<string> (fst customSymbol.OutputLabels.[index]) 
                    match inoutput with 
                    | true -> (homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + ((float index + 1.)/((float inputPortNum) + 1.)*props.Symb.H)) "start" "Middle" "10px" inputPortName (props.Symb.Rotate)),
                              (creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + ((float index + 1.)/((float inputPortNum) + 1.)*props.Symb.H)) (props.Symb.Pos.X) (props.Symb.Pos.Y + (float(index + 1)/(float inputPortNum + 1.)*props.Symb.H)) 2) 

                    | false -> (homotextual (props.Symb.Pos.X + gateWidth - inOutLines*1.7) (props.Symb.Pos.Y + (float(index + 1)/(float outputPortNum + 1.)*props.Symb.H)) "start" "Middle" "10px" "op" (props.Symb.Rotate)),
                               (creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + (float (index + 1)/(float outputPortNum + 1.)*props.Symb.H)) (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + (float (index + 1)/(float outputPortNum + 1.)*props.Symb.H)) 2 )

                let generateSVGChild =
                    let standard  =
                        [
                            //shape - done
                            rectum props.Symb.Pos.X props.Symb.Pos.Y gateWidth props.Symb.H color props //can do gateHeight*max inputs/outputs

                            //name of component - potentially change width based on name size - done
                            let name = Printf.StringFormat<string> customSymbol.Name                                     
                            homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + props.Symb.H/8.) "middle" "Middle" "14px" name (props.Symb.Rotate)
                        ]      
                    List.mapi (fun index (portName,_) -> generatePortsList index portName true) customSymbol.InputLabels
                    |> List.append (List.mapi (fun index (portName,_) -> generatePortsList index portName false) customSymbol.OutputLabels)
                    |> List.unzip
                    |> (fun (reactList1, reactList2) -> reactList1@reactList2)
                    |> List.append standard


                g   [
                    ](generateSVGChild)

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


    

//match props.Comp with
//     | Nor | Not | Nand | Xnor ->
//         circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius
//     | _ -> circus 0 0 0

//match props.Comp with
//| Not ->
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X0"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) props.Symb.Pos.X (props.Symb.Pos.Y + gateHeight/2.) 2
//    // Seq.ofList (renderPorts Visible 1 props.Symb)
//| _ ->
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2
//    // renderPorts Visible ((List.length props.Symb.OutputPorts)-1) props.Symb
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2


//match props.Comp with
//| And | Or | Xor ->
//    creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2
//| _ ->
//    creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2

//let renderPorts (portVisibility:PortVisibility) num sym =


//renderPorts Visible ((List.length props.Symb.OutputPorts)-1) props.Symb
        // // let individiualPorts =
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
//
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




















