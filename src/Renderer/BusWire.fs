﻿module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//


/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra informion for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separadatatype and Id or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
// type HighlightWire = 
//     | Wrong of CommonTypes.Red
//     | Fine of CommonTypes.Black
//     | Hovering of CommonTypes.Blue
//     | Selecting of CommonTypes.Green

type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcSymbol: CommonTypes.ComponentId // source symbol
    TargetSymbol: CommonTypes.ComponentId // target symbol
    SrcPort: CommonTypes.Port
    TargetPort: CommonTypes.Port
    Vertices: XYPos List
    Highlighted: bool
    Selected: bool
    BusWidth: int
    IsDragging : bool
    LastDragPos : XYPos List   
    }

type Model = {
    Symbol: Symbol.Model
    Wires: Wire list
    Color: CommonTypes.HighLightColor
    wBB: (XYPos*XYPos) list List
    }

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other chasnges needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.Port * CommonTypes.Port)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire
    | MouseMsg of MouseT
    | SelectWire of (Symbol.Symbol list * Wire list)
    | Dragging of (CommonTypes.ComponentId list * (Wire * int) list) * prevPos: XYPos * currPos: XYPos
    | UpdateBoundingBoxes of (CommonTypes.ComponentId list * (Wire * int) list)
    // | DraggingList of wId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    | EndDragging of wId : CommonTypes.ComponentId
    | EndDraggingList of wId : CommonTypes.ComponentId list *pagePos:XYPos



type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    Vertices: XYPos list
    Selected : bool
    Highlighted: bool
    BusWidth: int 
    ColorP: string
    StrokeWidthP: string 
    IsDragging : bool
    LastDragPos : XYPos List 
    PortInUse : bool
}

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

/// look up wire in WireModel
let wire (wModel: Model)(wId: CommonTypes.ConnectionId): Option<Wire> =
    wModel.Wires
    |> List.tryPick (function {Id = wId} as x -> Some x) 

/// newWireRoute calculates the wire route between 2 port positions. It returns a list of XY Positions, which are in the 
/// form of vertices, including the source port and target port positions. It calculates how many segments the wire
/// will have. 
let newWireRoute (sourcePort: XYPos) (targetPort: XYPos) : XYPos list =
    
    let threeSegWire : XYPos list =
        let xDifference = targetPort.X - sourcePort.X
        let midpoint = float (xDifference/2.0)
        let midX = sourcePort.X + midpoint
        [{X= sourcePort.X ; Y= sourcePort.Y}; {X= midX ; Y=sourcePort.Y}; {X= midX; Y= targetPort.Y}; {X=targetPort.X; Y= targetPort.Y}]
    
    let fiveSegWire : XYPos list = 
        let xDifference = targetPort.X - sourcePort.X
        let yDifference = targetPort.Y - sourcePort.Y
        let xMidpoint = float (xDifference/2.0)
        let yMidpoint = float (yDifference/2.0)
        let midX = sourcePort.X + xMidpoint
        let midY = sourcePort.Y + yMidpoint
        let offset = 20.0
        [{X=sourcePort.X; Y=sourcePort.Y};{X=(sourcePort.X+offset); Y= sourcePort.Y};{X=(sourcePort.X+offset); Y= midY};{X= float (targetPort.X - offset); Y= midY};{X=float(targetPort.X - offset); Y= targetPort.Y}; {X= targetPort.X ; Y=targetPort.Y}]
    
    if sourcePort.X + 10.< targetPort.X then threeSegWire 
    else fiveSegWire

/// A useful function to convert the list of vertices into a list of segments. A list of vertices is an XYPos list, whereas a list of
/// segments is a list in which each element is XYPos*XYPos (to represent start and end points).
let segmentList (vertexList: XYPos list) : (XYPos * XYPos) list= vertexList |> List.pairwise

/// Finding the bounding box for a given wire (ie, list of vertices). The padding determines how large you would like the bounding
/// box to be. Returns XYPos*XYPos list, with both the XYPos being the diagonal points in a bounding box. 
let wireBoundingBoxes (verticesList: XYPos list) =
    let padding = 10.0
    let rightX segment = segment |> List.sortBy (fun v -> v.X)
    let leftX segment = segment |> List.sortByDescending (fun v -> v.X)
    let topY segment = segment |> List.sortByDescending (fun v -> v.Y)
    let bottomY segment = segment |> List.sortBy (fun v -> v.Y)
    let findBox (startVertex: XYPos) (endVertex: XYPos) : (XYPos*XYPos)=
        if startVertex.X = endVertex.X then 
            let maxY = (bottomY [startVertex;endVertex]).Head
            let minY = (topY [startVertex; endVertex]).Head
            {minY with X=startVertex.X - padding}, {maxY with X=startVertex.X + padding}
        else 
            let maxX = (rightX[startVertex ; endVertex]).Head 
            let minX = (leftX[startVertex; endVertex]).Head
            {minX with Y=startVertex.Y - padding}, {maxX with Y=startVertex.Y + padding}
    let makeSegList = segmentList verticesList 
    makeSegList 
    |> List.map (fun x -> findBox (fst x) (snd x))


/// singleWireView maps the list of vertices to a list of segments, then draws each individual line by passing in the XY positions to singularLine.
/// We are able to view the wire through this function, as well as the bus width legend, and a change in colour if Selected, or if the wire has 
/// a larger bus width. 
let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            
                
            let singularLine (pairVertices: XYPos*XYPos) = 
                let fst, snd = pairVertices
              
                line [
                    X1 fst.X // accessing the x value of SrcP since its of type XYPos
                    Y1 fst.Y
                    X2 snd.X
                    Y2 snd.Y 
                    
                    
                    SVGAttr.Stroke props.ColorP //(if props.BusWidth = 1 then props.ColorP elif props.Selected = true then "yellow" else "darkorchid")
                    SVGAttr.StrokeWidth (if props.BusWidth = 1 then props.StrokeWidthP else "5px")
                    SVGAttr.StrokeLinecap "round"  ] []

            let busWidthLegend =
                let legendPos = posAdd props.Vertices.Head {X=10.;Y=5.}
                text [
                    X legendPos.X
                    Y legendPos.Y
                    Style [
                        FontSize "10px"
                        FontWeight "Bold"
                        Fill "Black"
                        TextAnchor "middle"
                        DominantBaseline "hanging"
                        ]
                ] [str <| sprintf "%i" props.BusWidth]

            //let multipleWireCircle =
                
            //    circle [
            //        X legendPos.X
            //        Y legendPos.Y
            //        Style [
            //            FontSize "10px"
            //            FontWeight "Bold"
            //            Fill "Black"
            //            TextAnchor "middle"
            //            DominantBaseline "hanging"
            //            ]
            //    ] [str <| sprintf "%i" props.BusWidth]
                
            let singleSeg = segmentList props.Vertices // takes in the list of vertices that make up a wire and maps these to segments. 
            let segmentsIntoLine = singleSeg |> List.map singularLine 
            (busWidthLegend:: segmentsIntoLine)
            |> ofList 
                     
         
                    )

/// The view function takes every wire in the model, and its attributes, and maps this to singleWireView. The helper
/// function convertIdToXYPos is not my code, but a teammates. 
let view (model:Model) (dispatch: Dispatch<Msg>)=

    let wires = 
        model.Wires 
        |> List.map (fun w ->
            let start = w.SrcPort.PortPos//convertIdToXYPos 1 w.SrcPort
            let final = w.TargetPort.PortPos//convertIdToXYPos 0 w.TargetPort
            let vertex = newWireRoute final start
            let BusWidth = w.BusWidth
            let Selected = w.Selected
            let Highlighted = w.Highlighted 
            let wireColour = match (BusWidth, Highlighted, Selected) with 
                             | (_, _, true) -> "yellow"
                             | (_, true, false) -> "red"
                             | (1,false, false) -> "black"
                             | (_, false, false )-> "darkorchid"
            let props = {
                key = w.Id
                WireP = w
                Selected= w.Selected
                BusWidth = w.BusWidth
                SrcP = start 
                TgtP = final 
                Vertices = vertex
                //ColorP = model.Color.Text()
                ColorP = wireColour
                StrokeWidthP = "2px"
                Highlighted = w.Highlighted
                IsDragging = false 
                LastDragPos = vertex 
                PortInUse = CommonTypes.Port.WireAttached}
            singleWireView props) // pass in the props for this given wire into singleWireView
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg)) 
    g [] [(g [] wires); symbols] // displaying the wires and symbols 

/// A function which creates a new Bounding Box for a wire. 
let createNewBB outp inp= 
    wireBoundingBoxes (newWireRoute (inp) (outp))

/// A function which creates a new wire. This is called from the AddWire message in the update function. 
let createNewWire (sourcePort:CommonTypes.Port) (targetPort:CommonTypes.Port) (model:Model) : Wire =
    if sourcePort.BusWidth <> targetPort.BusWidth
    then 
        let wireId = CommonTypes.ConnectionId (Helpers.uuid())
        {
            SrcSymbol = CommonTypes.ComponentId (Helpers.uuid()) 
            TargetSymbol = CommonTypes.ComponentId (Helpers.uuid())
            Id = wireId 
            SrcPort = sourcePort
            TargetPort = targetPort
            Vertices = newWireRoute targetPort.PortPos sourcePort.PortPos               //newWireRoute  (convertIdToXYPos 0 targetPortId) (convertIdToXYPos 1 sourcePortId)
            Selected = false
            BusWidth = 1                                                            //need to set this to something
            Highlighted = true                                                            
            IsDragging = false
            LastDragPos = newWireRoute targetPort.PortPos sourcePort.PortPos
        }
    else 
        let wireId = CommonTypes.ConnectionId (Helpers.uuid())
        {
            SrcSymbol = CommonTypes.ComponentId (Helpers.uuid()) 
            TargetSymbol = CommonTypes.ComponentId (Helpers.uuid())
            Id = wireId 
            SrcPort = sourcePort
            TargetPort = targetPort
            Vertices = newWireRoute targetPort.PortPos sourcePort.PortPos               //newWireRoute  (convertIdToXYPos 0 targetPortId) (convertIdToXYPos 1 sourcePortId)
            Selected = false
            BusWidth = sourcePort.BusWidth
            IsDragging = false
            LastDragPos = newWireRoute targetPort.PortPos sourcePort.PortPos
            Highlighted = false
        }


let isEven (segId: int) (wir: Wire): Option<bool> = 
    let noOfSeg = List.length wir.Vertices

    if noOfSeg = 5
    then 
        match segId with
        | 1 -> Some false
        | 2 -> Some true
        | 3 -> Some false
        | _ -> None
    else 
        match segId with
        | 1 -> Some false
        | _ -> None

let evenChange (currPos: XYPos) (mPos: XYPos): XYPos =
    {currPos with Y = mPos.Y}
let oddChange (currPos: XYPos) (mPos: XYPos): XYPos =
    {currPos with X = mPos.X}

let updateVertices (segId: int) (wir: Wire) (mPos: XYPos) : XYPos list = 
    
    let trueList segindex = 
        wir.Vertices 
        |> List.indexed
        |> List.map (fun (index,vertex) -> if (index = segindex || index = segindex+1) then evenChange vertex mPos else vertex)  

    let falseList idx = 
        wir.Vertices 
        |> List.indexed
        |> List.map (fun (i,v) -> if (i = idx || i = idx+1) then oddChange v mPos else v)
    
    
    match isEven segId wir with 
    | Some true -> trueList segId
    | Some false -> falseList segId
    | None -> failwithf "Error"


    /// Initialisation function. Begins with no wires, and uses the Symbol model as a base. 
let init () = 
    let symbols, cmd = Symbol.init()
    {Wires = []; Symbol = symbols;Color = CommonTypes.Red; wBB = []}, Cmd.none



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        //cmoe back to this - moving the symbol and its effect on wires
        let newBB = 
            List.map (fun w -> wireBoundingBoxes (newWireRoute w.TargetPort.PortPos w.SrcPort.PortPos)) model.Wires
        let sm,sCmd = Symbol.update sMsg model.Symbol 
        {model with Symbol=sm; wBB = newBB}, Cmd.map Symbol sCmd

    | AddWire (inputPort,outputPort) -> 
        let addNewWire = (createNewWire inputPort outputPort model):: model.Wires
        let addNewWireBB = (createNewBB inputPort.PortPos outputPort.PortPos):: model.wBB
        {model with Wires=addNewWire; wBB=addNewWireBB}, Cmd.none

    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

    | DeleteWire ->
        //first removing wires that are on symbols
        let remainingWiresAndBoxes = 
            let checkWire (wiresList, bBoxesList) (wire:Wire) boundingBox =
                let areAttachedSymbolsSelected =
                    match wire with
                    | Symolwire.SrcPort.HostId
                match wire.Selected with
                | true -> (wiresList, bBoxesList)
                | false -> match areAttachedSymbolsSelected with
                           | true -> (wiresList, bBoxesList)
                           | false -> (wiresList@[wire], bBoxesList@[boundingBox])
            List.fold2 checkWire ([],[]) model.Wires model.wBB
        // let wiresConnectedToSymbols = List.map () model.Wires
        // //then removing remaining wires selected
        // let selectedList = 
        //     let checkWire (wiresList, bBoxesList) (wireTest:Wire) boundingBox= 
        //         if wireTest.Selected = true
        //         then (wiresList@[wireTest], bBoxesList@[boundingBox])
        //         else (wiresList, bBoxesList)
        //     List.fold2 checkWire ([],[]) model.Wires model.wBB
        
        let remainingWires = fst remainingWiresAndBoxes
        let remainingBbox = snd remainingWiresAndBoxes
        {model with Wires=remainingWires; wBB=remainingBbox}, Cmd.ofMsg (Symbol (Symbol.DeleteSymbol))

    | SelectWire (symToSel, wiresToSel) ->
        let selectWires = 
            List.map (fun (wire:Wire) -> if List.contains wire wiresToSel
                                         then {wire with Selected=true}
                                         else {wire with Selected=false} ) model.Wires
        {model with Wires=selectWires}, Cmd.ofMsg (Symbol (Symbol.SelectSymbol symToSel))

    | Dragging ((symbolUpdated,[wireUpdated,segIndex]), prevPos, mousePos) ->
        //probably need to unselect the other selected wires?
        let updatedWires = List.map (fun wire -> if wire.Id = wireUpdated.Id
                                                 then {wire with Vertices=updateVertices segIndex wireUpdated mousePos}
                                                 else wire ) model.Wires
        {model with Wires=updatedWires}, Cmd.ofMsg (Symbol (Symbol.Dragging (symbolUpdated,prevPos,mousePos)))

    | UpdateBoundingBoxes (symbolUpdated,[wireUpdated,segIndex]) -> 
        //let updatedBBoxes = List.map (fun wire -> if wire.Id = wireUpdated.Id
        //                                          then wireBoundingBoxes wire.Vertices
        //                                          else wire) model.WX
        //let updatedBBoxes = List.mapi (fun wireIndex wire -> if wire.Id = wireUpdated.Id
        //                                                     then wireBoundingBoxes wireUpdated.Vertices
        //                                                     else wire) model.WX
        let updatedBBoxes = 
            let findIndex = 
                model.Wires
                |> List.indexed
                |> List.filter (fun (idx, wire) -> wire.Id = wireUpdated.Id )
            let decodeIndex = match findIndex with 
                              | [(idx, wire)] -> idx
                              | _ -> failwithf "Error"

            model.wBB 
            |> List.indexed
            |> List.map (fun (index, bb) -> if index = decodeIndex then wireBoundingBoxes wireUpdated.Vertices else bb )
        
        {model with wBB=updatedBBoxes}, Cmd.ofMsg (Symbol (Symbol.UpdateBBoxes (symbolUpdated)))

        // let updatesBbox =
        //     let indexforBbox = List.findIndex (fun w -> w.Id = rank) model.Wires
        //     let updateBBox index boxList =
        //         let diff2 = posDiff pagePos model.Wires.[index].LastDragPos
        //         let {X = correctX; Y= correctY} =  posAdd (model.Wires.[index].Pos) diff2 
        //         if index = indexforBbox then [correctX-10.,correctY-10.;correctX+10.+model.Wires.[index].W, correctY-10.; correctX+10.+model.Wires.[index].W, correctY+10. + model.Wires.[index].H; correctX-10.,correctY+10.+ model.Wires.[index].H] else boxList
        //     List.mapi (fun i p -> updateBBox i p) model.wBB
        // {model with Wires = dSymbols; wBB = updatesBbox}, Cmd.none


//---------------Other interface functions--------------------//

/// look up wire in WireModel
//let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
//    wModel.Wires 
//    |> List.contains (fun wire -> wire.Id = wId)

///// Given a point on the canvas, returns the wire ID of a wire within a few pixels
///// or None if no such. Where there are two close wires the nearest is taken. Used
///// to determine which wire (if any) to select on a mouse click
//let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 

//    let listOfBoundingBoxes = wModel.Wires |> List.map (fun x -> x.BoundingBoxes)
//    let isInside bblst  =
//        let inSeg lst =
//            let wireId, box1, box2 = lst
//            if (pos.X>= box1.X && pos.X <= box2.X) && (pos.Y >= box1.Y && pos.Y <= box2.Y) then (true, wireId) else (false, wireId)            
//        bblst 
//        |> List.map inSeg
    
//    let mapToBB = 
//        listOfBoundingBoxes 
//        |> List.collect isInside
//        |> List.filter (fun (x,y) -> x=true)

//    match mapToBB with 
//    | [(true, wireId)] -> Some wireId
//    | _ -> None
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"
    
let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"


    



