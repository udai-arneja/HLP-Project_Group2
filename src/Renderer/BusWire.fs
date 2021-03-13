module BusWire

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
/// extra information for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separate datatype and Id, or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcSymbol: CommonTypes.ComponentId // source symbol
    TargetSymbol: CommonTypes.ComponentId // target symbol
    SrcPort: string
    TargetPort: string
    Vertices: XYPos List
    Highlighted: bool
    BusWidth: int
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    wBB: (XYPos*XYPos) list List
    }

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (string * string)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire of int list
    | MouseMsg of MouseT
    | HighlightSingleWire of int list



type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    Vertices: XYPos list
    Highlighted : bool
    BusWidth: int 
    ColorP: string
    StrokeWidthP: string }


/// look up wire in WireModel
let wire (wModel: Model)(wId: CommonTypes.ConnectionId): Option<Wire> =
    wModel.WX
    |> List.tryPick (function {Id = wId} as x -> Some x | _ ->  None) 

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
/// We are able to view the wire through this function, as well as the bus width legend, and a change in colour if highlighted, or if the wire has 
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
                    
                    
                    SVGAttr.Stroke props.ColorP //(if props.BusWidth = 1 then props.ColorP elif props.Highlighted = true then "yellow" else "darkorchid")
                    SVGAttr.StrokeWidth (if props.BusWidth = 1 then props.StrokeWidthP else "5px")
                    SVGAttr.StrokeLinecap "round"  ] []

            let busWidthLegend =
                let legendPos = Symbol.posAdd props.Vertices.Head {X=10.;Y=5.}
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
                
            let singleSeg = segmentList props.Vertices // takes in the list of vertices that make up a wire and maps these to segments. 
            let segmentsIntoLine = singleSeg |> List.map singularLine 
            (busWidthLegend:: segmentsIntoLine)
            |> ofList 
                     
         
                    )

/// The view function takes every wire in the model, and its attributes, and maps this to singleWireView. The helper
/// function convertIdToXYPos is not my code, but a teammates. 
let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        model.WX 
        |> List.map (fun w -> 
            let convertIdToXYPos inOut (id:string) = // this function was not written by me, pmc18. This is a helper function from a teammate.
                match inOut with 
                |1 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.InputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
                      |>List.head
                      |>tupleToXYPos 
                |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.OutputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
                      |>List.head
                      |>tupleToXYPos     //find symbol Id --> go through symbol list --> go through inputlist in symbol --> find portid --> find port number --> calc XY pos
            let start = convertIdToXYPos 1 w.SrcPort
            let final = convertIdToXYPos 0 w.TargetPort
            let vertex = newWireRoute final start
            let BusWidth = w.BusWidth
            let Highlighted = w.Highlighted
            let wireColour = match (BusWidth, Highlighted) with 
                             | (_, true) -> "yellow"
                             | (1, false) -> "red"
                             | (_, _) -> "darkorchid"
            let props = {
                key = w.Id
                WireP = w
                Highlighted= w.Highlighted
                BusWidth = w.BusWidth
                SrcP = start 
                TgtP = final 
                Vertices = vertex
                //ColorP = model.Color.Text()
                ColorP = wireColour
                StrokeWidthP = "2px" }
            singleWireView props) // pass in the props for this given wire into singleWireView
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg)) 
    g [] [(g [] wires); symbols] // displaying the wires and symbols 

/// A function which creates a new Bounding Box for a wire. 
let createNewBB outp inp= 
    wireBoundingBoxes (newWireRoute (inp) (outp))

/// A function which creates a new wire. This is called from the AddWire message in the update function. 
let createNewWire (sourcePortId:string) (targetPortId:string) (busWidth: int) (model:Model) : Wire =
    
    let convertIdToXYPos inOut (id:string) =
        match inOut with 
        |1 -> printfn "bottomup %A %A" model.Symbol.Symbols id
              List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.InputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
              |>List.head
              |>tupleToXYPos 
        |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.OutputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
              |>List.head
              |>tupleToXYPos     //find symbol Id --> go through symbol list --> go through inputlist in symbol --> find portid --> find port number --> calc XY pos
    let wireId = CommonTypes.ConnectionId (Helpers.uuid())

    {
        SrcSymbol = CommonTypes.ComponentId (Helpers.uuid()) 
        TargetSymbol = CommonTypes.ComponentId (Helpers.uuid())
        Id = wireId 
        SrcPort = sourcePortId
        TargetPort = targetPortId
        Vertices = newWireRoute  (convertIdToXYPos 0 targetPortId) (convertIdToXYPos 1 sourcePortId)
        Highlighted = false
        BusWidth = busWidth
    }


/// Initialisation function. Begins with no wires, and uses the Symbol model as a base. 
let init () = 
    let symbols, cmd = Symbol.init()
    {WX = []; Symbol = symbols;Color = CommonTypes.Red; wBB = []}, Cmd.none



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    let convertIdToXYPos inOut (id:string) =
        match inOut with 
        |1 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> (string y.Id) = id) x.InputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
              |> List.head
              |> tupleToXYPos
        |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> (string y.Id) = id) x.OutputPorts) |> function |Some a -> [a.PortPos] |None -> []) model.Symbol.Symbols
              |>List.head
              |> tupleToXYPos

    match msg with
    | Symbol sMsg -> 
        let newBB = 
            List.map (fun w -> wireBoundingBoxes (newWireRoute (convertIdToXYPos 0 w.TargetPort) (convertIdToXYPos 1 w.SrcPort) )) model.WX
        let sm,sCmd = Symbol.update sMsg model.Symbol 
        {model with Symbol=sm; wBB = newBB}, Cmd.map Symbol sCmd 
    | AddWire (inp,outp) -> {model with WX = createNewWire inp outp 1 model:: model.WX; wBB = createNewBB (convertIdToXYPos 1 inp) (convertIdToXYPos 0 outp) :: model.wBB}, Cmd.none 
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))
    | DeleteWire (sIdList) ->
        let wiresToKeepIndex (lst:int) = List.filter (fun x -> List.tryFind (fun y -> y = x) sIdList |> function |Some a -> false |None -> true) [0..lst]
        let dWires = 
             wiresToKeepIndex ((model.WX.Length)- 1)
             |> List.map (fun i -> model.WX.[i]) // (fun index value ->  List.tryFind (fun x -> x = index) sIdList |> function |Some a -> [] |None -> [value]) 
        let dBbox =
            wiresToKeepIndex ((model.wBB.Length)- 1)
            |> List.map (fun i -> model.wBB.[i])
        {model with WX = dWires; wBB = dBbox}, Cmd.none
    | HighlightSingleWire (intv) -> // for now, my highlightSingleWire acts as a selectwire. 
        
        let selectedWireList =
            let defaultList = List.map (fun (x:Wire) -> {x with Highlighted = false}) model.WX
            let checker x =
                let outcome = 
                    List.tryFind (fun w -> w = x) intv
                match outcome with 
                    |Some a -> {defaultList.[x] with Highlighted = true}
                    |None -> {defaultList.[x] with Highlighted = false}
        
            [0..(defaultList.Length-1)]
            |> List.map checker
        {model with WX = selectedWireList}, Cmd.none

    

//---------------Other interface functions--------------------//

/// look up wire in WireModel
//let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
//    wModel.WX 
//    |> List.contains (fun wire -> wire.Id = wId)

///// Given a point on the canvas, returns the wire ID of a wire within a few pixels
///// or None if no such. Where there are two close wires the nearest is taken. Used
///// to determine which wire (if any) to select on a mouse click
//let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 

//    let listOfBoundingBoxes = wModel.WX |> List.map (fun x -> x.BoundingBoxes)
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


    



