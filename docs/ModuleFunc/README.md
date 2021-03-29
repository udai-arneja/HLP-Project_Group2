# Module Functions

>## Symbol functions

- rect (xPos: 'a) (yPos: 'b) (width: 'c) (height: 'd) (colour: string) (props: 'e) 
  - makes a rectangle using the arguments provided to assign the dimensions and fill colour.

- circle (xPos: 'a) (yPos: 'b) (rad: 'c)
  - makes a circle using the arguments provided to assign the dimensions

- text (xPos: 'a) (yPos: 'b) (textAnchor: 'c) (domBaseline: 'd) (fontSize: 'e) (displayText: StringFormat<string>) 
  - makes a text element using the arguments provided to assign the dimensions.

- line (x1Pos: 'a) (y1Pos: 'b) (x2Pos: 'c) (y2Pos: 'd) (width: 'c) 
  - makes a line using the arguments provided to assign the dimensions.

- createPortList (comp:Symbol) (portType:CommonTypes.PortType) (portNumber:int) (width:int) (numPorts)
  - This function creates each of the ports for a symbol, it is mapped over the list of inputs and outputs using the InputPortsList and OutputPortsList functions.

- createNewSymbol (inputs: int list) (outputs: int list) (comp:CommonTypes.ComponentType) 
  - creates a new variable/record of the type Symbol. 

- createCustomSymbol (comp:CommonTypes.CustomComponentType) 
  - this function is called to create the symbol for a custom component, the height of this symbol is set by the number of inputs/outputs. 

- createNewBoundingBox (inputs: int list) (outputs: int list) (sym: Symbol) 
  - creates a bounding box with dimesions similar to that of the symbol passed in as an argument. 

- portmove (portId:string) (inputYes:PortVisibility) (model:list<Symbol>) 
  - this function takes in a portId, whether it's an input or output port and the model, it then updates the port positions.  

>## BusWire functions

- wire (wModel: Model)(wId: CommonTypes.ConnectionId) 
  - looks up the wire in the wire model.

- newWireRoute  (targetPort: XYPos) (sourcePort: XYPos)  
  - newWireRoute calculates the wire route between 2 port positions. It returns a list of XY Positions, which are in the form of vertices, including the source port and target port positions. It calculates how many segments the wire will have. 

- segmentList (vertexList: XYPos list) 
  - A useful function to convert the list of vertices into a list of segments. A list of vertices is an XYPos list, whereas a list of segments is a list in which each element is XYPos*XYPos (to represent start and end points).

- wireBoundingBoxes (verticesList: XYPos list) 
  - Finding the bounding box for a given wire (ie, list of vertices). The padding determines how large you would like the bounding box to be. Returns XYPos*XYPos list, with both the XYPos being the diagonal points in a bounding box. 

- singleWireView 
  - singleWireView maps the list of vertices to a list of segments, then draws each individual line by passing in the XY positions to singularLine.We are able to view the wire through this function, as well as the bus width legend, and a change in colour if Selected, or if the wire has a larger bus width. 

- createNewBB outp inp 
  - A function which creates a new Bounding Box for a wire. 

- createNewWire (sourcePort:string) (targetPort:string) (model:Model) : Wire 
  - A function which creates a new wire. This is called from the AddWire message in the update function.

>## Sheet functions

- displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model) - 

- view (model:Model) (dispatch : Msg -> unit) - 

- inSelBox (model: Model) (sc:XYPos) (ec:XYPos): (CommonTypes.ComponentId list * (BusWire.Wire*int) list) - 

- wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : (BusWire.Wire * int) list - 

- renderBusWidth (dispatch: Msg -> unit) (width: int) (model: 'a) -

- update? 
