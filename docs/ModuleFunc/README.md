# Module Functions

>## Symbol functions

- `rect (xPos: 'a) (yPos: 'b) (width: 'c) (height: 'd) (colour: string) (props: 'e)`  
  - makes a rectangle using the arguments provided to assign the dimensions and fill colour.

- `circle (xPos: 'a) (yPos: 'b) (rad: 'c)` 
  - makes a circle using the arguments provided to assign the dimensions

- `text (xPos: 'a) (yPos: 'b) (textAnchor: 'c) (domBaseline: 'd) (fontSize: 'e) (displayText: StringFormat<string>)` 
  - makes a text element using the arguments provided to assign the dimensions.

- `line (x1Pos: 'a) (y1Pos: 'b) (x2Pos: 'c) (y2Pos: 'd) (width: 'c)` 
  - makes a line using the arguments provided to assign the dimensions.

- `createPortList (comp:Symbol) (portType:CommonTypes.PortType) (portNumber:int) (width:int) (numPorts): CommonTypes.Port` 
  - This function creates each of the ports for a symbol, it is mapped over the list of inputs and outputs using the InputPortsList and OutputPortsList functions. The individual port positions are set based on the component, port type and port number.  

- `createNewSymbol (inputs: int list) (outputs: int list) (comp:CommonTypes.ComponentType)` 
  - creates a new variable/record of the type Symbol. Certain factors (width, input/output ports) are set based on the component type. 

- `createCustomSymbol (comp:CommonTypes.CustomComponentType)` 
  - this function is called to create the symbol for a custom component, the height of this symbol is set by the number of inputs/outputs. 

- `createNewBoundingBox (inputs: int list) (outputs: int list) (sym: Symbol)` 
  - creates a bounding box around the symbol using two diagonally opposite vertices. The bounding box is fitted to the symbol.

- `portmove (portId:string) (inputYes:PortVisibility) (model:list<Symbol>)` 
  - this function takes in a portId, whether it's an input or output port and the model, it then updates the port positions. <- unsure

- `update (msg: Msg) (model: Model)` 
  - takes in a Msg and the Model, then returns an updated model with the changes that the Msg stated.  

>## BusWire functions

- `wire (wModel: Model)(wId: CommonTypes.ConnectionId) `
  - looks up the wire in the wire model.

- `runBusWidthInference model`
  - Converts all symbols to components and all wires to connections.
  - If there is an error, highlight the wires. 

- `autoroute (isHorizontal: bool) (nextPort: XYPos) (startPort:XYPos) (endPort: XYPos) (model:Model) (lastPos: XYPos) (count:int)`
  - A recursive function that routes the wire segment by segment. 
  - Recursion:
    - Base case. The source port y coordinate is the same as the source port x coordinate and with no symbol bounding boxes in its way. If the nextPort = endPort.
    - If there is a symbol bounding box in the way, it will backtrack it's steps to just before the symbol bounding box. 
    - Switch orientation. E.g horizontal line -> vertical line
    - Reroute to the corner of the bounding box (of the symbol in the way) from which, when the wire rotates again, it will be able to route to the y position of the target port. 
    - If there is another bounding box in the way, repeat. 

- `autoroute5 (isHorizontal: bool) (nextPort: XYPos) (startPort:XYPos) (endPort: XYPos) (model:Model)(lastPos:XYPos) (count:int)`
  - Works in the same way to autoroute, however, adapted for an initial 5 segment wire orientation. 

- `newWireRoute  (targetPort: XYPos) (sourcePort: XYPos)  `
  - newWireRoute calculates the wire route between 2 port positions. It returns a list of XY Positions, which are in the form of vertices, including the source port and target port positions. It calculates how many segments the wire will have. 

- `segmentList (vertexList: XYPos list)` 
  - A useful function to convert the list of vertices into a list of segments. A list of vertices is an XYPos list, whereas a list of segments is a list in which each element is XYPos*XYPos (to represent start and end points).

- `wireBoundingBoxes (verticesList: XYPos list) `
  - Finding the bounding box for a given wire (ie, list of vertices). The padding determines how large you would like the bounding box to be. Returns XYPos*XYPos list, with both the XYPos being the diagonal points in a bounding box. 

- `singleWireView `
  - singleWireView maps the list of vertices to a list of segments, then draws each individual line by passing in the XY positions to singularLine.We are able to view the wire through this function, as well as the bus width legend, and a change in colour if Selected, or if the wire has a larger bus width. 

- `view`
  - The view function takes every wire in the model, and its attributes, and maps this to singleWireView. 

- `createNewBB outp inp` 
  - A function which creates a new Bounding Box for a wire. 

- `createNewWire (sourcePort:string) (targetPort:string) (model:Model)` : Wire 
  - A function which creates a new wire. This is called from the AddWire message in the update function.

- `updateVertices (segId: int) (wir: Wire) (mPos: XYPos)` : XYPos list
  - updateVertices updates the vertices of the moving segment to match that of the mouse position. It takes in the index of the segment, the wire and the XYPos of the mouse. If the index is even it will only move vertically and vice versa. The initial list of vetices is indexed and then mapped over a function that will change the value of the vertices with indexes equal to the segment index and segment index + 1.


>## Sheet functions

- `displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model) `
  - Top level rendering of features 
    - Multi-select box
    - zoom, 
    - Dispatches mouse messages to update function
    - Draws the canvas

- `view (model:Model) (dispatch : Msg -> unit) `
  - The view function takes every connection in the model, and its attributes, and maps this to singleWireView. 

- `inSelBox (model: Model) (sc:XYPos) (ec:XYPos): (CommonTypes.ComponentId list * (BusWire.Wire*int) list) `
  - This function checks to see which symbols and wires are within the selection box. It is determined by the corners of the wires and the symbols. Finds the symbol and wire Id. Returns a tuple of the lists of the symbols and wires contained. 

- `wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : (BusWire.Wire * int) list` 
  - Checks to see if the mouse position is within that of a wire bounding box within the model. Checks segment by segment. If it is then it returns the wire id and the segment index. This is used for manual routing.

- `renderBusWidth (dispatch: Msg -> unit) (width: int) (model: 'a)` 
  - Renders the busWidth of a component

- `renderDropdownInput model dispatch `  
  - Allows user input to edit the buswidth of components.

- `renderItem component dispatch width model`
  - Create the component with the correct width.

- `renderDropdown model dispatch`
  - Displays the drop down menu with component options.
