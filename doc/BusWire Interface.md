

## INTERFACES

### Sheet -> BusWire 

The **BusWire** module will contain a list of bounding boxes for the wires which can be viewed and accessed by **Sheet**. **Sheet** will send the following messages to the **BusWire** module, based on this. **Sheet** is checking if the Wire is Valid to be routed.
```
AddWire (InputPortId : CommonTypes.Port.Id) (OutputPortId: CommonTypes.Port.Id)
```
**BusWire** can interface with **Symbol** to find the XYPos of the Ports
```
DeleteWires (WireIdList : CommonTypes.ConnectionId list)
```
```
HighlightWires (WireIdList : CommonTypes.ConnectionId list)
```
```
SelectWires (WireIdList : CommonTypes.ConnectionId list)
```
Further messages will need to be defined when manual routing is implemented. 
####MoveWire : for manual routing - potentially requiring XYPos/XYPos difference


### Symbol -> BusWire
The Symbol module can be accessed by BusWire. 
```
getPortLocation (portId: CommonTypes.Port.Id) : XYPos 
```
This interface function allows **BusWire** to pass in a PortId and obtain the XYPos of that Port. This enables BusWire to easily route between the 2 XY positions of the Ports. 
```
getBusWidth (portId: CommonTypes.Port.Id) : int
```
This function passes in a PortId to **Symbol**, and **Symbol** returns an integer for the number of bits of the given Port. 

**Priya Chhaya - pmc18 BusWire additional Documentation**
```
createNewWire (sourcePortId:string) (targetPortId:string) (busWidth: int) (model:Model) : Wire
```
createNewWire is called by the AddWire message from sheet and creates a wire.

The singleWireView function maps the list of vertices to a list of segments, and then draws each individual line by passing the XY positions into a subfunction 
singularLine. We are able to view the wire and the bus width legends through this function. 
```
view (model:Model) (dispatch: Dispatch<Msg>)
```
The view function pipes each wire in the model to be displayed in the singleWireView function. 
```
newWireRoute (sourcePort: XYPos) (targetPort: XYPos) : XYPos list 
```
newWireRoute function routes the wire between the 2 port XY positions and determines the number of segments in a wire. It returns a list of vertices. 
```
wireBoundingBoxes (verticesList: XYPos list) : XYPos*XYPos List 
```
the wireBoundingBoxes function takes in the list of vertices (list of XYPos) for the wire and calculates the wireBoundingBox by returning XYPos * XYPos list.

```
wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : CommonTypes.ConnectionId option
```

wireToSelectOpt function (within Sheet for easier functionality, in pmc18 code) is another function I wrote, which determines whether an XYPos of a mouse click is 
within the wire bounding box. If it is, it returns the wire Id, so we can send the highlight message to select it.  

```
createNewBB (outputPortPos : XYPos) (inputPortPos : XYPos)
```
createNewBB function creates a new bounding box from an input port position and output port position (by also calling newWireRoute within the function). This function is primarily a helper function). 

```
segmentList (vertexList: XYPos list) : (XYPos * XYPos) list
```
segmentList takes in a list of Vertices and returns a tuple of XYPos, describing each segment. Note that the list of vertices passed in include the source and target port locations. This is a useful helper function. 



