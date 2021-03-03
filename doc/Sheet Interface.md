## CHANGES TO COMMON TYPES

### Port Type

Adding the XYPos to **CommonTypes** of Port Type. This is to be intialised and updated by the **Symbol** - any updates should be calculated by **Symbol** through the given XYPos diff when a symbol is being moved. All other modules can access this updated XYPos.

## INTERFACES

### Overall Interfaces

#### Mouse Interface

All mouse interactions will be observed by **Sheet**. **Sheet** will then analyse and propagate the relevant information to **Buswire** and **Symbol**. Mouse information will be sent as type MouseT MouseMsg already defined in **CommonTypes**. In the propagation, the zoom of the canvas will be included - enables **Symbol** and **Buswire** entity movements to be matched with sheet (and mouse) cursor
movements.

If this has performance degradation, listeners/other methods of mouse interactions will be considered.

### Sheet -> Symbol
As above, **Sheet** sends messages to **Symbol**. The messages are as below:
```
AddSymbol (SymbolId: CommonTypes.ComponentId) (Position: XYPos) (SymbolType: Component.ComponentType) (NoOfInputPorts: int) (NoOfOutputPorts: int) :
```
* Considering changing this to a Record for better Type Protection and Readability
* This also indicates that **Sheet** makes the UUID - this can be moved to **Symbol** if **Symbol** makes bounding boxes for symbols rather than **Sheet**.

```
DeleteSymbol (SymbolIdList : CommonTypes.ComponentId List)
```
```
StartDragging (SymbolId : CommonTypes.ComponentId) (pagePos: XYPos)
Dragging (rank :CommonTypes.ComponentId) (pagePos: XYPos)
EndDragging (SymbolId : CommonTypes.ComponentId)
```
```
SymbolSelection (SymbolIdList : CommonTypes.ComponentId List)
```

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

### Simi Individual Interface Symbol Sheet
Very similar to above apart from a few features and the fact that indicies lists are used instead of Ids for some but these are interchangeable very easily 
Works on the basis that the wire/symbol list and wire/symbol bounding box list correspond so matching components and bounding boxes have the same index 
The bounding box is a part of the model for wire/ symbol but the bounding box calculations are done in sheet
The Wire bounding box calculation for whether the point is in a wire bounding box was done by Priya but the other calculations (point in symbol box and whether selecting encompasses symbol/wire box) was done by me 

    | ShowPorts of int list
     Show ports when mouse near the symbol bounding box -> int is index of symbol in symbol list
    | ShowValidPorts of string*string*XYPos
     When wiring, show ports that can be connected to so two inputs arent selected etc
    | EndDraggingList of sId : CommonTypes.ComponentId list *pagePos:XYPos
    | DraggingList of sId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
      Both used to move sections of components, keeping the relative distance

### Simi Individual Interface BusWire Sheet
    | HighlightSingleWire of int list
      Same as above but uses a list of selected indices



