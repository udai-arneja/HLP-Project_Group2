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
