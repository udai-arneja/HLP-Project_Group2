# Module Interfaces

#### Mouse Interface

All mouse interactions will be observed by **Sheet**. **Sheet** will then analyse and propagate the relevant information to **Buswire** and **Symbol**. Mouse information will be sent as type MouseT MouseMsg already defined in **CommonTypes**. In the propagation, the zoom of the canvas will be included - enables **Symbol** and **Buswire** entity movements to be matched with sheet (and mouse) cursor
movements.

If this has performance degradation, listeners/other methods of mouse interactions will be considered.

### Sheet -> Symbol
**Sheet** sends messages to **Symbol**. The messages are as below:
```
AddSymbol (inputs:int list) (outputs:int list) (comp:ComponentType)
```
- SymbolIds are created 
- The first int list is a list of the buswidths of the input ports and second is buswidths of output ports
- All buswidths are stored in the port type for BusWire to access 
- Any custom type information will be sent alongside the ComponentType part

```
DeleteSymbol
```
- This has no inputs, it goes through the list of selected symbols and deletes them alongside their bounding boxes.

```
Dragging (sId:CommonTypes.ComponentId) (pagePos: XYPos) (prevPagePos: XYPos)
```
- There are 2 dragging elements: dragging a list of symbols and dragging a symbol 
- This depends on how many symbols are selected which is information stored in the model by the IsSelected bool 
- If only one symbol is selected -> The first XYPos and initial dragging interface can be used to move it so ignore the second XYPos  
- Note on above: you can also use the second XYPos to calculate the difference and add to the Pos of the selected symbol (your choice) 
- If multiple symbols are selected -> The first XYPos shows last mouse position and second XYPos is the current position: using differences you can move all the elements selected, keeping their relative distance (my code on this is messy as always but it could be a good starting point if needed)
- Also neceassry to update port position here 

```
ToggleSymbol (selectedSymbol: ComponentId list)
```
- This is a list of componentIds sent as a message from BusWire
- It is select/deselect symbol and will be a list of the necessary Ids 
- Usually it will just be one but when you drag to select many then it'll be a list  
- All of the selected symbols information is stored within symbol now 
- Toggle between IsSelected = true or false 
- If you're sent an empty list it means deselect everything 

```
ShowValidPorts (inOut: PortVisibility) (portId: string) (mousePos:XYPos)
```
- This is for the first stage of wiring when we have selected a port and are now moving it to the next port 
- You get whether this intial port is an input or output, the portId and the XYPos of the cursor for moving the port 
- This is also the point where you show any of the valid ports on other symbols 
- Simi did a port status for visible, invisible, slidingport, validports for the different stages but there's definitely a much better way 
- Also need to reset the port position of the "sliding" port after this message stops sending 

```
Hovering (sId: ComponentId list)
```
- This is for when the mouse moves over a component and you need to show the ports on it 
- It is a list of ComponentIDs but will only be one Id (do we need this)

```
MouseMsg (MouseT)
```
- Mouse info with coords adjusted from top-level zoom
```
SnapSymbolToGrid (sId: CommponentId)
```
- Snaps the symbol to the closest corner of the grid
```
HighlightSymbol (sId: ComponentId)
```
- Highlights the symbol that is passed in as an argument/ makes a list of symbols to be highlighted
```
DuplicateSymbol
```
- Makes new symbols with the same parameters as the ones selected by mapping over the list of selected symbols and their parameters
```
DroppingNewSymbol (mousePos:XYPos)
```
- When a symbol is created it's on the mouse, this message drops the symbol onto the canvas

### Sheet -> BusWire 

```
Symbol (sMsg : Msg)
```
- This Msg contains a Msg  
```
AddWire (InputPortId : CommonTypes.Port.Id) (OutputPortId: CommonTypes.Port.Id)
```
- Sends 2 port Ids and calls the function createNewWire portId1 portId2 which routes between the 2 port positions
```
DeleteWire
```
- Deletes the selected wires
```
MouseMsg (mMsg : MouseT)
```
Mouse info with coords adjusted from top-level zoom
```
ToggleSelect (symToSel : list<ComponentId>) (wireAndSegList : list<wire * int>) 
```
- Toggles whether the wire is selected.
```
Dragging (((symbolUpdated: list<ComponentId>)*(wireAndSegList : list<wire * int>)*(prevPos : XYPos)*(mousePos : XYPos)) 
```
- If there are Wires within the dragging message, then we manual drag the wire by calling the updateVertices function. If the list of wires sent is empty, then the Symbol dragging list will contain symbols, and the message will be sent to Symbol.
```
SnapToGrid ((symToSel : list<ComponentId>)*(wireAndSegList :  list<wire * int>) 
```
- If there are wires within the SnaptoGrid message, then it will snap the wire selected to the segment selected to the grid. If there are no wires sent in the list, then the Symbol list will contain elements to be snapped to grid, and the message will be sent to Symbol. Snap to Grid works by snapping the top left corner of a symbol to a vertex of the grid, or a segment of a wire to a grid line.
```
UpdateWires ((newWire : list<Wire>) * (newBB : list<list<XYPos * XYPos>>) * (newSymbols : list<Symbol>) * (newsBB : list<XYPos * XYPos>))
```
- Updates the bounding box of the wires as they are being changed.
```
RunBusWidthInference
```
- Calls the function runBusWidthInference which includes bus width inference functionality from issie into our project. Can be seen through the toggle developer tools when running our application.

### Keyboard Interface 

- Ctrl-N 
  - Opens the drop down menu for symbol selection. 
- Alt-0
  - To run BusWidth inference.
- Alt-Up
  - To zoom in.
- Alt-Down 
  - To zoom out.
- Ctrl-D
  - To delete.
- Ctrl-C 
  - To duplicate.
 <!---



