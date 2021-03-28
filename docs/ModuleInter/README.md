# Module Interfaces

### BusWire Messages

| AddWire of (string, string)
- Input port Id then output port Id (the opposite of your source and target)
- BusWidth will be taken from symbol ports
- Highlight wire in Ok Red if buswidths don't match and None if anything else 

| ToggleSelect of (string list, string list)
- Component Id list and wire Id list 
- Take the second list and toggle IsSelected for all those wires 
- Send a message onto Symbol with the first id list -> message is ToggleSelect of (ComponentId list)
- All the selected information is stored with BusWire
- If you're sent an empty list it means deselect everything (Simi has a defaultlist function to make it all false if you want)
- Potential to send message ToggleSelect of (ComponentId list, PortId list) to symbol if a wire is selected to highlight the associated ports but this can be done later

| MoveSegment of (string list, int, XYPos)
- WireId of the moving wire, the index of the segment being moved and the XYPos of the mouse 
- You are doing the difference calculations for the XYPos just like when symbol moves -> there is a function for it in symbol
- Only middle 3 segments for 5 segment wires and 1 segment for 3 segment wires move
- Even index is horizontal segment; odd is vertical segment
- Need to discuss with symbol about whether the enable port will be on the bottom so it maye be necessary to check the "input"/target port wire but that can be dine later 
- Horizontal only moves up/down and vertical only moves left/right 
- Autoroute as you would first then make the adjustments to the final vertices 

| UpdateBBoxes of (string list, string list)
- (ComponentId list, WireId list)
- Same function as in symbol and once again you send the same message to symbol but with only the componentID list 
- When a symbol is being dragged it is likely that there is no wire selected but obviously you will still be autorouting as its position changes
- On this message update the bounding boxes of the wires for the only time apart from when moving segment  

| Delete
- No type -> just delete the selected wires
- when you delete a symbol, you must delete its associated wires by checking the ports around the selected symbols and seeing if the Ids are in any wires (Simi has most of this code in sheet but it needs some tweaking to work for you)


>## Interfaces

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
- What does it do? undo stores symbol and XYPos and returns symbol to symbol list in same position

### Sheet -> BusWire 

```
Symbol (sMsg : Msg)
```
This Msg contains a Msg  
```
AddWire (InputPortId : CommonTypes.Port.Id) (OutputPortId: CommonTypes.Port.Id)
```
**BusWire** can interface with **Symbol** to find the XYPos of the Ports
```
DeleteWire
```
```
MouseMsg (mMsg : MouseT)
```
```
ToggleSelect (symToSel : list<ComponentId>) (wireAndSegList : list<wire * int>) 
```
```
Dragging (((symbolUpdated: list<ComponentId>)*(wireAndSegList : list<wire * int>)*(prevPos : XYPos)*(mousePos : XYPos)) 
```
```
SnapToGrid ((symToSel : list<ComponentId>)*(wireAndSegList :  list<wire * int>) 
```
```
UpdateWires ((newWire : list<Wire>) * (newBB : list<list<XYPos * XYPos>>) * (newSymbols : list<Symbol>) * (newsBB : list<XYPos * XYPos>))
```
```
NewComponent
```
```
RunBusWidthInference
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

<!---
### Keyboard Interface 


