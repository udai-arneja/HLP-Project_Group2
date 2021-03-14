## New and Pretty Much Final Interfaces 
### To discuss/ for everyone to think about:
- Snap To Grid
- Issie Interface
- Custom Components
- Zoom
- Manual Routing
- Any additional functionality for the sections
- Each group please add any information on the model states and bouding boxes used at the bottom of this 
- Any issues ask the group chat 

### Progress Deadlines:
Wednesday 17th: 
  - Sheet: all messages done, zoom with keeping centre of page done, snap to grid plan 
  - BusWire: manual routing and functionality near completion
  - Symbol: functionality near completion especially regarding 
  
Thursday 18th: **Potential Group Call** 
  - All intefaces checked and compatible with clean code throughout 
  
Saturday Evening 20th: **Potential Group Call** 
 - Issie Interfacing start 
 
Wednesday 25th: **Potential Group Call**
 - A good draft that could be submitted (Issie interface not necessary for this)

### A Note on States:
- Bounding boxes are a part of BusWire and Symbol models and therefore they must be updated when necessary 
- BusWire has some messages to send to symbol (Simi will write at end of BusWire code)
- BusWire and Symbol: it is worth reading each other's sections as there is a lot more interconnectivity than before 
- All wireId, portId or ComponentId are sent as strings for now but can be changed after discussion

### Symbol Messages:
| AddSymbol of (ComponentType, int list, int list)
- Bounding box should be 10. bigger than the symbol and made up of 2 points (top left; bottom right)
- Due to drage and drop, the inital position should always be set to 10.;10.
- You create your own SymbolIds as sheet has no need to 
- The first int list is a list of the buswidths of the input ports and second is buswidths of output ports
- All buswidths are stored in the port type for BusWire to access --> common types needs to add int to port type for the buswidth
- Any custom type information will be sent alongside the ComponentType part 

| Drag of (XYPos, XYPos) 
- There are 2 dragging elements: dragging a list of symbols and dragging a symbol 
- This depends on how many symbols are selected which is noe information stored in your model by the IsSelected bool 
- If only one symbol is selected -> The first XYPos and initial dragging interface can be used to move it so ignore the second XYPos hwoever you will have to remove the "rank" part as we no longer send the component Id 
- Note on aboce: you can also use the second XYPos to calculate the difference and add to the Pos of the selected symbol (your choice) 
- If multiple symbols are selected -> The first XYPos shows last mouse position and second XYPos is the current position: using differences you can move all the elements selected, keeping their relative distance (my code on this is messy as always but it could be a good starting point if needed)
- Also neceassry to update port position here 
- If selected is all false then the last symbol in the symbol list is being dropped on the page -> it has just been created and it moving to its new location

| ToggleSelect of (string list)
- This is a list of componentIds sent as a message from BusWire
- It is select/deselect symbol and will be a list of the necessary Ids 
- Usually it will just be one but when you drag to select many then it'll be a list 
- We don't currently highlight symbol but this may change 
- All of the selected symbols information is stored within symbol now 
- Toggle between IsSelected = true or false 
- If you're sent an empty list it means deselect everything (Simi has a defaultlist function to make it all false if you want)

| ShowValidPorts of (string, string, XYPos)
- This is for the first stage of wiring when we have selected a port and are now moving it to the next port 
- You get whether this intial port is an input or outpur, the portId and the XYPos of the cursor for moving the port 
- This is also the point where you show any of the valid ports on other symbols 
- Simi did a port status for visinle, invisible, slidingport, validports for the different stages but there's definitely a much better way 
- Also need to reset the port position of the "sliding" port after this message stops sending 

| Hovering of (string list)
- This is for when the mouse moves over a component and you need to show the ports on it 
- It is a list of ComponentIDs but will only be one Id 

| UpdateBBoxes (string List)
- For the dragging or moving of a component so we only update the bounding boxes when movement has stopped
- Message comes from buswire 
- It is a ComponentId list 
- The Pos of the symbol would be changing as it is moved so the bounding box is from the symbol's last posiiton

| Delete
- This has no inputs -> it just means delete all the symbols that have been selected 

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

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


### Below an overview of the project is highlighted, with focus on the interfacing the three main modules ( **Symbol**,  **BusWire**,  **Sheet** ). For further information about individual modules, in particular their functions, see the individual documentation (doc folder).

## Interface documentation 

In the demo code the three modules each have views that create objects on the SVG canvas:
* Symbol: the component outlines
* BusWire: the connecting wires and wire annotations
* Sheet: The rubber band showing the new connection during drag-and drop connection. The dyanmically sized circles showing 
connection end-points. Possibly dynamic guides showing component algnment.

These have a parent child relationship:

`Sheet --> BusWire --> Symbol`

So the `BusWire` module can have a view function that depends on the `Symbol` module state (`Symbol.model`). The `Symbol` view 
function cannot depend on anything except `Symbol.model`. This dependence is needed because wire endpoints are defined 
by symbol and port positions.

Considering structure of:

`Sheet --> BusWire --> Symbol` and `Sheet --> Symbol`

Thus only one endpoint, but faster performance - hence this will be implemented if performance problems found.

<!---
Good overview of how Elmish large-scale structure is supported by various built-in Elmish functions. This is written in the context
of a web application with (effectively) multiple pages written as a SPA (single-page application). The sections on Commands are helpful, some other parts less so.
* [Working with Elmish](https://medium.com/@MangelMaxime/my-tips-for-working-with-elmish-ab8d193d52fd). 

## Information on SVG React Helpers for Elmish

See SVG syntax below, and the skeleton, and [w3schools](https://www.w3schools.com/graphics/svg_intro.asp) for how to use SVG elements. 
For project work the SVG elements certainly needed are:
* `line` (as in skeleton `Buswire`)
   * Possibly better alternatives `polyline` and `polygon` which take a sequence of points. See skeleton for example of polygon.
* For more complex objects `path` which can do anything.
* `text` for text - see [w3schools](https://www.w3schools.com/graphics/svg_text.asp) etc
* `g` which groups elemnets (like `div` for normal HTML), see skeleton.
* `svg` which creates an SVG canvas of specified size, see skeleton.

Note also that the order in which elements are put onto a canvas matters. the *last* element will be on top. In the skeleton
* the blue overlay translucent triangle is on top, because it is last in its `g` list of elements.
* That means that it alters the color of the elements underneath (opacity 0.1). Also mouse clicks are collected by that element
and not passed through to the underlying svg circles. Therefore the skeleton dragging does not work. 
The solution is to use bounding boxes and determine which object is clicke dprogrammatically.

-->

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

The SVG window is not focussed to receive key presses - electron does not therefore support them directly, and the `OnKeyPress`, 
`OnKeyDown` DOM attributes will now work. Instead you can use a subscription and define electron global shortcuts for 
the keys you need in (perhaps invisible) electron menus as in the skeleton code.

### Zoom

The skeleton code uses a fixed size SVG canvas of 1000X1000 pixels. This is then transformed by zoom. Possibly 1000X1000
is too small (though good for a demo) 2000X2000 would be more typical for zoom=1 to fill the screen, but it is not very critical.
Because zoom is variable all
that matters is the relative size of components (symbols) and the canvas, which determines how large is the canvas 
relative to the objects on it. Symbols will probably also have fixed size text and this must be readable after zoom.

It is important that zoom is separated from all other use of coordinates, so that the current zoom 
can be ignored when writing all other SVG code.

The skeleton code has working zoom, but does not quite do this perfectly. It would need to 

### SVG Geometry

### Bounding boxes

A common technique used in 2d drawing is that of bounding boxes. Objects are approximated by rectangles, specified by two pairs
of corner coordinates: *bounding boxes*. It is easy to write code that determines:
* Is a point within a bounding box?
* Do two bounding boxes intersect?
Code to do these things is inherently fast can get slower if there are a very large number of objects.
In that case there are various speedups by sorting the
box coordinates along one dimension and doing a binary search. The Draw2d (as currently used by Issie) authors claim that Draw2d is slow 
because of the very inefficient checks of bounding boxes used in its implementation. Even fast techniques can be a problem if
used badly!

One typical example is in deciding which object (or objects) are clicked by mouse clicks and rectangular drags:
* The skeleton code follows WS2 svg demo and uses custom mouse event handlers embedded in the svg.
* This is not flexible enough for project work.
* You should use the `mouseMsg` message to detect mouse `up`, `down` and movement. 
* You can determine which object is clicked programmatically by using bounding boxes and checking whether the click coordinates
lie inside the bounding box. This allows:
   * two objects close together to be disambiguated as required
   * wires that are thin to have larger bounding boxes
   * symbols that are complex to have suitably sized bounding boxes which are processed by other code as well as for mouse clicks.


## Elmish Helper SVG Syntax


React virtual DOM elements are created in Elmish using F# helper functions of form:

```
elementName (props:IProp list) (children: ReactElement list)
```

They divide into SVG elements (that can be used as children of an `svg` element that creates an SVG canvas) and DOM elments 
that correspond to HTML DOM elements. There are also some `void` elements:

```
voidElementName (props: IProp list)
```

That deals with elements that are not allowed to have children.

See [Fable standard react documentation](https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs) 
for the list of SVG, DOM and Void React element helper functions. The large number of valid `props` can be found listed
[here](https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Props.fs). Props can also be discovered 
from the IDE by autocompletion for example `SVGattr.`

## Electron Documentation

The Electron framework API - and associated Node libraries - is extensive and complex. 
Luckily 99% of what you need for the project work is demonstrated in this project skeleton. In order to change or
enhance these features you will sometimes need the [electron API documentation](https://www.electronjs.org/docs). Note
that 95% of the electron features are accessible via F# interfaces which define types functions and discriminated
unions.

One way to find useful code examples is the [Issie application](https://github.com/tomcl/ISSIE). The files under `src/renderer/UI`
all contain code using the electron framework. Fork and clone (or clone) the Issie repo to get a copy of the files. 
Build them once to get fully working intellisense and view the code (with type information and documentation) in an IDE.


## CURRENT INTERFACES

### Overall Interfaces

#### Mouse Interface

Each module current interfaces with the mouse separately.

### Sheet -> Symbol

#### Deleting Things

Message sent to **Symbol** (called: DeleteSymbol), indicating the component to be deleted (only contains UUID)

### Sheet -> BusWire

AddWire message : Tuple of 2 Port Ids - **BusWire** can then interface with **Symbol** to find the XYPos of the Ports

DeleteWire : List of WireIds

HighlightWire : List of WireIds

SelectWire : an XYPos or WireID given 

-  - 
- SelectWire - XYPos of mouse
- MoveWire? - for manual routing, not needed yet 

- Sheet is checking if the Wire is Valid to be routed. 

#### Deleting Things

Message sent to BusWire, indicating the wires to be deleted.

### Buswire -> Symbol
