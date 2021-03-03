### Below an overview of the project is highlighted, with focus on the interfacing the three main modules ( **Symbol**,  **BusWire**,  **Sheet** ). For further information about individual modules, in particular their functions, see the individual documentation (docs folder).

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
