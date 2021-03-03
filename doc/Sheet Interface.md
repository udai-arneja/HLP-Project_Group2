<!---
# SVG Draw Application Project Skeleton Code For HLP Project 2021

See [The SVG Demo README](https://github.com/tomcl/hlp21-svg-elmish-demo/blob/master/README.md) for 
code structure and how to build the dev environment: this is the same as the WS 3 SVG demo.

You should already have looked at the SVG Demo code and have some understanding of how an Elmish application works, using
a `model`, `view`, and `update` function with messages used to update the model. The project skeleton code 
illustrates a more complex example of Elmish code, 
with three separate modules: `Symbol`, `BusWire`, `Sheet` each operating as individual Elmish components.

Fork, clone, and build this repo. Look at the code in an IDE with intellisense.

In order to see the functionality currently used by [Issie](https://github.com/tomcl/ISSIE) you can 
download the Issie binaries and try it out. There is a video which introduces the project objectives and explains
what is good and bad about the current (Draw2D) Issie Draw block functionality. You can also fork and clone the Issie repo, 
and build its code. The build is very similar to the project skeleton
but a lot more complex (10K lines of F#).

The project work specification contains details of expected work during individual work phase in each of the three modules.

## Introduction

The skeleton code draws circles and (random) lines on an svg canvas. It is interactive; circles can be moved as in the WS 3 demo.
In addition lines are drawn between circles that move with the circles; this is similar to the functionality needed for symbols and
wires. The given implementation will need to be modified even for this basic functionality because:
* The skeleton drag mechanism does not use programmatic bounding boxes, but instead HTML to determine which circle is clicked.
* The skeleton `Symbol` code does not used correctly compensated coordinates. Try it setting zoom=3. How does dragging of circles
go wrong?
* The skeleton code does not understand ports - connection points on the edge of symbols.
* The skeleton wires are not right-angled multi-segment.

The skeleton introduces a modular structure which can be used to combine individual coding from 3 people into a single 
fully working schematic draw program, and some initial messages most of which will be useful in project code. You are free to
change message types as you like within  a group. Messages or functions `Symbol:symbolPos` owned by any component
used to communicate with other components should not be changed in a way that alters their specification without 
consultation with those affected.
-->

## Information on Elmish & Code Structure

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
