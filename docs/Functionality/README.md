# HLP Project Group2 

## Add symbol 
- Creates a symbol with parameters based off of the component type
- The ports and bounding boxes are also created
- Each symbol is given a UUID 

## Select symbols
- Selects symbols by setting the parameter isSelected to true for the particular symbols

## Delete symbol

- Searches all symbols to see which are selected and then removes the selected symbols from the model.

## Rotate

- Symbols can rotate so that their ports orient vertically with wires in the vertical position.
- All other features including labels will stay in original orientation. 

## Snap to grid

- Components snap to grid when moved and dropped.
- Wires snap to grid when manually routed.

## Add Wire 

- Creates a wire between a source and target port. 
- Takes, as an input, the source and target port ID's and finds the port by searching through the list of input/output ports. 

## Select wires
- selects wires by setting the parameter isSelected to true

## Delete wire 

- Searches all Wires to see which are selected and then removes the selected Wires from the model.

## Snap to grid (component)

- Depending on grid dimensions, divide component coordinate by division size to find the remainder. 
- Depending on remainder, either floor or ceiling the coordinate so that it jumps to the grid.

## Buswidth inference

- Taken from Issie and adapted to our implementation.
- Highlights when there is a error regarding the bus widths 

## Autorouting 

- Automatically autoroutes around any symbols in the way. This includes symbols that are later moved in the way of the wire. 

- A recursive function that works segment by segment to route the wire. 

## Manual routing

- Movement in all the segments that are not directly attached to the ports. 

- No movement in the wires coming directly out of the ports.

- Segments only move in a vertical/horizontal direction, never both. 

- Direction is determined by segment orientation. 

- Orientation is determined by segment index. Even segments are vertical and therefore only move along the x-axis. Odd segments are horizontal and therefore only move along the y-axis. 

- Sheet sends the segmentIndex to BusWire. BusWire determines if it's even in isEven. This will then correspond to either the evenChange or oddChange function which updates the current x/y coordinate with the mouse position.

- Vertices of the segments affected are updated.  

## Multi select: click by a box or individual symbols

- Selects multiple symbols at once
- Can delete and move multiple symbols at once.

## Copy and Paste, multiple symbols and wires

- Can copy and paste single components and wires
- Multi-select can be copy and pasted. 

## Undo

- Can undo a previous command.
- Can undo delete, move, multi select move, delete multi select.
- Stores the Id of the components and their position so it can reassign in the same location when undo is called. 

## Zoom into a segment/Zoom

- Can either draw a box to zoom to specific location
- Or zooms to 0,0 location.

## Dropdown menu

- Dropdown menu that allows you to add different components and to edit a custom component. 




 















 




