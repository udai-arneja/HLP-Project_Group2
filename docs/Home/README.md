<!-- Main page -->

# HLP Project Group2

>## Project Overview

The aim of the project is to create an interactive schematic drawing canvas specifcally to replace the drawing canvas used in Issie. This is split into 
three modules Symbol, Buswire and Sheet. Symbol's objective was to implement the view function of symbols for all of the Issie components, Buswire's objective was 
to implement the view function of the wires, lastly, Sheet's objective was to join everything together.     

These have a parent child relationship:

`Sheet --> BusWire --> Symbol`

So the `BusWire` module can have a view function that depends on the `Symbol` module state (`Symbol.model`). The `Symbol` view 
function cannot depend on anything except `Symbol.model`. This dependence is needed because wire endpoints are defined 
by symbol and port positions.

Considering structure of:

`Sheet --> BusWire --> Symbol` and `Sheet --> Symbol`

Thus only one endpoint, but faster performance - hence this will be implemented if performance problems found.

>## Symbol 

>## BusWire 

>## Sheet 