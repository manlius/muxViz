### 8 August 2014

Manlio De Domenico:

- Solved some issues with file paths when running on Windows
- Minor bugs in the GUI solved

### 22 July 2014

Manlio De Domenico:

- Added the sub-module "Dynamics" in the "Visualization" module, allowing to visualize/export snapshots of a given dynamics on the top of a multiplex network. Possibility to make an animated movie through ffmpeg included.

### 19 July 2014

Manlio De Domenico:

- Added possibility to keep desired orientation in successive renderings (it's useful!)

### 5 May 2014

Manlio De Domenico:

- Optimized calculation of degree/strength centralities of nodes in undirected networks

### 8 April 2014

Manlio De Domenico:

- Added Graphical User Interface (GUI) through RShiny
- Added support to multiplex data analysis (correlation, centrality and community detection) with interface to Octave 3.4.0 (or above) for tensorial operations
- Added support for dimensionality reduction
- Added support for changing color palette according to color brewer standards
- Added support to annular visualization
- Added support to export rendering
- Added a more detailed help
- Note: the old muxViz.R is kept because someone could like to render visualizations without passing through a GUI-
  However, this does not include an interface to tensorial analysis of multiplex data. 
  Therefore this code will be no longer developed

### 15 December 2013

Manlio De Domenico:

- Added support to OpenStreetMap grographic layouts
- Note: the old code is kept because someone could like to draw very simple boundaries .. the name is muxViz_old_geo.R
  and will be no longer developed


### 10 December 2013

Manlio De Domenico:

- Added support to shift the layers along x-axis to improve the perspective

### 7 December 2013

Manlio De Domenico:

- Added support to show aggregate network as additional layer
- Added support for custom graphic options for the aggregate network (independent from other layers)


### 25 November 2013


Manlio De Domenico:

- Added support for InfoMap community detection algorithm
- Added support for showing regional borders in geographic layouts


### 23 November 2013


Manlio De Domenico:

- Added support for external layout 
- Added support for geographic layout (automatic conversion from input)
- Added support for node labeling from external layout file
- Added support for community detection based on edge-betweenness
- Minor bugs solved


### 22 November 2013


Manlio De Domenico:

- Added support to read list of edgeslist from an external file (to keep the code less messy)
- Added support to layout each layer separately (for non-multiplex networks)
- Added support to layout each layer as a given layer of the list
- Added support for equally coloring the communities smaller than given size
- Added a new layout, for faster and better visualization of "complicated" networks
- Added a few more graphic options


### 18 November 2013

Manlio De Domenico:

- First release of beta v0.1
