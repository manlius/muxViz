### Jan 2021

Manlio De Domenico:

- Version 3.0 T'Challa released!
- Include standalone R lib to use muxViz from scripts rather than the GUI

### 10 Dec 2018

Manlio De Domenico:

Minor in analysis:

- Solved bug in visualization of the network of layers
- Fixed normalization for clustering calculation in directed/undirected networks
- Fixed calculation of K-coreness versatility

### 27 Jan 2018

Manlio De Domenico:

Major in analysis:

- Updates in memory efficiency and performances for centrality calculation. Requires installation of RSpectra (that should happen automatically).

Other:

- Moved to colourpicker package

### 12 June 2017

Manlio De Domenico:


Major in Analysis:

- [Milestone] Removed dependence on Octave: multilayer analysis fully written in R! 
- Removed support for multislice community detection by modularity maximization, because that code requires octave/matlab. Waiting for an R package or a standalone binary to include
- Added support for analysis of multilayer and single-layer triads (# triangles and transitivity)
- Added possibility to study the mixing of multilayer communities (ie, how communities are interconnected each other) by means of an interactive chord diagram
- Added basic multiscale analysis in community detection. By varying the relax rate in Multiplex Infomap, it is now possible to study how the number of communities and the code length change accordingly. A Sankey diagram of the results allow to visualize how communities are joined or disjoined.


Major in GUI:

- [Milestone] It is now possible to save the current state of the session, automatically saving the configuration of all panels, the results of the analysis and the plots. Added a "Session Manager" with the possibility to load/restore (or delete permanently) previously saved sessions
- Importing data simplified. With default options it's possible to import data in just 2 clicks, within the same panel
- Added "Matrix Explorer" in the import module, to allow interactive visual inspection of multilayer and aggregate adjacency matrices 
- Added possibility to color and size inter- and intra-layer edges from external file
- Motifs moved to Diagnostics framework
- Help no more present as a side panel. Instead, a small link is shown in the top-right corner
- Design improved and larger workspace

Other:

- Fixed many minor bugs
- Fixed call to binaries in Windows (thanks to Kim Klark)
- Fixed weighting edges in viz of non-edge-colored networks (thanks to Patricia Gonçalves)
- Added support for multiple languages (several languages in progress, but only English completed)
- Removed dendrogram image in Reducibility. When an interactive version can be developed, it will be used



### 4 December 2015

Manlio De Domenico:

- The output of reducibility analysis is now an interactive heatmap, instead of static image
- The output of ALL correlation analyses is now an interactive heatmap, instead of static image
- Added basic analysis of single-layer components per layer in Diagnostics / Statistics
- Added multilayer connected components diagnostics with interactive map for deeper inspection
- Added interactive bar chart for distribution of nodes per multilayer community in each layer
- Added possibility to analyze multilayer and single-layer communities within the same session (as for versatility analysis)
- Added interactive bar chart for distribution of nodes per multilayer component in each layer
- Added possibility to analyze multilayer and single-layer components within the same session (as for versatility analysis)
- Added possibility to color nodes by component calculated in single-layer or multilayer analysis
- Added possibility to color nodes by community calculated in single-layer or multilayer analysis
- Added possibility to query the multilayer network at node level (returns ego-networks per layer) and edge level
- Added possibility to color nodes according to query results
- Added a correlation analysis based on the Frobenius distance between layers. The matrices used for the calculation encode the shortest-path distance between all pairs of nodes (calculated in each layer separately).
- [Apparently] solved file recognition issue for Windows platforms
- Added a Console for advanced debugging. It can be found from menu File > Console
In the next future a dedicated language will be developed to allow interaction with muxViz without requiring 
any knowledge about its source code


### 19 November 2015

Manlio De Domenico:

- Added support for community detection with Multiplex Infomap
- Added new correlation analysis: node overlapping (complementary to existing edge overlapping)
- Added new Statistics panel in Diagnostics, for basic statistics about nodes and edges:
        - Nodes per layer
        - Edges per layer
        - Link density per layer
        - Diameter per layer
        - Mean path length per layer
- In community detection, community "0" is assigned to isolated nodes
- Added a new interactive heatmap for exploring communities
- Solved a bug in external node coloring/sizing (thanks Licheng!)


### 8 November 2015

Manlio De Domenico:

Major in Visualization:

- Added the possibility to set custom color and size for nodes from external file (without needing to pass through the timeline, that was a bit weird temporary solution)
- Added two new templates for aggregated information to be used for calculating nodes' layout. For instance, beside full aggregation, one could opt for edges union (unweighted version of aggregation) or edges intersection (a kind of overlap). 
- Added possibility to size and color nodes according to multilayer or single-layer centrality, without restarting the session

Major in GUI:

- Added possibility to toggle boxes, it might be useful when some are not used
- Added color picking tools thanks to shinyjs package

Minor:

- Solved minor bugs with uniform node sizing
- Added a new Graphics menu, where allowed type of points and lines, as well as color palettes, are reported
- Added possibility to shift layers along y-axis
- Improved usability of the GUI
 

### 27 October 2015

Manlio De Domenico:

Solved minor bug for rotations in 2D rendering


### 18 October 2015

Manlio De Domenico:

Added support for standard degree centrality even in case of weighted networks


### 4 October 2015

Manlio De Domenico:

Added full support to alternative output device, different from RGL


### 3 October 2015

Manlio De Domenico

- Added possibility to account or not for interlayer links to exclude isolated nodes
- Solved a visualization bug related to top-ranked nodes coloring
 

### 26 September 2015

Manlio De Domenico:

Added possibility to automatically wrap long node labels.


### August/September 2015

Manlio De Domenico:

New descriptors:

- Added multiplexity centrality
- Added k-coreness centrality
- Added Louvain community detection per layer
    
New structures:

- Added support to import extended edges list format for:
      1. Interconnected multiplex networks
      2. Interdependent networks
      3. General multilayer networks
- Added support for visualization of interconnected layers (for interlinks specified in the input data)
- Added support for analysis of any type of supported multilayer networks

GUI:

- Introduced conditional rules to show available options, to facilitate usability
- Added the possibility to export the results of analyses

New analytic tools and visualization options:

- Added support for (interactive) analysis of centrality descriptors:
       1. Visualization of nodes ranked by centrality
       2. Histogram 
       3. Scatter 
       Simultaneous comparison between descriptors obtained from multilayer, aggregate and layers is allowed
- Added support for calculation and interactive visualization of the network of layers (through d3)
- Added support for coloring nodes according to centrality measures
- Added support for coloring nodes and edges uniformly
- Added the possibility to show 3D axes in the rendered figure
- Added the possibility to change color and opacity of each layer separately
- Added a new visualization mode (matrix disposition of layers)
- Added the possibility to hide one or more layers in the rendering. Intra- and inter-links are hidden.

Technical:
- Solved apparent incompatibility with igraph 1.0.1
	

### 2 July 2015

Manlio De Domenico:

- Fixed compatibility with latest version of Shiny (0.12.1)
- Fixed minor bugs (filling centrality and community tables, updating annular viz)
- Restyled the GUI (completely!)
- Added a heatmap to quickly visualize communities and patterns
- Support multilayer motif analysis
- Added information about available datasets


### 17 January 2015

Manlio De Domenico:

- Fixed progressBar compatibility with latest version of Shiny


### 13 August 2014

Manlio De Domenico:

- Added possibility to import edges list where nodes are identified by labels (either arbitrary integers or strings) instead of sequential integers. Removed one of the biggest annoyance in muxViz!

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
