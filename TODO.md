muxViz 2.x
==========

The following list reflects the possible improvements of muxViz. 

## TODO Visualization

#### High priority

- Add possibility to color intelinks from timeline
- Some measures of multiplexity etc could be shown as an interactive piechart

###### Notes

    There are open issues with the native RGL device. This is far beyond the scope of muxViz for the moment, but maybe someone could find a way to solve them

    To the best of my knowledge:

    1. Transparency for nodes and edges does not work for the current version of RGL 
    2. Edge bending does not work for the current version of RGL

    Adding additional interactivity (eg, node selection, layer selection, etc) to the rgl device would be really great and it is necessary to improve user's experience.


#### Medium priority

- Include the colourPicker package (see why on the Google Group)

- Give the possibility to show nodes' label instead of their IDs in annular viz

- Layered visualization of adjacency matrices in a 3D environment

- Community-piechart in muxviz to show overlapping modules? (see [this paper](http://arxiv.org/abs/1408.2925) and also [multi-layer pie charts](http://www.r-bloggers.com/how-to-draw-venn-pie-agram-multi-layer-pie-chart-in-r/) for additional ideas)


#### Low priority
 
- Reducibility: the dendrogram could have colored branches like [this](http://stackoverflow.com/questions/18036094/how-to-create-a-dendrogram-with-colored-branches), possibly according to a threshold

- Reducibility: possibility to generate a circular dendrogram and to apply cuts (requires a new package to be installed)

- Reducibility: it could be useful to allow modifying font size and offset of label in reducibility dendrogram

- The timeline could be extended to allow "layer" as an entity

- Add possibility to control the border of layers (a possibility could be to add segments in appropriate positions)

- In the 2D rendering, allow for different types of node shape. 


## TODO Analysis 

#### High priority

- [Milestone] Add possibility to generate markdown reports [Link1](https://shiny.rstudio.com/articles/generating-reports.html) [Link2](http://shiny.rstudio.com/gallery/download-knitr-reports.html)


- Add support to calculation of [shortest paths](http://dl.acm.org/citation.cfm?id=2615687) and their visualization, giving possibility to specify origin/destination, width and color of the path. Add new tab Paths in Analysis to establish paths (distinguish between coding and non-coding interlinks) between any two nodes. Possibility to highlight the result in the viz

- Additional centrality descriptors (betweenness, closeness, random walk betweenness, random walk closeness) should be included as soon as possible (see [this paper](http://dl.acm.org/citation.cfm?id=2615687) for some references). 

- Multiplex community detection: given that relax rate can tune the size of partitions, one can do a batch processing for various values and then show results by using an alluvial plot


#### Medium priority

- It could be interesting to allow scatter plots of descriptors calculated in the multiplex against those ones calculated in the aggregate or in each layer

- Synthetic multilayer network generator: 
  1. rings, stars, Erdos-Renyi, scale-free, ...
  2. overlapping community ([paper](http://arxiv.org/abs/1408.2925))
  3. with given inter-layer assortativity ([paper](http://arxiv.org/abs/1311.2906))
  4. with given inter-layer edge intersection ([paper](http://arxiv.org/abs/1405.0425))



#### Low priority

- When a richer layout file is provided (ie, with more columns/attributes of nodes), it could be useful to allow selection of sub-sets of the data, according to specific cuts. A dedicated panel for this task would be desirable

- It would interesting to have the scatterplots allowing to select subset of nodes according to attributes

- It would be useful to color/size nodes according to additional attributes specified in richer layout files 


## TODO Technical

#### High priority

- Add support for "tab" character in import

- Improvement of the "console"

- Support for multiple languages (Done; only English completed; other languages available, to be extended)

- It would be nice to make a configuration file to guide the installation process, including "language" and "geo" (if geo should be available or not, to save installation time)

- Save the orientation of the rgl plot to allow future import to do exactly the same plot. [See here](http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot)
 
 - In general it would be great to export all tables and graphic settings, to allow import without redoing the whole analysis. This would be really useful (and requires a file format). For the moment, the supported functionality allows to save the current state of the session for later restoring.

- Add [spinner gif](https://github.com/daattali/advanced-shiny/blob/master/plot-spinner/app.R) while loading

