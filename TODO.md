muxViz 0.2
==========

The following list reflects the possible improvements of muxViz. I am in debt with Mason A. Porter for some of the points below.

## TODO Visualization

#### High priority

- One should be able to color nodes/edges not just by community but by anything the user wants (for instance, centrality)

- The user should also be able to color things across layers. There will be times when that is desirable (e.g. when the nodes are not all the same in all layers).

- It would be nice to have the code also do this with a 'network of networks' layout

- If networks of networks are allowed (or a network of layers can be deduced from the interconnected multilayer network), it could be useful (and cool!) to use [alluvial](http://www.r-bloggers.com/alluvial-diagrams/) and [circos-like](http://circos.ca/) plots to quickly visualize them

- Community-piechart in muxviz to show overlapping modules? (see [this paper](http://arxiv.org/abs/1408.2925) and also [multi-layer pie charts](http://www.r-bloggers.com/how-to-draw-venn-pie-agram-multi-layer-pie-chart-in-r/) for additional ideas)

- Layered visualization of adjacency matrices in a 3D environment


- There are open issues with the native RGL device. This is far beyond the scope of muxViz for the moment, but maybe someone could find a way to solve them

To the best of my knowledge:

1. Transparency for nodes and edges does not work for the current version of RGL 
2. Edge bending does not work for the current version of RGL

Adding additional interactivity (eg, node selection, layer selection, etc) to the rgl device would be really great and it is necessary to improve user's experience.


##### Medium priority

- More complicated interconnections, as for instance between different nodes in different layers, should be available at some point. Currently, only multiplexes/time-varying networks are considered, where each node is interconnected only to its replicas in other layers

- Add a new radio below coloring by top rank, allowing to color uniformly all nodes, all edges in all layers!

- Add possibility to control the border of layers

- Add possibility to pass a vector of colors to color layers, as well as a single color

- Light problem: insert a button to reset lights

#### Low priority

- Reducibility: it could be useful to allow modifying font size and offset of label in reducibility dendrogram

- Reducibility: the dendrogram could have colored branches like [this](http://stackoverflow.com/questions/18036094/how-to-create-a-dendrogram-with-colored-branches), possibly according to a threshold

- Reducibility: possibility to generate a circular dendrogram and to apply cuts


## TODO Analysis 

##### High priority

- Additional centrality descriptors (betweenness, closeness, random walk betweenness, random walk closeness) should be included as soon as possible (see [this paper](http://dl.acm.org/citation.cfm?id=2615687) for some references). 

- Add support to calculation of [shortest paths](http://dl.acm.org/citation.cfm?id=2615687) and their visualization

- Additional centrality descriptors: node/edge multiplexity, overlapping, clustering/transitivity (see [paper1](http://arxiv.org/abs/1405.0425) [paper2](http://arxiv.org/abs/1308.3182) [paper3](http://arxiv.org/abs/1403.1546) [paper4](http://arxiv.org/abs/1307.6780))

- Additional centrality descriptor: K-coreness, defined as max(kc1,kc2,...,kcL) as in [this paper](http://journals.aps.org/pre/abstract/10.1103/PhysRevE.90.032816)

- It could be useful to export tables to files

- Motifs? See the possibility to port FANMOD

- Multiplex Infomap? See the possibility to port or to make a stand-alone package/plugin for igraph

- Connected component calculation (according to existing literature) and coloring. Code should be already done in muxNet for some cases

##### Medium priority

- The calculation of some centrality measures can be avoided when the multiplex is undirected (e.g., if the eigenvector centrality is calculated, HITS centrality would give the same result for undirected networks and we can exploit this to avoid re-calculation). This is already done for strength, for instance.

- It could be useful allowing to plot the histogram/density of centrality measures, single/multi-layer, including possibility of log-binning


- Update multislice community detection to the new version "Louvain Random"

- Add Louvain community detection per layer (included in igraph package, see this code)

- Calculation of connected components and corresponding coloring

- Plot of the quality function (and a table with the values) in the reducibility analysis might be useful

- Button in Export to export in one folder some files with all information (centrality, correlation, community). [web1](http://shiny.rstudio.com/articles/download.html) [web2](https://gist.github.com/SachaEpskamp/5796467)

- Synthetic multilayer network generator: 
  1. rings, stars, Erdos-Renyi, scale-free, ...
  2. overlapping community ([paper](http://arxiv.org/abs/1408.2925))
  3. with given inter-layer assortativity ([paper](http://arxiv.org/abs/1311.2906))
  4. with given inter-layer edge intersection ([paper](http://arxiv.org/abs/1405.0425))


##### Low priority

- When a richer layout file is provided (ie, with more columns/attributes of nodes), it could be useful to allow selection of sub-sets of the data, according to specific cuts. A dedicated panel for this task would be desirable





## TODO Technical

##### High priority

- Full porting of Octave code to R: packages like 
  [RcppOctave](http://cran.r-project.org/web/packages/RcppOctave/vignettes/RcppOctave.pdf)
  and 
  [R.matlab](http://cran.r-project.org/web/packages/R.matlab/R.matlab.pdf)
  and
  [matlab](http://cran.r-project.org/web/packages/matlab/matlab.pdf)
  are promising, but a full porting would be the best option.
  It seems that on Mac and Linux it is possible to exploit the already existing
  linear algebra R packages by forcing R to use a faster BLAS version. 
  On a Mac OS X this is easily achieved by
  
  sudo ln -sf /System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/Current/libBLAS.dylib /Library/Frameworks/R.framework/Resources/lib/libRblas.dylib


- When single layers are provided, support for any input format allowed by igraph (currently working only with edges list) is required

- Allow the possibility to import generalized edgelist (all layers are specified in a single file)

- Save the orientation of the rgl plot to allow future import to do exactly the same plot. [See here](http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot)
 
 - In general it would be great to export all tables and graphic settings like in gephi, to allow import without redoing the whole analysis. this would be really useful (and requires a file format). For the moment, if there is a button for exporting results (see points above), here we need just a button to save current configuration and import it.






