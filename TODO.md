muxViz 1.0
==========

The following list reflects the possible improvements of muxViz. 

## TODO Visualization

#### High priority

- It would be nice to have the code with a 'network of networks' layout


###### Notes

    There are open issues with the native RGL device. This is far beyond the scope of muxViz for the moment, but maybe someone could find a way to solve them

    To the best of my knowledge:

    1. Transparency for nodes and edges does not work for the current version of RGL 
    2. Edge bending does not work for the current version of RGL

    Adding additional interactivity (eg, node selection, layer selection, etc) to the rgl device would be really great and it is necessary to improve user's experience.


#### Medium priority

- Add possibility to control the border of layers (a possibility could be to add segments in appropriate positions)

- Community-piechart in muxviz to show overlapping modules? (see [this paper](http://arxiv.org/abs/1408.2925) and also [multi-layer pie charts](http://www.r-bloggers.com/how-to-draw-venn-pie-agram-multi-layer-pie-chart-in-r/) for additional ideas)

- Layered visualization of adjacency matrices in a 3D environment


#### Low priority

- Reducibility: it could be useful to allow modifying font size and offset of label in reducibility dendrogram

- Reducibility: the dendrogram could have colored branches like [this](http://stackoverflow.com/questions/18036094/how-to-create-a-dendrogram-with-colored-branches), possibly according to a threshold

- Reducibility: possibility to generate a circular dendrogram and to apply cuts

- The timeline could be extended to allow "layer" as an entity


## TODO Analysis 

#### High priority

- Additional centrality descriptors (betweenness, closeness, random walk betweenness, random walk closeness) should be included as soon as possible (see [this paper](http://dl.acm.org/citation.cfm?id=2615687) for some references). 

- Add support to calculation of [shortest paths](http://dl.acm.org/citation.cfm?id=2615687) and their visualization

- Additional centrality descriptors: clustering/transitivity (see [paper1](http://arxiv.org/abs/1405.0425) [paper2](http://arxiv.org/abs/1308.3182) [paper3](http://arxiv.org/abs/1403.1546) [paper4](http://arxiv.org/abs/1307.6780))

- Multiplex Infomap? See the possibility to port or to make a stand-alone package/plugin for igraph

- Connected components calculation (according to existing literature) and coloring. Code should be already done in muxNet for some cases. Plots of size distribution

#### Medium priority

- The calculation of some centrality measures can be avoided when the multiplex is undirected (e.g., if the eigenvector centrality is calculated, HITS centrality would give the same result for undirected networks and we can exploit this to avoid re-calculation). This is already done for strength, for instance.

- Update multislice community detection to the new version "Louvain Random"

- Similarly to centrality analysis, it could be useful an interactive histogram for the size of communities in the multiplex, the aggregate and per layer

- It could be interesting to allow scatter plots of descriptors calculated in the multiplex against those ones calculated in the aggregate or in each layer

- Button in Export to export in one folder some files with all information (centrality, correlation, community). [web1](http://shiny.rstudio.com/articles/download.html) [web2](https://gist.github.com/SachaEpskamp/5796467)

- Synthetic multilayer network generator: 
  1. rings, stars, Erdos-Renyi, scale-free, ...
  2. overlapping community ([paper](http://arxiv.org/abs/1408.2925))
  3. with given inter-layer assortativity ([paper](http://arxiv.org/abs/1311.2906))
  4. with given inter-layer edge intersection ([paper](http://arxiv.org/abs/1405.0425))


#### Low priority

- When a richer layout file is provided (ie, with more columns/attributes of nodes), it could be useful to allow selection of sub-sets of the data, according to specific cuts. A dedicated panel for this task would be desirable



## TODO Technical

#### High priority

- Save the orientation of the rgl plot to allow future import to do exactly the same plot. [See here](http://stackoverflow.com/questions/16362381/save-the-orientation-of-a-rgl-plot3d-plot)
 
 - In general it would be great to export all tables and graphic settings like in gephi, to allow import without redoing the whole analysis. this would be really useful (and requires a file format). For the moment, if there is a button for exporting results (see points above), here we need just a button to save current configuration and import it. [See here](http://www.r-bloggers.com/persistent-data-storage-in-shiny-apps/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29)

###### Notes

    Full porting of Octave code to R: packages like [RcppOctave](http://cran.r-project.org/web/packages/RcppOctave/vignettes/RcppOctave.pdf) and [R.matlab](http://cran.r-project.org/web/packages/R.matlab/R.matlab.pdf) and [matlab](http://cran.r-project.org/web/packages/matlab/matlab.pdf) are promising, but a full porting would be the best option.
    It seems that on Mac and Linux it is possible to exploit the already existing linear algebra R packages by forcing R to use a faster BLAS version. On a Mac OS X this is easily achieved by
  
    sudo ln -sf /System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/Current/libBLAS.dylib /Library/Frameworks/R.framework/Resources/lib/libRblas.dylib
