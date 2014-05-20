muxViz 0.2
==========

The following list reflects the possible improvements of muxNet. I am in debt with Mason A. Porter for some of the points below:

##### High priority

- Full porting of Octave code to R: packages like 
  <http://cran.r-project.org/web/packages/RcppOctave/vignettes/RcppOctave.pdf>
  and
  <http://cran.r-project.org/web/packages/R.matlab/R.matlab.pdf>
  and
  <http://cran.r-project.org/web/packages/matlab/matlab.pdf>
  are promising, but a full porting would be the best option

- The calculation of some centrality measures can be avoided when the multiplex is undirected (e.g., if the eigenvector centrality is calculated, HITS centrality would give the same result for undirected networks and we can exploit this to avoid re-calculation). This is already done for strength, for instance.

- Support for any input format allowed by igraph (currently working only with edges list)

##### Low priority

- More complicated interconnections, as for instance between different nodes in different layers, should be available at some point. Currently, only multiplexes/time-varying networks are considered, where each node is interconnected only to its replicas in other layers

- Calculation of connected components and corresponding coloring

- Plot of the quality function in the reducibility analysis might be useful

- One should be able to color not just by community but by anything the user wants (for instance, centrality)

- The user should also be able to color things across layers. There will be times when that is desirable (e.g. when the nodes are not all the same in all layers).

- It would be nice to have the code also do this with a 'network of networks' layout

- It could be useful to export tables to files

- It could be useful to allow modifying font size and offset of label in reducibility dendrogram

- The dendrogram could have colored branches like this:
<http://stackoverflow.com/questions/18036094/how-to-create-a-dendrogram-with-colored-branches>

- There are open issues with the native RGL device. This is far beyond the scope of muxViz, but maybe someone could find a way to solve them

To the best of my knowledge:

- Transparency for nodes and edges does not work for the current version of RGL 
- Edge bending does not work for the current version of RGL
