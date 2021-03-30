muxViz v3.0 (T'Challa)
=========

* **v3.0** is the latest and continuously updated. It is not based on a GUI and allows to use muxViz fully from R scripts (with `igraph`and `ggplot2` packages, for instance)

In case of anomalous/unexpected behavior, please report it to the dedicated [Google Group](https://groups.google.com/forum/#!forum/muxviz). Note that emails to personal address might remain unanswered. 

For more information, read the main [README](../README.md).


#### Citation

If you use muxViz (or any part of muxViz, or images available in the gallery) for your multilayer analysis and visualization, you should cite the paper

Manlio De Domenico, Mason A. Porter, Alex Arenas, Multilayer Analysis and Visualization of Networks, published in [Journal of Complex Networks 3, 159-176 (2015)](http://comnet.oxfordjournals.org/content/3/2/159) (Open Access)

Manlio De Domenico, Multilayer Networks: Analysis and Visualization. Introduction to muxViz with R. To be published by Springer-Verlag (2021)

Please, note that muxViz is based on some algorithms developed in other studies. You should cite the original paper(s) every time that you use those algorithms. 

## v3.0: The Standalone Library

The standalone library allows to use muxViz from R scripts without requiring to pass from any GUI, which is left for v2.0. If you want to use v3.0 you should:

* Download and install R. Note that the data analysis would benefit from the enhanced R environment developed by Microsoft: it is strongly recommended to download and install Microsoft R Open from

<https://mran.microsoft.com/download/>

* manually download muxViz or clone it:

		git clone https://github.com/manlius/muxViz

* download and install the required (or useful) dependencies with:

		for(pack in c("Matrix","RSpectra","tidyverse","GGally","compiler","ggraph","tictoc","rgl","igraph","RColorBrewer","viridis","corrplot")){
			install.packages(pack)
		}
		
* set the work directory to the `standalone` folder by

		setwd("/user/path/muxviz/standalone")

Then include the following header in your scripts:

	source("muxLib.R")

or directly source the library from your current working directory:

	source("/user/path/muxviz/standalone/muxLib.R")

To speed up your interaction with muxViz, the folder `standalone` contains some self-explanatory example scripts which show how to use the most salient analytical techniques available in the library:

* `community_detection.R`* `connected_components.R`* `example_coverage_optimized.R`* `example_coverage.R`* `example_overlapping_generator.R`* `example_plot_edgecolored_heatmap_6panels.R`* `example_plot_edgecolored_paths_coupling.R`* `example_plot_edgecolored_paths.R`* `example_plot_edgecolored.R`* `example_transitivity_new.R`* `layer-layer_corr.R`* `multi_motifs.R`Note that to work properly, you must (manually, for the moment) compile `fanmod` and `Infomap` on your system, and move the binaries in the `standalone folder`. If you are familiar with package creation and want to help to make this process smoother, you are more than welcome.Typical outputs are:

![Visualization of edge-colored multigraphs](../gallery/mux_sbm_heatmaps.png "Visualization of edge-colored multigraphs")

![Network with community structure](../gallery/multi_sbm_infomap.png "Network with community structure")
![Partition analysis](../gallery/multi_sbm_infomap_table.png "Partition analysis")



Unfortunately, at this stage, documentation is still poor and requires some time.

## File format

There is no specific file format required in this version, since the user is allowed to freely manipulate the data to create the desired input, usually in form of igraph objects or supra-adjacency matrices. See the examples for details. 

For more information, read the main [README](../README.md).
