muxViz v0.2
=========

### Visualization and Analysis of Multilayer Networks

muxViz is a platform for the visualization and the analysis of interconnected multilayer networks. The current implementation exploits a Graphical User Interface (working with any browser) to provide access to many customizable graphic options to render networks. The great novelty of this version is the support for the analysis of multiplex data:

- Multilayer correlation analysis
- Multilayer centrality analysis and annular representation
- Multilayer community structure detection
- Dimensionality reduction

Support for data analysis is not mandatory and requires a working installation of GNU Octave 3.4.0 or above. 

![muxViz GUI](gallery/gui1.png "muxViz GUI")

##### Requirements and Installation

Download and install a copy of Octave (3.4.0 or above):

<http://www.gnu.org/software/octave/download.html>

Octave should be accessible through command line from any folder (i.e., it is mandatory to add it in your PATH environment variable). If you are not familiar with this simple task, you should visit <http://www.java.com/en/download/help/path.xml>

muxViz requires R v3.0.2 (or above). Download and install a copy of R from

<http://www.r-project.org/>

The following packages are required within the R environment:

- shiny 0.8.0 (or above)
- ShinyDash 0.0.1 (or above)
- shinyIncubator 0.2.0 (or above)
- igraph v0.6.5 (or above)
- gplots 2.12.1 (or above)
- RGL 0.93.963 (or above)
- googleVis 0.4.7 (or above)
- digest 0.6.4 (or above)
- mapproject v1.2-1 (or above)
- openstreetmap v0.3.1 (or above)
- RColorBrewer 1.0-5 (or above)
- raster 2.2-31 (or above)
- fields 6.9.1 (or above)
- lattice 0.20-27 (or above)
- clue 0.3-47 (or above)

However, the main script should be able to detect the missing packages and install them, automatically. Therefore, it is likely that you *do not* need to install them by yourself.

You can download muxViz from Github: <https://github.com/manlius/muxViz/archive/master.zip>

To work properly with geographical networks, the GDAL  (Geospatial Data Abstraction Library) is required. GDAL can be found here: <http://www.gdal.org/>
Note that GDAL should be installed *before* running muxViz for the first time.

###### Very quick installation on a Linux

If you use a Linux (Ubuntu-like) distribution, you are very lucky, because the following BASH script will do the job for you:

    #download Octave and R from their repository
    wget http://ftp.gnu.org/gnu/octave/octave-3.6.0.tar.gz
    wget http://cran.es.r-project.org/src/base/R-3/R-3.0.3.tar.gz
    DIR=$PWD
    
    #install Octave
    sudo apt-get build-dep octave
    sudo mv octave-3.6.0.tar.gz ~
    cd ~
    tar xvf octave-3.6.0.tar.gz
    cd octave-3.6.0
    ./configure
    make
    sudo make install
    cd $DIR
    
    #install R
    sudo apt-get build-dep r-base-core
    sudo mv R-3.0.3.tar.gz ~
    cd ~
    tar xvf R-3.0.3.tar.gz
    cd R-3.0.3
    ./configure
    make
    sudo make install
    
    #install GDAL
    sudo apt-get install libgdal1-dev libproj-dev

Finally, if your system has a working installation of Octave, R and GDAL, you can download the last version of muxViz, unzip it and type the following within R environment:

	source('muxVizGUI.R')

This should be enough. The script will check for the required packages and will try to automatically install the missing ones. The whole process might take a few minutes, the first time you run muxViz.


### Gallery

Here there are a few multilayer networks rendered by muxViz.
	
![Network with community structure](gallery/muxViz_community3.png "Network with community structure")
![Air transportation network](gallery/muxViz_airports_osm6e.png "Air transportation network")
![Real collaboration network](gallery/muxViz_16Layers_Real2.png "Collaboration network")

Please, also note that after a proper set up it is possible to use muxVizGUI also for the visualization (and basic analysis) of single-layer networks (equivalent to old monoxViz 0.1).

![Network with community structure](gallery/monoxViz_community_example2.png "Network with community structure")
![London Tube + OpenStreetMap](gallery/monoxViz_london_tube.png "London Tube + OpenStreetMap")
![Yeast genetic interaction network (GCC)](gallery/monoxViz_yeast_interaction_lcc.png "Yeast genetic interaction network (GCC)")



### Usage

##### Format of an input file

The configuration file is a ASCII file including the list of layers to be included in a multiplex, the corresponding labels and the possible layout file to define node properties (e.g., ID, labels, geographic coordinates, etc).

Format of a configuration file:

	path_layer_X;label_layer_X;layout_layer_X

where 

- path_layer_X: [mandatory] specify the path and the filename to the edges list to be used as layer
- label_layer_X: [optional] specify the label to be used in the rendering for that layer
- layout_layer_X: [optional] specify the path and the filename to the file containing information about nodes

    
##### Format of a layout file


The first line of the file must specify the name of the correponding node attributes. Allowed attributes:

- nodeID:      [mandatory] numerical integer id to identify each node
- nodeLabel: [optional] string specifying the label attribute
- nodeX:       [optional] float value specifying the Cartesian coordinate x for the layout
- nodeY:       [optional] float value specifying the Cartesian coordinate x for the layout
- nodeLat:    [optional] float value specifying the latitude for the geographic layout
- nodeLong: [optional] float value specifying the longitude for the geographic layout

The order of the columns should not be relevant.
If nodeLat and nodeLong are specified, they will be automatically converted to Cartesian coordinates (through Mercator projection).

The properties of each node in the multilayer must be specified or default values will be used (i.e., automatic labeling and layouting). If the number of nodes in the network is different from the number of nodes provided in the layout file, it will be assumed that something is wrong with the layout file and default values will be used.



Other scripts included
======================

### muxViz v0.1 (CLI)

Usage from shell command line (no support to GUI and no longer developed):

	R CMD BATCH muxViz_CLI.R

If used with the default options for the first time, muxViz will plot a multiplex with 100 nodes and 4 layers with community-like topology. The output should be a file 'muxViz.png' in the same folder, similar to 'muxViz_example.png' provided with the package.

Please, explicitly cite muxViz if you find it useful for your visualizations.

If you use muxVizGUI for your data analysis, please, cite the relevant papers where all descriptors and diagnostics are defined: you will find a quick help with references to relevant papers in all pages dedicated to data analysis.


### monoxViz v0.1 (CLI)


Support the visualization of classical single-layer networks (no longer developed).

The muxViz package now includes a script for the visualization of standard networks. It supports both 2D and 3D layouting with openGL, it is fully integrated with OpenStreetMap and preserves all the features developed to customize visualizations with muxViz.


 
Copyright
=========

##### (C) Copyright 2013-2014, Manlio De Domenico.

This code has no warranty whatsoever and any kind of support is provided.
You are free to do what you like with this code as long as you leave this copyright in place.
Please, explicitly cite muxViz if you find it useful for your visualizations.

(C) Copyright 2013-2014, Manlio De Domenico (manlio.dedomenico at urv.cat) 

Each file in this folder is part of the muxViz package. 

muxViz is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 

muxViz is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 

You should have received a copy of the GNU General Public License along with the package. If not, see <http://www.gnu.org/licenses/>. 


Credits
=========

This work has been partially supported by European Commission FET-Proactive project PLEXMATH (Grant No. 317614) (<http://www.plexmath.eu/>), the European project devoted to the investigation of multi-level complex systems and has been developed at the Alephsys Lab (<http://deim.urv.cat/~alephsys/>),

I am in debt with A. Arenas for proposing this project, with Mason A. Porter and A. Sole-Ribalta for invaluable suggestions and feedbacks. 

I would like to thank Inderjit S. Jutla, Lucas G. S. Jeub, and Peter J. Mucha for making their code about multislice community detection available. 

Finally, I would like to acknowledge the precious help of S. Agnello in designing and testing the Graphical User Interface of muxViz.