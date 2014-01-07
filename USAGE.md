muxViz v0.1
=========

muxViz can be customized from the many parameters inside the code, at the beginning of the file, between the delimiters

    - Input Parameters
    - End Parameters


Format of an input file
-----------------------------

The input is a ASCII file where there is a list of layers. The format of this file is

path_layer_X;label_layer_X;layout_layer_X

where 

    - path_layer_X: specify the path and the filename to the edges list to be used as layer
    - label_layer_X: specify the label to be used in the rendering for that layer
    - layout_layer_X: specify some properties of the nodes (see below)

    
Format of a layout file
-----------------------------

The first line of the file must specify the name of the correponding node attributes:

    - nodeID:      [mandatory] numerical integer id to identify each node
    - nodeLabel: [optional] string specifying the label attribute
    - nodeX:       [optional] float value specifying the Cartesian coordinate x for the layout
    - nodeY:       [optional] float value specifying the Cartesian coordinate x for the layout
    - nodeLat:    [optional] float value specifying the latitude for the geographic layout
    - nodeLong: [optional] float value specifying the longitude for the geographic layout

The order of the columns should not be relevant.
If nodeLat and nodeLong are specified, they will be automatically converted to Cartesian coordinates (through Mercator projection).

The properties of each node in the multilayer must be specified or default values will be used (i.e., automatic labeling and layouting).


Usage
-----------------------------

Usage within R environment:

        source('muxViz.R')

Usage from shell command line:

        R CMD BATCH muxViz.R


monoxViz v0.1
===========

It is now available the muxViz counterpart for visualization of classical single-layer networks. Usage is the same, except that the input network, the layout file, etc, are provided within the R script and no more from an external file.
