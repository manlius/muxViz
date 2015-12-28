muxViz v1.x
===========

### Very quick installation on Linux

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
    sudo mv R-3.2.0.tar.gz ~
    cd ~
    tar xvf R-3.2.0.tar.gz
    cd R-3.2.0
    ./configure
    make
    sudo make install
    
    #install GDAL
    sudo apt-get install libgdal1-dev libproj-dev
    

##### Ubuntu 14.04    

One of our users, Lucio Agostinho Rocha, reported the following solution for installation on this system.

To install octave 3.6.0, after the first 'make install' it is necessary to replace the following line:

_GL_WARN_ON_USE (gets, "gets is a security hole - use fgets instead");

In files: 
- libgnu/stdio.h 
- libgnu/stdio.in.h

With this contents:

	#if defined(__GLIBC__) && !defined(__UCLIBC__) && !__GLIBC_PREREQ(2, 16)
	_GL_WARN_ON_USE (gets, "gets is a security hole - use fgets instead");
	#endif

To load muxViz in R environment, he solved modifying the muxVizGUI.R file:

Before:

	devtools::install_github("shiny-incubator", "rstudio")

After:

	devtools::install_github("rstudio/shiny-incubator", "rstudio")

Thank you Lucio!


### Possible errors when using Motifs

The issue is that there is a conflict between the current version of igraph and shinyjs. More details can be found here: <https://github.com/igraph/igraph/issues/846>

While waiting for a new release of igraph, solving the issue, we can install the dev version:

	devtools::install_github("igraph/rigraph")
	
On Mac OS X this could be a bit tricky, because R might use clan instead of gcc/g++ to compile. A solution is to create the file

	~/.R/Makevars
	
if it does not exist, and set the following parameters:

	CFLAGS +=             -O3 -Wall -pipe -pedantic -std=gnu99
	CXXFLAGS +=           -O3 -Wall -pipe -Wno-unused -pedantic

	VER=-4.2
	CC=gcc$(VER)
	CXX=g++$(VER)
	SHLIB_CXXLD=g++$(VER)
	FC=gfortran
	F77=gfortran
	MAKE=make -j8

Restart R and try again to install the dev version of igraph.

### Possible errors when using Correlation 

It seems that the new versions of Octave (above 4.0) deprecated the function "cor", using "corr" instead.
Users that install new versions of Octave, can manually change the occurrences of "cor(" with "corr(" in the file muxOctaveLib.m to solve the issue.