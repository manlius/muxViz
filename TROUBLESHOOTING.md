muxViz v1.x
===========

Topics in this troubleshooting:

- **Very quick installation on Linux**- 
- **Octave, Multimap or FANMOD not found**
- **Possible errors when using Motifs**
- **Possible errors when using Correlation**
- **Install muxViz with R 3.3 or higher**

---

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


### Octave, Multimap or FANMOD not found

muxViz uses an API to three external softwares: Octave, Multimap (known as multiplex infomap) and Fanmod.

The home screen of muxViz will indicate if the available binaries are correctly installed and can be used by the platform. If it is not the case, a WARNING message is generally displayed. 

##### Should I care about missing Octave?

It depends. If you want to use muxViz only for visualization purposes or for single-layer analysis of your networks, then you can skip a working installation of Octave.

###### Installing Octave

If you are interested in multilayer analysis, then Octave is a fundamental requirement (that will be removed from the next version of muxViz). 

Download and install a copy of Octave (3.4.0 or above):

<http://www.gnu.org/software/octave/download.html>

Octave should be accessible through command line from any folder (i.e., it is mandatory to add it in your PATH environment variable). If you are not familiar with this simple task, you should visit <http://www.java.com/en/download/help/path.xml>

##### Should I care about missing FANMOD and/or Multimap?

It depends. If Multimap is missing, you will not be able to perform multilayer community detection based on the generalization of the Infomap algorithm for this type of networks. If FANMOD is missing, you will not be able to perform multiplex motif analysis of your network.

###### Installing FANMOD and/or Multimap

If you are interested in using these features, and the default installation shows a WARNING, you just need to compile the software on your own machine. This is an easy task on all OSs.

1. Verify that you have a c++ compiler on your machine. On Linux and most recent versions of Mac OS X, it is already installed. In Windows, you might need some extra work ([Install MinGW on Windows 10](http://www.mingw.org/wiki/howto_install_the_mingw_gcc_compiler_suite) [Video tutorial to install MinGW on Windows 7](https://www.youtube.com/watch?v=k3w0igwp-FM)). You might want to take a look at the official web site: [Installing GCC](https://gcc.gnu.org/wiki/InstallingGCC) [Host/Target specific installation notes for GCC](https://gcc.gnu.org/install/specific.html)

2. Go into the `src` folder that is placed inside the muxViz folder
3. Unzip the software (`fanmod_src.zip` and/or `Multiplex-Infomap_src.zip`)
4. Enter into the software folder from the terminal and run `make`. This step is expected to go smoothly, because after a few seconds the GCC compiler will produce binary executables *ad hoc* for your system. If it is not the case, take a look at the [official web page](http://www.gnu.org/software/make/manual/make.html) and if you still can not compile, post on our Google Group
5. Copy the produced binaries inside the `bin` folder that is placed inside the muxViz folder
6. Verify that the software is correctly named as `fanmod_linux` and/or `multiplex-infomap_linux`


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

### Install muxViz with R 3.3 or higher

muxViz depends on several R packages that, when updated by their developers, might cause issues on muxViz. In general, it might happen that you use a very new version of R (muxViz was developed for R 3.2): you should still be able to use muxViz by simply patching the initial sanity check it does to ensure full compativility.

Open muxVizGUI.R and edit:

- Line 1: `if(grep("3.3",version$version.string)!=1){`
- Line 12: comment it out as `#devtools::install_github("trestletech/ShinyDash")`  to avoid the error *"package ‘ShinyDash’ is not available (for R version 3.3.0)"*

Finally, install Shiny Dash from R console: `devtools::install_github("ShinyDash", "trestletech")`

Now muxViz should start and work.

Thank you Maria Pereda!