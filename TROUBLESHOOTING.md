muxViz v0.2
=========

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

