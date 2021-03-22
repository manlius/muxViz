muxViz v2.x
===========

Topics in this troubleshooting:

- **Very quick installation on Linux**
- **Multimap or FANMOD not found**
- **Possible errors when using Motifs**
- **Possible errors with rgdal**
- **Possible errors when using Correlation**
- **Possible errors with rjava (any OS)**
- **Possible errors with rjava (latest MacOSs)**
- **Install muxViz with R 3.3 or higher**
- **Use existing Linear Algebra Library**

---


### Very quick installation on Linux

If you use a Linux (Ubuntu-like) distribution, you are very lucky, because the following BASH script will do the job for you:

    #download R from their repository
    wget http://cran.es.r-project.org/src/base/R-3/R-3.0.3.tar.gz
    DIR=$PWD
        
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

To load muxViz in R environment, he solved modifying the muxVizGUI.R file:

Before:

	devtools::install_github("shiny-incubator", "rstudio")

After:

	devtools::install_github("rstudio/shiny-incubator", "rstudio")

Thank you Lucio!


### Multimap or FANMOD not found

muxViz uses an API to external softwares: Multimap (known as multiplex infomap) and Fanmod.

The home screen of muxViz will indicate if the available binaries are correctly installed and can be used by the platform. If it is not the case, a WARNING message is generally displayed. 

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

### Possible errors with rgdal

To work properly with geographical networks, the GDAL (Geospatial Data Abstraction Library) is required and should be installed before running muxViz for the first time. GDAL should be available as an R package and should be easily installed just by typing

	install.packages("sp")
	install.packages("rgdal")

within the R environment. However, in a few cases it can be more complicated and some users reported problems for its installation. If this is also your case you might want to take a look at some suggestions on [stackoverflow](http://stackoverflow.com/questions/15248815/rgdal-package-installation) or on [spatial.ly](http://spatial.ly/2010/11/installing-rgdal-on-mac-os-x/). 

In any case, it is highly recommend to visit the GDAL website and follow the hints provided [there](http://trac.osgeo.org/gdal/wiki/BuildHints).


### Possible errors with rjava (any OS)

Some users reported that, when using muxVix for the first time, they get the following error:

	Warning: Error in : package or namespace load failed for ‘OpenStreetMap’:
	 .onLoad failed in loadNamespace() for 'rJava', details:

One possible solution is to open the terminal and type

	R CMD javareconf

to reconfigure java to work correctly within R. You might read about possible solutions for [Linux](https://stackoverflow.com/questions/3311940/r-rjava-package-install-failing) and [Mac OS X](https://github.com/MTFA/CohortEx/wiki/Run-rJava-with-RStudio-under-OSX-10.10,-10.11-(El-Capitan)-or-10.12-(Sierra)).

Thanks Sneha Rajen!

### Possible errors with rjava (latest MacOSs)

It can happen that newest MacOS generate installation issues with rjava. 
MacOS Users should take a look at the following approaches: [1](http://www.owsiak.org/r-java-rjava-and-macos-adventures/), [2](https://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite), [3](http://osxdaily.com/2017/06/29/how-install-java-macos-high-sierra/), [4](https://github.com/MTFA/CohortEx/wiki/Run-rJava-with-RStudio-under-OSX-10.10,-10.11-(El-Capitan)-or-10.12-(Sierra)).

In general try make sure R is configured with full Java support (including JDK). Run

	sudo R CMD javareconf

to add Java support to R. If you still can't install it, read below. 


A possible solution for MacOS versions *before* Sierra:

	sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

But it may happen that your version of MacOS + Java does not have that path. To find the correct path try

	/usr/libexec/java_home

to obtain something like 

	/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

Look for the dylib file:

	find $(/usr/libexec/java_home) -name "libjvm"

If the result is null, then you might need to install (more) Java. Understand which Java you have installed already and where:

	/usr/libexec/java_home -V

If you see something like this:

    1.8.0_162, x86_64:  "Java SE 8" /Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home
    1.6.0_65-b14-468, x86_64:   "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
    1.6.0_65-b14-468, i386: "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

    /Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home

skip the next two paragraphs, otherwise install Java SE 6 and 8. 

#### Install Java SE 6

For Java SE 6 go to Apple support, [download](https://support.apple.com/downloads/DL1572/en_US/javaforosx.dmg) and install.

#### Install Java SE 8

Oracle website is the not the best user-friendly website around. Check instructions [here](https://docs.oracle.com/javase/8/docs/technotes/guides/install/mac_jdk.html) and download Java SE 8 from [here](http://www.oracle.com/technetwork/java/javase/downloads/java-archive-javase8-2177648.html).

You should download and install the file 

	jdk-8u162-macosx-x64.dmg 
	
or similar, for Java SE 8. Note that it will ask you to register an account: download is free, but you can't skip the registration phase (2 min required).

#### If Java SE 6 and SE 8 are installed

Run again 

	/usr/libexec/java_home -V

and hope to see 


    1.8.0_162, x86_64:  "Java SE 8" /Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home
    1.6.0_65-b14-468, x86_64:   "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
    1.6.0_65-b14-468, i386: "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

    /Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home
    
so that everything is installed correctly. Then run

	java -version

and hope to see something like
	
	java version "1.8.0_162"
	Java(TM) SE Runtime Environment (build 1.8.0_162-b12)
	Java HotSpot(TM) 64-Bit Server VM (build 25.162-b12, mixed mode)

Time to tell R how to use our Java: 

	sudo R CMD javareconf

and pray to see at the end of the terminal something like

	JAVA_HOME        : /Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home/jre
	Java library path: $(JAVA_HOME)/lib/server
	JNI cpp flags    : -I$(JAVA_HOME)/../include -I$(JAVA_HOME)/../include/darwin
	JNI linker flags : -L$(JAVA_HOME)/lib/server -ljvm
	Updating Java configuration in /Library/Frameworks/R.framework/Resources
	Done.

Now, in the terminal, paste the following:

	unset JAVA_HOME
	R --quiet -e 'install.packages("rJava", type="source", repos="http://cran.us.r-project.org")'

to install rJava from source with Java 8 JDK. 
If it works without errors, let's check everything is fine:

	R --quiet -e 'library("rJava"); .jinit(); .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")'

and you should get something like 

	1.8.0_162-b12 
	
as a result.




### Install muxViz with R 3.3 or higher

muxViz depends on several R packages that, when updated by their developers, might cause issues on muxViz. In general, it might happen that you use a very new version of R (muxViz was developed for R 3.2): you should still be able to use muxViz by simply patching the initial sanity check it does to ensure full compativility.

Open muxVizGUI.R and edit:

- Line 1: `if(grep("3.3",version$version.string)!=1){`
- Line 12: comment it out as `#devtools::install_github("trestletech/ShinyDash")`  to avoid the error *"package ‘ShinyDash’ is not available (for R version 3.3.0)"*

Finally, install Shiny Dash from R console: `devtools::install_github("ShinyDash", "trestletech")`

Now muxViz should start and work.

Thank you Maria Pereda!

### Use existing Linear Algebra Library


On Mac and Linux it is possible to exploit the already existing linear algebra R packages by forcing R to use a faster BLAS version. On a Mac OS X this is easily achieved by
  
    sudo ln -sf /System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/Current/libBLAS.dylib /Library/Frameworks/R.framework/Resources/lib/libRblas.dylib
