if(!require(shiny)){
    install.packages("shiny")
    install.packages("devtools")
    library(devtools)
    devtools::install_github("ShinyDash", "trestletech")
    devtools::install_github("shiny-incubator", "rstudio")
}else{
    #check the version, and in case update to the latest one
    if(as.numeric(strsplit(packageDescription("shiny")$Version,"[.]")[[1]][2])<10){
        #install.packages("shiny")        
        library(devtools)
        devtools::install_github("rstudio/shiny")
    }
}
if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
}
if(!require(colorspace)){
    install.packages("colorspace")
}
if(!require(digest)){
    install.packages("digest")
}
if(!require(googleVis)){
    install.packages("googleVis")
}
if(!require(gplots)){
    install.packages("gplots")
}
if(!require(rgl)){
    install.packages("rgl")
}
if(!require(igraph)){
    install.packages("igraph")
}
if(!require(mapproj)){
    install.packages("mapproj")
}
if(!require(rgdal)){
    install.packages("rgdal")
}
if(!require(OpenStreetMap)){
    install.packages("OpenStreetMap")
}
if(!require(fields)){
    install.packages("fields")
}
if(!require(clue)){
    install.packages("clue")
}



library(shiny)
library(ShinyDash)

#uncomment the line below for detailed log of your session. Can be useful for debug purposes
#options(shiny.trace=TRUE)

runApp(getwd())

