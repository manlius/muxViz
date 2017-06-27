if(version$major<3 || (version$major==3 && version$minor<2.0)){
    stop(paste("WARNING! muxViz requires R 3.2.x or higher in order to work properly. \nPlease update your R framework.\n"))
}

if(!require(devtools)){
    install.packages("devtools")
}
if(!require(shiny)){
    install.packages("shiny")
    library(devtools)
    devtools::install_github("trestletech/ShinyDash")
    devtools::install_github("rstudio/shiny-incubator")
}else{
    #check the version, and in case update to the latest one
    if(packageDescription("shiny")$Version!="1.0.3"){
        install.packages("shiny")        
        #library(devtools)
        #devtools::install_github("rstudio/shiny")
    }
}
if(!require(shinyjs)){
    install.packages("shinyjs")
}
if(!require(shinydashboard)){
    install.packages("shinydashboard")
}
if(!require(markdown)){
    install.packages("markdown")
}
if(!require(session)){
    install.packages("session")
}
if(!require(d3heatmap)){
    devtools::install_github("rstudio/d3heatmap")
}
if(!require(ggplot2)){
    install.packages("ggplot2")
}
if(!require(rCharts)){
    install_github('ramnathv/rCharts')
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
if(!require(d3Network)){
    install.packages("d3Network")
}
if(!require(Matrix)){
    install.packages("Matrix")
}
if(!require(networkD3)){
    install.packages("networkD3")
}
if(!require(dplyr)){
    install.packages("dplyr")
}

library(shiny)
library(ShinyDash)
library(shinydashboard)

#muxViz <- function(){
#    #' Function to load muxViz
#    #'
#    #' @author Manlio De Domenico, \email{manlio.dedomenico@gmail.com}
#    #'
#    #' @examples
#    #' muxViz()
#    #'
#    #' @import shiny
#    #' @import ShinyDash
#    #' @import shinydashboard
#    #' @export
#    #'
#    enableBookmarking(store = "server")
#    runApp(getwd())    
#}

#uncomment the line below for detailed log of your session. Can be useful for debug purposes
#options(shiny.trace=TRUE)

enableBookmarking(store = "server")
runApp(getwd())

#shinyApp(ui, server)
