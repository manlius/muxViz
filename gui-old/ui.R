library(shiny)
library(ShinyDash)
library(markdown)
library(shinydashboard)
library(rCharts)
#library(shinyIncubator)
library(digest)
library(shinyjs)
library(colourpicker)
library(d3heatmap)
library(networkD3)
source("version.R")
source("language.R")

#library("shinythemes")


#RGB colors table
#http://www.javascripter.net/faq/rgbtohex.htm
paletteWeb <- "<a href='http://www.colorschemer.com/online.html' target='_blank'>Online Color Palette</a>, <a href='http://colorbrewer2.org'>Online Color Scheme</a>"
myBoxCnt <- 0

#http://getbootstrap.com/components/#glyphicons
#http://fontawesome.io/icons/
#http://rstudio.github.io/shinydashboard/appearance.html#icons

#interactive heatmaps
#http://www.statsblogs.com/2013/12/15/visualization-of-2012-crime-rates-of-different-states-in-the-us-using-rcharts/

paletteChoiceArray <- sort(row.names(brewer.pal.info[1]))


buildPath <- function(folder,objname){
    folder <- gsub("\"","", folder )
    objname <- gsub("\"","", objname )
    if( Sys.info()["sysname"]=="Windows" ){
        #return( paste(getwd(),folder,objname,sep="\\") )
        return( paste0("\"",paste(getwd(),folder,objname,sep="\\"),"\"") )
    }else{
        #return( paste(getwd(),folder,objname,sep="/") )        
        return( paste(getwd(),folder,objname,sep="/") )
    }
}

buildTmpPath <- function(objname){
    return(  buildPath("tmp",objname) )
}

concatenatePath <- function(folder,objname){
    folder <- gsub("\"","", folder )
    objname <- gsub("\"","", objname )
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste0("\"",paste(folder, paste0(objname, collapse="\\"), sep="\\"),"\"") )
    }else{
        return( paste(folder, paste0(objname, collapse="/"), sep="/") )
    }
}

getExecutablePath <- function(exec_name){
    path <- ""
    if( Sys.info()["sysname"]=="Windows" ){
        path <- buildPath("bin",paste0(exec_name,"_windows.exe"))    
    }else if( Sys.info()["sysname"]=="Linux" ){
        path <- buildPath("bin",paste0(exec_name,"_linux"))
    }else{
        path <- buildPath("bin",paste0(exec_name,"_macosx"))
    }
    
    return(path)
}

fanmodCheck <- function(){
    res <- system(getExecutablePath("fanmod"), ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res==255){
        return(paste0("<i class='fa fa-check'></i> ",getText("txtFanmodCheck"),"<br>"))
    }else{
        return(paste0("<i class='fa fa-warning'></i> ",getText("txtFanmodCheckWarning"),"<br>"))
    }
}

multimapCheck <- function(){
    res <- system(getExecutablePath("multiplex-infomap"), ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res==255){
        return(paste0("<i class='fa fa-check'></i> ",getText("txtMultimapCheck"),"<br>"))
    }else{
        return(paste0("<i class='fa fa-warning'></i> ",getText("txtMultimapCheckWarning"),"<br>"))
    }
}

muxbenchCheck <- function(){
    res1 <- system(getExecutablePath("muxbench1"), ignore.stdout = F, ignore.stderr = F, intern=F)
    res2 <- system(getExecutablePath("muxbench2"), ignore.stdout = F, ignore.stderr = F, intern=F)
    res3 <- system(getExecutablePath("muxbench3"), ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res1==0 && res2==0 && res3==0){
        return(paste0("<i class='fa fa-check'></i> ",getText("txtMuxbenchCheck"),"<br>"))
    }else{
        return(paste0("<i class='fa fa-warning'></i> ",getText("txtMuxbenchCheck"),"<br>"))
    }
}


myBox <- function(id, Title, Type="basic", ...){
    myBoxCnt <<- myBoxCnt + 1
    if(Type=="basic"){
        return(
            div(style="background-color: #FFFFFF; border-width: 1px; border-style: solid; border-color: #3286AD; margin: 10px 5px 10px 5px; -moz-border-radius: 15px; border-radius: 15px;", 
                div(style="background-color: #3286AD; color: #FFFFFF; -moz-border-top-right-radius: 15px; -moz-border-top-left-radius: 15px; border-top-left-radius: 15px; border-top-right-radius: 15px; text-align: center; font-family: 'Arial';", HTML(paste0("<font size='+1'><strong>",Title,"</strong></font>&nbsp;<i id=\"btnRollBox",myBoxCnt,"\" class=\"fa fa-caret-square-o-up\" onclick=\"rollBox(",paste0("btnRollBox",myBoxCnt,", RollBox",myBoxCnt),")\"></i>"))),
                div(style="padding: 5px 5px 5px 5px;", id=paste0("RollBox",myBoxCnt),
                    list(...)
                )
            )
        )
    }
    if(Type=="info"){
        return(
            div(style="background-color: #FFFFFF; border-width: 1px; border-style: solid; border-color: #165400; margin: 10px 5px 10px 5px; -moz-border-radius: 15px; border-radius: 15px;", 
                div(style="background-color: #165400; color: #FFFFFF; -moz-border-top-right-radius: 15px; -moz-border-top-left-radius: 15px; border-top-left-radius: 15px; border-top-right-radius: 15px; text-align: center; font-family: 'Arial';", HTML(paste0("<font size='+1'><strong>",Title,"</strong></font>"))),
                div(style="padding: 5px 5px 5px 5px;", 
                    list(...)
                )
            )
        )
    }
}

textInputRow <- function (inputId, label, value = "", width=NULL) {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,class="input-small", width=width))
}

checkboxInputRow <- function (inputId, label, value = "") {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "checkbox", value = value, class="input-small"))
}

placeHelp <- function(){
    HTML(paste("<div style=\"float: right;\">", icon("question-circle"), "Help", "</div>"))
}

placeInfo <- function(){
    HTML(paste(icon("info-circle"), "Info"))
}

placeWarn <- function(){
    HTML(paste("<font color=\"red\">",icon("exclamation-circle"), "Re-apply layout", "</font>"))
}

shinyUI(function(request) {
    bootstrapPage(
#shinyUI(fluidPage(shinythemes::themeSelector(),
    tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
    # Load D3.js
    tags$head(tags$script(src = 'http://d3js.org/d3.v3.min.js')),
    tags$head(tags$script(src = "muxviz.js")),
    tags$head(tags$title("muxViz")),
    #headerPanel("muxViz Graphical User Interface"),

    navbarPage("",
        tabPanel(icon("home"),
            sidebarLayout(position = "right",
                sidebarPanel( 
                    helpText(HTML(paste(getText("muxVizVersion"),muxVizVersion))),
                    helpText(HTML(paste(getText("muxVizUpdate"),muxVizUpdate))),
                    hr(),
                    helpText(HTML(paste(getText("muxVizSystem"),Sys.info()["sysname"]))),
                    helpText(HTML(paste(Sys.info()["version"]))),
                    hr(),
                    helpText(HTML(paste(version["version.string"][[1]]))),
                    hr(),
                    HTML(multimapCheck()),
                    HTML(fanmodCheck())
                    #HTML(muxbenchCheck())
                ),
                mainPanel(
                    HTML("<img src='img/home.png' height='550' style='{margin-left: auto; margin-right: auto;}'>")                
                )
            )
        ),
    navbarMenu(getText("navBarFile"),
        tabPanel("Import",
            actionLink("btnImportHelp", placeHelp() ),
            tabsetPanel(
                tabPanel("Import Networks", 
                    fluidRow(
                        column(width = 9,
                            myBox("boxNetworkModel", getText("NetworkModel"), "basic",
                                HTML("<img src='img/network_type.png' width='100%' alt=''/>"),
                                radioButtons('radMultiplexModel', '',
                                            c(Edge_Colored='MULTIPLEX_IS_EDGECOLORED',
                                                Interconnected_Multiplex='MULTIPLEX_IS_INTERCONNECTED',
                                                Interdependent_Multiplex='MULTIPLEX_IS_INTERDEPENDENT',
                                                General_Multilayer='MULTIPLEX_IS_MULTILAYER'
                                                ),
                                                selected='MULTIPLEX_IS_EDGECOLORED',
                                                inline=T
                                            )
                                )
                            ),
                        column(width = 3,
                    conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
                                myBox("boxMultiplexType", getText("boxMultiplexType"), "basic",
                                    radioButtons('radMultiplexType', '',
                                        c(Ordinal='MULTIPLEX_IS_ORDERED',
                                            Categorical='MULTIPLEX_IS_CATEGORICAL'),
                                            selected='MULTIPLEX_IS_CATEGORICAL'
                                        ),
                                    actionLink("btnMultiplexTypeInfo", placeInfo() ),
                                    tags$br(),
                                    tags$br(),
                                    textInputRow("txtOmega", label=HTML(getText("txtOmega")), "1"),
                                    helpText(getText("helpParameters"))
                                    )
                                ),
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                helpText(getText("txtNoParameters"))
                                )

                            )
                        ),
                    fluidRow(
                        column(width = 3,
                            myBox("boxProjectID", getText("ProjectID"), "basic",
                                textInput("txtProjectName", label=HTML(paste0("<strong>",getText("txtProjectName"),"</strong>")), paste0("muxViz_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")))
                                    )
                                ),
                            myBox("boxConfiguration", getText("Configuration"), "basic",
                                #helpText(HTML("<h4>Open Configuration File</h4>")),
                                #helpText(HTML(paste0("<strong><font color='#262626'>",getText("ConfigurationFileFormat"),"</font></strong>"))),
                                checkboxInput('chkConfigFileHeader', getText("chkConfigFileHeader"), FALSE),
                                textInputRow("txtConfigFileSep", label=HTML(getText("txtConfigFileSep")), ";"),
                                fileInput('project_file', "",
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                                        )
                                        #HTML(paste0("<strong>",getText("ConfigurationFile"),"</strong>"))
                                )
                            ),
#todo: here conditional panel depending on project_file
                            column(width = 3,
                                myBox("boxInputFormat", getText("InputFormat"), "basic",
                                    checkboxInput('chkEdgeListFileHeader', getText("chkEdgeListFileHeader"), FALSE),
                                    textInput("txtEdgeListFileSep", label=getText("txtEdgeListFileSep"), " "),
                                    helpText(getText("txtEdgeListFileSepHelp2")),
                                    checkboxInput("chkEdgeListLabel", getText("chkEdgeListLabel"), FALSE),
                                    conditionalPanel("input.chkEdgeListLabel",
                                        checkboxInput("chkEdgeListLabel2", getText("chkEdgeListLabel2"), FALSE)
                                        ),
                                    actionLink("btnEdgeListFileSepInfo", placeInfo() )
                                    )
                                ),
                                column(width = 3,
                                myBox("boxNetworkFormat", getText("NetworkFormat"), "basic",
                                    selectInput("selEdgeListType", HTML(paste0("<strong>",getText("selEdgeListType"),"</strong>")), 
                                        choices = c(getText("Undirected"), getText("Directed"))),
                                    checkboxInput("chkEdgeListWeighted", getText("chkEdgeListWeighted"), FALSE),
                                    checkboxInput("chkEdgeListUndirectedBoth", getText("chkEdgeListUndirectedBoth"), FALSE),
                                    checkboxInput("chkRESCALE_WEIGHT",getText("chkRESCALE_WEIGHT"),FALSE),
                                    checkboxInput('chkOutputEdgelistTable',HTML(getText("chkOutputEdgelistTable")),FALSE)
                                    ),
                                    HTML("<center>"),
                                    actionButton("btnImportNetworks", getText("btnImportNetworks")),
                                    HTML("</center>")
                                )

                        ),
                    tags$hr(),
                    helpText(HTML(paste0("<h4>",getText("ConfigurationFileSummary"),"</h4>"))),
                    tableOutput("layersTable"),
                    tags$hr(),
                    conditionalPanel(condition="input.btnImportNetworks>0",
                        myBox("boxNetworkSummary", getText("NetworkSummary"),"info",
                        #helpText(HTML("<h4>Summary for the multilayer network</h4>")),
                            htmlWidgetOutput(
                                outputId = 'projectSummaryHTML',
                                HTML("Layers: <span id=\"sumLayers\"></span><br>Type of layer: <span id=\"sumLayerType\"></span><br><br>Number of nodes: <span id=\"sumNodes\"></span><br>&nbsp;&nbsp;&nbsp;&nbsp;Minimum ID: <span id=\"sumMinNodeID\"></span><br>&nbsp;&nbsp;&nbsp;&nbsp;Maximum ID: <span id=\"sumMaxNodeID\"></span><br>Number of edges: <span id=\"sumEdges\"></span><br><br>External layout: <span id=\"sumIsLayoutExternal\"></span><br>Is geographical: <span id=\"sumIsLayoutGeographic\"></span><br>")
                                #seems that I cant use the getText here, it does not load the HTML..
                                )
                            ),
                        tags$hr(),
                        HTML("<h4>Matrix Explorer</h4>"),
                        checkboxInput("chkShowMatrixExplorer", HTML(getText("chkShowMatrixExplorer"))),
                        tabPanel("",
                            selectInput("selMatrixEplorerHeatmapColorPalette", HTML(getText("ColorPalette")), choices = paletteChoiceArray),
                            actionLink("btnMatrixExplorerPaletteInfo", placeInfo() ),
                            checkboxInput("chkMatrixEplorerHeatmapShowDendrogram", getText("ApplyClustering")),
                            tabsetPanel(id="tabsetMatrixEplorerMultilayer",
                                tabPanel("Multilayer",
                                    uiOutput("matrixExplorerHeatmapMultilayerUI")
                                    ),
                                tabPanel("Aggregate",
                                    uiOutput("matrixExplorerHeatmapAggregateUI")
                                    )
                                )
                            )
                        )
                    ),
                tabPanel("Edges Tables",
                    helpText(HTML(paste0("<h4>",getText("ImportedEdgeLists"),"</h4>"))),
                    conditionalPanel(condition="input.btnImportNetworks>0",
                        checkboxInput(inputId = "edgelistTablePageable", label = getText("Pageable"), TRUE),
                        conditionalPanel("input.edgelistTablePageable==true",
                            numericInput(inputId = "edgelistTablePageSize",label = getText("EdgesPerPage"),100)
                            ),  
                            htmlOutput("edgelistTable")
                        )
                    )
                ),
                tags$hr(),
                value=1
            ),
        tabPanel("Console",
            actionLink("btnConsoleHelp", placeHelp() ),
            tags$textarea(id="Console", rows=20, cols=145, ""),
            HTML("<center>"),
            actionButton("btnRunConsole" ,getText("Run")),
            HTML("</center>")
            ),
        tabPanel("Load/Save session",        
            actionLink("btnSessionManagerHelp", placeHelp() ),
            
            HTML(paste("<font color=\"red\">",icon("exclamation-circle"), getText("WarningMessage1"), "</font>")),
            fluidRow(
                column(width = 5,
                    myBox("boxLoadSession", "Session Manager", "basic",
                        uiOutput("selOutputSavedSessionsList"),
                        checkboxInput('chkConfirmDeleteSavedSession', getText("chkConfirmDeleteSavedSession"), FALSE),
                        actionButton("btnLoadSession", HTML(paste(icon("upload"), getText("btnLoadSession")))),
                        actionButton("btnDeleteSavedSession", HTML(paste(icon("trash"), getText("btnDeleteSavedSession")))),
                        bookmarkButton(getText("btnSaveSession"))
                        )
                    )
                )
            )
#        tabPanel("Generate report",
#            HTML("<center>"),
#            downloadButton("btnGenerateReport", getText("btnGenerateReport")),
#            HTML("</center>")
#            )

#        tabPanel("Open",
#            HTML("<center>"),
#            fileInput('session_file', HTML(paste0("<strong>",getText("SessionFile"),"</strong>")),
#            accept=c('.mux')),
#            actionButton("btnOpenSession" ,getText("btnOpenSession")),
#            HTML("</center>")
#            ),
#        tabPanel("Save",
#            HTML("<center>"),
#            actionButton("btnSaveSession" ,getText("btnSaveSession")),
#            HTML("</center>")
#            )

        ),
        tabPanel("Query",
            actionLink("btnQueryHelp", placeHelp() ),
            conditionalPanel(condition="input.btnImportNetworks>0",
                fluidRow(
                    column(width = 3,
                        myBox("boxQuerySetUp", getText("QuerySetUp"), "basic",
                            selectInput("selQueryType", HTML(getText("selQueryType")), choices = c(getText("Nodes"), getText("Edges"))),
                            conditionalPanel(condition="input.selQueryType=='Nodes'",
                                uiOutput("selQueryNodesOutputID"),
                                uiOutput("selQueryNodesLayersOutputID")
                            ),
                            conditionalPanel(condition="input.selQueryType=='Edges'",
                                uiOutput("selQueryEdgesNodesFromOutputID"),
                                uiOutput("selQueryEdgesNodesToOutputID"),
                                uiOutput("selQueryEdgesLayersOutputID")
                            ),
                            checkboxInput("chkQueryShowLabels", getText("chkQueryShowLabels"), F),
                            HTML("<center>"),
                            actionButton("btnQuery", getText("Query")),
                            HTML("</center>")
                            )
                        )
                    ),
                htmlOutput("queryNodesTable"),
                downloadButton('downQueryNodesTable', getText("downQueryNodesTable")) 
            )
        ),
        tabPanel("Diagnostics",        
            tabsetPanel(id="tabsetDiagnostics",
                tabPanel("Statistics",
                    tabsetPanel(id="tabsetStatistics",
                        tabPanel("Nodes",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxNodes", getText("Nodes"), "basic",
                                        helpText(getText("txtNumbNonIsolated")),
                                        checkboxInput(inputId = "nodeStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnNodeStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("nodeStatisticsPlot","nvd3") 
                                    )
                                )
                            ),
                        tabPanel("Edges",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxEdges", getText("Edges"), "basic",
                                        helpText(getText("txtNumbEdgesPerLayer")),
                                        checkboxInput(inputId = "edgeStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnEdgeStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("edgeStatisticsPlot","nvd3")                                        
                                    )
                                )
                            ),
                        tabPanel("Density",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxDensity", getText("Density"), "basic",
                                        helpText(getText("txtDensityPerLayer")),
                                        checkboxInput(inputId = "densityStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnDensityStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("densityStatisticsPlot","nvd3")                                        
                                    )
                                )
                            ),
                        tabPanel("Components",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxComponents", getText("Components"), "basic",
                                        helpText(getText("txtComponentsPerLayer")),
                                        checkboxInput(inputId = "componentsStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnComponentsStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("componentsStatisticsPlot","nvd3")                                        
                                    )
                                )
                            ),
                        tabPanel("Diameter",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxDiameter", getText("Diameter"), "basic",
                                        helpText(getText("txtDiameterPerLayer")),
                                        checkboxInput(inputId = "diameterStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnDiameterStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("diameterStatisticsPlot","nvd3")                                        
                                    )
                                )
                            ),
                        tabPanel("Mean Path Length",
                            fluidRow(
                                column(width = 3,
                                    myBox("boxMeanPathLength", getText("MeanPathLength"), "basic",
                                        helpText(getText("txtMPLPerLayer")),
                                        checkboxInput(inputId = "meanPathLengthStatisticsLogy", label = "Log y", F),
                                        HTML("<center>"),
                                        actionButton("btnMeanPathLengthStatistics", getText("Plot")),
                                        HTML("</center>")
                                        )
                                    ),
                                column(width=6,
                                    showOutput("meanPathLengthStatisticsPlot","nvd3")                                        
                                    )
                                )                                
                            )
                        )
                    ),
                tabPanel("Correlation",
                    actionLink("btnGlobalDiagnosticsCorrelationHelp", placeHelp() ),
                    conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                        helpText(getText("txtNoParametersInterdependent"))
                    ),
                    conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                        fluidRow(
                            column(width = 3,
                                myBox("boxInterLayerCorrelation", getText("txtInterLayerCorrelation"), "basic",
                                    #HTML('<h4>General diagnostics</h4>'),
                                    checkboxInput('chkMULTIPLEX_NODEOVERLAPPING', getText("chkMULTIPLEX_NODEOVERLAPPING"), TRUE),
                                    checkboxInput('chkMULTIPLEX_OVERLAPPING', getText("chkMULTIPLEX_OVERLAPPING"), TRUE),
                                    checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_PEARSON', getText("chkMULTIPLEX_INTERASSORTATIVITY_PEARSON"), TRUE),
                                    checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN', getText("chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN"), TRUE),
                                    checkboxInput('chkMULTIPLEX_SHORTESTPATH', getText("chkMULTIPLEX_SHORTESTPATH"), TRUE),                                            
                                    selectInput("selAssortativityType", HTML(getText("selAssortativityType")), 
                                        choices = c("TT", "II", "OO", "IO", "OI"))
                                    )
                                ),
                            column(width = 3,
                                myBox("boxCorrelationGraphicalOptions", getText("txtGraphicalOptions"),"basic",
                                    checkboxInput('chkEXPORT_MATRIX_PLOT', HTML(getText("chkEXPORT_MATRIX_PLOT")), FALSE),
                                    selectInput("selAssortativityTypeColorPalette", HTML(getText("ColorPalette")), 
                                        choices = paletteChoiceArray),
                                    actionLink("btnAssortativityPaletteInfo", placeInfo() )
                                    ),
                                HTML("<center>"),
                                actionButton("btnCalculateCorrelationDiagnostics", getText("btnCalculateCorrelationDiagnostics")),
                                HTML("</center>")
                                )
                            ),
                        tags$hr(),
                        HTML(paste0('<h4>',getText("txtGlobalDiagnostics"),'</h4>')),
                        tabsetPanel(
                            tabPanel("Node Overlapping",
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_NODEOVERLAPPING",                                    
                                    HTML(paste0('<h5>',getText("txtNodeOverlapping"),'</h5>')),
                                    checkboxInput("chkOverlappingNodeHeatmapShowDendrogram", getText("ApplyClustering"), F),
                                    uiOutput("overlappingNodeHeatmapUI"),
                                    tags$br(),
                                    htmlOutput("overlappingNodeSummaryTable"),    
                                    downloadButton('downOverlappingNodeSummaryTable', getText("downOverlappingNodeSummaryTable")),
                                    tags$hr()
                                )
                            ),                                
                            tabPanel("Edge Overlapping",                                
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_OVERLAPPING",
                                    htmlWidgetOutput(
                                            outputId = 'globalDiagnosticsOverlapping',
                                            HTML(paste(
                                            '<h5>Edge Overlapping</h5>',
                                            'Mean Global Overlapping: <span id="sumAvgGlobalOverlapping"></span><br>',
                                            '<br>'
                                            ))
                                    ),
                                    checkboxInput("chkOverlappingEdgeHeatmapShowDendrogram", getText("ApplyClustering"), F),
                                    uiOutput("overlappingEdgeHeatmapUI"),
                                    tags$br(),
                                    htmlOutput("overlappingSummaryTable"),    
                                    downloadButton('downOverlappingSummaryTable', getText("downOverlappingSummaryTable")),
                                    tags$hr()
                                )
                            ),                                
                            tabPanel("Deg. Pearson",    
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_PEARSON",                            
                                    helpText(HTML(paste0("<h5>",getText("txtInterlayerAssortativityPearson"),"</h5>"))),
                                    checkboxInput("chkInterPearsonHeatmapShowDendrogram", getText("ApplyClustering"), F),
                                    uiOutput("interPearsonHeatmapUI"),
                                    tags$br(),
                                    htmlOutput("interPearsonSummaryTable"),
                                    downloadButton('downInterPearsonSummaryTable', getText("downInterPearsonSummaryTable")),
                                    tags$hr()
                                )
                            ),                                
                            tabPanel("Deg. Spearman",                                
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN",
                                    helpText(HTML(paste0("<h5>",getText("txtInterlayerAssortativitySpearman"),"</h5>"))),
                                    checkboxInput("chkInterSpearmanHeatmapShowDendrogram", getText("ApplyClustering"), F),
                                    uiOutput("interSpearmanHeatmapUI"),
                                    tags$br(),
                                    htmlOutput("interSpearmanSummaryTable"),
                                    downloadButton('downInterSpearmanSummaryTable', getText("downInterSpearmanSummaryTable"))
                                )
                            ),
                            tabPanel("SP Distance",                                
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_SHORTESTPATH",
                                    helpText(HTML(paste0("<h5>",getText("ShortestPathDistanceSimilarity"),"</h5>"))),
                                    checkboxInput("chkDistanceSimilarityHeatmapShowDendrogram", getText("ApplyClustering"), F),
                                    uiOutput("distanceSimilarityHeatmapUI"),
                                    tags$br(),
                                    htmlOutput("distanceSimilaritySummaryTable"),
                                    downloadButton('downDistanceSimilaritySummaryTable', getText("downDistanceSimilaritySummaryTable"))
                                    )
                                )
                            )
                        )
                    ),
                tabPanel("Centrality",
                    actionLink("btnCentralityHelp", placeHelp() ),
                    
                    fluidRow(
                        column(width = 3,
                            myBox("boxCentrality", "Centrality", "basic",
                                radioButtons('radCentralityAlgorithm', '',
                                    c(Multilayer='CENTRALITY_MULTILAYER',
                                        Single_Layer='CENTRALITY_SINGLELAYER'),
                                        selected='CENTRALITY_MULTILAYER'
                                    )
#checkboxInput("chkNODE_CENTRALITY_MULTIPLEX",getText("chkNODE_CENTRALITY_MULTIPLEX"),TRUE)
                            ),
                            HTML("<center>"),
                            actionButton("btnCalculateCentralityDiagnostics", getText("btnCalculateCentralityDiagnostics")),
                            HTML("</center>")
                        ),
                        column(width = 3,
                            myBox("boxDescriptors", "Descriptors", "basic",
                                #HTML('<h4>Centrality</h4>'),
                                checkboxInput("chkNODE_CENTRALITY_DEGREE","Degree (in-, out- & tot)",TRUE),
                                checkboxInput("chkNODE_CENTRALITY_STRENGTH","Strength (in-, out- & tot)",F),
                                checkboxInput("chkNODE_CENTRALITY_PAGERANK","PageRank",F),
                                checkboxInput("chkNODE_CENTRALITY_EIGENVECTOR","Eigenvector",F),
                                checkboxInput("chkNODE_CENTRALITY_HUB","Hub",F),
                                checkboxInput("chkNODE_CENTRALITY_AUTHORITY","Authority",F),
                                checkboxInput("chkNODE_CENTRALITY_KATZ","Katz",F),
                                checkboxInput("chkNODE_CENTRALITY_KCORE","K-core",F),
                                checkboxInput("chkNODE_CENTRALITY_MULTIPLEXITY","Multiplexity",F)
                                )
                            )
                        ),
                    tags$hr(),
                    HTML(paste0('<h4>',getText("txtCentralityDiagnostics"),'</h4>')),
                    conditionalPanel(condition="input.btnCalculateCentralityDiagnostics>0",
                        checkboxInput(inputId = "centralityTablePageable", label = getText("Pageable"), TRUE),
                        conditionalPanel("input.centralityTablePageable==true",
                                uiOutput("numOutputCentralityTableNodesPerPage")
                            ),
                        tabsetPanel(
                            tabPanel("Multilayer",
                                htmlOutput("centralityTable"),
                                downloadButton('downCentralityTable', getText("downCentralityTable"))
                                ),
                            tabPanel("Single layer",
                                htmlOutput("centralityTableSingleLayer"),
                                downloadButton('downCentralityTableSingleLayer', getText("downCentralityTableSingleLayer"))
                                )                                        
                            ),
                        tags$hr(),
                        fluidRow(
                            column(width = 3,
                                myBox("boxDiagnosticsAnalysis", getText("txtDiagnosticsAnalysis"), "basic",
                                    uiOutput("selDiagnosticsCentralityVizOutputID"),
                                    radioButtons('radDiagnosticsCentralityType', getText("Analysis"),
                                        c(TopRanked='DIAGNOSTICS_ANALYSIS_TOPRANKED',
                                            Distribution='DIAGNOSTICS_ANALYSIS_DISTRIBUTION',
                                            Scatter='DIAGNOSTICS_ANALYSIS_SCATTER'),
                                            selected='DIAGNOSTICS_ANALYSIS_TOPRANKED'
                                        ),
                                    HTML(paste0("<strong>",getText("txtNetworksToInclude"),"</strong>:")),
                                    checkboxInput(inputId = "chkCentralityAnalysisStructureMultiplex", label = "Multilayer", T),
                                    checkboxInput(inputId = "chkCentralityAnalysisStructureLayer", label = "Layer(s)", F),
                                    conditionalPanel(condition="input.chkCentralityAnalysisStructureLayer",
                                        textInput("txtDiagnosticsCentralityStructureLayer", label=HTML(paste0("<strong>",getText("txtDiagnosticsCentralityStructureLayer"),"</strong>")), "1")
                                        ), 
                                    checkboxInput(inputId = "chkCentralityAnalysisStructureAggregate", label = "Aggregate", T),
                                    conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_DISTRIBUTION'",
                                        textInput("txtDiagnosticsCentralityDistributionBins", label=HTML(paste0("<strong>",getText("txtDiagnosticsCentralityDistributionBins"),"</strong>")), "30"),
                                        checkboxInput(inputId = "centralityAnalysisDistributionLogx", label = "Log x", F),
                                        checkboxInput(inputId = "centralityAnalysisDistributionLogy", label = "Log y", F)
                                        ),
                                    conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_TOPRANKED'",
                                        textInput("txtDiagnosticsCentralityTopRankedBins", label=HTML(paste0("<strong>",getText("txtDiagnosticsCentralityTopRankedBins"),"</strong>")), "20"),
                                        checkboxInput(inputId = "centralityAnalysisTopRankedLog", label = "Log", F)
                                        ),
                                    conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_SCATTER'",
                                        uiOutput("selDiagnosticsCentralityVizScatterOutputID"),
                                        textInput('txtDiagnosticsCentralityVizScatterColorTransparency', label=getText("txtDiagnosticsCentralityVizScatterColorTransparency"), "0.7"),
                                        uiOutput("selDiagnosticsCentralityVizScatterSizeOutputID"),
                                        checkboxInput(inputId = "centralityAnalysisScatterLogx", label = "Log x", F),
                                        checkboxInput(inputId = "centralityAnalysisScatterLogy", label = "Log y", F),
                                        checkboxInput(inputId = "centralityAnalysisScatterLogRadius", label = "Log Radius", F)
                                        ),
                                    HTML("<center>"),
                                    actionButton("btnCentralityDiagnosticsAnalysis", getText("Plot")),
                                    HTML("</center>")
                                    )
                                ),
                            column(width=6,
                                showOutput("centralityAnalysisPlot","nvd3")                                        
                                )
                            ),
                        tags$hr()
                        )
                    ),
                tabPanel("Triads",
                    actionLink("btnTriadsHelp", placeHelp() ),
                    fluidRow(
                        column(width = 3,
                            myBox("boxTriads", "Triadic closure", "basic",
                                #helpText("Note that directed networks will be transformed to undirected before calculation."),
                                radioButtons('radTriadicClosureAlgorithm', '',
                                    c(Multilayer='TRIADIC_CLOSURE_MULTILAYER',
                                        Single_Layer='TRIADIC_CLOSURE_SINGLELAYER'),
                                        selected='TRIADIC_CLOSURE_MULTILAYER'
                                    ),
conditionalPanel(condition="input.radTriadicClosureAlgorithm=='TRIADIC_CLOSURE_MULTILAYER'"
                                    )
                                ),
                                HTML("<center>"),
                                actionButton("btnCalculateTriadsDiagnostics", "Calculate Triadic Closure"),
                                conditionalPanel(condition="input.btnCalculateTriadsDiagnostics>0",
                                    actionButton("btnRefreshTriadsDiagnostics", HTML(paste(icon("refresh"), getText("RefreshResults"))))
                                    ),
                                HTML("</center>")
                            )
                        ),
                    tags$hr(),
                    HTML('<h4>Triadic Closure</h4>'),
                    conditionalPanel(condition="input.btnCalculateTriadsDiagnostics>0",
                        selectInput("selTriadsHeatmapColorPalette", HTML(getText("ColorPalette")), choices = paletteChoiceArray),
                        actionLink("btnTriadsPaletteInfo", placeInfo() ),
                        checkboxInput("chkTriadsHeatmapShowDendrogram", getText("ApplyClustering")),
                        checkboxInput(inputId = "triadsTablePageable", label = getText("Pageable"), TRUE),
                        checkboxInput(inputId = "triadsHeatmapShowLabels", label = getText("ShowNodeLabels"), TRUE),
                        conditionalPanel("input.triadsTablePageable==true",
                            uiOutput("numOutputTriadsTableNodesPerPage")
                            ),  
                        tabsetPanel(
                            tabPanel("Multilayer",
                                uiOutput("triadsHeatmapUI"),
                                tags$br(),
                                HTML(paste0('<h5>',getText("triadsHeatmapUI"),'</h5>')),
                                showOutput("triadsDistributionPlot","nvd3"),
                                HTML('<h5>Tables</h5>'),                                
                                htmlOutput("triadsSummaryTable"),
                                downloadButton('downTriadsSummaryTable', getText("downTriadsSummaryTable")),
                                tags$hr(),
                                htmlOutput("triadsTable"),
                                downloadButton('downTriadsTable', getText("downTriadsTable"))
                                ),
                            tabPanel("Single layer",
                                uiOutput("triadsHeatmapSingleLayerUI"),
                                tags$br(),
                                HTML(paste0('<h5>',getText("triadsHeatmapSingleLayerUI"),'</h5>')),
                                showOutput("triadsDistributionPlotSingleLayer","nvd3"),
                                HTML('<h5>Tables</h5>'),                                
                                htmlOutput("triadsSummaryTableSingleLayer"),
                                downloadButton('downTriadsSummaryTableSingleLayer', getText("downTriadsSummaryTableSingleLayer")),
                                tags$hr(),
                                htmlOutput("triadsTableSingleLayer"),
                                downloadButton('downTriadsTableSingleLayer', getText("downTriadsTableSingleLayer"))
                                )   
                            )
                        )
                    ),
                tabPanel("Motifs",
                    actionLink("btnMotifsHelp", placeHelp() ),
                    fluidRow(
                        column(width = 3,
                            myBox("boxMotifs", "Algorithm options","basic",
                                selectInput("selMotifSize", HTML(paste0("<strong>",getText("selMotifSize"),"</strong>")), choices = as.character(3:4)),
                                textInput("txtMotifSamples", HTML(paste0("<strong>",getText("txtMotifSamples"),"</strong>")), "100000"),
                                
                                selectInput("selMotifNullModel", HTML(paste0("<strong>",getText("selMotifNullModel"),"</strong>")), 
                                            choices = c("Local const", "Global const", "No regard")),
                                textInput("txtMotifRandomNetworks", HTML(paste0("<strong>",getText("txtMotifRandomNetworks"),"</strong>")), "1000"),
                                textInputRow("txtMotifRandomExchangePerEdges", HTML("<strong>Exchanges per edge:</strong>"), "3"),
                                textInputRow("txtMotifRandomExchangeAttempts", HTML("<strong>Exchange attempts:</strong>"), "3", width="10px") 
                                )
                            ),
                        column(width = 3,
                            myBox("boxMotifs2", "Statistical cuts","basic",
                                checkboxInputRow('chkMotifAbsZscore', HTML("<strong>|Z-score| ></strong>"), FALSE),
                                textInputRow("txtMotifAbsZscore", "", "3"),
                                checkboxInputRow('chkMotifPvalue', HTML("<strong>p-value <</strong>"), FALSE),
                                textInputRow("txtMotifPvalue", "", "0.05"),
                                checkboxInputRow('chkMotifFrequency', HTML("<strong>Frequency ></strong>"), FALSE),
                                textInputRow("txtMotifFrequency", "", "0.01")
                                ),
                            myBox("boxMotifs3", "Graphical options", "basic",
                                selectInput("selMotifResultsSortBy", HTML(paste0("<strong>",getText("SortBy"),"</strong>")), 
                                    choices = c("Frequency", "Z-score", "p-value")),
                                    selectInput("selMotifColorPalette", HTML(getText("ColorPalette")), 
                                            choices = append(as.vector(paletteChoiceArray),"random")),
                                    actionLink("btnMotifsPaletteInfo", placeInfo() ),
                                    helpText(getText("selMotifColorPalette"))
                                ),
                            HTML("<center>"),
                            actionButton("btnCalculateMotifs", getText("btnCalculateMotifs")),
                            HTML("</center>")
                            )
                        ),
                        tags$hr(),
                        
                        HTML('<h4>Multilayer motifs</h4>'),
                        conditionalPanel(condition="input.btnCalculateMotifs>0",
                            #imageOutput("jsdMatrixSummaryImage",width = "100%", height = "700px"),
                            #imageOutput("reducibilityDendrogramSummaryImage",width = "100%", height = "700px"),
                            #HTML('<center><h5>Quality function</h5></center>'),
                            #showOutput("reducibilityQualityFunction","nvd3"),
                            #dataTableOutput('motifsTable'),
                            plotOutput("motifsColorLegend"),
                            htmlOutput('motifsGvisTable'),
                            downloadButton('downMotifsTable', getText("downMotifsTable")),
                            tags$hr()
                        )
                    ),
                tabPanel("Conn. Components",
                    actionLink("btnComponentsHelp", placeHelp() ),
                    fluidRow(
                        column(width = 3,
                            myBox("boxConnectedComponents", "Connected Components", "basic",
                                helpText(getText("helpBoxConnectedComponents")),
                                radioButtons('radConnectedComponentsAlgorithm', '',
                                    c(Multilayer='CONNECTED_COMPONENTS_MULTILAYER',
                                        Single_Layer='CONNECTED_COMPONENTS_SINGLELAYER'),
                                        selected='CONNECTED_COMPONENTS_MULTILAYER'
                                    ),
conditionalPanel(condition="input.radConnectedComponentsAlgorithm=='CONNECTED_COMPONENTS_MULTILAYER'"
                                #selectInput("selConnectedComponentsMuxType", "Method:", 
                                    # choices = c("Simple", "Extended")),
#                                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
#                                                helpText("Hint: for Multilayer, the inter-layer strength must be set > 0 from the 'Mux Set Up' tab")
#                                                )
                                    )
#conditionalPanel(condition="input.radConnectedComponentsAlgorithm==''CONNECTED_COMPONENTS_SINGLELAYER",
#                                            selectInput("selConnectedComponentsSingleLayerType", HTML("Components type:"), 
#                                                choices = c("Weak", "Strong"))
#                                        ),

                                ),
                            HTML("<center>"),
                            actionButton("btnCalculateComponentsDiagnostics", getText("btnCalculateComponentsDiagnostics")),
                            conditionalPanel(condition="input.btnCalculateComponentsDiagnostics>0",
                                actionButton("btnRefreshComponentsDiagnostics", HTML(paste(icon("refresh"), getText("RefreshResults"))))
                                ),
                            HTML("</center>")
                            )
                        ),
                    tags$hr(),
                    HTML('<h4>Connected Components</h4>'),
                    conditionalPanel(condition="input.btnCalculateComponentsDiagnostics>0",
                        selectInput("selComponentsHeatmapColorPalette", HTML(getText("ColorPalette")), choices = paletteChoiceArray),
                        actionLink("btnComponentsPaletteInfo", placeInfo() ),
                        checkboxInput("chkComponentsHeatmapShowDendrogram", getText("ApplyClustering")),
                        checkboxInput(inputId = "componentsTablePageable", label = getText("Pageable"), TRUE),
                        checkboxInput(inputId = "componentsHeatmapShowLabels", label = getText("ShowNodeLabels"), TRUE),

                        conditionalPanel("input.componentsTablePageable==true",
                            uiOutput("numOutputComponentsTableNodesPerPage")
                            ),  
                        tabsetPanel(
                            tabPanel("Multilayer",
                                uiOutput("componentsHeatmapUI"),
                                tags$br(),
                                HTML(paste0('<h5>',getText("componentsHeatmapUI"),'</h5>')),
                                showOutput("componentsDistributionPlot","nvd3"),
                                HTML('<h5>Tables</h5>'),                                
                                htmlOutput("componentsSummaryTable"),
                                downloadButton('downComponentsSummaryTable', getText("downComponentsSummaryTable")),
                                tags$hr(),
                                htmlOutput("componentsTable"),
                                downloadButton('downComponentsTable', getText("downComponentsTable"))
                                ),
                            tabPanel("Single layer",
                                uiOutput("componentsHeatmapSingleLayerUI"),
                                tags$br(),
                                HTML(paste0('<h5>',getText("componentsHeatmapSingleLayerUI"),'</h5>')),
                                showOutput("componentsDistributionPlotSingleLayer","nvd3"),
                                HTML('<h5>Tables</h5>'),                                
                                htmlOutput("componentsSummaryTableSingleLayer"),
                                downloadButton('downComponentsSummaryTableSingleLayer', getText("downComponentsSummaryTableSingleLayer")),
                                tags$hr(),
                                htmlOutput("componentsTableSingleLayer"),
                                downloadButton('downComponentsTableSingleLayer', getText("downComponentsTableSingleLayer"))
                                )   
                            )
                        )
                    ),
                tabPanel("Community",
                    actionLink("btnCommunityHelp", placeHelp() ),
                    fluidRow(
                        column(width = 3,
                            myBox("boxCommunity", "Algorithm", "basic",    
                                uiOutput("communityChoices"),
                                uiOutput("communityParameters")                                    
                                ),
                            HTML("<center>"),
                            actionButton("btnCalculateCommunityDiagnostics", getText("btnCalculateCommunityDiagnostics")),
                            conditionalPanel(condition="input.btnCalculateCommunityDiagnostics>0",
                                actionButton("btnRefreshCommunityDiagnostics", HTML(paste(icon("refresh"), getText("RefreshResults"))))
                                ),
                            HTML("</center>")
                            )
                        ),
                    tags$hr(),
                    HTML('<h4>Communities</h4>'),
                    conditionalPanel(condition="input.btnCalculateCommunityDiagnostics>0",
                        selectInput("selCommunityHeatmapColorPalette", HTML(getText("ColorPalette")), choices = paletteChoiceArray),
                        actionLink("btnCommunityPaletteInfo", placeInfo() ),
                        checkboxInput("chkCommunityHeatmapShowDendrogram", getText("ApplyClustering")),
                        checkboxInput(inputId = "communityTablePageable", label = getText("Pageable"), TRUE),
                        checkboxInput(inputId = "communityHeatmapShowLabels", label = getText("ShowNodeLabels"), TRUE),
                        conditionalPanel("input.communityTablePageable==true",
                            uiOutput("numOutputCommunityTableNodesPerPage")
                            ),  
                        tabsetPanel(
                            tabPanel("Multilayer",
                                uiOutput("communityHeatmapUI"),
                                tags$br(),
                                tags$br(),
                                HTML(paste0('<h5>',getText("communityMultiplexAlternativeViz"),'</h5>')),
                                fluidRow(
                                    column(3,
                                        HTML("<img src='img/chord.png' height='64' width='64' style='{margin-left: auto; margin-right: auto;}'>"),
                                        actionButton("btnPrintCommunityMultiplexChord", 
                                                             getText("btnPrintCommunityMultiplexChord"))
                                        )
                                    ),  
                                tags$br(),
                                tags$br(),
                                conditionalPanel("input.chkMultimapBatchExploration",
                                    HTML(paste0('<h5>',getText("communityMultiplexBatchViz"),'</h5>')),
                                    fluidRow(
                                        column(3,
                                            HTML("<img src='img/sankey.png' height='64' width='64' style='{margin-left: auto; margin-right: auto;}'>"),
                                            actionButton("btnPrintCommunityMultiplexSankey", 
                                                         getText("btnPrintCommunityMultiplexSankey"))
                                            )
                                        ),
                                    tags$br(),
                                    fluidRow(
                                        column(4,
                                            showOutput("batchMultiplexCommunityData1","nvd3")
                                            ),
                                        column(4,
                                            showOutput("batchMultiplexCommunityData2","nvd3")
                                            )
                                        )
                                    ),  
                                tags$br(),
                                tags$br(),
                                HTML(paste0('<h5>',getText("communityHeatmapUI"),'</h5>')),
                                showOutput("communityDistributionPlot","nvd3"),
                                HTML('<h5>Tables</h5>'),
                                htmlOutput("communitySummaryTable"),
                                downloadButton('downCommunitySummaryTable', getText("downCommunitySummaryTable")),
                                tags$hr(),
                                htmlOutput("communityTable"),
                                downloadButton('downCommunityTable', getText("downCommunityTable")),
                                tags$hr(),
                                conditionalPanel("input.chkMultimapBatchExploration",
                                    htmlOutput("communityBatchTable"),
                                    downloadButton('downCommunityBatchTable', getText("downCommunityBatchTable"))  
                                    )
                                ),
                            tabPanel("Single layer",
                                uiOutput("communityHeatmapSingleLayerUI"),
                                tags$br(),
                                HTML(paste0('<h5>',getText("communityHeatmapSingleLayerUI"),'</h5>')),
                                showOutput("communityDistributionPlotSingleLayer","nvd3"),
                                HTML('<h5>Tables</h5>'),
                                htmlOutput("communitySummaryTableSingleLayer"),
                                downloadButton('downCommunitySummaryTableSingleLayer', getText("downCommunitySummaryTableSingleLayer")),
                                tags$hr(),
                                htmlOutput("communityTableSingleLayer"),
                                downloadButton('downCommunityTableSingleLayer', getText("downCommunityTableSingleLayer"))
                                )
                            )
                        )  
                    ),
                tabPanel("Network of layers",
                    actionLink("btnGlobalDiagnosticsNetworkLayersHelp", placeHelp() ),
                    conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                        HTML("<center>"),
                        tags$br(),
                        actionButton("btnRenderNetworkOfLayers", getText("btnRenderNetworkOfLayers")),
                        HTML("</center>"),
                        HTML("<center>"),
                        htmlOutput('networkOfLayersPlot'),
                        HTML("</center>")
                        )
                    ),
                tabPanel("Annular Viz",
                    actionLink("btnGlobalDiagnosticsAnnularVizHelp", placeHelp() ),
                    conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                        helpText(getText("panelGlobalDiagnosticsAnnularVizHelp"))
                    ),
                    conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                        fluidRow(
                            column(width = 3,
                                myBox("boxAnnular", "Metric", "basic",
                                    radioButtons('radAnularVizCorrelationMethod', getText("radAnularVizCorrelationMethod"),
                                        c(Spearman='ANULAR_VIZ_CORRELATION_SPEARMAN',
                                            Pearson='ANULAR_VIZ_CORRELATION_PEARSON',
                                            Jensen_Shannon_Divergence='ANULAR_VIZ_CORRELATION_JSD'),
                                            selected='ANULAR_VIZ_CORRELATION_SPEARMAN'
                                        ),
                                    textInput("txtANULAR_VIZ_BINS",label=getText("txtANULAR_VIZ_BINS"),"50"),
                                    checkboxInput("chkANULAR_VIZ_LOG",getText("chkANULAR_VIZ_LOG"),FALSE)
                                    ),
                                HTML("<center>"),
                                actionButton("btnAnularViz", getText("btnAnularViz")),
                                HTML("</center>")
                                ),
                            column(width = 3,
                                myBox("boxAnnular2", "Graphical Options","basic",                                        
                                    uiOutput("selAnularVizOutputFeatureID"),
                                    uiOutput("selAnularVizOutputLayerID"),
                                    textInputRow("txtANULAR_VIZ_RCORE",label=getText("txtANULAR_VIZ_RCORE"),"0.3"),
                                    textInputRow("txtANULAR_VIZ_RING_DISPLACEMENT", label=getText("txtANULAR_VIZ_RING_DISPLACEMENT"),"0.01"),
                                    checkboxInput("chkANULAR_VIZ_CELL_BORDER", getText("chkANULAR_VIZ_CELL_BORDER"),FALSE),
                                    checkboxInput("chkANULAR_VIZ_SHOW_NODE_LABEL", getText("chkANULAR_VIZ_SHOW_NODE_LABEL"),FALSE),
                                    textInputRow("txtANULAR_VIZ_FONT_SIZE", label=getText("txtANULAR_VIZ_FONT_SIZE"),"1.5"),
                                    selectInput("selAnularColorPalette", HTML(getText("ColorPalette")), 
                                        choices = paletteChoiceArray),
                                    actionLink("btnAnnularPaletteInfo", placeInfo() )
                                    )
                                )
                            ),
                        tags$hr(),
                        conditionalPanel(condition="input.btnAnularViz>0 && input.btnCalculateCentralityDiagnostics>0",
                            helpText(HTML("<h5>Multiplex</h5>")),
                            helpText(getText("helpAnularVizSummaryMuxImage")),
                            imageOutput("anularVizSummaryMuxImage",width = "100%", height = "600px"),
                            tags$hr(),
                            helpText(HTML(paste0("<h5>",getText("outputAnularVizImages"),"</h5>"))),
                            helpText(getText("helpOutputAnularVizImages")),
                            uiOutput("outputAnularVizImages"),
                            tags$hr()                                                  
                            ),
                        tags$hr()
                        )
                    )
                )
            ),
        tabPanel("Visualization",
            actionLink("btnVisualizationHelp", placeHelp() ),
            HTML(paste("<div style=\"float: right; margin: 10px;\">", 
                                 actionButton("btnRenderNetworks", getText("btnRenderNetworks")), 
                                 "</div>")),
            HTML(paste("<div style=\"float: right; margin: 10px;\">", 
                                 actionButton("btnApplyLayout", getText("btnApplyLayout")),
                                 "</div>")),
            tabsetPanel(
                tabPanel("Layout",
                    fluidRow(
                        column(width = 9,
                            myBox("boxVisualization", "Layout Algorithm (Network of Layers)", "basic",
                                HTML("<img src='img/layout_type.png' width='100%' alt=''/>"),
                                radioButtons('radNetworkOfLayersLayoutType', '',
                                            c(One_line='NETWORK_LAYERS_LAYOUT_ONELINE',
                                                Multi_line='NETWORK_LAYERS_LAYOUT_MULTILINE',
                                                Force_directed='NETWORK_LAYERS_LAYOUT_FORCEDIRECTED',
                                                Matrix='NETWORK_LAYERS_LAYOUT_MATRIX'
                                                ),
                                                selected='NETWORK_LAYERS_LAYOUT_ONELINE',
                                                inline=T
                                            ),
                                    conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_MULTILINE'",
                                        textInputRow("txtNetworkLayersMultilineRows", label=getText("txtNetworkLayersMultilineRows"), 2),
                                        textInputRow("txtNetworkLayersMultilineCols", label=getText("txtNetworkLayersMultilineCols"), 2),
                                        helpText(getText("helptxtNetworkLayersMultilineCols"))
                                    ),
                                    conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_MATRIX'",
                                        textInputRow("txtNetworkLayersMatrixRows", label=getText("txtNetworkLayersMatrixRows"), 2),
                                        textInputRow("txtNetworkLayersMatrixCols", label=getText("txtNetworkLayersMatrixCols"), 2),
                                        helpText(getText("helptxtNetworkLayersMatrixCols"))
                                    ),
                                    conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_FORCEDIRECTED'",
                                        HTML(paste("<font color=\"red\">",icon("exclamation-circle"), getText("boxVisualization1"), "</font>")),
                                        helpText(getText("boxVisualization1b"))
                                    )
                                )
                            )
                        ),
                    fluidRow(
                        column(width = 4,
                            myBox("boxVisualization2", "Layout Algorithm (Layer)", "basic",
                                #HTML('<h4>Algorithm to be used to visualize nodes in the multiplex network</h4>'),
                                radioButtons('radLayoutAlgorithm', '',
                                    c(Fruchterman_Reingold='LAYOUT_FRUCHTERMAN_REINGOLD',
                                        LGL='LAYOUT_LGL',
                                        DRL='LAYOUT_DRL',
                                        Spring='LAYOUT_SPRING',
                                        Kamada_Kawai='LAYOUT_KAMADA_KAWAI',
                                        Reingold_Tilford='LAYOUT_REINGOLD_TILFORD',
                                        Combined='LAYOUT_COMBINED'),
                                        selected='LAYOUT_COMBINED'
                                    ),
                                conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_ONELINE'",
                                    radioButtons('radLayoutDimension', '',
                                        c(Two_Dimensional='LAYOUT_DIMENSION_2D',
                                            Three_Dimensional='LAYOUT_DIMENSION_3D'),
                                            selected='LAYOUT_DIMENSION_2D', inline=TRUE
                                        ),
                                        checkboxInput("chkPLOT_AS_EDGE_COLORED", HTML(getText("chkPLOT_AS_EDGE_COLORED")),FALSE),
                                        checkboxInput("chkPLOT_WITH_RGL",HTML("openGL"),TRUE),
                                        helpText(getText("helpchkPLOT_WITH_RGL"))
                                    ),
                                actionLink("btnWarn1", placeWarn())
                                )
                            ),
                        column(width = 4,
                            myBox("boxVisualization3", "Graphical Options","basic",
                                #HTML('<h4>Type of visualization</h4>'),
                                radioButtons('radLayoutType', getText("radLayoutType"),
                                    c(Multiplex='LAYOUT_MULTIPLEX',
                                        By_LayerID='LAYOUT_BY_LAYER_ID',
                                        Independent='LAYOUT_INDEPENDENT'),
                                        selected='LAYOUT_MULTIPLEX', inline=TRUE
                                    ),
                                conditionalPanel(condition="input.radLayoutType=='LAYOUT_MULTIPLEX'",
                                    radioButtons('radLayoutTypeMultiplex', getText("radLayoutTypeMultiplex"),
                                        c(Aggregate='LAYOUT_MULTIPLEX_AGGREGATE',
                                            Union='LAYOUT_MULTIPLEX_UNION',
                                            Intersection='LAYOUT_MULTIPLEX_INTERSECTION'),
                                            selected='LAYOUT_MULTIPLEX_AGGREGATE', inline=FALSE
                                        )
                                    ),
                                conditionalPanel(condition="input.radLayoutType=='LAYOUT_BY_LAYER_ID'",
                                    #this is a dynamic object changing because of input
                                    uiOutput("selOutputLayerID")
                                    ),
                                textInputRow("txtLAYOUT_MAXITER", 
                                    label=HTML(getText("txtLAYOUT_MAXITER")),
                                    value="1000", width="20px"
                                    ),
                                tags$br(),
                                actionLink("btnWarn2", placeWarn())
                                )
                            )
                        )
                    ),
                tabPanel("Graphics",
                    fluidRow(
                        column(width = 4,
                            myBox("boxVisualization4", "Plot Options","basic",
                                #HTML('<h4>Options for the rendering of the multiplex</h4>'),
                                textInputRow('txtPLOT_TITLE', label=getText("txtPLOT_TITLE"), "", width="70px"),
                                textInputRow('txtPLOT_SUBTITLE', label=getText("txtPLOT_SUBTITLE"), "", width="50px"),
                                colourpicker::colourInput("colBACKGROUND_COLOR", getText("colBACKGROUND_COLOR"), "white")
                                ),
                            conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                myBox("boxVisualization5", "Light Options (RGL)","basic",
                                    checkboxInput("chkPLOT_LIGHT", getText("chkPLOT_LIGHT"),FALSE),
                                    textInputRow('txtPLOT_LIGHT_PHI', label=getText("txtPLOT_LIGHT_PHI"), "20", width="30px"),
                                    textInputRow('txtPLOT_LIGHT_THETA', label=getText("txtPLOT_LIGHT_THETA"), "30", width="30px"),
                                    HTML("<center>"),
                                    actionButton("btnResetLights", getText("btnResetLights")),
                                    HTML("</center>"),
                                    helpText(getText("helpbtnResetLights"))
                                    )
                                ),
                            conditionalPanel(condition="!input.chkPLOT_WITH_RGL",
                                myBox("boxVisualization6", "Window relative margins","basic",
                                    helpText(getText("helpboxVisualization6")),
                                    textInputRow('txtMARGIN_LEFT', label=getText("txtMARGIN_LEFT"), "1"),
                                    textInputRow('txtMARGIN_RIGHT', label=getText("txtMARGIN_RIGHT"), "1"),
                                    textInputRow('txtMARGIN_TOP', label=getText("txtMARGIN_TOP"), "1"),
                                    textInputRow('txtMARGIN_BOTTOM', label=getText("txtMARGIN_BOTTOM"), "1"),
                                    actionLink("btnWarn3", placeWarn())
                                    )
                                )
                            ),
                        column(width = 4,
                            myBox("boxVisualization7", "3D Options", "basic",    
                                conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                    checkboxInput('chkPLOT_AXES3D', label=getText("chkPLOT_AXES3D"), F),
                                    checkboxInput("chkPLOT_REMEMBER_ORIENTATION", getText("chkPLOT_REMEMBER_ORIENTATION"),TRUE),
                                    textInputRow('txtPLOT_FOV', label=HTML(getText("txtPLOT_FOV")), "20", width=15)
                                    ),
                                conditionalPanel(condition="!input.chkPLOT_WITH_RGL",
                                    HTML(getText("boxVisualization7a")),
                                    tags$br(),
                                    textInputRow('txtPLOT_ROTX', label=HTML('X-axis:'), "0", width="30"),
                                    textInputRow('txtPLOT_ROTY', label=HTML('Y-axis:'), "60", width="30"),
                                    textInputRow('txtPLOT_ROTZ', label=HTML('Z-axis:'), "0", width="30"),
                                    helpText(getText("boxVisualization7b")),
                                    actionLink("btnWarn4", placeWarn()),
                                    
#                                            radioButtons('radPlotNonRGLQuickLayout', 'Quick layout:',
#                                                c(Horizontal='PLOT_NONRGL_QUICK_LAYOUT_HORIZONTAL',
#                                                    Vertical='PLOT_NONRGL_QUICK_LAYOUT_VERTICAL',
#                                                    Custom='PLOT_NONRGL_QUICK_LAYOUT_NONE'),
#                                                    selected='PLOT_NONRGL_QUICK_LAYOUT_HORIZONTAL', inline=TRUE
#                                                ),
                                    helpText("Horizontal layout: x=0°, y=60°, z=0°, shift>0."),
                                    helpText("Vertical layout: x=60°, y=45°, z=-45°, shift=0")
                                    ),
                                tags$hr(),
                                HTML(getText("boxVisualization7c")),
                                textInputRow('txtLAYER_SHIFT_X', label=HTML('X-axis:'), "0.8", width="30"),
                                textInputRow('txtLAYER_SHIFT_Y', label=HTML('Y-axis:'), "0", width="30"),
                                textInputRow('txtLAYER_SCALE', label=HTML(getText("txtLAYER_SCALE")), "4"),
                                textInputRow('txtLAYER_SPACE', label=HTML(getText("txtLAYER_SPACE")), "3", width="30"),
                                tags$br(),
                                actionLink("btnWarn5", placeWarn())
                                )
                            )
                        )
                    ),
                tabPanel("Multiplex",
                    conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                        fluidRow(
                            column(width = 4,
                                myBox("boxVisualization8", "Inter-link Options","basic",
                                    checkboxInput("chkINTERLINK_SHOW",HTML(getText("chkINTERLINK_SHOW")),TRUE),
                                    HTML(paste("<font color=\"red\">",icon("exclamation-circle"), getText("boxVisualization8"), "</font>")),
                                    colourpicker::colourInput("colINTERLINK_COLOR", getText("colINTERLINK_COLOR"), "#D8D8D8"),
                                    selectInput('selINTERLINK_TYPE', getText("selINTERLINK_TYPE"), choices=                                                c("dotted", "solid", "dashed", "dotdash", "longdash", "twodash")), 
                                    textInputRow('txtINTERLINK_WIDTH', label=getText("txtINTERLINK_WIDTH"), "0.4"),
                                    textInputRow('txtINTERLINK_TRANSP', label=getText("txtINTERLINK_TRANSP"), "0.2"),
                                    helpText(getText("boxVisualization8b"))
                                    )
                                )
                            )
                        ),
                        conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
                            helpText(getText("boxVisualization8c"))
                        )
                    ),
                tabPanel("Layers",
                    fluidRow(
                        column(width = 4,
                            conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                myBox("boxVisualization9", "Layer Options", "basic",
                                    checkboxInput("chkLAYER_SHOW","Show layers",TRUE),
                                    textInputRow('txtLAYER_LABEL_PREFIX', label=getText("txtLAYER_LABEL_PREFIX"), "L"),
                                    helpText(getText("helptxtLAYER_LABEL_PREFIX")),
                                    colourpicker::colourInput("colLAYER_COLOR", getText("colLAYER_COLOR"), "grey"),
                                    helpText(getText("helpcolLAYER_COLOR")),
                                    textInputRow('txtLAYER_TRANSP', label=getText("txtLAYER_TRANSP"), "0.08"),
                                    helpText(getText("txtLAYER_TRANSPb")),
                                    helpText(getText("txtLAYER_TRANSPc")),
                                    HTML(paste0("<strong>",getText("boxVisualization9b"),"</strong>")),
                                    checkboxInput("chkLAYER_ID_SHOW_TOPLEFT","top-left",FALSE),
                                    checkboxInput("chkLAYER_ID_SHOW_TOPRIGHT","top-right",FALSE),
                                    checkboxInput("chkLAYER_ID_SHOW_BOTTOMLEFT","bottom-left",TRUE),
                                    checkboxInput("chkLAYER_ID_SHOW_BOTTOMRIGHT","bottom-right",FALSE),
                                    textInputRow('txtLAYER_ID_FONTSIZE', label=getText("txtLAYER_ID_FONTSIZE"), "1.5")
                                    )
                                )
                            ),
                        column(width = 4,
                            conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                myBox("boxVisualization10", "Aggregate Options", "basic",
                                    checkboxInput("chkAGGREGATE_SHOW",HTML(getText("chkAGGREGATE_SHOW")),FALSE),
                                    actionLink("btnWarn6", placeWarn()),
                                    tags$hr(),
                                    textInputRow('txtLAYER_AGGREGATE_LABEL_PREFIX', label=getText("txtLAYER_AGGREGATE_LABEL_PREFIX"), "Aggregate"),
                                    colourpicker::colourInput("colLAYER_AGGREGATE_COLOR", getText("colLAYER_AGGREGATE_COLOR"), "blue"),
                                    textInputRow('txtLAYER_AGGREGATE_TRANSP', label=getText("txtLAYER_AGGREGATE_TRANSP"), "0.08"),
                                    helpText(getText("txtLAYER_AGGREGATE_TRANSPb"))
                                    )
                                ),
                            myBox("boxVisualization11", "Inactive Layers","basic",
                                textInput("txtLAYERS_ACTIVE", label=getText("txtLAYERS_ACTIVE")),
                                helpText(getText("txtLAYERS_ACTIVEb"))
                                )
                            ),
#                        ),
#                    fluidRow(
                        column(width = 4,
                            conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                myBox("boxVisualization12", "Geographical Options", "basic",
                                    helpText(getText("boxVisualization12b")),
                                    checkboxInput("chkGEOGRAPHIC_BOUNDARIES_SHOW", HTML(paste0("<strong>Layer</strong> ",getText("chkGEOGRAPHIC_BOUNDARIES_SHOW"))),TRUE),
checkboxInput("chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW", HTML(paste0("<strong>Aggregate</strong> ",getText("chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW"))),TRUE),
                                    selectInput("selOSMType", HTML(getText("selOSMType")), 
                                        choices = c("bing","mapbox","mapquest-aerial","osm","osm-bbike-german","osm-transport","stamen-toner","stamen-watercolor")),
                                    actionLink("btnGeographicMap", placeInfo() ),
                                    tags$hr(),
                                    helpText(paste0(HTML("<h5>",getText("boxVisualization12d"),"</h5>"))),
                                    textInputRow('txtGEOGRAPHIC_LAT_MIN', label='Min lat:', ""),
                                    textInputRow('txtGEOGRAPHIC_LAT_MAX', label='Max lat:', ""),
                                    textInputRow('txtGEOGRAPHIC_LONG_MIN', label='Min lon:', ""),
                                    textInputRow('txtGEOGRAPHIC_LONG_MAX', label='Max lon:', ""),
                                    helpText(getText("boxVisualization12c")),
                                    actionLink("btnWarn7", placeWarn())
                                    )
                                )
                            )
                        )
                    ),
                tabPanel("Nodes",
                    fluidRow(
                        column(width = 3,
                            myBox("boxVisualization13", "Node Size", "basic",              
                                conditionalPanel(condition="input.btnCalculateCentralityDiagnostics==0",
                                    radioButtons('radNodeSizeType', getText("radNodeSizeType"),
                                        c(Uniform='NODE_SIZE_PROPORTIONAL_TO_UNIFORM',
                                            External='NODE_SIZE_PROPORTIONAL_TO_EXTERNAL'
                                            ),
                                            selected='NODE_SIZE_PROPORTIONAL_TO_UNIFORM'
                                        )
                                    ),
                                conditionalPanel(condition="input.btnCalculateCentralityDiagnostics>0",
                                    uiOutput("selVizNodeSizeOutputID")
                                    ),
                                radioButtons('radNodeSizeType2', getText("radNodeSizeType2"),
                                    c(Constant='NODE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                        Log='NODE_SIZE_PROPORTIONAL_TYPE_LOG',
                                        LogLog='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                        selected='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG', inline=F
                                    ),
                                textInputRow('txtNODE_DEFAULT_SIZE', label=getText("txtNODE_DEFAULT_SIZE"), "10")
                                )
                            ),
                        column(width = 3,
                            myBox("boxVisualization14", "Node Color", "basic",
                                radioButtons('radNodeColor', getText("radNodeColor"),
                                    c(Random='NODE_COLOR_RANDOM',
                                        Community='NODE_COLOR_COMMUNITY',
                                        Component='NODE_COLOR_COMPONENT',
                                        Centrality='NODE_COLOR_CENTRALITY',
                                        TopRank='NODE_COLOR_TOPRANK',
                                        Uniform='NODE_COLOR_UNIFORM',
                                        Query='NODE_COLOR_QUERY',
                                        External='NODE_COLOR_EXTERNAL'),
                                        selected='NODE_COLOR_UNIFORM'
                                    ),
                                hr(),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_EXTERNAL'",
                                    colourpicker::colourInput("colNodeColorFileDefaultNodesColor", getText("colNodeColorFileDefaultNodesColor"), "#959595")
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_CENTRALITY' && input.btnCalculateCentralityDiagnostics>0",
                                    uiOutput("selVizNodeColorOutputID"),
                                    radioButtons('radNodeColorType2', getText("radNodeColorType2"),
                                        c(Constant='NODE_COLOR_PROPORTIONAL_TYPE_NORMAL',
                                            Log='NODE_COLOR_PROPORTIONAL_TYPE_LOG',
                                            LogLog='NODE_COLOR_PROPORTIONAL_TYPE_LOGLOG'),
                                            selected='NODE_COLOR_PROPORTIONAL_TYPE_NORMAL'
                                        ),
                                    selectInput("selCentralityColorPalette", HTML(getText("ColorPalette")), 
                                        choices = as.vector(paletteChoiceArray)),
                                    actionLink("btnCentralityColorPaletteInfo", placeInfo() ),
                                    textInputRow("txtNODE_COLOR_CENTRALITY_BINS", label="Bins:", "30")
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_CENTRALITY' && input.btnCalculateCentralityDiagnostics==0",
                                    helpText(getText("helpText1"))
                                    ),                                            
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_RANDOM'",
                                    selectInput("selMultiplexColorPalette", HTML(getText("ColorPalette")), 
                                        choices = append(as.vector(paletteChoiceArray),"random")),
                                        actionLink("btnMultiplexColorPaletteInfo", placeInfo() )
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_UNIFORM'",
                                    colourpicker::colourInput("colNODE_COLOR_UNIFORM_COLOR", getText("colNODE_COLOR_UNIFORM_COLOR"), "#F2F2F2")
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_TOPRANK' && input.btnCalculateCentralityDiagnostics>0",
                                    uiOutput("selVizNodeColorTopOutputID"),
                                    textInput('txtNODE_COLOR_TOP', label=getText("txtNODE_COLOR_TOP"), "5"),
                                    colourpicker::colourInput("colNODE_COLOR_TOP_COLOR_TOP", getText("colNODE_COLOR_TOP_COLOR_TOP"), "#FF0000"),
                                    colourpicker::colourInput('colNODE_COLOR_TOP_COLOR_OTHERS', getText("colNODE_COLOR_TOP_COLOR_OTHERS"), "#F2F2F2"),         
                                    checkboxInput("chkNODE_LABELS_SHOW_ONLY_TOP", getText("chkNODE_LABELS_SHOW_ONLY_TOP"),TRUE),                   
                                    colourpicker::colourInput('colNODE_COLOR_TOP_LABELS_FONT_COLOR', getText("colNODE_COLOR_TOP_LABELS_FONT_COLOR"), "#000000")
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_TOPRANK' && input.btnCalculateCentralityDiagnostics==0",
                                    helpText(getText("helpText2"))
                                    ),                                            
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMMUNITY' && input.btnCalculateCommunityDiagnostics>0",    
                                    #textInput("txtCOMMUNITY_MIN_SIZE",label="Color-code with the same RGB all nodes in communities smaller than (useful for evidencing larger communities, not valid for the multiplex):","1"),
                                    uiOutput("selVizNodeColorCommunityTypeOutputID"),
                                    selectInput("selCommunityColorPalette", HTML(getText("ColorPalette")), 
                                        choices = append(as.vector(paletteChoiceArray),"random")),
                                    actionLink("btnCommunityColorPaletteInfo", placeInfo() )
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_QUERY' && input.btnQuery>0",
                                    colourpicker::colourInput('colQUERY_NODES_NODE_COLOR', getText("colQUERY_NODES_NODE_COLOR"), "#FF6246"),
                                    colourpicker::colourInput('colQUERY_NODES_NODE_NEIGH_COLOR', getText("colQUERY_NODES_NODE_NEIGH_COLOR"), "#669DC1"),
                                    colourpicker::colourInput('colQUERY_NODES_NODE_OTHER_COLOR', getText("colQUERY_NODES_NODE_OTHER_COLOR"), "#959595"),
                                    checkboxInput("chkNODE_LABELS_SHOW_ONLY_QUERY", getText("chkNODE_LABELS_SHOW_ONLY_QUERY"),TRUE),                   
                                    colourpicker::colourInput('colNODE_COLOR_QUERY_LABELS_FONT_COLOR', getText("colNODE_COLOR_QUERY_LABELS_FONT_COLOR"), "#000000")
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMMUNITY' && input.btnCalculateCommunityDiagnostics==0",
                                    helpText(getText("helpText3"))
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMPONENT' && input.btnCalculateComponentsDiagnostics>0",    
                                    uiOutput("selVizNodeColorComponentTypeOutputID"),
                                    selectInput("selComponentColorPalette", HTML(getText("ColorPalette")), 
                                        choices = append(as.vector(paletteChoiceArray),"random")),
                                    actionLink("btnComponentColorPaletteInfo", placeInfo() )
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMPONENT' && input.btnCalculateComponentsDiagnostics==0",
                                    helpText(getText("helpText4"))
                                    ),
                                conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_QUERY' && input.btnQuery==0",
                                    helpText(getText("helpText5"))
                                    ) 
                                )
                            ),
#                        ),
#                    fluidRow(
                        column(width = 4,
                            myBox("boxVisualization15", "Other Options", "basic", 
                                checkboxInput("chkNODE_ISOLATED_HIDE", getText("chkNODE_ISOLATED_HIDE"),TRUE),
                                conditionalPanel(condition="input.chkNODE_ISOLATED_HIDE && input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                    checkboxInput("chkNODE_ISOLATED_HIDE_INTERLINKS", getText("chkNODE_ISOLATED_HIDE_INTERLINKS"),TRUE)
                                    ),
                                textInputRow('txtNODE_TRANSP', label=getText("txtNODE_TRANSP"), "0.8"),
                                helpText(getText("helpText6")),
                                textInput("txtNODE_FRAME_COLOR", getText("txtNODE_FRAME_COLOR"),""),
                                helpText(getText("helpText7")),
                                checkboxInput("chkNODE_LABELS_SHOW", getText("chkNODE_LABELS_SHOW"),FALSE),
                                helpText(getText("helpText8")),
                                conditionalPanel("input.chkNODE_LABELS_SHOW",
                                    checkboxInput("chkNODE_LABELS_SHOW_WRAP", getText("chkNODE_LABELS_SHOW_WRAP"),FALSE),
                                    conditionalPanel(condition="input.chkNODE_LABELS_SHOW_WRAP",
                                        textInputRow('txtNODE_LABELS_WRAP', label=getText("txtNODE_LABELS_WRAP"), "10"),
                                        helpText(getText("helpText9")),
                                        textInputRow('txtNODE_LABELS_WRAP_OFFSET', label=getText("txtNODE_LABELS_WRAP_OFFSET"), "0"),
                                        helpText(getText("helpText10"))
                                        ),
                                    textInputRow('txtNODE_LABELS_DISTANCE', label=getText("txtNODE_LABELS_DISTANCE"), "1."),
                                    textInputRow('txtNODE_LABELS_FONT_SIZE', label=getText("txtNODE_LABELS_FONT_SIZE"), "0.5"),
                                    colourpicker::colourInput('colNODE_LABELS_FONT_COLOR', getText("colNODE_LABELS_FONT_COLOR"), "#2F2F2F")
                                    )
                                )
                            ),
                        column(width = 3,
                            conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_EXTERNAL' || input.selVizNodeSizeID=='External' || input.radNodeSizeType=='NODE_SIZE_PROPORTIONAL_TO_EXTERNAL'",
                                myBox("boxVisualization16", "External color/size", "basic",    
                                    helpText(getText("helpText11")),
                                    #checkboxInput('chkNodeColorFileHeader', 'Header', TRUE),
                                    textInput("txtNodeColorFileSep", label=getText("txtNodeColorFileSep"), " "),
                                    fileInput('nodecolor_file', HTML(paste0("<strong>* ",getText("nodecolor_file"),"</strong>")),
                                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                    HTML("<center>"),
                                    actionButton("btnImportNodeColor", getText("Import")),
                                    HTML("</center>")
                                    )
                                )
                            )
                        )
                    ),
                tabPanel("Edges",
                    fluidRow(
                        column(width = 3,
                            myBox("boxVisualization17", "Edge Size","basic",
                                radioButtons('radEdgeSizeType', getText("radEdgeSizeType"),
                                    c(Uniform='EDGE_SIZE_PROPORTIONAL_TO_UNIFORM',
                                       External='EDGE_SIZE_PROPORTIONAL_TO_EXTERNAL',
                                        Weight='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT'),
                                        selected='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT', inline=F
                                    ),
                                radioButtons('radEdgeSizeType2', getText("radEdgeSizeType2"),
                                    c(Constant='EDGE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                        Log='EDGE_SIZE_PROPORTIONAL_TYPE_LOG',
                                        LogLog='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                        selected='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG', inline=F
                                    ),
                                textInputRow('txtEDGE_DEFAULT_SIZE', label=getText("txtEDGE_DEFAULT_SIZE"), "2")

                                )
                            ),
                        column(width = 3,
                            myBox("boxVisualization18", "Edge Color", "basic",
                                radioButtons('radEdgeColor', getText("radEdgeColor"),
                                    c(Random='EDGE_COLOR_RANDOM',
                                        Uniform='EDGE_COLOR_UNIFORM',
                                        External='EDGE_COLOR_EXTERNAL'),
                                        selected='EDGE_COLOR_UNIFORM'
                                    ),
                                hr(),
                                conditionalPanel(condition="input.radEdgeColor=='EDGE_COLOR_EXTERNAL'",
                                    colourpicker::colourInput("colEdgeColorFileDefaultEdgesColor", getText("colEdgeColorFileDefaultEdgesColor"), "#F2F2F2")
                                    ),
                                conditionalPanel(condition="input.radEdgeColor=='EDGE_COLOR_RANDOM'",
                                    selectInput("selMultiplexEdgeColorPalette", HTML(getText("ColorPalette")), 
                                        choices = append(as.vector(paletteChoiceArray),"random")),
                                        actionLink("btnMultiplexEdgeColorPaletteInfo", placeInfo() )
                                    ),
                                conditionalPanel(condition="input.radEdgeColor=='EDGE_COLOR_UNIFORM'",                                    
                                    colourpicker::colourInput('colEDGE_COLOR', getText("colEDGE_COLOR"), "#F2F2F2")
                                    #helpText(getText("helpText12"))
                                    )
                                )
                            ),
#                        ),
#                    fluidRow(
                        column(width = 4,
                            myBox("boxVisualization19", "Other Options", "basic",
                                textInputRow('txtEDGE_BENDING', label=getText("txtEDGE_BENDING"), "0"),
                                helpText("0 means straight; max 1"),
                                textInputRow('txtEDGE_TRANSP', label=getText("txtEDGE_TRANSP"), "0.2"),
                                helpText(getText("helpText13")),
                                textInputRow('txtLAYER_ARROW_SIZE', label=getText("txtLAYER_ARROW_SIZE"), "0.2"),
                                textInputRow('txtLAYER_ARROW_WIDTH', label=getText("txtLAYER_ARROW_WIDTH"), "0.2")
                                )
                            ),
                        column(width = 3,
                            conditionalPanel(condition="input.radEdgeColor=='EDGE_COLOR_EXTERNAL' || input.radEdgeSizeType=='EDGE_SIZE_PROPORTIONAL_TO_EXTERNAL'",
                                myBox("boxVisualization21", "External color/size", "basic",    
                                    helpText(getText("helpText20")),
                                    #checkboxInput('chkEdgeColorFileHeader', 'Header', TRUE),
                                    textInput("txtEdgeColorFileSep", label=getText("txtEdgeColorFileSep"), " "),
                                    fileInput('edgecolor_file', HTML(paste0("<strong>* ",getText("edgecolor_file"),"</strong>")),
                                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                    HTML("<center>"),
                                    actionButton("btnImportEdgeColor", getText("Import")),
                                    HTML("</center>")
                                    )
                                )
                            )                               
                        )
                    ),
                tabPanel("Export",
                    br(),
                    fluidRow(
                        column(width = 4,
                            myBox("boxVisualization20", "Options", "basic",
                                conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                    radioButtons('radRGLExport', getText("radRGLExport"),
                                        c(PNG='RGL_EXPORT_PNG',
                                            PDF='RGL_EXPORT_PDF',
                                            SVG='RGL_EXPORT_SVG',
                                            webGL='RGL_EXPORT_WEBGL'),
                                            selected='RGL_EXPORT_PNG', inline=TRUE
                                        ),
                                    HTML("<center>"),
                                    conditionalPanel(condition="input.radRGLExport=='RGL_EXPORT_PNG'",
                                        actionButton("btnExportRendering","Export PNG")
                                        ),
                                    conditionalPanel(condition="input.radRGLExport=='RGL_EXPORT_PDF'",
                                        actionButton("btnExportRenderingPDF","Export PDF")
                                        ),
                                    conditionalPanel(condition="input.radRGLExport=='RGL_EXPORT_SVG'",
                                        actionButton("btnExportRenderingSVG","Export SVG")
                                        ),
                                    conditionalPanel(condition="input.radRGLExport=='RGL_EXPORT_WEBGL'",
                                        actionButton("btnExportRenderingWeb","Export webGL")
                                        ),
                                    HTML("</center>")
                                    ),
                                conditionalPanel(condition="!input.chkPLOT_WITH_RGL",
                                    radioButtons('radNORGLExport', getText("radNORGLExport"),
                                        c(PNG='NORGL_EXPORT_PNG',
                                            PDF='NORGL_EXPORT_PDF'),
                                            selected='NORGL_EXPORT_PNG', inline=TRUE
                                        ),
                                    conditionalPanel(condition="input.radNORGLExport=='NORGL_EXPORT_PNG'",
                                        textInputRow("txtExportRenderingClassicPNGWidth", "Width (px):", "1024"),
                                        textInputRow("txtExportRenderingClassicPNGHeight", "Height (px):", "768"),
                                        textInputRow("txtExportRenderingClassicPNGResolution", "Resolution (dpi):", "300"),
                                        tags$br(),
                                        HTML("<center>"),
                                        actionButton("btnExportRenderingClassicPNG","Export PNG"),
                                        HTML("</center>")
                                        ),
                                    conditionalPanel(condition="input.radNORGLExport=='NORGL_EXPORT_PDF'",
                                        textInputRow("txtExportRenderingClassicPDFWidth", "Width (px):", "1024"),
                                        textInputRow("txtExportRenderingClassicPDFHeight", "Height (px):", "768"),
                                        tags$br(),
                                        HTML("<center>"),
                                        actionButton("btnExportRenderingClassicPDF","Export PDF"),
                                        HTML("</center>")
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
            value=0
            ),
        tabPanel("Reducibility",
            actionLink("btnReducibilityHelp", placeHelp() ),
            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                helpText(getText("helpText14"))
            ),
            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                fluidRow(
                    column(width = 3,
                        myBox("boxReducibility", "Algorithms","basic",
                            #HTML('<h4>Algorithms to be used to calculate the correlation between layers and clustering</h4>'),
                            radioButtons('radReducibilityCorrelationMethod', getText("radReducibilityCorrelationMethod"),
c(Jensen_Shannon_Divergence='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'),
                                selected='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'),
                            tags$hr(),
                            selectInput("selReducibilityClusterMethod", getText("selReducibilityClusterMethod"), 
                                choices = c("ward.D2","single","complete","average","mcquitty","median","centroid"))
                            )
                        ),
                    column(width = 3,
                        myBox("boxReducibility2", "Graphical Options", "basic",
                            selectInput("selReducibilityColorPalette", HTML(getText("ColorPalette")), 
                                choices = paletteChoiceArray),
                            actionLink("btnReducibilityPaletteInfo", placeInfo() ),
                            textInputRow("txtREDUCIBILITY_HEATMAP_FONT_SIZE",label=getText("txtREDUCIBILITY_HEATMAP_FONT_SIZE"),"1.5")
                            ),
                        HTML("<center>"),
                        actionButton("btnCalculateReducibility", getText("btnCalculateReducibility")),
                        HTML("</center>")
                        #actionButton("btnExportReducibilityRendering","Export PNG"),
                        )
                    ),
                    tags$hr(),
                    
                    HTML('<h4>Structural reducibility</h4>'),
                    conditionalPanel(condition="input.btnCalculateReducibility>0",
                        uiOutput("reducibilityHeatmapUI"),
                        #imageOutput("jsdMatrixSummaryImage",width = "100%", height = "700px"),
                        #imageOutput("reducibilityDendrogramSummaryImage",width = "100%", height = "700px"),
                        tags$br(),
                        HTML('<h5>Reduction Quality</h5>'),
                        showOutput("reducibilityQualityFunction","nvd3"),
                        tags$hr()
                    )
                )
            ),
        tabPanel("Dynamics",
            actionLink("btnDynamicsHelp", placeHelp() ),
            fluidRow(
                column(width = 3,
                    myBox("boxDynamics", "Timeline file","basic",
                        #helpText(HTML("<h4>Open Timeline File</h4>")),
                        helpText(HTML(paste0("<strong><font color='#262626'>* ", getText("helpText16"),"</font></strong>"))),
                        #checkboxInput('chkTimelineFileHeader', 'Header', TRUE),
                        textInput("txtTimelineFileSep", label=getText("txtTimelineFileSep"), " "),
                        fileInput('timeline_file', HTML(paste0("<strong>* ", getText("timeline_file"),"</strong>")),
                                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                            ),
                        HTML("<center>"),
                        actionButton("btnImportTimeline", getText("btnImportTimeline")),
                        HTML("</center>")
                        ),
                    HTML("<center>"),
                    checkboxInput("chkTIMELINE_RENDER_TO_FILE", getText("chkTIMELINE_RENDER_TO_FILE"),FALSE),
                    actionButton("btnRenderDynamicsSnapshots", getText("btnRenderDynamicsSnapshots")),
                    hr(),
                    actionButton("btnFFMPEGDynamicsSnapshots", getText("btnFFMPEGDynamicsSnapshots")),
                    HTML("</center>")
                    ),
                column(width = 4,
                    myBox("boxDynamics2", "Graphical Options", "basic",
                        textInput("txtTimelineDefaultNodesSize", label=getText("txtTimelineDefaultNodesSize"), "10"),
                        helpText(getText("helpText17")),
                        colourpicker::colourInput("colTimelineDefaultNodesColor", getText("colTimelineDefaultNodesColor"), "#959595"),
                        textInput("txtTimelineDefaultEdgesSize", label=getText("txtTimelineDefaultEdgesSize"), "1"),
                        helpText(getText("helpText18")),
                        colourpicker::colourInput("colTimelineDefaultEdgesColor", getText("colTimelineDefaultEdgesColor"), "#959595"),
                        helpText(getText("helpText18"))
                        #tags$hr(),
                        #textInput("txtFFMPEG_PATH",label="Full path of your ffmpeg binary to make the video (if not valid, video will not be made):",""),
                        #textInput("txtFFMPEG_FLAGS",label="Parameters to be passed to ffmpeg (if not valid, video will not be made):",""),
                        )
                    )
                ),
        helpText(HTML("<h4>Summary for the timeline</h4>")),
        conditionalPanel(condition="input.btnImportTimeline>0 && input.btnRenderNetworks>0",
            htmlWidgetOutput(
                        outputId = 'projectTimelineHTML',
                        HTML(paste(
                        'Time steps: <span id="timelineTimesteps"></span><br>',
                        'Affecting the dynamics of nodes: <span id="timelineAffectNodes"></span><br>',
                        'Affecting the dynamics of edges: <span id="timelineAffectEdges"></span><br>',
                        '<br>'
                        ))
                    )
                )
            ),
        tabPanel("Graphics",
            mainPanel(
                tabsetPanel(
                    tabPanel("Points",
                        HTML("<img width='800' src='img/pointtypes.png' alt=''/>")
                        ),
                    tabPanel("Lines",
                        HTML("<img width='800' src='img/linetypes.png' alt=''/>")
                        ),
                    tabPanel("Colors",
                        HTML("<img width='800' src='img/colorpalettes.png' alt=''/>")
                        ),
                    tabPanel("Maps",
                        HTML("<img width='800' src='img/backmaps.png' alt=''/>")
                        )
                    )
                )
            ),
        tabPanel("Data",
            HTML("<center>"),
            HTML("<i class='fa fa-flask fa-4x'></i><h1>Available data</h1><br>"),
            HTML("</center>"),
            fluidRow(
                column(width = 5,
                    showOutput("dataPieChart","nvd3")
                    ),
                column(width = 6,
                    showOutput("dataScatterPlot","nvd3")
                    )
                ),
            HTML("<center>"),
            HTML("<a href='https://comunelab.fbk.eu/data.php' target='_blank'><i class='fa fa-download fa-4x'></i><h1>Go to the download page</h1></a><br>"),
            HTML("</center>"),
            #tableOutput("dataTable")
            dataTableOutput('dataTable')
            ),
        navbarMenu("Help",
            tabPanel("Help",
                mainPanel(
                    htmlWidgetOutput(
                        outputId = 'tabHelp',
                            HTML("<img src='img/logo_small.jpg' alt=''/>"),
                            HTML(getText("GlobalHelp"))
                        ),
                    value=4
                    )
                ),
            tabPanel("Citation",
                mainPanel(
                    htmlWidgetOutput(
                        outputId = 'tabCitation',
                        HTML("<img src='img/logo_small.jpg' alt=''/>"),
                        HTML("<h3>Citation</h3>"),
                            HTML("If you use <span>muxViz</span> for your analysis and visualization of multilayer networks, you should cite the following paper:<br>"),
                            br(),
                            HTML('<ul><li>M. De Domenico, M. Porter and A. Arenas, <i>MuxViz: a tool for multilayer analysis and visualization of networks</i>, Journal of Complex Networks 3, 159 (2015) [<a href="http://comnet.oxfordjournals.org/content/3/2/159" target="_blank">Open</a>]</li></ul>'),
                            HTML("<span>muxViz</span> is based on several algorithms. Please, cite the papers corresponding to the algorithms used in your research.")
                        )
                    )
                ),
            tabPanel("About",
                mainPanel(
                    htmlWidgetOutput(
                        outputId = 'tabCredits',
                        HTML("<img src='img/logo_small.jpg' alt=''/>"),
                        HTML("<h3>muxViz Project</h3>"),
                        
                        HTML("Software released under <a href='http://www.gnu.org/licenses/gpl-3.0.html'  target='_blank'>GPLv3</a><br>"),
                        HTML("Developed by <a href='http://deim.urv.cat/~manlio.dedomenico/index.php' target='_blank'>Manlio De Domenico</a><br><a href='http://deim.urv.cat' target='_blank'>School of Computer Science and Mathematics</a>, <a href='http://www.urv.cat' target='_blank'>Universitat Rovira i Virgili</a><br><br>"),
                        HTML("Visit the <a href='http://deim.urv.cat/~manlio.dedomenico/muxviz.php' target='_blank'>project page</a> or the <a href='https://github.com/manlius/muxViz' target='_blank'>Github repo</a>"),
                        HTML(paste(
                        '<h3>Credits</h3>',
                        'This work has been partially supported by <a href="http://www.plexmath.eu/" target="_blank">European Commission FET-Proactive project PLEXMATH (Grant No. 317614)</a>, the European project devoted to the investigation of multi-level complex systems and has been developed at the <a href="http://deim.urv.cat/~alephsys/" target="_blank">Alephsys Lab</a>.<br><br>',
                        'I am in debt with <a href="http://deim.urv.cat/~aarenas/" target="_blank">A. Arenas</a> for proposing this project, with <a href="http://people.maths.ox.ac.uk/porterm/" target="_blank">Mason A. Porter</a> and with the muxViz community for invaluable suggestions and feedbacks.',
                        '<br><br>',
                        'I would like to thank Inderjit S. Jutla, Lucas G. S. Jeub, and Peter J. Mucha for making their code about multislice community detection available.',
                        '<br><br>',
                        'Finally, I would like to acknowledge the precious help of S. Agnello in designing and testing this Graphical User Interface to muxViz.',
                        tags$hr(),
                        '<br>'
                        ))
                    ),
                value=5
                )
            ),
            tabPanel("License",
                mainPanel(
                    htmlWidgetOutput(
                        outputId = 'tabLicense',
                        HTML("<img src='img/logo_small.jpg' alt=''/>"),
                        HTML(paste(
                        '<h3>License</h3>',
                        '<span>muxViz</span> makes use of many packages and its license is compatible with the license of each used package. <span>muxViz</span> is Open Source and makes use of free software only: <a href="http://www.r-project.org/" target="_blank">R</a> (GNU GPLv2) and <a href="" target="_blank">muxNet</a> (Next release, GNU GPLv3 License, <a href="muxnet_license.txt" target="_blank">see the original license</a>).<br><br>',
                        'This code has no warranty whatsoever and any kind of support is provided. You are free to do what you like with this code as long as you leave this copyright in place. Please, explicitly cite <span>muxViz</span> if you find it useful for your visualizations and analyses.',
                        '<br><br>',
                        '(C) Copyright 2013-2017, Manlio De Domenico (manlio.dedomenico at urv.cat)',
                        '<br><br>',
                        '<span>muxViz</span> is free software: you can redistribute it and/or modify it under the terms of the <a href="http://www.gnu.org/licenses/gpl-3.0.html" target="_blank">GNU General Public License</a> as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.',
                        '<br><br>',
                        '<span>muxViz</span> is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.',
                        '<br><br>',
                        'You should have received a copy of the GNU General Public License along with the package. If not, see <a href="http://www.gnu.org/licenses/" target="_blank">www.gnu.org/licenses/</a>.',
                        tags$hr(),
                        '<br>'
                        ))
                    ),
                value=6
                )
            ),
            tabPanel("Donate",
                mainPanel(
                    htmlWidgetOutput(
                        outputId = 'tabDonate',
                        HTML("<img src='img/logo_small.jpg' alt=''/>"),
                        HTML(paste(
                        '<h3>Donation</h3>',
                        '<span>muxViz</span> is a free and open-source platform that has been used for scientific purposes in a variety of disciplines, including computational social science, computational neuroscience, computational biology, computational psycholinguistics, multi-modal transportation engineering and physics. <br><br>Since January 2016, <span>muxViz</span> is periodically updated and maintained for free by its developer and its enthusiastic community of users (the muxVizers).<br><br>',
                        'If <span>muxViz</span> helps you with your research and reduces your time to develop, you can give us a cup of good coffee :)',
                        tags$br(),
                        tags$br(),
                        '<strong>BTC</strong>: 19RGUCZjSzvbjY5xEWz3GNg7MnMiPyEgbB',
                        tags$br(),
                        tags$br(),
                        '<strong>ETH</strong>: 0xe5027b44782a4b832a23773ffdb57658aff62e31',
                        tags$br(),
                        tags$br(),                        
                        '<strong>XMR</strong>: 49pQ1B2m2UHaKotqF8h1E89um9rnMPvAHEfGUET1icQUaxBV1KFhuTNVpsF6Kf4bf5dZwWvmdzYn4D7rKcL4v2ee3tH4E2L',
                        tags$br(),
                        tags$br(),
                        '<strong>Paypal</strong>:<br>',
                        tags$br(),
                        '<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=manlio.dedomenico@gmail.com&item_name=muxViz&item_number=muxViz+development&currency_code=USD" target="_blank"><img src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border=0></a>',
                        tags$hr(),
                        tags$br()
                        ))
                    ),
                value=7
                )
            )
        )
    )
)
})

