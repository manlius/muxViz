library(shiny)
library(ShinyDash)
library(markdown)
library(shinydashboard)
library(rCharts)
#library(shinyIncubator)
source("version.R")

#RGB colors table
#http://www.javascripter.net/faq/rgbtohex.htm
paletteWeb <- "<a href='http://www.colorschemer.com/online.html' target='_blank'>Online Color Palette</a>, <a href='http://colorbrewer2.org'>Online Color Scheme</a>"

#http://getbootstrap.com/components/#glyphicons
#http://fontawesome.io/icons/
#http://rstudio.github.io/shinydashboard/appearance.html#icons

#interactive heatmaps
#http://www.statsblogs.com/2013/12/15/visualization-of-2012-crime-rates-of-different-states-in-the-us-using-rcharts/

paletteChoiceArray <- sort(row.names(brewer.pal.info[1]))

octaveCheck <- function(){
    res <- system("octave -v", ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res==0){
        return("<i class='fa fa-check'></i> Octave found.<br>")
    }else{
        return("<i class='fa fa-warning'></i> Octave not found. Multilayer descriptors will not be calcualted.<br>")
    }
}

buildPath <- function(folder,objname){
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste(getwd(),folder,objname,sep="\\") )
    }else{
        return( paste(getwd(),folder,objname,sep="/") )        
    }
}

concatenatePath <- function(folder,objname){
    if( Sys.info()["sysname"]=="Windows" ){
        return( paste(folder,objname,sep="\\") )
    }else{
        return( paste(folder,objname,sep="/") )        
    }
}

getExecutablePath <- function(exec_name){
    path <- ""
    if( Sys.info()["sysname"]=="Windows" ){
        path <- buildPath("bin",paste0(exec_name,"_windows"))    
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
        return("<i class='fa fa-check'></i> Fanmod found.<br>")
    }else{
        return("<i class='fa fa-warning'></i> Fanmod not found. Analysis of motifs will not be available.<br>")
    }
}

multimapCheck <- function(){
    res <- system(getExecutablePath("multimap"), ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res==0){
        return("<i class='fa fa-check'></i> Multimap found.<br>")
    }else{
        return("<i class='fa fa-warning'></i> Multimap not found. Only multislice community detection will be available.<br>")
    }
}

muxbenchCheck <- function(){
    res1 <- system(getExecutablePath("muxbench1"), ignore.stdout = F, ignore.stderr = F, intern=F)
    res2 <- system(getExecutablePath("muxbench2"), ignore.stdout = F, ignore.stderr = F, intern=F)
    res3 <- system(getExecutablePath("muxbench3"), ignore.stdout = F, ignore.stderr = F, intern=F)
    if(res1==0 && res2==0 && res3==0){
        return("<i class='fa fa-check'></i> muxBenchmark(s) found.<br>")
    }else{
        return("<i class='fa fa-warning'></i> muxBenchmark(s) not found.<br>")
    }
}


myBox <- function(Title, Type="basic", ...){
    if(Type=="basic"){
        return(
            div(style="background-color: #FFFFFF; border-width: 1px; border-style: solid; border-color: #3286AD; margin: 10px 5px 10px 5px; -moz-border-radius: 15px; border-radius: 15px;",
                div(style="background-color: #3286AD; color: #FFFFFF; -moz-border-top-right-radius: 15px; -moz-border-top-left-radius: 15px; border-top-left-radius: 15px; border-top-right-radius: 15px; text-align: center; font-family: 'Arial';", HTML(paste0("<font size='+1'><strong>",Title,"</strong></font>"))),
                div(style="padding: 5px 5px 5px 5px;",
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

textInputRow <- function (inputId, label, value = "") {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

checkboxInputRow <- function (inputId, label, value = "") {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "checkbox", value = value, class="input-small"))
}

shinyUI(bootstrapPage(
#        sidebarPanel(
#        )
    tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
    # Load D3.js
    tags$head(tags$script(src = 'http://d3js.org/d3.v3.min.js')),
    tags$head(tags$title("muxViz")),
    #headerPanel("muxViz Graphical User Interface"),

    navbarPage("",
        tabPanel(icon("home"),
            sidebarLayout(position = "right",
                sidebarPanel( 
                    helpText(HTML(paste("muxViz Version:",muxVizVersion))),
                    helpText(HTML(paste("Last update:",muxVizUpdate))),
                    hr(),
                    helpText(HTML(paste("System:",Sys.info()["sysname"]))),
                    helpText(HTML(paste(Sys.info()["version"]))),
                    hr(),
                    helpText(HTML(paste(version["version.string"][[1]]))),
                    hr(),
                    HTML(octaveCheck()),
                    #HTML(multimapCheck()),
                    HTML(fanmodCheck())
                    #HTML(muxbenchCheck())
                ),
                mainPanel(
                    HTML("<img src='img/home.png' height='550' style='{margin-left: auto; margin-right: auto;}'>")                
                )
            )
        ),
    navbarMenu("File",
        tabPanel("Import",
            sidebarLayout(position="right",
                sidebarPanel(
                    HTML("<h3>Quick help</h3>"),
                    htmlWidgetOutput(
                        outputId = 'projectImport',
                        HTML(paste(
                        '<ul>',
                        '<li>Give a name to your analysis (or use the default name)</li>',
                        '<li>In the "Config file" tab, select and open the configuration file</li>',
                        '<li>Use the "Import networks" tab, to import the networks found in your configuration file</li>',
                        '</ul>',
                        '<h4>Network models</h4>',
                        '<span>Edge-colored:</span> Intra-layer edges are colored; any pair of layers has at least one node in common; inter-layer links are missing.',
                        '<br><br>',
                        '<span>Interconnected:</span> Intra-layer edges are colored; any pair of layers has at least one node in common; inter-layer links connect the replicas of each node across layers.',
                        '<br><br>',
                        '<span>Interdependent:</span> Nodes are colored; any pair of layers has no nodes in common; inter-layer links connect nodes of different type across layers.',
                        '<br><br>',
                        '<span>General:</span> Nodes are colored; some pair of layers can have no nodes in common; inter-layer links can connect node replicas or different nodes across layers.',
                        '<br><br>',
                        'Note that input format is different for each model (to exploit available information and reduce memory). See the help for details about each format.'
                            ))
                        )
                    ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("ID",
                            fluidRow(
                                column(width = 5,
                                    myBox("Project ID", "basic",
                                        textInput("txtProjectName", label=HTML("<strong>* Assign an ID to this analysis:</strong>"), paste("muxViz_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),sep=""))
                                        )
                                    )
                                )
                            ),
                        tabPanel("Config file", 
                            myBox("Network model", "basic",
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
                                ),
                            fluidRow(
                                column(width = 5,
                                    myBox("Configuration", "basic",
                                        #helpText(HTML("<h4>Open Configuration File</h4>")),
                                        helpText(HTML("<strong><font color='#262626'>* Input format for the config. file:</font></strong>")),
                                        checkboxInput('chkConfigFileHeader', 'Header', FALSE),
                                        textInput("txtConfigFileSep", label=HTML("Separator:"), ";"),
                                        fileInput('project_file', HTML('<strong>* Open the configuration file:</strong>'),
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                        )
                                    )
                                ),
                            tags$hr(),
                            #myBox("Configuration file table", "info",
                            helpText(HTML("<h4>Configuration file table</h4>")),
                            tableOutput("layersTable")
                            #    )
                            ),
                        tabPanel("Import networks",       
                            fluidRow(
                                column(width = 5,
                                    myBox("Input Format", "basic",
                                        checkboxInput('chkEdgeListFileHeader', 'Header', FALSE),
                                        textInput("txtEdgeListFileSep", label=HTML("Separator (default is one space; the same separator is expected for layout file):"), " "),
                                        helpText(HTML("<font color='red'>If nodes in edges lists are identified by their label instead of a sequential integer ID, check the box below</font> (note that you must specify sequential integer IDs in the nodeID column of the layout file)")),
                                        checkboxInput("chkEdgeListLabel", "Edges list of labeled nodes (instead of sequential integer)", FALSE)
                                        )
                                    ),
                                    column(width = 5,
                                    myBox("Network Format", "basic",
                                        selectInput("selEdgeListType", HTML("<strong>* EdgeList Format:</strong>"), 
                                            choices = c("Undirected", "Directed")),
                                        checkboxInput("chkEdgeListWeighted", "Weighted", FALSE),
                                        checkboxInput("chkEdgeListUndirectedBoth", "Both directions are specified (for undirected networks only)", FALSE),
                                        checkboxInput("chkRESCALE_WEIGHT","Rescale weights by the minimum",FALSE),
                                        checkboxInput('chkOutputEdgelistTable',HTML("Print edges lists in a table (<font color='red'>slow for large networks</font>)"),FALSE)
                                        ),
                                        HTML("<center>"),
                                        actionButton("btnImportNetworks", "Import Network"),
                                        HTML("</center>")
                                    )
                                ),
                            tags$hr(),
                            conditionalPanel(condition="input.btnImportNetworks>0",
                                myBox("Summary for the multilayer network","info",
                                #helpText(HTML("<h4>Summary for the multilayer network</h4>")),
                                    htmlWidgetOutput(
                                        outputId = 'projectSummaryHTML',
                                        HTML(paste(
                                        'Layers: <span id="sumLayers"></span><br>',
                                        'Type of layer: <span id="sumLayerType"></span><br><br>',
                                        'Number of nodes: <span id="sumNodes"></span><br>',
                                        '&nbsp;&nbsp;&nbsp;&nbsp;Minimum ID: <span id="sumMinNodeID"></span><br>',
                                        '&nbsp;&nbsp;&nbsp;&nbsp;Maximum ID: <span id="sumMaxNodeID"></span><br>',
                                        'Number of edges: <span id="sumEdges"></span><br>',
                                        '<br>',
                                        'External layout: <span id="sumIsLayoutExternal"></span><br>',
                                        'Is geographical: <span id="sumIsLayoutGeographic"></span><br>'
                                        ))
                                        )
                                    )
                                )
                            ),
                        tabPanel("Edges Tables",
                            helpText(HTML("<h4>Imported Edges Lists</h4>")),
                            conditionalPanel(condition="input.btnImportNetworks>0",
                                checkboxInput(inputId = "edgelistTablePageable", label = "Pageable", TRUE),
                                conditionalPanel("input.edgelistTablePageable==true",
                                    numericInput(inputId = "edgelistTablePageSize",label = "Edges per page",100)
                                    ),  
                                    htmlOutput("edgelistTable")
                                )
                            )
                        ),
                        tags$hr(),
                        value=1     
                    )
                )
            )
#        tabPanel("Open",
#            sidebarLayout(position="right",
#                sidebarPanel(
#                    HTML("<h3>Quick help</h3>"),
#                    htmlWidgetOutput(
#                        outputId = 'projectOpen',
#                        HTML(paste(
#                        '<h5>TODO</h5>'
#                            ))
#                        )
#                ),
#                mainPanel(
#                    HTML("<center>"),
#                    actionButton("btnOpenSession" ,"Open muxViz session from file"),
#                    HTML("</center>")
#                    )
#                )
#            ),
#        tabPanel("Save",
#            sidebarLayout(position="right",
#                sidebarPanel(
#                    HTML("<h3>Quick help</h3>"),
#                    htmlWidgetOutput(
#                        outputId = 'projectSave',
#                        HTML(paste(
#                        '<h5>TODO</h5>'
#                            ))
#                        )
#                    ),
#                mainPanel(
#                    HTML("<center>"),
#                    actionButton("btnSaveSession" ,"Save current muxViz session"),
#                    HTML("</center>")
#                    )
#                )
#            )
        ),
        tabPanel("Diagnostics",
            sidebarLayout(position="right",
                sidebarPanel(
                    conditionalPanel(condition="input.tabsetDiagnostics=='Mux Set up'",
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectGlobalDiagnosticsMuxSetUp',
                            HTML(paste(
                            'Use this tab to set up your multilayer network. Specify the type (ordinal, like in the case of temporal networks, or categorical, like in the case of fully interconnected layers) and the strength of inter-layer links (it will be the same for all inter-links).'
                                ))
                            )
                        ),
                    conditionalPanel(condition="input.tabsetDiagnostics=='Annular Viz'",
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectGlobalDiagnosticsAnnularViz',
                            HTML(paste(
                            '<h5>Annular Plot</h5>',
                            HTML("BE CAREFUL: this module is supposed to work *AFTER* you calculated diagnostics in the multilayer, using the tensorial formulation. If you try to use this module without calculating multilayer centralities, an error will be generated.</font><br><br>")
                                ))
                            )
                        ),                        
                    conditionalPanel(condition="input.tabsetDiagnostics=='Centrality'",
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectGlobalDiagnosticsCentrality',
                                HTML(paste(
                                '<h5>Node Versatility</h5>',
                                'This module calculates <a href="http://en.wikipedia.org/wiki/Centrality" target="_blank">centrality</a> of nodes in the network.',
                                '<br><br>',
                                'The diagnostics for single-layer networks are widely described in the literature: see <a href="http://en.wikipedia.org/wiki/http://en.wikipedia.org/wiki/Degree_(graph_theory)" target="_blank">Degree centrality</a>, <a href="http://en.wikipedia.org/wiki/PageRank" target="_blank">PageRank centrality</a>, <a href="http://en.wikipedia.org/wiki/Centrality#Eigenvector_centrality" target="_blank">Eigenvector centrality</a>, <a href="http://en.wikipedia.org/wiki/HITS_algorithm" target="_blank">Hub centrality</a>, <a href="http://en.wikipedia.org/wiki/HITS_algorithm" target="_blank">Authority centrality</a>, <a href="http://en.wikipedia.org/wiki/Katz_centrality" target="_blank">Katz centrality</a> and <a href="https://en.wikipedia.org/wiki/Degeneracy_(graph_theory)" target="_blank">K-coreness</a> for further information. Multiplexity is defined by the fraction of layers where node exists (note that it is not defined for the aggregate network).',
                                '<br><br>',
                                '<hr>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> M. De Domenico et al, <i>Mathematical Formulation of Multilayer Networks</i>, Phys. Rev. X 3, 041022 (2013) [<a href="http://prx.aps.org/abstract/PRX/v3/i4/e041022" target="_blank">Open</a>]',
                                '<li> M. De Domenico et al, <i>Ranking in interconnected multilayer networks reveals versatile nodes</i>, Nature Communications 6, 6868 (2015) [<a href="http://www.nature.com/ncomms/2015/150423/ncomms7868/full/ncomms7868.html" target="_blank">View</a>]',
                                '<li> N. Azimi-Tafreshi, J. Gomez-Gardenes, and S. N. Dorogovtsev, <i>k−core percolation on multiplex networks</i>, Phys. Rev. E 90, 032816 (2014) [<a href="http://journals.aps.org/pre/abstract/10.1103/PhysRevE.90.032816" target="_blank">View</a>]',
                                '</ul>',
                                '<br>'
                                ))
                            )
                        ),
                    conditionalPanel(condition="input.tabsetDiagnostics=='Network of layers'",
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectGlobalDiagnosticsNetworkLayers',
                                HTML(paste(
                                '<h5>Network of layers</h5>',
                                'This module calculates the network of layers induced by a multilayer network. Visualizing the network of layers allows to understand how each layer depends on other layers. In the case of edge-colored multigraphs it is not calculated because the result would be a network of isolated nodes.',
                                '<br><br>',
                                '<hr>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> M. De Domenico et al, <i>Mathematical Formulation of Multilayer Networks</i>, Phys. Rev. X 3, 041022 (2013) [<a href="http://prx.aps.org/abstract/PRX/v3/i4/e041022" target="_blank">Open</a>]',
                                '</ul>',
                                '<br>'
                                ))
                            )
                        ),       
                    conditionalPanel(condition="input.tabsetDiagnostics=='Correlation'",
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectGlobalDiagnosticsCorrelation',
                            HTML(paste(
                            '<h5>Mean global overlapping</h5>',
                            'Measure the fraction of edges which are common to all layers. Valid also in the case of weighted networks. This is a measure of similarity between layers.',
                            '<h5>Inter-layer assortativity (Pearson)</h5>',
                            'Calculate the <a href="http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient" target="_blank">Pearson correlation</a> between the degree (strength) of nodes and their counterparts in other layers, for all pairs of layers. This is another measure of similarity between layers.',
                            '<h5>Inter-layer assortativity (Spearman)</h5>',
                            'Calculate the <a href="http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient" target="_blank">Spearman correlation</a> between the degree (strength) of nodes and their counterparts in other layers, for all pairs of layers. This measure is recommended when the assumptions underlying a Pearson test are not satisfied. This is another measure of similarity between layers.',
                            '<hr>',
                            '<strong>References</strong>:',
                            '<ul>',
                            '<li> M. De Domenico et al, <i>Ranking in interconnected multilayer networks reveals versatile nodes</i>, Nature Communications 6, 6868 (2015) [<a href="http://www.nature.com/ncomms/2015/150423/ncomms7868/full/ncomms7868.html" target="_blank">View</a>]',
                            '</ul>',
                            '<br>'
                                ))
                            )    
                        ),
                    conditionalPanel(condition="input.tabsetDiagnostics=='Community'",
                        HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectCommunity',
                                HTML(paste(
                                'This module unveils the <a href="http://en.wikipedia.org/wiki/Community_structure" target="_blank">community structure</a> of the network.',
                                '<h5>Multiplex</h5>',
                                'This method uses the multislice community detection proposed by <a href="http://www.sciencemag.org/content/328/5980/876" target="_blank">Mucha et al</a> to partition the network accounting for the interconnected topology. The option ORDINAL considers a multiplex with interconnections existing only between a layer and its adjacent layers (ordered as imported), while the option CATEGORICAL considers a multiplex with interconnections existing between all pairs of layers. See also <a href="http://netwiki.amath.unc.edu/GenLouvain/GenLouvain" target="_blank"></a> for further details.',
                                '<br>',
                                'It is worth mentioning that this method (and its generalization to more complex interconnected topologies) can be described using the same tensorial formulation (see <a href="http://prx.aps.org/abstract/PRX/v3/i4/e041022" target="_blank">De Domenico et al</a>) adopted for the calculation of centrality diagnostics. For the current implementation:<br>',
                                '<hr>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> P. Mucha et al, <i>Community Structure in Time-Dependent, Multiscale, and Multiplex Networks</i>, Science 328, 876 (2010) [<a href="http://www.sciencemag.org/content/328/5980/876" target="_blank">Open</a>]',
                                '</ul>',
                                '<h5>Infomap</h5>',
                                'This method performs community detection in each layer separately using the Infomap algorithm proposed by <a href="http://www.pnas.org/content/105/4/1118" target="_blank">Rosvall and Bergstrom</a>. The community structure is found by <a href="http://en.wikipedia.org/wiki/Minimum_description_length" target="_blank">minimizing the expected description length</a> of a <a href="http://en.wikipedia.org/wiki/Random_walk" target="_blank">random walker</a> trajectory in each network. See <a href="http://igraph.org/r/doc/" target="_blank">igraph doc</a> for further details.',
                                '<h5>Louvain</h5>',
                                'This method performs community detection in each layer separately using the Louvain multi-level algorithm proposed by <a href="http://iopscience.iop.org/1742-5468/2008/10/P10008" target="_blank">Blondel et al</a>. The community structure is found by a heuristic method that is based on modularity optimization. See <a href="http://igraph.org/r/doc/" target="_blank">igraph doc</a> for further details.',
                                '<h5>Random Walk Trap</h5>',
                                'This method performs community detection in each layer separately using the Walktrap algorithm proposed by <a href="http://link.springer.com/chapter/10.1007%2F11569596_31" target="_blank">Pons and Latapy</a>. The community structure is found by trying to identify densely connected subgraphs via <a href="http://en.wikipedia.org/wiki/Random_walk" target="_blank">random walks</a> (exploiting the fact that random walks tend to stay within the same community). See <a href="http://igraph.org/r/doc/" target="_blank">igraph doc</a> for further details.',
                                '<h5>Edge Betweenness</h5>',
                                'This method performs community detection in each layer separately using the Edge Betweenness algorithm proposed by <a href="http://pre.aps.org/abstract/PRE/v69/i2/e026113" target="_blank">Newman and Girvan</a>. The community structure is found by exploiting the fact that edges connecting different communities are traversed by a larger number of shortest paths, i.e., all the ones from a module to another: gradually removing edges with highest edge-betweenness will provide the modular structure in each network. See <a href="http://igraph.org/r/doc/" target="_blank">igraph doc</a> for further details.',
                                '<br>'
                                ))
                            )    
                        )
                ),
                mainPanel(
                    tabsetPanel(id="tabsetDiagnostics",
                        tabPanel("Mux Set up",
                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
                                fluidRow(
                                    column(width = 5,
                                        myBox("Multiplex Type", "basic",
                                            helpText(HTML('Multiplex networks can be <strong>ORDINAL</strong> (interconnections exist only between pairs of adjacent layers) or <strong>CATEGORICAL</strong> (interconnections exist between all pairs of layers). Select the option below:')),
                                            radioButtons('radMultiplexType', '',
                                                c(Ordinal='MULTIPLEX_IS_ORDERED',
                                                    Categorical='MULTIPLEX_IS_CATEGORICAL'),
                                                    selected='MULTIPLEX_IS_CATEGORICAL'
                                                )
                                            )
                                        ),
                                    column(width=5,
                                        myBox("Parameters", "basic",
                                            textInput("txtOmega", label=HTML("Strength of inter-layer connections:"), "0"),
                                            helpText("This does not affect the analysis if you work with Interconnected Multiplex or Interdependent Network models (where the inter-layer weight is explicitly specified for each link from the input). If your network is an interconnected multiplex with the same weight for all inter-layer links, you might import as Edge-Colored Network and assign here a value larger than zero.")
                                        )
                                    )
                                )
                            ),
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                helpText("No options to set up for non-edge-colored network models.")
                            )
                        ),
                        tabPanel("Correlation",
                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                                helpText("No options to set up for interdependent network models.")
                            ),
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                                fluidRow(
                                    column(width = 5,
                                        myBox("Inter-layer Correlation", "basic",
                                            #HTML('<h4>General diagnostics</h4>'),
                                           checkboxInput('chkMULTIPLEX_OVERLAPPING', 'Mean global overlapping', TRUE),                
                                            checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_PEARSON', 'Inter-layer assortativity (Pearson correlation)', TRUE),
                                        checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN', 'Inter-layer assortativity (Spearman correlation)', TRUE),
                                        selectInput("selAssortativityType", HTML("Assortativity type (T=Total, I=In-going, O=Out-going):"), 
                                            choices = c("TT", "II", "OO", "IO", "OI"))
                                            )
                                        ),
                                    column(width=5,
                                        myBox("Graphical Options","basic",
                                            checkboxInput('chkEXPORT_MATRIX_PLOT', HTML('Export each layer as an image (<font color="red">slow for larger networks</font>)'), FALSE),
                                            selectInput("selAssortativityTypeColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                                choices = paletteChoiceArray)
                                            ),
                                        HTML("<center>"),
                                        actionButton("btnCalculateCorrelationDiagnostics", "Calculate Correlation Diagnostics"),
                                        HTML("</center>")
                                        )
                                    ),
                                tags$hr(),
                                HTML('<h4>Global diagnostics</h4>'),
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_OVERLAPPING",
                                    htmlWidgetOutput(
                                                outputId = 'globalDiagnosticsOverlapping',
                                                HTML(paste(
                                                '<h5>Overlapping</h5>',
                                                'Mean Global Overlapping: <span id="sumAvgGlobalOverlapping"></span><br>',
                                                '<br>'
                                                ))
                                        ),
                                        imageOutput("overlappingSummaryImage"),
                                        htmlOutput("overlappingSummaryTable"),    
                                        downloadButton('downOverlappingSummaryTable', 'Export'),
                                        tags$hr()
                                    ),                                
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_PEARSON",
                                    helpText(HTML("<h5>Inter-layer Assortativity: Pearson</h5>")),
                                    imageOutput("interPearsonSummaryImage"),
                                    htmlOutput("interPearsonSummaryTable"),
                                    downloadButton('downInterPearsonSummaryTable', 'Export'),
                                    tags$hr()
                                    ),                                
                                conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN",
                                    helpText(HTML("<h5>Inter-layer Assortativity: Spearman</h5>")),
                                    imageOutput("interSpearmanSummaryImage"),
                                    htmlOutput("interSpearmanSummaryTable"),
                                    downloadButton('downInterSpearmanSummaryTable', 'Export')
                                    )
                                )
                            ),
                        tabPanel("Centrality",
                            fluidRow(
                                column(width = 5,
                                    myBox("Framework", "basic",
                                        checkboxInput("chkNODE_CENTRALITY_MULTIPLEX","Use tensorial calculation (uncheck this for calculation in each layer separately)",TRUE)
                                    ),
                                    HTML("<center>"),
                                    actionButton("btnCalculateCentralityDiagnostics", "Calculate Centrality Diagnostics"),
                                    HTML("</center>")
                                ),
                                column(width = 5,
                                    myBox("Descriptors", "basic",
                                        #HTML('<h4>Centrality</h4>'),
                                        checkboxInput("chkNODE_CENTRALITY_DEGREE","Degree (in-going, out-going and total)",TRUE),
                                        checkboxInput("chkNODE_CENTRALITY_STRENGTH","Strength (in-going, out-going and total)",F),
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
                            HTML('<h4>Centrality diagnostics</h4>'),
                            conditionalPanel(condition="input.btnCalculateCentralityDiagnostics>0",
                                checkboxInput(inputId = "centralityTablePageable", label = "Pageable", TRUE),
                                conditionalPanel("input.centralityTablePageable==true",
                                        uiOutput("numOutputCentralityTableNodesPerPage")
                                    ),
                                htmlOutput("centralityTable"),
                                downloadButton('downCentralityTable', 'Export'),
                                tags$hr(),
                                fluidRow(
                                    column(width = 4,
                                        myBox("Diagnostics analysis", "basic",
                                            uiOutput("selDiagnosticsCentralityVizOutputID"),
                                            radioButtons('radDiagnosticsCentralityType', 'Analysis',
                                                c(TopRanked='DIAGNOSTICS_ANALYSIS_TOPRANKED',
                                                    Distribution='DIAGNOSTICS_ANALYSIS_DISTRIBUTION',
                                                    Scatter='DIAGNOSTICS_ANALYSIS_SCATTER'),
                                                    selected='DIAGNOSTICS_ANALYSIS_TOPRANKED'
                                                ),
                                            HTML("<strong>Networks to include:</strong>:"),
                                            checkboxInput(inputId = "chkCentralityAnalysisStructureMultiplex", label = "Multilayer", T),
                                            checkboxInput(inputId = "chkCentralityAnalysisStructureLayer", label = "Layer(s)", F),
                                            conditionalPanel(condition="input.chkCentralityAnalysisStructureLayer",
                                                textInput("txtDiagnosticsCentralityStructureLayer", label=HTML("<strong>Use the following layer (eg 1,3,7 to use more than one layer):</strong>"), "1")
                                                ), 
                                            checkboxInput(inputId = "chkCentralityAnalysisStructureAggregate", label = "Aggregate", T),
                                            conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_DISTRIBUTION'",
                                                textInput("txtDiagnosticsCentralityDistributionBins", label=HTML("<strong>Number of bins:</strong>"), "30"),
                                                checkboxInput(inputId = "centralityAnalysisDistributionLogx", label = "Log x", F),
                                                checkboxInput(inputId = "centralityAnalysisDistributionLogy", label = "Log y", F)
                                                ),
                                            conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_TOPRANKED'",
                                                textInput("txtDiagnosticsCentralityTopRankedBins", label=HTML("<strong>Show the top:</strong>"), "20"),
                                                checkboxInput(inputId = "centralityAnalysisTopRankedLog", label = "Log", F)
                                                ),
                                            conditionalPanel(condition="input.radDiagnosticsCentralityType == 'DIAGNOSTICS_ANALYSIS_SCATTER'",
                                                uiOutput("selDiagnosticsCentralityVizScatterOutputID"),
                                                textInput('txtDiagnosticsCentralityVizScatterColorTransparency', label='Circle transparency (from 0 to 1; 1 means full color):', "0.7"),
                                                uiOutput("selDiagnosticsCentralityVizScatterSizeOutputID"),
                                                checkboxInput(inputId = "centralityAnalysisScatterLogx", label = "Log x", F),
                                                checkboxInput(inputId = "centralityAnalysisScatterLogy", label = "Log y", F),
                                                checkboxInput(inputId = "centralityAnalysisScatterLogRadius", label = "Log Radius", F)
                                                ),
                                            HTML("<center>"),
                                            actionButton("btnCentralityDiagnosticsAnalysis", "Plot"),
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
                        tabPanel("Community",
                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                                helpText("No options to set up for interdependent network models.")
                            ),
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                                fluidRow(
                                    column(width = 5,
                                        myBox("Algorithm", "basic",
                                            #HTML('<h4>Algorithm to be used for Community Detection</h4>'),
                                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
                                                radioButtons('radCommunityAlgorithm', '',
                                                    c(Multiplex='COMMUNITY_MULTIPLEX',
                                                        Infomap='COMMUNITY_INFOMAP',
                                                        Louvain='COMMUNITY_LOUVAIN',
                                                        Random_Walk_Trap='COMMUNITY_RANDOM_WALK_TRAP',
                                                        Edge_Betweenness='COMMUNITY_EDGE_BETWEENNESS'),
                                                        selected='COMMUNITY_MULTIPLEX'
                                                    )
                                                ),
                                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                                radioButtons('radCommunityAlgorithm', '',
                                                    c(Infomap='COMMUNITY_INFOMAP',
                                                        Louvain='COMMUNITY_LOUVAIN',
                                                        Random_Walk_Trap='COMMUNITY_RANDOM_WALK_TRAP',
                                                        Edge_Betweenness='COMMUNITY_EDGE_BETWEENNESS'),
                                                        selected='COMMUNITY_INFOMAP'
                                                    )
                                                ),
                                            uiOutput("communityParameters")                                            
                                            ),
                                        HTML("<center>"),
                                        actionButton("btnCalculateCommunityDiagnostics", "Calculate Community Structure"),
                                        HTML("</center>")
                                        )
                                    ),
                                tags$hr(),
                                HTML('<h4>Communities</h4>'),
                                conditionalPanel(condition="input.btnCalculateCommunityDiagnostics>0",
                                    imageOutput("matCommunitySummaryImage"),
                                    htmlOutput("communitySummaryTable"),
                                    downloadButton('downCommunitySummaryTable', 'Export'),
                                    tags$hr(),
                                    checkboxInput(inputId = "communityTablePageable", label = "Pageable", TRUE),
                                    conditionalPanel("input.communityTablePageable==true",
                                        uiOutput("numOutputCommunityTableNodesPerPage")
                                        ),  
                                    htmlOutput("communityTable"),
                                    downloadButton('downCommunityTable', 'Export')
                                    )
                                )  
                            ),
                        tabPanel("Network of layers",
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                HTML("<center>"),
                                tags$br(),
                                actionButton("btnRenderNetworkOfLayers", "Render the network of layers"),
                                HTML("</center>"),
                                HTML("<center>"),
                                htmlOutput('networkOfLayersPlot'),
                                HTML("</center>")
                                )
                            ),
                        #tabPanel("Components",
                        #    checkboxInput('chkPERFORM_COMPONENT_DETECTION', 'Find connected components', TRUE)
                        #    ),
                            #tags$hr(),
                            #HTML('<h4>Components</h4>'),
                            #conditionalPanel(condition="input.btnCalculateXXXDiagnostics>0",
                            #    htmlOutput("componentsSummaryTable"),
                            #checkboxInput(inputId = "componentsTablePageable", label = "Pageable", TRUE),
                            #conditionalPanel("input.componentsTablePageable==true",
                            #        numericInput(inputId = "componentsTablePageSize",label = "Nodes per page",20)
                            #        ),  
                            #htmlOutput("componentsTable")
                            #),
                          #  tags$hr()
                         #   ),
                        tabPanel("Annular Viz",
                            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                                helpText("No options to set up for interdependent network models.")
                            ),
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                                fluidRow(
                                    column(width = 5,
                                        myBox("Metric", "basic",
                                            #HTML('<h4>Annular representation</h4>'),
                                            radioButtons('radAnularVizCorrelationMethod', 'Correlation method for ring ordering:',
                                                c(Spearman='ANULAR_VIZ_CORRELATION_SPEARMAN',
                                                    Pearson='ANULAR_VIZ_CORRELATION_PEARSON',
                                                    Jensen_Shannon_Divergence='ANULAR_VIZ_CORRELATION_JSD'),
                                                    selected='ANULAR_VIZ_CORRELATION_SPEARMAN'
                                                ),
                                            textInput("txtANULAR_VIZ_BINS",label="Number of bins (tune the colors)","50"),
                                            checkboxInput("chkANULAR_VIZ_LOG","Logarithmic binning",FALSE)
                                            ),
                                        HTML("<center>"),
                                        actionButton("btnAnularViz", "Render Annular Plot"),
                                        HTML("</center>")
                                        ),
                                    column(width=5,
                                        myBox("Graphical Options","basic",                                        
                                            uiOutput("selAnularVizOutputFeatureID"),
                                            uiOutput("selAnularVizOutputLayerID"),
                                            textInput("txtANULAR_VIZ_RCORE",label="Radius of the core","0.3"),
                                            textInput("txtANULAR_VIZ_RING_DISPLACEMENT",label="Distance between rings","0.01"),
                                            checkboxInput("chkANULAR_VIZ_CELL_BORDER","Show cell border",FALSE),
                                            checkboxInput("chkANULAR_VIZ_SHOW_NODE_LABEL","Show node IDs around the annular representation",FALSE),
                                            textInput("txtANULAR_VIZ_FONT_SIZE",label="Font size:","1.5"),
                                            selectInput("selAnularColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                                choices = paletteChoiceArray)
                                            )
                                        )
                                    ),
                                tags$hr(),
                                conditionalPanel(condition="input.btnAnularViz>0 && input.btnCalculateCentralityDiagnostics>0",
                                    helpText(HTML("<h5>Multiplex</h5>")),
                                    helpText("Each ring represents a descriptor. Each cell corresponds to a node, with cells following the same radial trajectory corresponding to the same node. Colors code the membership of the node in each ring. The thickness of a ring is proportional to its information entropy."),
                                    imageOutput("anularVizSummaryMuxImage",width = "100%", height = "600px"),
                                    tags$hr(),
                                    
                                    helpText(HTML("<h5>Comparison per centrality</h5>")),
                                    helpText("Each ring represents a layer. Each cell corresponds to a node, with cells following the same radial trajectory corresponding to the same node. Colors code the membership of the node in each ring. The thickness of a ring is proportional to its information entropy."),
                                    uiOutput("outputAnularVizImages"),
                                    tags$hr()                                                  
                                    ),
                                tags$hr()
                                )
                            )
                        )
                    )
                )
            ),
        tabPanel("Visualization",
            sidebarLayout(position="right",
                sidebarPanel(
                    HTML("<h3>Rendering</h3>"),
                    HTML("Configure the graphical options for layers, nodes and edges. When you are ready, click on the button below:"),
                    br(),
                    HTML("<center>"),
                    actionButton("btnRenderNetworks", "Render Network / Apply Viz Options"),
                    HTML("</center>"),
                    br(),
                    HTML("<h3>Quick help</h3>"),
                    htmlWidgetOutput(
                        outputId = 'projectLayout',
                        HTML(paste(
                        '<strong>Suggestions for layout of each layer (see <a href="http://igraph.org/r/doc/" target="_blank">igraph layout doc</a> for further details):</strong><br>',
                        '<ul>',
                        '<li> <span>Fruchterman-Reingold</span> for networks with less than 1000 nodes',
                        '<li> <span>Large graph layout (LGL)</span> for networks with more than 1000 nodes',
                        '<li> <span>Force-directed fast (DRL)</span> for networks with more than 1000 nodes',
                        '<li> <span>Spring</span> for networks with less than 100 nodes',
                        '<li> <span>Kamada-Kawai</span> for networks with less than 100 nodes',
                        '<li> <span>Reingold-Tilford</span> for networks with more than 1000 nodes',
                        '<li> <span>Combined</span></span> for large networks',
                        '</ul>'
                        ))
                        ),
                    htmlWidgetOutput(
                        outputId = 'projectLayoutType',
                        HTML(paste(
                        '<strong>Type of visualization:</strong><br>',
                        '<ul>',
                        '<li> <span>Multiplex</span> accounts for all layers simultaneously',
                        '<li> <span>By LayerID</span> forces all layers to layout of LayerID (specified in the textbox below)',
                        '<li> <span>Independent</span> applies the layout to each layer separately',
                        '</ul>',
                        '<br>'
                        ))
                        )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Layout",
                            myBox("Layout Algorithm (Network of Layers)", "basic",
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
                                        textInputRow("txtNetworkLayersMultilineRows", label="Number of rows:", 2),
                                        textInputRow("txtNetworkLayersMultilineCols", label="Number of columns:", 2),
                                        helpText("Note: this layout will not show the aggregate network.")
                                    ),
                                    conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_MATRIX'",
                                        textInputRow("txtNetworkLayersMatrixRows", label="Number of rows:", 2),
                                        textInputRow("txtNetworkLayersMatrixCols", label="Number of columns:", 2),
                                        helpText("Note: this layout will not show the aggregate network.")
                                    ),
                                    conditionalPanel(condition="input.radNetworkOfLayersLayoutType == 'NETWORK_LAYERS_LAYOUT_FORCEDIRECTED'",
                                        #HTML("<font color='red'>Very experimental.</font>"),
                                        HTML("<font color='red'>Very experimental. Layers could overlap in some cases.</font>"),
                                        helpText("Note: this layout will not show the aggregate network.")
                                    )
                                ),

                            fluidRow(
                                column(width=5,
                                    myBox("Layout Algorithm (Layer)", "basic",
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
                                                checkboxInput("chkPLOT_AS_EDGE_COLORED",HTML("Visualize as edge-colored multigraph (<font color='red'>Very experimental! Works only with centrality/community in the multiplex.</font>)"),FALSE),
                                                checkboxInput("chkPLOT_WITH_RGL",HTML("Use openGL (<font color='red'>Very experimental! If unchecked, standard device will be used</font>)"),TRUE)
                                            )
                                        )
                                    ),
                                column(width=5,
                                    myBox("Graphical Options","basic",
                                        #HTML('<h4>Type of visualization</h4>'),
                                        radioButtons('radLayoutType', '',
                                            c(Multiplex='LAYOUT_MULTIPLEX',
                                                By_LayerID='LAYOUT_BY_LAYER_ID',
                                                Independent='LAYOUT_INDEPENDENT'),
                                                selected='LAYOUT_MULTIPLEX', inline=TRUE
                                            ),
                                        #this is a dynamic object changing because of input
                                        uiOutput("selOutputLayerID"),
                                        textInput("txtLAYOUT_MAXITER", 
                                            label=HTML("Maximum number of iterations:"), 
                                            "1000"
                                            )
                                        ),
                                    HTML("<center>"),
                                    actionButton("btnApplyLayout", "Apply Layout"),
                                    HTML("</center>")
                                    )
                                )
                            ),
                        tabPanel("Graphics",
                            fluidRow(
                                column(5,
                                    myBox("Plot Options","basic",
                                        #HTML('<h4>Options for the rendering of the multiplex</h4>'),
                                        textInput('txtPLOT_TITLE', label='Plot title:', ""),
                                        textInput('txtPLOT_SUBTITLE', label='Plot subtitle:', ""),
                                        textInput('txtBACKGROUND_COLOR', label='Background color (any valid R type):', "white")
                                        ),
                                    conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                        myBox("Light Options (RGL)","basic",
                                            checkboxInput("chkPLOT_LIGHT","Add a light to the plot (to improve visualization):",FALSE),
                                            textInput('txtPLOT_LIGHT_PHI', label='Phi coordinate (deg):', "20"),
                                            textInput('txtPLOT_LIGHT_THETA', label='Theta coordinate (deg):', "30"),
                                            HTML("<center>"),
                                            actionButton("btnResetLights", "Reset"),
                                            HTML("</center>"),
                                            helpText("It could be necessary to close the rgl window before rendering again.")
                                            )
                                        )
                                    ),
                                column(5,
                                    myBox("3D Options", "basic",    
                                        conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                            checkboxInput('chkPLOT_AXES3D', label='Show axes (for RGL)', F),
                                            checkboxInput("chkPLOT_REMEMBER_ORIENTATION","Remember previous orientation in a new rendering (for RGL)",TRUE),
                                            textInput('txtPLOT_FOV', label=HTML('Default field of view (degrees; for RGL):'), "20")
                                            ),
                                        conditionalPanel(condition="!input.chkPLOT_WITH_RGL",
                                            HTML('For non-RGL visualization, rotate by angle (degrees) around (<font color="red">must apply the layout again</font>):'),
                                            tags$br(),
                                            textInputRow('txtPLOT_ROTX', label=HTML('X-axis:'), "0"),
                                            textInputRow('txtPLOT_ROTY', label=HTML('Y-axis:'), "120"),
                                            textInputRow('txtPLOT_ROTZ', label=HTML('Z-axis:'), "0"),
                                            helpText("Note: order of rotations is (x,y,z)."),
                                            
#                                            radioButtons('radPlotNonRGLQuickLayout', 'Quick layout:',
#                                                c(Horizontal='PLOT_NONRGL_QUICK_LAYOUT_HORIZONTAL',
#                                                    Vertical='PLOT_NONRGL_QUICK_LAYOUT_VERTICAL',
#                                                    Custom='PLOT_NONRGL_QUICK_LAYOUT_NONE'),
#                                                    selected='PLOT_NONRGL_QUICK_LAYOUT_HORIZONTAL', inline=TRUE
#                                                ),
                                            helpText("Horizontal layout: x=0°, y=120°, z=0°, shift>0."),
                                            helpText("Vertical layout: x=120°, y=90°, z=-90°, shift=0")
                                            ),
                                        textInput('txtLAYER_SHIFT', label=HTML('Shift layers (along horizontal axis to improve perspective, <font color="red">must apply the layout again</font>) by:'), "0.8"),
                                        textInput('txtLAYER_SCALE', label=HTML('Scale layers (<font color="red">must apply the layout again</font>) by:'), "4"),
                                        textInput('txtLAYER_SPACE', label=HTML('Space between layers (<font color="red">must apply the layout again</font>) by:'), "3")
                                        )
                                    )
                                )
                            ),
                        tabPanel("Multiplex",
                            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                fluidRow(
                                    column(5,
                                        myBox("Inter-link Options","basic",
                                            checkboxInput("chkINTERLINK_SHOW",HTML("Show inter-links (<font color='red'>resource consuming</font>, recommended for small networks):"),TRUE),
                                            #textInput('txtINTERLINK_SHOW_FRACTION', label='Show only this random fraction of inter-links (from 0 to 1):', "0.2"),
                                            textInput('txtINTERLINK_COLOR', label='Inter-link color (any valid R type):', "#D8D8D8"),
                                            selectInput('selINTERLINK_TYPE', 'Inter-link line style:', choices=                                                c("dotted", "solid", "dashed", "dotdash", "longdash", "twodash")),                                                
                                            textInput('txtINTERLINK_WIDTH', label='Inter-link width:', "0.4"),
                                            textInput('txtINTERLINK_TRANSP', label='Inter-link transparency (from 0 to 1; 1 means full color):', "0.2")
                                            )
                                        )
                                    )
                                ),
                                conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_EDGECOLORED'",
                                    helpText("No options to set up for edge-colored network models.")
                                )
                            ),
                        tabPanel("Layers",
                            fluidRow(
                                column(5,
                                    conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                        myBox("Layer Options", "basic",
                                            checkboxInput("chkLAYER_SHOW","Show layers:",TRUE),
                                            textInput('txtLAYER_LABEL_PREFIX', label='Layer label prefix (overwritten by label, if any, provided with the config file):', "L"),
                                            textInput('txtLAYER_COLOR', label='Layer color (any valid R type; use commas to indicate a color for each layer):', "gray"),
                                            textInput('txtLAYER_TRANSP', label='Layer transparency (from 0 to 1; 1 means full color; use commas to indicate a value for each layer):', "0.08"),
                                            HTML("Show labels on:"),
                                            checkboxInput("chkLAYER_ID_SHOW_TOPLEFT","top-left",FALSE),
                                            checkboxInput("chkLAYER_ID_SHOW_BOTTOMLEFT","bottom-left:",TRUE),
                                            checkboxInput("chkLAYER_ID_SHOW_TOPRIGHT","top-right:",FALSE),
                                            checkboxInput("chkLAYER_ID_SHOW_BOTTOMRIGHT","bottom-right:",FALSE),
                                            textInput('txtLAYER_ID_FONTSIZE', label='Font size for labels:', "1.5")
                                            )
                                        )
                                    ),
                                column(5,
                                    conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                        myBox("Aggregate Options", "basic",
                                            checkboxInput("chkAGGREGATE_SHOW",HTML("Show aggregate network as separate layer (<font color='red'>must apply the layout again</font>):"),FALSE),
                                            textInput('txtLAYER_AGGREGATE_LABEL_PREFIX', label='Aggregate layer label:', "Aggregate"),
                                            textInput('txtLAYER_AGGREGATE_COLOR', label='Aggregate layer color (any valid R type):', "blue"),
                                            textInput('txtLAYER_AGGREGATE_TRANSP', label='Aggregate layer transparency (from 0 to 1; 1 means full color):', "0.08")
                                            )
                                        ),
                                    myBox("Inactive Layers","basic",
                                        textInput("txtLAYERS_ACTIVE", label="Deactivate the following layer(s) from the visualization (keep it blank to keep all layers active; use commas to indicate more than one layer, eg 1,2,5):")
                                        )
                                    )
                                ),
                            fluidRow(
                                column(5,
                                    conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                        myBox("Geographical Options", "basic",
                                            checkboxInput("chkGEOGRAPHIC_BOUNDARIES_SHOW","[Layer] Show geographical boundaries if geographical layout is provided",TRUE),
                                            checkboxInput("chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW","[Aggregate] Show geographical boundaries if geographical layout is provided",TRUE),                            
                                            selectInput("selOSMType", HTML("If geographical layout is provided, use the following background (requires Internet connection):</strong>"), 
                                                choices = c("bing","mapbox","mapquest-aerial","osm","osm-bbike-german","osm-transport","stamen-toner","stamen-watercolor")),
                                            helpText(HTML("<h5>Custom set of all geographical boundaries (<font color='red'>must apply the layout again</font>)</h5>")),
                                            textInput('txtGEOGRAPHIC_LAT_MIN', label='Minimum latitude (default: automatic):', ""),
                                            textInput('txtGEOGRAPHIC_LAT_MAX', label='Maximum latitude (default: automatic):', ""),
                                            textInput('txtGEOGRAPHIC_LONG_MIN', label='Minimum longitude (default: automatic):', ""),
                                            textInput('txtGEOGRAPHIC_LONG_MAX', label='Maximum longitude (default: automatic):', "")
                                            )
                                        )
                                    )
                                )
                            ),
                        tabPanel("Nodes",
                            fluidRow(
                                column(5,
                                    myBox("Node Size", "basic",              
                                        conditionalPanel(condition="input.btnCalculateCentralityDiagnostics==0",
                                            radioButtons('radNodeSizeType', 'Node size proportional to:',
                                                c(Uniform='NODE_SIZE_PROPORTIONAL_TO_UNIFORM'),
                                                    selected='NODE_SIZE_PROPORTIONAL_TO_UNIFORM'
                                                )
                                            ),
                                        conditionalPanel(condition="input.btnCalculateCentralityDiagnostics>0",
                                            uiOutput("selVizNodeSizeOutputID")
                                            ),
                                        radioButtons('radNodeSizeType2', 'Type of proportionality:',
                                            c(Constant='NODE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                                Log='NODE_SIZE_PROPORTIONAL_TYPE_LOG',
                                                LogLog='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                                selected='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG', inline=TRUE
                                            ),
                                        textInput('txtNODE_DEFAULT_SIZE', label='Default size (used for fine tuning of Uniform, Log and LogLog option):', "10")
                                        )
                                    ),
                                column(5,
                                    myBox("Node Color", "basic",
                                        radioButtons('radNodeColor', 'Node color:',
                                            c(Random='NODE_COLOR_RANDOM',
                                                Community='NODE_COLOR_COMMUNITY',
                                                #Component='NODE_COLOR_COMPONENT',
                                                Centrality='NODE_COLOR_CENTRALITY',
                                                TopRank='NODE_COLOR_TOPRANK',
                                                Uniform='NODE_COLOR_UNIFORM'),
                                                selected='NODE_COLOR_UNIFORM'
                                            ),
                                        hr(),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_CENTRALITY' && input.btnCalculateCentralityDiagnostics>0",
                                            uiOutput("selVizNodeColorOutputID"),
                                            radioButtons('radNodeColorType2', 'Type of proportionality:',
                                                c(Constant='NODE_COLOR_PROPORTIONAL_TYPE_NORMAL',
                                                    Log='NODE_COLOR_PROPORTIONAL_TYPE_LOG',
                                                    LogLog='NODE_COLOR_PROPORTIONAL_TYPE_LOGLOG'),
                                                    selected='NODE_COLOR_PROPORTIONAL_TYPE_NORMAL'
                                                ),
                                            selectInput("selCentralityColorPalette", HTML("Color palette for coloring by centrality (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                                choices = as.vector(paletteChoiceArray)),
                                            textInputRow("txtNODE_COLOR_CENTRALITY_BINS", label="Bins:", "30")
                                            ),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_CENTRALITY' && input.btnCalculateCentralityDiagnostics==0",
                                            helpText("You need to calculate diagnostics before using this option.")
                                            ),                                            
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_RANDOM'",
                                            selectInput("selMultiplexColorPalette", HTML("Color palette for nodes' default color (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                                choices = append(as.vector(paletteChoiceArray),"random"))
                                            ),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_UNIFORM'",
                                            textInput('txtNODE_COLOR_UNIFORM_COLOR', label='Color (any valid R type):', "#F2F2F2")
                                            ),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_TOPRANK' && input.btnCalculateCentralityDiagnostics>0",
                                            uiOutput("selVizNodeColorTopOutputID"),
                                            textInput('txtNODE_COLOR_TOP', label='Number of top-ranked nodes to consider (monoplex or multiplex centrality is considered, according to latest calculation):', "5"),
                                            textInput('txtNODE_COLOR_TOP_COLOR_TOP', label='Color of top-ranked nodes (any valid R type):', "#FF0000"),
                                            textInput('txtNODE_COLOR_TOP_COLOR_OTHERS', label='Color of the other nodes and all edges (any valid R type):', "#F2F2F2"),         
                                            checkboxInput("chkNODE_LABELS_SHOW_ONLY_TOP","Show nodes labels only for top-ranked nodes:",TRUE),                   
                                            textInput('txtNODE_COLOR_TOP_LABELS_FONT_COLOR', label='Color of nodes labels (any valid R type):', "#000000")
                                            ),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_TOPRANK' && input.btnCalculateCentralityDiagnostics==0",
                                            helpText("You need to calculate diagnostics before using this option.")
                                            ),                                            
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMMUNITY' && input.btnCalculateCommunityDiagnostics>0",    
                                            textInput("txtCOMMUNITY_MIN_SIZE",label="Color-code with the same RGB all nodes in communities smaller than (useful for evidencing larger communities, not valid for the multiplex):","1"),
                                            selectInput("selCommunityColorPalette", HTML("Color palette for coloring communities (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                                choices = append(as.vector(paletteChoiceArray),"random"))
                                            ),
                                        conditionalPanel(condition="input.radNodeColor=='NODE_COLOR_COMMUNITY' && input.btnCalculateCommunityDiagnostics==0",
                                            helpText("You need to calculate diagnostics before using this option.")
                                            ) 
                                        )
                                    )
                                ),
                            fluidRow(
                                column(5,
                                    myBox("Other Options", "basic", 
                                        checkboxInput("chkNODE_ISOLATED_HIDE","Exclude isolated nodes from the visualization",TRUE),
                                        conditionalPanel(condition="input.chkNODE_ISOLATED_HIDE && input.radMultiplexModel!='MULTIPLEX_IS_EDGECOLORED'",
                                            checkboxInput("chkNODE_ISOLATED_HIDE_INTERLINKS","Find isolated nodes while accounting for interlayer links",TRUE)
                                            ),
                                        textInput('txtNODE_TRANSP', label='Node transparency (from 0 to 1; 1 means full color):', "0.8"),
                                        textInput("txtNODE_FRAME_COLOR", label="Node frame color (any valid R type; keep it blank to use the same as node color):",""),
                                        checkboxInput("chkNODE_LABELS_SHOW","Show nodes labels (recommended only for small networks):",FALSE),
                                        checkboxInput("chkNODE_LABELS_SHOW_WRAP","Wrap labels",FALSE),
                                        conditionalPanel(condition="input.chkNODE_LABELS_SHOW_WRAP",
                                            textInput('txtNODE_LABELS_WRAP', label='Wrap length (with respect to blank spaces)', "10"),
                                            textInput('txtNODE_LABELS_WRAP_OFFSET', label='Wrap offset (usually 0; use this to fine-tune the distance between wrapped labels)', "0")
                                            ),
                                        textInput('txtNODE_LABELS_DISTANCE', label='Distance of labels from nodes:', "1."),
                                        textInput('txtNODE_LABELS_FONT_SIZE', label='Size of nodes labels :', "0.5"),
                                        textInput('txtNODE_LABELS_FONT_COLOR', label='Color of nodes labels (any valid R type):', "#2F2F2F")
                                        )
                                    )
                                )
                            ),
                        tabPanel("Edges",
                            fluidRow(
                                column(5,
                                    myBox("Edge Size","basic",
                                        radioButtons('radEdgeSizeType', 'Edge size proportional to:',
                                            c(Uniform='EDGE_SIZE_PROPORTIONAL_TO_UNIFORM',
                                                Weight='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT'),
                                                selected='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT', inline=TRUE
                                            ),
                                        radioButtons('radEdgeSizeType2', 'Type of proportionality:',
                                            c(Constant='EDGE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                                Log='EDGE_SIZE_PROPORTIONAL_TYPE_LOG',
                                                LogLog='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                                selected='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG', inline=TRUE
                                            ),
                                        textInput('txtEDGE_DEFAULT_SIZE', label='Default size (used for fine tuning of Uniform, Log and LogLog option):', "2")
                                        )
                                    ),
                                column(5,
                                    myBox("Other Options", "basic",
                                        textInput('txtEDGE_BENDING', label='Bending factor (0 means straight; max 1):', "0"),
                                        textInput('txtEDGE_TRANSP', label='Edge transparency (from 0 to 1; 1 means full color):', "0.2"),
                                        textInput('txtLAYER_ARROW_SIZE', label='Arrow size:', "0.2"),
                                        textInput('txtLAYER_ARROW_WIDTH', label='Arrow width:', "0.2")
                                        )
                                    )
                                )
                            ),
                        tabPanel("Export",
                            br(),
                            fluidRow(
                                column(5,
                                    myBox("Options", "basic",
                                        conditionalPanel(condition="input.chkPLOT_WITH_RGL",
                                            radioButtons('radRGLExport', 'Select the format:',
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
                                            radioButtons('radNORGLExport', 'Select the format:',
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
                    )
                )
            ),
        tabPanel("Reducibility",
            conditionalPanel(condition="input.radMultiplexModel=='MULTIPLEX_IS_INTERDEPENDENT'",
                helpText("No options to set up for interdependent network models.")
            ),
            conditionalPanel(condition="input.radMultiplexModel!='MULTIPLEX_IS_INTERDEPENDENT'",
                sidebarLayout(position="right",
                    sidebarPanel(
                        HTML("<h3>Quick help</h3>"),
                        htmlWidgetOutput(
                            outputId = 'projectReducibilityCorrelation',
                            HTML(paste(
                            'An information-theoretical approach is adopted to reduce the dimensionality of the multiplex network, while minimizing information loss. This method allows to describe the network using a smaller number of layers, by aggregating the redundant ones.<br>',
                            'The metric distance is calculated by means of the <a href="http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence" target="_blank">Jensen-Shannon divergence</a> and <a href="http://en.wikipedia.org/wiki/Hierarchical_clustering" target="_blank">hierarchical clustering</a> of layers is employed according to the algorithm defined by the user among those available.<br>',
                            '<hr>',
                            '<strong>References:</strong><br>',
                            '<ul>',
                            '<li> M. De Domenico, V. Nicosia, A. Arenas and V. Latora, <i>Structural reducibility of multilayer networks</i>, Nature Communications 6, 6864 (2015) [<a href="http://www.nature.com/ncomms/2015/150423/ncomms7864/full/ncomms7864.html" target="_blank">View</a>]',
                            '</ul>'
                                )
                            ))
                    ),
                    mainPanel(
                        fluidRow(
                            column(5,
                                myBox("Algorithms","basic",
                                    #HTML('<h4>Algorithms to be used to calculate the correlation between layers and clustering</h4>'),
                                    HTML("<h5>Layer correlation</h5>"),
                                    radioButtons('radReducibilityCorrelationMethod', '',
    c(Jensen_Shannon_Divergence='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'),
                                        selected='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'),
                                    tags$hr(),
                                    HTML("<h5>Hierarchical clustering</h5>"),
                                    selectInput("selReducibilityClusterMethod", "", 
                                        choices = c("ward","single","complete","average","mcquitty","median","centroid"))
                                    )
                                ),
                            column(5,
                                myBox("Graphical Options", "basic",
                                    selectInput("selReducibilityColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                        choices = paletteChoiceArray),
                                    textInput("txtREDUCIBILITY_HEATMAP_FONT_SIZE",label="Font size:","1.5")
                                    ),
                                HTML("<center>"),
                                actionButton("btnCalculateReducibility", "Calculate Reducibility"),
                                HTML("</center>")
                                #actionButton("btnExportReducibilityRendering","Export PNG"),
                                )
                            ),
                            tags$hr(),
                            
                            HTML('<h4>Structural reducibility</h4>'),
                            conditionalPanel(condition="input.btnCalculateReducibility>0",
                                imageOutput("jsdMatrixSummaryImage",width = "100%", height = "700px"),
                                imageOutput("reducibilityDendrogramSummaryImage",width = "100%", height = "700px"),
                                HTML('<center><h5>Quality function</h5></center>'),
                                showOutput("reducibilityQualityFunction","nvd3"),
                                tags$hr()
                            )
                        )
                    )
                )
            ),
        tabPanel("Motifs",
            sidebarLayout(position="right",
                sidebarPanel(
                    HTML("<h3>Quick help</h3>"),
                    htmlWidgetOutput(
                        outputId = 'projectMotifs',
                        HTML(paste(
                            "We provide an interface to <a href='http://theinf1.informatik.uni-jena.de/motifs/' target='_blank'>FANMOD</a> and the following is extracted from its <a href='http://theinf1.informatik.uni-jena.de/motifs/fanmod-manual.pdf' target='_blank'>manual</a>. This module will <strong>NOT</strong> perform motifs analysis per layer or of single-layer networks.",
                                '<hr>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> S. Wernicke and F. Rasche, <i>FANMOD: a tool for fast network motif detection</i>, Bioinformatics 22, 1152 (2006) [<a href="http://bioinformatics.oxfordjournals.org/content/22/9/1152.full" target="_blank">Open</a>]',
                                '</ul>',
                                '<br>',
                            "<h5>Motif size</h5>",
                            "Size of the motifs to be searched. Limited to 3 and 4.", 
                            "<h5>Samples</h5>",
                            "How many samples shall be taken inorder to estimate the number of subgraphs (motif candidates) that will be found during the search.",
                            "<h5>Null model</h5>",
                            "Random networks are generated from the original network by a series of edge switching operations, preserving the original degree sequence. With directed networks, the following randomization models are available:",
                            "<ul>",
                            "<li><strong>Local const.</strong> Unidirectional edges are only exchanged with unidirectional ones. The same applies for bidirectional edges. Therefore, thenumber of incident bidirectional edges remains locally constant, that is, it is constant for each vertex. This model is the only one which can be applied to undirected networks.",
                            "<li><strong>Global const.</strong> The number of bidirectional edges is kept constant in the overall network. However, a specific vertex may loose or gain incident bidirectional edges.",
                            "<li><strong>No regard.</strong> Bidirectional edges can be created and destroyed during randomization. This option usually increases the number of bidirectional edges compared to the original network, which is often unwanted because it makes unidirectional edges falsely appear significant.",
                            "</ul>",
                            "When randomizing the network, the <strong>edges are exchanged</strong> one after the other. This number states how often the program walks over all the edges. Usually the default (3) fits here, but if the results from the random networks are too similar to those of the original one, then you should increase this number.<br>",
                            "When it is an edge's turn to be exchanged, an exchange partner edge is randomly selected from those edges that fulfil the desired properties. If the partner is not suitable for the exchange, another partner is selected, until the exchange succeeds or the number of <strong>exchange attempts</strong> is exceeded. So, if too few exchanges succeed, this number should be increased. But keep in mind that a lot of unsuccessful exchanges may also result from the input network structure."
                            ))
                        )
                    ),
                mainPanel(
                    fluidRow(
                        column(5,
                            myBox("Algorithm options","basic",
                                selectInput("selMotifSize", HTML("<strong>Motif size:</strong>"), choices = as.character(3:4)),
                                textInput("txtMotifSamples", HTML("<strong>Number of samples for approximated calculation (larger is better)</strong>"), "100000"),
                                
                                selectInput("selMotifNullModel", HTML("<strong>Null model:</strong>"), 
                                            choices = c("Local const", "Global const", "No regard")),
                                textInput("txtMotifRandomNetworks", HTML("<strong>Number of random networks to use for assessing significance</strong>"), "1000"),
                                textInput("txtMotifRandomExchangePerEdges", HTML("<strong>Exchanges per edge:</strong>"), "3"),
                                textInput("txtMotifRandomExchangeAttempts", HTML("<strong>Exchange attempts:</strong>"), "3")
                                )
                            ),
                        column(5,
                            myBox("Statistical cuts","basic",
                                checkboxInputRow('chkMotifAbsZscore', HTML("<strong>|Z-score| ></strong>"), FALSE),
                                textInputRow("txtMotifAbsZscore", "", "3"),
                                checkboxInputRow('chkMotifPvalue', HTML("<strong>p-value <</strong>"), FALSE),
                                textInputRow("txtMotifPvalue", "", "0.05"),
                                checkboxInputRow('chkMotifFrequency', HTML("<strong>Frequency ></strong>"), FALSE),
                                textInputRow("txtMotifFrequency", "", "0.01")
                                ),
                            myBox("Graphical options", "basic",
                                selectInput("selMotifResultsSortBy", HTML("<strong>Sort by:</strong>"), 
                                    choices = c("Frequency", "Z-score", "p-value")),
                                    selectInput("selMotifColorPalette", HTML("Color palette for coloring edges (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                            choices = append(as.vector(paletteChoiceArray),"random")),
                                    helpText("(Note that if you change the palette and restart the analysis, you might need to empty your browser cache to see the correct results)")
                                ),
                            HTML("<center>"),
                            actionButton("btnCalculateMotifs", "Calculate Motifs"),
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
                            downloadButton('downMotifsTable', 'Export'),
                            tags$hr()
                        )
                    )
                )
            ),
        tabPanel("Dynamics",
            sidebarLayout(position="right",
                sidebarPanel(
                    HTML("<h3>Quick help</h3>"),
                    htmlWidgetOutput(
                        outputId = 'projectDynamics',
                        HTML(paste(
                        'This module allows the visualization of a dynamical process on the top of a multiplex network (although with an adequate timeline input it can also deal with time-varying networks where nodes and edges can appear or disappear). Before importing the timeline (i.e., the file where you specify the dynamics, see the Help for further details) it is mandatory to render the multiplex network at least once to obtain a complete visualization with the desired graphical options.<br>',
                        'This module is designed to generate the snapshots of your dynamical process (e.g., random walks, diffusion, epidemics and/or information spreading). The snapshots are exported in the export/timeline folder.<br><br>',
                        'Note that to build an animated visualization from the snapshots you need to use an external/third-party software for merging. For this task, you can use your favorite software, although we recommend <a href="https://www.ffmpeg.org/" target="_blank">FFmpeg</a>. See the Help to know more details about this easy task.',
                        '<br>'
                        ))
                    )
                ),
                mainPanel(
                    fluidRow(
                        column(5,
                            myBox("Timeline file","basic",
                                #helpText(HTML("<h4>Open Timeline File</h4>")),
                                helpText(HTML("<strong><font color='#262626'>* Input format for the timeline file:</font></strong>")),
                                #checkboxInput('chkTimelineFileHeader', 'Header', TRUE),
                                textInput("txtTimelineFileSep", label=HTML("Separator (default is one space):"), " "),
                                fileInput('timeline_file', HTML('<strong>* Open the timeline file:</strong>'),
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                                    ),
                                HTML("<center>"),
                                actionButton("btnImportTimeline", "Import Timeline"),
                                HTML("</center>")
                                ),
                            HTML("<center>"),
                            checkboxInput("chkTIMELINE_RENDER_TO_FILE","Render snapshots to file",FALSE),
                            actionButton("btnRenderDynamicsSnapshots", "Render Dynamics Snapshots"),
                            hr(),
                            actionButton("btnFFMPEGDynamicsSnapshots", "Make Video (require ffmpeg)"),
                            HTML("</center>")
                            ),
                        column(5,
                            myBox("Graphical Options", "basic",
                                textInput("txtTimelineDefaultNodesSize", label=HTML("Default size of all nodes (leave this blank to use the Rendering setup):"), "10"),
                                textInput("txtTimelineDefaultNodesColor", label=HTML("Default color of all nodes (any valid R type; leave this blank to use the Rendering setup):"), "#959595"),
                                textInput("txtTimelineDefaultEdgesSize", label=HTML("Default size of all edges (leave this blank to use the Rendering setup):"), "1"),
                                textInput("txtTimelineDefaultEdgesColor", label=HTML("Default color of all edges (any valid R type; leave this blank to use the Rendering setup):"), "#959595")
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
                    )
                )
            ),
        tabPanel("Data",
            HTML("<center>"),
            HTML("<i class='fa fa-flask fa-4x'></i><h1>Available data</h1><br>"),
            HTML("</center>"),
            fluidRow(
                column(5,
                    showOutput("dataPieChart","nvd3")
                    ),
                column(6,
                    showOutput("dataScatterPlot","nvd3")
                    )
                ),
            HTML("<center>"),
            HTML("<a href='http://deim.urv.cat/~manlio.dedomenico/data.php' target='_blank'><i class='fa fa-download fa-4x'></i><h1>Go to the download page</h1></a><br>"),
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
                            HTML(paste("<h4>Import</h4>",
                            "<strong>'Import' tab/'Config file' tab to select and open the configuration file.</strong><br>",
                            "<h5>Edge-colored networks</h5>",
                            "The configuration file is a ASCII file including the list of layers to be included in a multiplex, the corresponding labels and the possible layout file to define node properties (e.g., ID, labels, geographical coordinates, etc).<br><br>",
                            "<u>Format of a configuration file:</u> (<a href='example_config.txt' target='_blank'>click here</a> to open an example)<br>",
                            "<pre>path_layer_X;label_layer_X;path_layout_layer_X</pre>",
                            "where<br>",
                            "<ul>",
                            "<li> <strong>path_layer_X</strong>: [mandatory] specify the path and the filename to the edges list to be used as layer",
                            "<li> <strong>label_layer_X</strong>: [optional] specify the label to be used in the rendering for that layer",
                            "<li> <strong>path_layout_layer_X</strong>: [optional] specify the path and the filename to the file containing information about nodes",
                            "</ul>",
                            "Each line in the configuration file indicates one layer, and the network format for each layer will be \"standard edges list\" (see below).",
                            "<h5>Non edge-colored networks</h5>",
                            "If the multilayer is not edge-colored (i.e., inter-links are allowed), only one line is specified in the configuration file, with format:",
                            "<pre>path_multilayer;path_to_layers_info;path_to_layers_layout</pre>",
                            "where<br>", 
                            "<ul>",
                            "<li> <strong>path_multilayer</strong>: [mandatory] specify the path and the filename to the extended edges list to be used",
                            "<li> <strong>path_to_layers_info</strong>: [mandatory] specify the path and the filename to the file containing information about layers",
                            "<li> <strong>path_to_layers_layout</strong>: [mandatory] specify the path and the filename to the file containing information about nodes",
                            "</ul>",
                            "In this case the network format will be \"extended edges list\" (see below).",
                            "<h5>Standard edges list</h5>",
                            "A typical edges list is expected to be a file with at most three columns, giving the list of edges from a node (first column) to other nodes (second column), possibly weighted by an integer or floating number (third column). For instance:",
                            "<br>",
                            "<pre>",
                            "	1 2 0.5<br>",
                            "	1 3 1.4<br>",
                            "	...<br>",
                            "	18 124 0.1<br>",
                            "</pre>",
                            "is a typical weighted edges list. Non-sequential integer IDs can be used by checking the appropriate box before importing the networks. In this second case, it is mandatory to provide a layout file (see next section) reporting each node label (field nodeLabel). This would look like",
                            "<pre>",
                            "	nodeLabel<br>",
                            "	alice<br>",
                            "	bob<br>",
                            "	john<br>",
                            "	david<br>",
                            "	...<br>",
                            "</pre>",
                            "<h5>Extended edges list</h5>",
                            "An extended edges list is a new format that allows to specify all possible types of links, intra- and inter-layer. Each line specifies the source node (first column) and the source layer (second column), the destination node (third column) and the destination layer (fourth column), possibly weighted by an integer or floating number (fifth column). For instance:",
                            "<pre>",
                            "	1 1 2 1 0.5<br>",
                            "	1 1 3 1 1.4<br>",
                            "	...<br>",
                            "	18 2 124 2 0.1<br>",
                            "</pre>",
                            "is a typical weighted extended edges list. For label-based extended edges lists, the same rules of the standard edges lists apply.",
                            "<br><br>",
                            "<u>Format of a layout file:</u> (<a href='example_layout.txt' target='_blank'>click here</a> to open an example)<br>",
                            "The first line of the file must specify the name of the correponding node attributes. Allowed attributes:",
                            "<ul>",
                            "<li> <strong>nodeID</strong>: [mandatory] numerical integer id to identify each node",
                            "<li> <strong>nodeLabel</strong>: [optional] string specifying the label attribute",
                            "<li> <strong>nodeX</strong>: [optional] float value specifying the Cartesian coordinate x for the layout",
                            "<li> <strong>nodeY</strong>: [optional] float value specifying the Cartesian coordinate x for the layout",
                            "<li> <strong>nodeLat</strong>: [optional] float value specifying the latitude for the geographical layout",
                            "<li> <strong>nodeLong</strong>: [optional] float value specifying the longitude for the geographical layout",
                            "</ul>",
                            "The order of the columns is not relevant. If nodeLat and nodeLong are specified, they will be automatically converted to Cartesian coordinates (through Mercator projection).<br>",
                            "The properties of each node in the multilayer must be specified or default values will be used (i.e., automatic labeling and layouting). If the number of nodes in the network is different from the number of nodes provided in the layout file, it will be assumed that something is wrong with the layout file and default values will be used.",
                            "<br><br>",
                            "<u>Format of a layer-info file:</u> (<a href='example_layer.txt' target='_blank'>click here</a> to open an example)<br>",
                            "The first line of the file must specify the name of the correponding layer attributes. Allowed attributes:",
                            "<ul>",
                            "<li> <strong>layerID</strong>: [mandatory] numerical integer id to identify each layer",
                            "<li> <strong>layerLabel</strong>: [optional] string specifying the label attribute",
                            "</ul>",
                            "The order of the columns should not be relevant.",
                            "<br><br>",
                            "<strong>'Import' tab/'Import' tab to import the networks in your configuration file.</strong><br>",
                            "The network files are expected to be in edges list format. For edge-colored networks, one file for each layer with format <br>",
                            "<pre>nodeID1 nodeID2</pre>",
                            "in the case of unweighted edges list, or<br>",
                            "<pre>nodeID1 nodeID2 weight</pre>",
                            "in the case of weighted edge list.<br>",
                            "The nodes ID are expected to refer to the same node in each layer. For instance, the ID 1234 with label 'myNode' is expected to indicate the same entity in all layers.",
                            "<br>",
                            "The case of non-edge-colored network is a generalization of the above rules.",
                            "<hr>",
                            "<h4>Diagnostics: calculation and representation</h4>",
                            "<strong>'Diagnostics' tab to set up and start the calculation.</strong><br>",
                            "First, set up the properties of the multiplex network clicking in the tab 'Mux set up'.<br>",
                            "Successively, it is possible to calculate different types of diagnostics:",
                            "<ul>",
                            "<li> <strong>Correlation</strong>: calculate edge overlap and inter-layer degree-degree correlations;",
                            "<li> <strong>Centrality</strong>: calculate several descriptors of influence/importance in the network. It is possible to choose to perform calculations in each layer separately or accounting for the whole multiplex structure;",
                            "<li> <strong>Community</strong>: unveil the community structure according to different methods. It is possible to choose to perform calculations in each layer separately or accounting for the whole multiplex structure.",
                            "</ul>",
                            "The results can be analyzed using interactive analytic tools. Annular visualization of centrality descriptors is allowed and accessible through the 'Annular Viz' tab. It is worth remarking that annular visualization is possible only <u>after centrality diagnostics have been calculated</u>.",
                            "<br>",
                            "<hr>",                            
                            "<h4>Visualization</h4>",
                            "<strong>'Visualization' tab to set up the visualization.</strong><br>",
                            "Several sub-tabs are present, each one allowing to set up particular graphic options.<br>",
                            "The 'Layout' sub-tab controls the algorithms responsible for the position of nodes and layers in the network. The 'Graphics' sub-tab controls graphic options such as the shift between layers, the lights for the openGL rendering, the plot title, and so on. Note that changing some of those options might require the layout to be re-applied. Options requiring re-application of layout are highlighted.<br>",
                            "The remaining sub-tabs control the appearance of 'Multiplex', 'Layers', 'Nodes' and 'Edges'.<br>",
                            "Once the set up is completed, click on the 'Render Network / Apply Viz Options' button.<br><br>",
                            "Note that it is possible to export the visualization either to a png or to a web page, by means of export buttons available in 'Export' sub-tab.<br><br>",
                            "<h4>Visualization of dynamics</h4>",
                            "This module allows to build nice animated visualizations corresponding to dynamical processes on the top of a multilayer network. For instance, one can visualize the movements of one (or more) random walker(s) in the network, or the spreading of an epidemics or of a meme in a social network, the traffic (and possible congestions) in a transport/communication network, etc.<br>",
                            "The idea is to feed the module with a 'timeline' file where the change of the state of nodes and edges in the multilayer network are specified at each time step. The 'state' of an object can be altered by changing its color and/or its size. For instance, in the case of an epidemics spreading in a country, the size of each node (e.g., a metapopulation describing a city) can be proportional to the population and the color can encode the amount of infected people. This description allows a wide variety of dynamics to be represented and visualized: for instance, setting the size of nodes and edges to zero when required, it is possible to visualize a time-varying multilayer network where nodes and edges appear or disappear over time.<br>",
                            "<u>Format of a timeline file:</u> (<a href='example_timeline.txt' target='_blank'>click here</a> to open an example)<br>",
                            "The first line of the file must specify the name of the correponding timeline attributes. Allowed attributes:",
                            "<ul>",
                            "<li> <strong>timeStep</strong>: [mandatory] numerical integer id to identify time steps",
                            "<li> <strong>labelStep</strong>: [mandatory] string specifying the snapshot label",
                            "<li> <strong>entity</strong>: [mandatory] string specifying if the object to modify is 'node' or 'edge'",
                            "<li> <strong>layerID</strong>: [mandatory] numerical integer id to identify each layer",
                            "<li> <strong>nodeID</strong>: [mandatory] numerical integer id if entity is 'node' and string (e.g., '3-7', corresponding to the link from node 3 to node 7) if entity is 'edge'.",
                            "<li> <strong>color</strong>: [mandatory] string specifying the color to be assigned",
                            "<li> <strong>sizeFactor</strong>: [mandatory] float value specifying the relative size of the entity, scaling with respect to the default size",
                            "</ul>",
                            "The order of the columns is not relevant. If the network has L layers and you want to include the aggregate network in the visualization, then use L+1 in the layerID field for it.<br><br>",
                            "To keep users with the freedom to use their favorite video making software, the output of muxViz consists of png files representing the temporal snapshots of the dynamics. Snapshots are saved in the folder 'export/timeline/project_name'. Successively, users can use their favorite software to merge the sequence of snapshots into a single video. We recommend to use <a href='https://www.ffmpeg.org/' target='_blank'>FFmpeg</a> with the following parameters:",
                            "<pre>ffmpeg -r 1  -i /path/xyz_%05d.png -c:v libx264 -pix_fmt yuv420p -r 25 output_file.mp4</pre>",
                            "<hr>",
                            "<h4>Reducibility</h4>",
                            "<strong>Reduce the dimensionality of the multiplex.</strong><br>",
                            "First, set up the 'Algorithms' to be used for the calculation. The output will be displayed in the 'Results' sub-tab.",
                            "<br><br>"
                            ))
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
                        'I am in debt with <a href="http://deim.urv.cat/~aarenas/" target="_blank">A. Arenas</a> for proposing this project, with <a href="http://people.maths.ox.ac.uk/porterm/" target="_blank">Mason A. Porter</a> and A. Sole-Ribalta for invaluable suggestions and feedbacks.',
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
                        '<span>muxViz</span> makes use of many packages and its license is compatible with the license of each used package. <span>muxViz</span> is Open Source and makes use of free software only: <a href="https://www.gnu.org/software/octave/" target="_blank">GNU Octave</a>, <a href="http://www.r-project.org/" target="_blank">R</a> (GNU GPLv2), <a href="http://netwiki.amath.unc.edu/GenLouvain/" target="_blank">Generalized Louvain Community Detection</a> (FreeBSD License, <a href="genlouvain_license.txt" target="_blank">see the original license</a>) and <a href="" target="_blank">muxNet</a> (Next release, GNU GPLv3 License, <a href="muxnet_license.txt" target="_blank">see the original license</a>).<br><br>',
                        'This code has no warranty whatsoever and any kind of support is provided. You are free to do what you like with this code as long as you leave this copyright in place. Please, explicitly cite <span>muxViz</span> if you find it useful for your visualizations and analyses.',
                        '<br><br>',
                        '(C) Copyright 2013-2015, Manlio De Domenico (manlio.dedomenico at urv.cat)',
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
            )
        )
    )
))

