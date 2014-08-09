library(shiny)
library(ShinyDash)
library(shinyIncubator)

#RGB colors table
#http://www.javascripter.net/faq/rgbtohex.htm
paletteWeb <- "<a href='http://www.colorschemer.com/online.html' target='_blank'>Online Color Palette</a>, <a href='http://colorbrewer2.org'>Online Color Scheme</a>"

paletteChoiceArray <- sort(row.names(brewer.pal.info[1]))


shinyUI(bootstrapPage(
    progressInit(),
#pageWithSidebar(    
    tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
    #headerPanel("muxViz Graphical User Interface"),
    headerPanel(""),
    helpText(HTML("<img src='img/logo_small.jpg' alt=''/>")),
    
    sidebarPanel(
        conditionalPanel(condition="input.conditionedPanels!=5",
            helpText(HTML("<h3>Analysis ID</h3>")),
            textInput("txtProjectName", label=HTML("<strong>* Assign an ID to this analysis:</strong>"), paste("muxViz_",as.character(format(Sys.time(), "%d-%m-%Y_%H%M%S")),sep="")),
            tags$hr(),
            helpText(HTML("<h3>muxViz Project</h3>")),
            
            helpText(HTML("Software released under <a href='http://www.gnu.org/licenses/gpl-3.0.html  target='_blank'>GPLv3</a>")),
            helpText(HTML("Developed by <a href='http://deim.urv.cat/~manlio.dedomenico/index.php target='_blank'>Manlio De Domenico</a><br><a href='http://deim.urv.cat' target='_blank'>School of Computer Science and Mathematics</a>, <a href='http://www.urv.cat' target='_blank'>Universitat Rovira i Virgili</a>")),
            helpText(HTML("Visit the <a href='http://deim.urv.cat/~manlio.dedomenico/muxviz.php' target='_blank'>project page</a> or the <a href='https://github.com/manlius/muxViz' target='_blank'>Github repo</a>"))
        ),
        conditionalPanel(condition="input.conditionedPanels==5",        
            helpText(HTML("<h3>muxViz Project</h3>")),
            
            helpText(HTML("Developed by <a href='http://deim.urv.cat/~manlio.dedomenico/index.php target='_blank'>Manlio De Domenico</a><br><a href='http://deim.urv.cat' target='_blank'>School of Computer Science and Mathematics</a>, <a href='http://www.urv.cat' target='_blank'>Universitat Rovira i Virgili</a>")),
            helpText(HTML("Visit the <a href='http://deim.urv.cat/~manlio.dedomenico/muxviz.php' target='_blank'>project page</a> or the <a href='https://github.com/manlius/muxViz' target='_blank'>Github repo</a>"))
        )
    ),
  
    mainPanel(        
            tabsetPanel(
                tabPanel("Import", 
                    tabsetPanel(
                        tabPanel("Config file", 
                            helpText(HTML("<h4>Open Configuration File</h4>")),
                            helpText(HTML("<strong><font color='#262626'>* Input format for the config. file:</font></strong>")),
                            checkboxInput('chkConfigFileHeader', 'Header', FALSE),
                            textInput("txtConfigFileSep", label=HTML("Separator:"), ";"),
                            fileInput('project_file', HTML('<strong>* Open the configuration file:</strong>'),
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                            ),
                        tabPanel("Import networks",                        
                            checkboxInput('chkEdgeListFileHeader', 'Header', FALSE),
                            textInput("txtEdgeListFileSep", label=HTML("Separator (default is one space):"), " "),
                            selectInput("selEdgeListType", HTML("<strong>* EdgeList Format:</strong>"), 
                                choices = c("Undirected", "Directed")),
                            checkboxInput("chkEdgeListWeighted", "Weighted", FALSE),
                            checkboxInput("chkEdgeListUndirectedBoth", "Both directions are specified (for undirected networks only)", FALSE),
                            checkboxInput("chkRESCALE_WEIGHT","Rescale weights by the minimum",FALSE),
                            checkboxInput('chkOutputEdgelistTable',HTML("Print edges lists in a table (<font color='red'>slow for large networks</font>)"),FALSE),
                            actionButton("btnImportNetworks", "Import Network")
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
                        helpText(HTML("<h4>Configuration file table</h4>")),
                        tableOutput("layersTable"), 
                        tags$hr(),
                        helpText(HTML("<h4>Summary for the multilayer network</h4>")),
                        conditionalPanel(condition="input.btnImportNetworks>0",
                            htmlWidgetOutput(
                                      outputId = 'projectSummaryHTML',
                                      HTML(paste(
                                        'Layers in the multiplex: <span id="sumLayers"></span><br>',
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
                            ),
                        tags$hr(),
                        value=1
                    ),
                tabPanel("Diagnostics", 
                    tabsetPanel(
                        tabPanel("Mux Set up",
                            helpText(HTML('Multiplex networks can be <strong>ORDINAL</strong> (interconnections exist only between pairs of adjacent layers) or <strong>CATEGORICAL</strong> (interconnections exist between all pairs of layers). Select the option below:')),
                            radioButtons('radMultiplexType', '',
                                c(Ordinal='MULTIPLEX_IS_ORDERED',
                                    Categorical='MULTIPLEX_IS_CATEGORICAL'),
                                    selected='MULTIPLEX_IS_CATEGORICAL'
                                ),
                            tags$hr(),
                            textInput("txtOmega", label=HTML("Strength of inter-layer connections:"), "1"),
                            textInput("txtGamma", label=HTML("Resolution parameter (for multislice community detection):"), "1")
                            ),
                        tabPanel("Correlation",
                            HTML('<h4>General diagnostics</h4>'),
                            actionButton("btnCalculateCorrelationDiagnostics", "Calculate Correlation Diagnostics"),
                            tags$hr(),
                            checkboxInput('chkMULTIPLEX_OVERLAPPING', 'Mean global overlapping', TRUE),
                            
                            checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_PEARSON', 'Inter-layer assortativity (Pearson correlation)', TRUE),
                            checkboxInput('chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN', 'Inter-layer assortativity (Spearman correlation)', TRUE),
                            selectInput("selAssortativityType", HTML("Assortativity type (T=Total, I=In-going, O=Out-going):"), 
                                choices = c("TT", "II", "OO", "IO", "OI")),
                            checkboxInput('chkEXPORT_MATRIX_PLOT', HTML('Export each layer as an image (<font color="red">slow for larger networks</font>)'), FALSE),
                            selectInput("selAssortativityTypeColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                choices = paletteChoiceArray),
                            tags$hr(),
                            HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectGlobalDiagnostics',
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
                                '<li> M. De Domenico et al, <i>Centrality in Interconnected Multilayer Networks</i>, arXiv:1311.2906 (2013) [<a href="http://arxiv.org/abs/1311.2906" target="_blank">Open</a>]',
                                '</ul>',
                                '<br>'
                                ))
                                )    
                            ),
                        tabPanel("Centrality",
                            HTML('<h4>Centrality</h4>'),
                            actionButton("btnCalculateCentralityDiagnostics", "Calculate Centrality Diagnostics"),
                            tags$hr(),
                            checkboxInput("chkNODE_CENTRALITY_MULTIPLEX","Use tensorial calculation (uncheck this for calculation in each layer separately)",TRUE),
                            checkboxInput("chkNODE_CENTRALITY_STRENGTH","Degree (strength for weighted networks; in-going, out-going and total)",TRUE),
                            checkboxInput("chkNODE_CENTRALITY_PAGERANK","PageRank",F),
                            checkboxInput("chkNODE_CENTRALITY_EIGENVECTOR","Eigenvector",F),
                            checkboxInput("chkNODE_CENTRALITY_HUB","Hub",F),
                            checkboxInput("chkNODE_CENTRALITY_AUTHORITY","Authority",F),
                            checkboxInput("chkNODE_CENTRALITY_KATZ","Katz",F),
                            tags$hr(),
                            HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectCentrality',
                                HTML(paste(
                                'This module calculates <a href="http://en.wikipedia.org/wiki/Centrality" target="_blank">centrality</a> of nodes in the network.',
                                '<br><br>',
                                'The diagnostics for single-layer networks are widely described in the literature: see <a href="http://en.wikipedia.org/wiki/http://en.wikipedia.org/wiki/Degree_(graph_theory)" target="_blank">Degree centrality</a>, <a href="http://en.wikipedia.org/wiki/PageRank" target="_blank">PageRank centrality</a>, <a href="http://en.wikipedia.org/wiki/Centrality#Eigenvector_centrality" target="_blank">Eigenvector centrality</a>, <a href="http://en.wikipedia.org/wiki/HITS_algorithm" target="_blank">Hub centrality</a>, <a href="http://en.wikipedia.org/wiki/HITS_algorithm" target="_blank">Authority centrality</a> and <a href="http://en.wikipedia.org/wiki/Katz_centrality" target="_blank">Katz centrality</a> for further information.',
                                '<br><br>',
                                '<hr>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> M. De Domenico et al, <i>Mathematical Formulation of Multilayer Networks</i>, Phys. Rev. X 3, 041022 (2013) [<a href="http://prx.aps.org/abstract/PRX/v3/i4/e041022" target="_blank">Open</a>]',
                                '<li> M. De Domenico et al, <i>Centrality in Interconnected Multilayer Networks</i>, arXiv:1311.2906 (2013) [<a href="http://arxiv.org/abs/1311.2906" target="_blank">Open</a>]',
                                '</ul>',
                                '<br>'
                                ))
                                )    
                            ),
                        tabPanel("Community",
                            HTML('<h4>Algorithm to be used for Community Detection</h4>'),
                            actionButton("btnCalculateCommunityDiagnostics", "Calculate Community Structure"),
                            tags$hr(),
                            checkboxInput('chkPERFORM_COMMUNITY_DETECTION', 'Perform community detection', TRUE),
                            radioButtons('radCommunityAlgorithm', '',
                                c(Multiplex='COMMUNITY_MULTIPLEX',
                                    Infomap='COMMUNITY_INFOMAP',
                                    Random_Walk_Trap='COMMUNITY_RANDOM_WALK_TRAP',
                                    Edge_Betweenness='COMMUNITY_EDGE_BETWEENNESS'),
                                    selected='COMMUNITY_MULTIPLEX'
                                ),
                            textInput("txtCOMMUNITY_MIN_SIZE",label="Color-code with the same RGB all nodes in communities smaller than (useful for evidencing larger communities, not valid for the multiplex):","1"),
                            tags$hr(),
                            HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectCommunity',
                                HTML(paste(
                                'This module unveils the <a href="http://en.wikipedia.org/wiki/Community_structure" target="_blank">community structure</a> of the network.',
                                '<h5>Multiplex</h5>',
                                'This method uses the multislice community detection proposed by <a href="http://www.sciencemag.org/content/328/5980/876" target="_blank">Mucha et al</a> to partition the network accounting for the interconnected topology. The option ORDINAL considers a multiplex with interconnections existing only between a layer and its adjacent layers (ordered as imported), while the option CATEGORICAL considers a multiplex with interconnections existing between all pairs of layers. See also <a href="http://netwiki.amath.unc.edu/GenLouvain/GenLouvain" target="_blank"></a> for further details.',
                                '<br>',
                                'It is worth mentioning that this method (and its generalization to more complex interconnected topologies) can be described using the same tensorial formulation (see <a href="http://prx.aps.org/abstract/PRX/v3/i4/e041022" target="_blank">De Domenico et al</a>) adopted for the calculation of centrality diagnostics. For the current implementation:<br>',
                                '<strong>References</strong>:',
                                '<ul>',
                                '<li> P. Mucha et al, <i>Community Structure in Time-Dependent, Multiscale, and Multiplex Networks</i>, Science 328, 876 (2010) [<a href="http://www.sciencemag.org/content/328/5980/876" target="_blank">Open</a>]',
                                '</ul>',
                                '<h5>Infomap</h5>',
                                'This method performs community detection in each layer separately using the Infomap algorithm proposed by <a href="http://www.pnas.org/content/105/4/1118" target="_blank">Rosvall and Bergstrom</a>. The community structure is found by <a href="http://en.wikipedia.org/wiki/Minimum_description_length" target="_blank">minimizing the expected description length</a> of a <a href="http://en.wikipedia.org/wiki/Random_walk" target="_blank">random walker</a> trajectory in each network. See <a href="http://igraph.sourceforge.net/doc/R/walktrap.community.html" target="_blank">igraph doc</a> for further details.',
                                '<h5>Random Walk Trap</h5>',
                                'This method performs community detection in each layer separately using the Walktrap algorithm proposed by <a href="http://link.springer.com/chapter/10.1007%2F11569596_31" target="_blank">Pons and Latapy</a>. The community structure is found by trying to identify densely connected subgraphs via <a href="http://en.wikipedia.org/wiki/Random_walk" target="_blank">random walks</a> (exploiting the fact that random walks tend to stay within the same community). See <a href="http://igraph.sourceforge.net/doc/R/infomap.html" target="_blank">igraph doc</a> for further details.',
                                '<h5>Edge Betweenness</h5>',
                                'This method performs community detection in each layer separately using the Edge Betweenness algorithm proposed by <a href="http://pre.aps.org/abstract/PRE/v69/i2/e026113" target="_blank">Newman and Girvan</a>. The community structure is found by exploiting the fact that edges connecting different communities are traversed by a larger number of shortest paths, i.e., all the ones from a module to another: gradually removing edges with highest edge-betweenness will provide the modular structure in each network. See <a href="http://igraph.sourceforge.net/doc/R/community.edge.betweenness.html" target="_blank">igraph doc</a> for further details.',
                                '<br>'
                                ))
                                )    
                            ),
                        #tabPanel("Components",
                        #    checkboxInput('chkPERFORM_COMPONENT_DETECTION', 'Find connected components', TRUE)
                        #    ),
                        tabPanel("Results/Tables",
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
                                    tags$hr()
                                ),                                
                            conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_PEARSON",
                                helpText(HTML("<h5>Inter-layer Assortativity: Pearson</h5>")),
                                imageOutput("interPearsonSummaryImage"),
                                htmlOutput("interPearsonSummaryTable"),
                                tags$hr()
                                ),                                
                            conditionalPanel(condition="input.btnCalculateCorrelationDiagnostics>0 && input.chkMULTIPLEX_INTERASSORTATIVITY_SPEARMAN",
                                helpText(HTML("<h5>Inter-layer Assortativity: Spearman</h5>")),
                                imageOutput("interSpearmanSummaryImage"),
                                htmlOutput("interSpearmanSummaryTable")
                                ),                                
                            tags$hr(),
                            HTML('<h4>Centrality diagnostics</h4>'),
                            conditionalPanel(condition="input.btnCalculateCentralityDiagnostics>0",
                                checkboxInput(inputId = "centralityTablePageable", label = "Pageable", TRUE),
                                conditionalPanel("input.centralityTablePageable==true",
                                        uiOutput("numOutputCentralityTableNodesPerPage")
                                        ),
                                htmlOutput("centralityTable")
                            ),
                            tags$hr(),
                            HTML('<h4>Communities</h4>'),
                            conditionalPanel(condition="input.btnCalculateCommunityDiagnostics>0",
                                htmlOutput("communitySummaryTable"),
                            tags$hr(),
                            checkboxInput(inputId = "communityTablePageable", label = "Pageable", TRUE),
                            conditionalPanel("input.communityTablePageable==true",
                                    uiOutput("numOutputCommunityTableNodesPerPage")
                                    ),  
                            htmlOutput("communityTable")
                            ),
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
                            tags$hr()
                            ),
                        tabPanel("Annular Viz",
                            HTML('<h4>Annular representation</h4>'),
                            actionButton("btnAnularViz", "Render Annular Plot"),
                            tags$hr(),
                            HTML("'<font color='red'>BE CAREFUL: this module is supposed to work *AFTER* you calculated diagnostics in the multiplex, using the tensorial formulation. If you try to use this module without calculating multiplex centralities, an error will be generated.</font><br><br>"),
                            radioButtons('radAnularVizCorrelationMethod', 'Correlation method for ring ordering:',
                                c(Spearman='ANULAR_VIZ_CORRELATION_SPEARMAN',
                                    Pearson='ANULAR_VIZ_CORRELATION_PEARSON',
                                    Jensen_Shannon_Divergence='ANULAR_VIZ_CORRELATION_JSD'),
                                    selected='ANULAR_VIZ_CORRELATION_SPEARMAN'
                                ),
                            textInput("txtANULAR_VIZ_BINS",label="Number of bins (tune the colors)","50"),
                            checkboxInput("chkANULAR_VIZ_LOG","Logarithmic binning",FALSE),
                            uiOutput("selAnularVizOutputFeatureID"),
                            uiOutput("selAnularVizOutputLayerID"),
                            textInput("txtANULAR_VIZ_RCORE",label="Radius of the core","0.3"),
                            textInput("txtANULAR_VIZ_RING_DISPLACEMENT",label="Distance between rings","0.01"),
                            checkboxInput("chkANULAR_VIZ_CELL_BORDER","Show cell border",FALSE),
                            checkboxInput("chkANULAR_VIZ_SHOW_NODE_LABEL","Show node IDs around the annular representation",FALSE),
                            textInput("txtANULAR_VIZ_FONT_SIZE",label="Font size:","1.5"),
                            selectInput("selAnularColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                choices = paletteChoiceArray),
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
                    ),
                tabPanel("Visualization", 
                    actionButton("btnRenderNetworks", "Render Network / Apply Viz Options"),
                    tags$hr(),
                    tabsetPanel(
                        tabPanel("Layout",
                            HTML('<h4>Algorithm to be used to visualize nodes in the multiplex network</h4>'),
                            actionButton("btnApplyLayout", "Apply Layout"),
                            tags$hr(),
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
                            radioButtons('radLayoutDimension', '',
                                c(Two_Dimensional='LAYOUT_DIMENSION_2D',
                                    Three_Dimensional='LAYOUT_DIMENSION_3D'),
                                    selected='LAYOUT_DIMENSION_2D'
                                ),
                            checkboxInput("chkPLOT_AS_EDGE_COLORED",HTML("Visualize as edge-colored multigraph (<font color='red'>Very experimental! Works only with centrality/community in the multiplex.</font>)"),FALSE),
                            tags$hr(),
                            HTML('<h4>Type of visualization</h4>'),
                            radioButtons('radLayoutType', '',
                                c(Multiplex='LAYOUT_MULTIPLEX',
                                    By_LayerID='LAYOUT_BY_LAYER_ID',
                                    Independent='LAYOUT_INDEPENDENT'),
                                    selected='LAYOUT_MULTIPLEX'
                                ),
                            #this is a dynamic object changing because of input
                            uiOutput("selOutputLayerID"),
                            textInput("txtLAYOUT_MAXITER", 
                                label=HTML("Maximum number of iterations:"), 
                                "1000"
                                ),    
                            tags$hr(),
                            HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectLayout',
                                HTML(paste(
                                '<strong>Suggestions for layout (see <a href="http://igraph.sourceforge.net/doc/R/layout.html" target="_blank">igraph layout doc</a> for further details):</strong><br>',
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
                        tabPanel("Graphics",
                            #HTML('<h4>Options for the rendering of the multiplex</h4>'),
                            textInput('txtPLOT_TITLE', label='Plot title:', ""),
                            textInput('txtPLOT_SUBTITLE', label='Plot subtitle:', ""),
                            textInput('txtPLOT_FOV', label='Default field of view (degrees):', "20"),
                            textInput('txtBACKGROUND_COLOR', label='Background color (any valid R type):', "white"),
                            textInput('txtLAYER_SHIFT', label=HTML('Shift layers (along horizontal axis to improve perspective, <font color="red">must apply the layout again</font>) by:'), "0.8"),
                            textInput('txtLAYER_SCALE', label=HTML('Scale layers (<font color="red">must apply the layout again</font>) by:'), "4"),
                            textInput('txtLAYER_SPACE', label=HTML('Space between layers (<font color="red">must apply the layout again</font>) by:'), "3"),
                            checkboxInput("chkPLOT_REMEMBER_ORIENTATION","Remember previous orientation in a new rendering",TRUE),
                            checkboxInput("chkPLOT_LIGHT","Add a light to the plot (to improve visualization):",FALSE),
                            textInput('txtPLOT_LIGHT_PHI', label='Phi coordinate (deg):', "20"),
                            textInput('txtPLOT_LIGHT_THETA', label='Theta coordinate (deg):', "30")
                            ),
                        tabPanel("Multiplex",
                            checkboxInput("chkINTERLINK_SHOW",HTML("Show inter-links (<font color='red'>resource consuming</font>, recommended for small networks):"),FALSE),
                            textInput('txtINTERLINK_SHOW_FRACTION', label='Show only this random fraction of inter-links (from 0 to 1):', "0.2"),
                            textInput('txtINTERLINK_COLOR', label='Inter-link color (any valid R type):', "black"),
                            textInput('txtINTERLINK_TYPE', label='Inter-link line style (any valid R type):', "dotted"),
                            textInput('txtINTERLINK_WIDTH', label='Inter-link width:', "1"),
                            textInput('txtINTERLINK_TRANSP', label='Inter-link transparency (from 0 to 1; 1 means full color):', "0.2"),
                            selectInput("selMultiplexColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                choices = append(as.vector(paletteChoiceArray),"random"))
                            ),
                        tabPanel("Layers",
                            checkboxInput("chkLAYER_SHOW","Show layers:",TRUE),
                            textInput('txtLAYER_LABEL_PREFIX', label='Layer label prefix (overwritten by label, if any, provided with the config file):', "L"),
                            textInput('txtLAYER_COLOR', label='Layer color (any valid R type):', "gray"),
                            textInput('txtLAYER_TRANSP', label='Layer transparency (from 0 to 1; 1 means full color):', "0.08"),
                            checkboxInput("chkGEOGRAPHIC_BOUNDARIES_SHOW","Show geographical boundaries if geographical layout is provided",TRUE),
                            
                            tags$hr(),
                            checkboxInput("chkAGGREGATE_SHOW",HTML("Show aggregate network as separate layer (<font color='red'>must apply the layout again</font>):"),TRUE),
                            textInput('txtLAYER_AGGREGATE_LABEL_PREFIX', label='Aggregate layer label:', "Aggregate"),
                            textInput('txtLAYER_AGGREGATE_COLOR', label='Aggregate layer color (any valid R type):', "blue"),
                            textInput('txtLAYER_AGGREGATE_TRANSP', label='Aggregate layer transparency (from 0 to 1; 1 means full color):', "0.08"),
                            checkboxInput("chkGEOGRAPHIC_BOUNDARIES_AGGREGATE_SHOW","Show geographical boundaries if geographical layout is provided",TRUE),

                            tags$hr(),
                            helpText(HTML("<h5>Common settings</h5>")),
                            checkboxInput("chkLAYER_ID_SHOW_TOPLEFT","Show labels on top-left:",FALSE),
                            checkboxInput("chkLAYER_ID_SHOW_BOTTOMLEFT","Show labels on bottom-left:",TRUE),
                            checkboxInput("chkLAYER_ID_SHOW_TOPRIGHT","Show labels on top-right:",FALSE),
                            checkboxInput("chkLAYER_ID_SHOW_BOTTOMRIGHT","Show labels on bottom-right:",FALSE),
                            textInput('txtLAYER_ID_FONTSIZE', label='Font size for labels:', "1.5"),
                            
                            selectInput("selOSMType", HTML("If geographical layout is provided, use the following background (requires Internet connection):</strong>"), 
                                choices = c("bing","mapbox","mapquest-aerial","osm","osm-bbike-german","osm-transport","stamen-toner","stamen-watercolor")),
                            helpText(HTML("<h5>Custom set of all geographical boundaries (<font color='red'>must apply the layout again</font>)</h5>")),
                            textInput('txtGEOGRAPHIC_LAT_MIN', label='Minimum latitude (default: automatic):', ""),
                            textInput('txtGEOGRAPHIC_LAT_MAX', label='Maximum latitude (default: automatic):', ""),
                            textInput('txtGEOGRAPHIC_LONG_MIN', label='Minimum longitude (default: automatic):', ""),
                            textInput('txtGEOGRAPHIC_LONG_MAX', label='Maximum longitude (default: automatic):', ""),
                            tags$hr()
                            ),
                        tabPanel("Nodes",
                            radioButtons('radNodeSizeType', 'Node size proportional to:',
                                c(Uniform='NODE_SIZE_PROPORTIONAL_TO_UNIFORM',
                                    Strength='NODE_SIZE_PROPORTIONAL_TO_STRENGTH',
                                    PageRank='NODE_SIZE_PROPORTIONAL_TO_PAGERANK',
                                    Eigenvector='NODE_SIZE_PROPORTIONAL_TO_EIGENVECTOR',
                                    Hub='NODE_SIZE_PROPORTIONAL_TO_HUB',
                                    Authority='NODE_SIZE_PROPORTIONAL_TO_AUTHORITY',
                                    Katz='NODE_SIZE_PROPORTIONAL_TO_KATZ'),
                                    selected='NODE_SIZE_PROPORTIONAL_TO_STRENGTH'
                                ),
                            radioButtons('radNodeSizeType2', 'Type of proportionality:',
                                c(Constant='NODE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                    Log='NODE_SIZE_PROPORTIONAL_TYPE_LOG',
                                    LogLog='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                    selected='NODE_SIZE_PROPORTIONAL_TYPE_LOGLOG'
                                ),
                            checkboxInput("chkNODE_ISOLATED_HIDE","Exclude isolated nodes from the visualization",TRUE),
                            tags$hr(),
                            textInput('txtNODE_DEFAULT_SIZE', label='Default size (used for fine tuning of Uniform, Log and LogLog option):', "2"),
                            textInput('txtNODE_TRANSP', label='Node transparency (from 0 to 1; 1 means full color):', "0.2"),
                            checkboxInput("chkNODE_LABELS_SHOW","Show nodes labels (recommended only for small networks):",FALSE),
                            textInput('txtNODE_LABELS_DISTANCE', label='Distance of labels from nodes:', "0.5"),
                            textInput('txtNODE_LABELS_FONT_SIZE', label='Size of nodes labels :', "1.5"),
                            textInput('txtNODE_LABELS_FONT_COLOR', label='Color of nodes labels (any valid R type):', "#2F2F2F"),
                            tags$hr(),
                            radioButtons('radNodeColor', 'Node color:',
                                c(Random='NODE_COLOR_RANDOM',
                                    Community='NODE_COLOR_COMMUNITY',
                                    #Component='NODE_COLOR_COMPONENT',
                                    TopRank='NODE_COLOR_TOPRANK'),
                                    selected='NODE_COLOR_COMMUNITY'
                                ),
                            textInput('txtNODE_COLOR_TOP', label='Number of top-ranked nodes to consider:', "5"),
                            textInput('txtNODE_COLOR_TOP_COLOR_TOP', label='Color of top-ranked nodes (any valid R type):', "#FF0000"),
                            textInput('txtNODE_COLOR_TOP_COLOR_OTHERS', label='Color of the other nodes (any valid R type):', "#F2F2F2"),         
                            checkboxInput("chkNODE_LABELS_SHOW_ONLY_TOP","Show nodes labels only for top-ranked nodes:",TRUE),                   
                            textInput('txtNODE_COLOR_TOP_LABELS_FONT_COLOR', label='Color of nodes labels (any valid R type):', "#000000"),
                            tags$hr()
                            ),
                        tabPanel("Edges",
                            textInput('txtEDGE_BENDING', label='Bending factor (0 means straight; max 1):', "0.2"),
                            radioButtons('radEdgeSizeType', 'Edge size proportional to:',
                                c(Uniform='EDGE_SIZE_PROPORTIONAL_TO_UNIFORM',
                                    Weight='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT'),
                                    selected='EDGE_SIZE_PROPORTIONAL_TO_WEIGHT'
                                ),
                            radioButtons('radEdgeSizeType2', 'Type of proportionality:',
                                c(Constant='EDGE_SIZE_PROPORTIONAL_TYPE_NORMAL',
                                    Log='EDGE_SIZE_PROPORTIONAL_TYPE_LOG',
                                    LogLog='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG'),
                                    selected='EDGE_SIZE_PROPORTIONAL_TYPE_LOGLOG'
                                ),
                            textInput('txtEDGE_DEFAULT_SIZE', label='Default size (used for fine tuning of Uniform, Log and LogLog option):', "0.6"),
                            textInput('txtEDGE_TRANSP', label='Edge transparency (from 0 to 1; 1 means full color):', "0.2"),
                            textInput('txtLAYER_ARROW_SIZE', label='Arrow size:', "0.5"),
                            textInput('txtLAYER_ARROW_WIDTH', label='Arrow width:', "0.5")
                            ),
                        tabPanel("Export",
                            actionButton("btnExportRendering","Export PNG"),
                            actionButton("btnExportRenderingWeb","Export for Web")
                            ),
                        tabPanel("Dynamics",
                            HTML('<h4>Visualization of dynamical processes</h4>'),
                            actionButton("btnRenderDynamicsSnapshots", "Render Dynamics Snapshots"),
                            actionButton("btnFFMPEGDynamicsSnapshots", "Make Video (require ffmpeg)"),
                            tags$hr(),
                            
                            helpText(HTML("<h4>Open Timeline File</h4>")),
                            helpText(HTML("<strong><font color='#262626'>* Input format for the timeline file:</font></strong>")),
                            #checkboxInput('chkTimelineFileHeader', 'Header', TRUE),
                            textInput("txtTimelineFileSep", label=HTML("Separator (default is one space):"), " "),
                            fileInput('timeline_file', HTML('<strong>* Open the timeline file:</strong>'),
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                                ),
                            actionButton("btnImportTimeline", "Import Timeline"),
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
                            ),
                            tags$hr(),
                            textInput("txtTimelineDefaultNodesSize", label=HTML("Default size of all nodes (leave this blank to use the Rendering setup):"), "20"),
                            textInput("txtTimelineDefaultNodesColor", label=HTML("Default color of all nodes (any valid R type; leave this blank to use the Rendering setup):"), "#959595"),
                            textInput("txtTimelineDefaultEdgesSize", label=HTML("Default size of all edges (leave this blank to use the Rendering setup):"), "1"),
                            textInput("txtTimelineDefaultEdgesColor", label=HTML("Default color of all edges (any valid R type; leave this blank to use the Rendering setup):"), "#959595"),
                            #tags$hr(),
                            #textInput("txtFFMPEG_PATH",label="Full path of your ffmpeg binary to make the video (if not valid, video will not be made):",""),
                            #textInput("txtFFMPEG_FLAGS",label="Parameters to be passed to ffmpeg (if not valid, video will not be made):",""),
                            tags$hr(),
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
                            )
                        ),
                    value=0
                    ),
                tabPanel("Reducibility", 
                    actionButton("btnCalculateReducibility", "Calculate Reducibility"),
                    #actionButton("btnExportReducibilityRendering","Export PNG"),
                    tags$hr(),
                    tabsetPanel(
                        tabPanel("Algorithms",
                            HTML('<h4>Algorithms to be used to calculate the correlation between layers and clustering</h4>'),
                            radioButtons('radReducibilityCorrelationMethod', '',
                                c(Jensen_Shannon_Divergence='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'),
                                    selected='REDUCIBILITY_METHOD_CORRELATION_JENSEN_SHANNON'
                                ),
                            tags$hr(),
                            selectInput("selReducibilityClusterMethod", HTML("Algorithm to be used for hierarchical clustering:</strong>"), 
                                choices = c("ward","single","complete","average","mcquitty","median","centroid")),
                            selectInput("selReducibilityColorPalette", HTML("Color palette to use (<a href='colorbrewer.html' target='_blank'>see palettes and their codes</a>):"), 
                                choices = paletteChoiceArray),
                            textInput("txtREDUCIBILITY_HEATMAP_FONT_SIZE",label="Font size:","1.5"),
                            tags$hr(),
                            HTML("<h3>Quick help</h3>"),
                            htmlWidgetOutput(
                                outputId = 'projectReducibilityCorrelation',
                                HTML(paste(
                                'An information-theoretical approach is adopted to reduce the dimensionality of the multiplex network, while minimizing information loss. This method allows to describe the network using a smaller number of layers, by aggregating the redundant ones.<br>',
                                'The metric distance is calculated by means of the <a href="http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence" target="_blank">Jensen-Shannon divergence</a> and <a href="http://en.wikipedia.org/wiki/Hierarchical_clustering" target="_blank">hierarchical clustering</a> of layers is employed according to the algorithm defined by the user among those available.<br>',
                                '<hr>',
                                '<strong>References:</strong><br>',
                                '<ul>',
                                '<li> M. De Domenico, V. Nicosia, A. Arenas and V. Latora, <i>Dimensionality reduction of multi-layer interconnected complex networks</i>, arXiv:1405.0425 (2014) [<a href="http://arxiv.org/abs/1405.0425" target="_blank">Open</a>]',
                                '</ul>'
                                    )
                                ))
                            ),
                        tabPanel("Results",
                            HTML('<h4>Hierarchical merging of the multiplex network</h4>'),
                            imageOutput("jsdMatrixSummaryImage",width = "100%", height = "700px"),
                            imageOutput("reducibilityDendrogramSummaryImage",width = "100%", height = "700px")
                            )
                        )
                    ),
                tabPanel("Help", 
                    htmlWidgetOutput(
                        outputId = 'tabHelp',
                            HTML(paste("<h4>Import</h4>",
                            "<strong>'Import' tab/'Config file' tab to select and open the configuration file.</strong><br>",
                            "The configuration file is a ASCII file including the list of layers to be included in a multiplex, the corresponding labels and the possible layout file to define node properties (e.g., ID, labels, geographical coordinates, etc).<br><br>",
                            "<u>Format of a configuration file:</u> (<a href='example_config.txt' target='_blank'>click here</a> to open an example)<br>",
                            "<pre>path_layer_X;label_layer_X;path_layout_layer_X</pre>",
                            "where<br>",
                            "<ul>",
                            "<li> <strong>path_layer_X</strong>: [mandatory] specify the path and the filename to the edges list to be used as layer",
                            "<li> <strong>label_layer_X</strong>: [optional] specify the label to be used in the rendering for that layer",
                            "<li> <strong>path_layout_layer_X</strong>: [optional] specify the path and the filename to the file containing information about nodes",
                            "</ul>",
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
                            "<strong>'Import' tab/'Import' tab to import the networks in your configuration file.</strong><br>",
                            "The network files are expected to be in edges list format, one file for each layer with format <br>",
                            "<pre>nodeID1 nodeID2</pre>",
                            "in the case of unweighted edges list, or<br>",
                            "<pre>nodeID1 nodeID2 weight</pre>",
                            "in the case of weighted edge list.<br>",
                            "The nodes ID are expected to refer to the same node in each layer. For instance, the ID 1234 with label 'myNode' is expected to indicate the same entity in all layers.",
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
                            "The results of each analysis will be reported/displayed in the 'Results/Tables' sub-tab. Annular visualization of centrality descriptors is allowed and accessible through the 'Annular Viz' tab. It is worth remarking that annular visualization is possible only <u>after centrality diagnostics have been calculated</u>.",
                            "<br>",
                            "<hr>",                            
                            "<h4>Visualization</h4>",
                            "<strong>'Visualization' tab to set up the visualization.</strong><br>",
                            "Several sub-tabs are present, each one allowing to set up particular graphic options.<br>",
                            "The 'Layout' sub-tab controls the algorithms responsible for the position of nodes in the network. The 'Graphics' sub-tab controls graphic options such as the shift between layers, the lights for the openGL rendering, the plot title, and so on. Note that changing some of those options might require the layout to be re-applied. Options requiring re-application of layout are highlighted.<br>",
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
                    ),
                tabPanel("Credits/License", 
                    htmlWidgetOutput(
                        outputId = 'tabCredits',
                        HTML(paste(
                        '<h3>License</h3>',
                        '<span>muxViz</span> makes use of many packages and its license is compatible with the license of each used package. <span>muxViz</span> is Open Source and makes use of free software only: <a href="https://www.gnu.org/software/octave/" target="_blank">GNU Octave</a>, <a href="http://www.r-project.org/" target="_blank">R</a> (GNU GPLv2), <a href="http://netwiki.amath.unc.edu/GenLouvain/" target="_blank">Generalized Louvain Community Detection</a> (FreeBSD License, <a href="genlouvain_license.txt" target="_blank">see the original license</a>) and <a href="" target="_blank">muxNet</a> (Next release, GNU GPLv3 License, <a href="muxnet_license.txt" target="_blank">see the original license</a>).<br><br>',
                        'This code has no warranty whatsoever and any kind of support is provided. You are free to do what you like with this code as long as you leave this copyright in place. Please, explicitly cite <span>muxViz</span> if you find it useful for your visualizations and analyses.',
                        '<br><br>',
                        '(C) Copyright 2013-2014, Manlio De Domenico (manlio.dedomenico at urv.cat)',
                        '<br><br>',
                        '<span>muxViz</span> is free software: you can redistribute it and/or modify it under the terms of the <a href="http://www.gnu.org/licenses/gpl-3.0.html" target="_blank">GNU General Public License</a> as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.',
                        '<br><br>',
                        '<span>muxViz</span> is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.',
                        '<br><br>',
                        'You should have received a copy of the GNU General Public License along with the package. If not, see <a href="http://www.gnu.org/licenses/" target="_blank">www.gnu.org/licenses/</a>.',
                        tags$hr(),
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
                    ),
                id = "conditionedPanels"
            )
        )
))

