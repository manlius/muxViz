addpath("octave");

%import the muxOctaveLib
muxOctaveLib;

%import the configuration file
LayersList = {};
muxOctaveConfig;

NodesTensor = {}; 
Layers = 0;
SupraAdjacencyMatrix = 0;

if isExtendedEdgesList
    [SupraAdjacencyMatrix,Layers,Nodes] = BuildSupraAdjacencyMatrixFromFile(MultiLayerEdgesListFile,Flags,MaxNodes,FirstNodeLabel);
    NodesTensor = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes);
else
    [NodesTensor,Layers,Nodes] = BuildMultiplexFromFileList(LayersList,Flags,MaxNodes,FirstNodeLabel);         
    LayersTensor = BuildLayersTensor(Layers,Nodes,OmegaParameter,MultisliceType);
    SupraAdjacencyMatrix = BuildSupraAdjacencyMatrix(NodesTensor,LayersTensor,Layers,Nodes);
endif

% Calculate the centralities

SupraTransitionMatrix = BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SupraAdjacencyMatrix,Layers,Nodes);

CentralityVector = GetOverallPageRankCentrality(SupraTransitionMatrix,Layers,Nodes);
outputFile = strcat(AnalysisName,"_centrality_pagerank.txt");
dlmwrite (outputFile, CentralityVector, 'delimiter',' ')
printf("Multislice centrality output to: %s\n",outputFile);

#aggregate
Aggregate = GetAggregateMatrix(NodesTensor,Layers,Nodes);
TransitionMatrix = BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(Aggregate,1,Nodes);
CentralityVector = GetOverallPageRankCentrality(TransitionMatrix,1,Nodes);
outputFile = strcat(AnalysisName,"_centrality_pagerank_aggregate.txt");
dlmwrite (outputFile, CentralityVector, 'delimiter',' ')
printf("Multislice centrality output to: %s\n",outputFile);
