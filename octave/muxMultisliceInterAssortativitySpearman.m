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


if Layers > 1    
    [InterPearson,InterSpearman] = GetInterAssortativityTensor(SupraAdjacencyMatrix,Layers,Nodes,Flags,InterAssortativityType);
    outputFile = strcat(AnalysisName,"_interassortativity_spearman");
    outputFile = strcat(outputFile,InterAssortativityType);
    outputFile = strcat(outputFile,".txt");
    dlmwrite (outputFile, InterSpearman,'delimiter',' ')
    printf("Multislice inter-assortativity (Spearman) output to: %s\n",outputFile);
end    
