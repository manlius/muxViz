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

Components = 0;
ComponentsSize = 0;

if strcmp(ConnectedComponentsType,"simple")
    [Components,ComponentsSize] = GetConnectedComponentsSimple(SupraAdjacencyMatrix,Layers,Nodes);
end

if strcmp(ConnectedComponentsType,"extended")
    [Components,ComponentsSize] = GetConnectedComponentsExtended(SupraAdjacencyMatrix,Layers,Nodes);
end

outputFile = strcat(AnalysisName,"_components_membership.txt");
dlmwrite (outputFile, Components', 'delimiter',' ')
printf("Multislice components output to: %s\n",outputFile);

%Calculation for the aggregate will be done inside R to speed up

