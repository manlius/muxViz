addpath("octave");

%import the muxOctaveLib
muxOctaveLib;

%import the configuration file
LayersList = {};
muxOctaveConfig;

NodesTensor = {}; 
Layers = 0;
[NodesTensor,Layers,Nodes] = BuildMultiplexFromFileList(LayersList,Flags,MaxNodes,FirstNodeLabel);
                
LayersTensor = BuildLayersTensor(Layers,Nodes,OmegaParameter,MultisliceType);
        
SupraAdjacencyMatrix = BuildSupraAdjacencyMatrix(NodesTensor,LayersTensor,Layers,Nodes);

if Layers > 1    
    [InterPearson,InterSpearman] = GetInterAssortativityTensor(SupraAdjacencyMatrix,Layers,Nodes,Flags,InterAssortativityType);
    outputFile = strcat(AnalysisName,"_interassortativity_pearson");
    outputFile = strcat(outputFile,InterAssortativityType);
    outputFile = strcat(outputFile,".txt");
    dlmwrite (outputFile, InterPearson,'delimiter',' ')
    printf("Multislice inter-assortativity (Pearson) output to: %s\n",outputFile);
end    
