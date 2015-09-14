addpath("octave");

%import the muxOctaveLib
muxOctaveLib;

%import the configuration file
LayersList = {};
muxOctaveConfig;

%note that the only analysis supported here is by interlinking edge-colored graphs in ordered or categorical way

NodesTensor = {}; 
Layers = 0;
[NodesTensor,Layers,Nodes] = BuildMultiplexFromFileList(LayersList,Flags,MaxNodes,FirstNodeLabel);

[S,Q] = GetMultisliceCommunityGeneralizedLouvain(NodesTensor, Layers, Nodes, GammaParameter, OmegaParameter, MultisliceType);

MembershipMatrix = uint32(S);
outputFile = strcat(AnalysisName,"_community_membership.txt");
dlmwrite (outputFile, MembershipMatrix, 'delimiter',' ')
printf("Multislice community membership output to: %s\n",outputFile);
outputFile = strcat(AnalysisName,"_community_modularity.txt");
dlmwrite (outputFile, Q)
printf("Multislice community modularity output to: %s\n",outputFile);

#aggregate
Aggregate = {};
Aggregate{1} = GetAggregateMatrix(NodesTensor,Layers,Nodes);
[S,Q] = GetMultisliceCommunityGeneralizedLouvain(Aggregate, 1, Nodes, GammaParameter, OmegaParameter, MultisliceType);

MembershipMatrix = uint32(S);
outputFile = strcat(AnalysisName,"_community_membership_aggregate.txt");
dlmwrite (outputFile, MembershipMatrix, 'delimiter',' ')
printf("Multislice community membership output to: %s\n",outputFile);
outputFile = strcat(AnalysisName,"_community_modularity_aggregate.txt");
dlmwrite (outputFile, Q)
printf("Multislice community modularity output to: %s\n",outputFile);
