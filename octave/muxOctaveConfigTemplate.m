%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARAMETER SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify the name for this analysis
AnalysisName = "biomuxViz";
isExtendedEdgesList = 0;

% Specify here the edges list corresponding to each layer
% (For ordered slices, the specified integer id will be used)
LayersList{1} = "Bench/100nodes/ab_sample1_N100.edges";
LayersList{2} = "Bench/100nodes/er0.5_sample2_N100.edges";
LayersList{3} = "Bench/100nodes/co4_sample3_N100.edges";
LayersList{4} = "Bench/100nodes/ab_sample4_N100.edges";

% In case you have an interconnected/interdependent multilayer, comment above and uncomment below
% and set isExtendedEdgesList = 1;
% Format is: node layer node layer [weight]
% MultiLayerEdgesListFile = "path/to/your/extended/edgelist";

% Flags to define the type of edges list: 
% D(irected), W(eighted) or U(ndirected with)D(irected input)
Flags = "UD";

% Number of nodes in the multiplex (0 means automatic detection)
MaxNodes = 0;

% Numerical label of the first node
FirstNodeLabel = 0;

% Parameters below valid only for edge-colored networks that are fully interconnected by muxviz
GammaParameter = 1;
OmegaParameter = 1;

% Type can be: "ordered" or "categorical" [Note: lowercase!]
% See http://netwiki.amath.unc.edu/GenLouvain/GenLouvain
% Ordered Multislice Matrix
% Define the cell array A of square symmetric NxN matrices of equal size each representing 
% one of the layers ordered, undirected network "slices". 
% Categorical Multislice Matrix
% The distinction between ordered slices and categorical slices manifests in the presence 
% of all-to-all identity arcs between slices.
% Valid only for edge-colored networks that are fully interconnected by muxviz
MultisliceType = "categorical";

% Type can be "OO", "II", "OI", "IO" or "TT" [Note: uppercase]
% where O = out-oing degree/strength, I = in-going degree/strength, T = total degree/strength

InterAssortativityType = "TT";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END OF PARAMETER SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

