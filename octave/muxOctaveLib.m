%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MuxNetLib: Octave library for Multiplex Network Analysis in muxViz
%
% Version: 0.1
% Last update: May 2014
% Authors: Manlio De Domenico
%
% History:
%
% May 2014: First release, including part of muxNet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [A,N] = loadNetworkFromFile(fileName,Flags,Nodes=0,FirstNode=0)
    A = load(fileName);
    
    %if the first node is numbered by zero shift of 1 unit to relabel
    if FirstNode==0
        A(:,1) = A(:,1) + 1; 
        A(:,2) = A(:,2) + 1; 
    endif
    
    if Nodes==0
        N = max(max(A(:,1)),max(A(:,2)));
    else
        N = Nodes;
    endif
    
    if max(ismember(Flags, 'W'))==0
        %input is assumed to be an unweighted edge list
        %add a column of ones as weight for connected nodes
        %if there are more columns, we have to remove them
        
        if columns(A)>2
            A = A(:,1:2);
        endif

        A = [A 1.0*ones(size(A,1),1)];
    else
        if columns(A)>3
            A = A(:,1:3);
        endif
    endif

    A = spconvert(A);

    A(size(A,1)+1:N,size(A,2)+1:N) = 0;

    if ismember("D",Flags) && ismember("U",Flags)
        %input is undirected but provided in directed shape, we need to sum the transpose
        fprintf(2,"#Input is undirected but in directed shape\n");
        A = A + A';
    endif
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [NodesTensor,Layers,Nodes] = BuildMultiplexFromFileList(LayersList,Flags,Nodes=0,FirstNode=0)    
    % for input from different folders and put the full path in the LayerList
    % Flags must be a cell array defining the network type: D(irected), W(eighted), U(ndirected with)D(irected input)
    
    NodesTensor = {};
    Ni = {};

    Layers = length(LayersList);
    for i = 1:Layers
        if strcmp(LayersList{i},"NULL")
            if Nodes == 0
                fprintf(2,"\tBuildMultiplexFromFileList: ERROR! You requested a null layer, without specifying the number of nodes. Aborting process.\n");
                exit
            end
            
            Ni{i} = Nodes;
            NodesTensor{i} = sparse(Nodes,Nodes);

            Ei = 0;
            file = "Null layer";
            printf("#Layer %d: %d Nodes %d Edges (file: %s)\n",i,Ni{i},Ei,file);
        else
            [NodesTensor{i},Ni{i}] = loadNetworkFromFile(LayersList{i},Flags,Nodes,FirstNode);

            path = cellstr(strsplit (LayersList{i}, "/"));
            file = path{length(path)};
            Ei = sum(sum(NodesTensor{i}>0));
            printf("#Layer %d: %d Nodes %d Edges (file: %s)\n",i,Ni{i},Ei,file);
        end
        
            
        %check that the number of nodes in each layer is the same
        if i>1
            if Ni{i} != Ni{i-1}
                error("    BuildMultiplexFromFileList: ERROR! The number of nodes mismatches: %d (layer %d) vs %d (layer %d)\n",Ni{i},i,Ni{i-1},i-1);
                %pause;
            end
        end
    end    

    printf("#Flags: %s\n",Flags);

    % if everything is fine, we assign the number of nodes
    Nodes = Ni{1};

endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function LayersTensor = BuildLayersTensor(Layers,Nodes,OmegaParameter,MultisliceType)
    %Build the network of layers used to build the multiplex
    if Layers>1
        if strcmp(MultisliceType,"ordered")
            LayersTensor = (diag(ones(1,Layers-1),1) + diag(ones(1,Layers-1),-1))*OmegaParameter;
        end
        if strcmp(MultisliceType, "categorical")
            LayersTensor = ones(Layers,Layers)*OmegaParameter;
            LayersTensor = LayersTensor - diag(diag(LayersTensor));
        end
    else
        LayersTensor = 0;
        fprintf(2,"--> I will proceed with algorithm for one layer\n");
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function SupraAdjacencyMatrix = BuildSupraAdjacencyMatrix(NodesTensor,LayersTensor,Layers,Nodes)
    Identity = speye(Nodes);
    
    %simple and easy way, probably the correct one
    SupraAdjacencyMatrix = sparse(blkdiag(NodesTensor{}) + kron(LayersTensor,Identity));
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [QMatrix,LMatrix,Eigenvalues] = SolveEigenvalueProblem(Matrix)
    [QMatrix,LMatrix] = eig(Matrix);
    Eigenvalues=sort(diag(LMatrix));
endfunction

function [QMatrix,LMatrix] = GetLargestEigenv(Matrix)
    %This flag force the library to use methods to find approximated leading eigenvalue/vector
    %However, empirical evidence shows that such methods (coming with octave) are not so
    %stable, therefore if the final output looks "weird", a full exact method for the calculation
    %should be used. Unfortunately, the exact method will heavily slow down the computation
    %Use with care at your own risk
    UseFastMethodLargestEigenvalue = 0;

    %we must distinguish between symmetric and nonsymmetric matrices to have correct results

    if !UseFastMethodLargestEigenvalue
        [QMatrix,LMatrix] = eig(Matrix);
        [LambdaVector,IdxVector]=sort(diag(LMatrix));
        Idx = length(LambdaVector);
        LeadingEigenvalue = LambdaVector(Idx);
        LeadingEigenvector = QMatrix(:,IdxVector(Idx));
                
        QMatrix = LeadingEigenvector;
        LMatrix = LeadingEigenvalue;
    else
        if all(all(Matrix == Matrix'))
            %symmetric
            [QMatrix,LMatrix] = eigs(Matrix,1,'la');
        else
            %asymmetric
            [QMatrix,LMatrix] = eigs(Matrix,1,'lr');
        endif
        
        %check if the eigenvector has all negative components.. in that case we change the sign
        %first, set to zero everything that is so small that can create problems even if it compatible with zero
        QMatrix(find(QMatrix>-1e-12 & QMatrix<1e-12)) = 0;
        %now verify that all components are negative and change sign
        if all(floor(QMatrix<0) + floor(QMatrix==0))
            QMatrix = -QMatrix;
        endif

    endif

endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [NodesTensor] = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes)
    % create the nodes tensor from a supradajcency matrix, ie, extracts diagonal blocks
    
    NodesTensor = {};

    for i = 1:Layers
        NodesTensor{i} = sparse(SupraAdjacencyMatrix(1+ (i-1)*Nodes:i*Nodes,1+ (i-1)*Nodes:i*Nodes));
    end
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [BlockTensor] = SupraAdjacencyToBlockTensor(SupraAdjacencyMatrix,Layers,Nodes)
    % create the nodes tensor from a supradajcency matrix, ie, extracts all blocks
    
    BlockTensor = {};

    for i = 1:Layers
        for j = 1:Layers
            %BlockTensor{(i-1)*Layers + j} = SupraAdjacencyMatrix(1+ (i-1)*Nodes:i*Nodes,1+ (j-1)*Nodes:j*Nodes);
            BlockTensor{i,j} = sparse(SupraAdjacencyMatrix(1+ (i-1)*Nodes:i*Nodes,1+ (j-1)*Nodes:j*Nodes));
        endfor
    endfor
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function Aggregate = GetAggregateMatrix(NodesTensor,Layers,Nodes)    
    Aggregate = NodesTensor{1};
    
    for alpha = 2:Layers
        Aggregate += NodesTensor{alpha};
    endfor
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reducibility of Multilayer Networks 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function LaplacianMatrix = GetLaplacianMatrix(AdjacencyMatrix)
    %Calculate the laplacian matrix from an adjacency matrix
    
    N = length(AdjacencyMatrix);
    u = ones(N,1);

    %laplacian
    LaplacianMatrix = diag(AdjacencyMatrix*u) - AdjacencyMatrix;
    
    %check
    if sum(LaplacianMatrix*u) > 1.e-8
        error("ERROR! The Laplacian matrix has rows that don't sum to 0. Aborting process.\n");
        sum(LaplacianMatrix*u)
        %pause;
    endif
endfunction


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function DensityMatrix = BuildDensityMatrix(AdjacencyMatrix)
    %Calculate the density matrix from an adjacency matrix
    %   References: 
    %   S. L. Braunstein, S. Ghosh, S. Severini, Annals of Combinatorics 10, No 3, (2006)
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    DensityMatrix = GetLaplacianMatrix(AdjacencyMatrix);

    %normalize to degree sum
    DensityMatrix = DensityMatrix/(trace(DensityMatrix));
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function Eigenvalues = GetEigenvaluesOfDensityMatrix(DensityMatrix)
    %Calculate the eigenvalues of a density matrix
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    Eigenvalues = eig(DensityMatrix);

    %check that eigenvalues sum to 1
    if abs(sum(Eigenvalues)-1)>1e-8
        error("ERROR! Eigenvalues dont sum to 1! Aborting process.");
    endif
endfunction    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function Eigenvalues = GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix(AdjacencyMatrix)
    DensityMatrix = BuildDensityMatrix(AdjacencyMatrix);
    Eigenvalues = GetEigenvaluesOfDensityMatrix(DensityMatrix);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [RenyiEntropy] = GetRenyiEntropyFromAdjacencyMatrix(AdjacencyMatrix, q)
    %Calculate the quantum Renyi entropy of a network
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    %   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, arXiv:1405.0425 (2014)

    Eigenvalues = GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix(AdjacencyMatrix);

    if q==1.
        %Von Neuman quantum entropy
        RenyiEntropy = -sum(Eigenvalues(Eigenvalues>0).*log(Eigenvalues(Eigenvalues>0)));
    else
        %Renyi quantum entropy
        RenyiEntropy = (1 - sum(Eigenvalues(Eigenvalues>0).^q))/(q-1);
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [JSD] = GetJensenShannonDivergence(AdjacencyMatrix1,AdjacencyMatrix2,VNEntropy1,VNEntropy2)
    %Calculate the Jensen-Shannon Divergence of two networks
    %   References: 
    %   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, arXiv:1405.0425 (2014)


    %M = 0.5 * (RHO + SIGMA)
    %JSD: 0.5 * DKL( RHO || M ) + 0.5 * DKL( SIGMA || M )
    %DKL( A || B ) = tr[ A log A - A log B ] = -entropy(A) - tr[ A log B ]
    %
    %JSD: 0.5 * ( -entropy(RHO) - entropy(SIGMA) - tr[ RHO log M ] - tr[ SIGMA log M ] )
    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] - tr[ M log M ] )
    %         -0.5 * [ entropy(RHO) + entropy(SIGMA) ] + entropy(M) 

    DensityMatrix1 = BuildDensityMatrix(AdjacencyMatrix1);
    DensityMatrix2 = BuildDensityMatrix(AdjacencyMatrix2);
    DensityMatrixM = (DensityMatrix1 +DensityMatrix2)/2.;
        
    EigenvaluesM = eig(DensityMatrixM);
    CrossEntropyM = -sum(EigenvaluesM(EigenvaluesM>0).*log(EigenvaluesM(EigenvaluesM>0)));
            
    JSD = CrossEntropyM - 0.5*(VNEntropy1 + VNEntropy2);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Topological Descriptors of Multilayer Networks 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [AvGlobOverl] = GetAverageGlobalOverlapping(SupraAdjacencyMatrix,Layers,Nodes)
    %   References: 
    %   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, arXiv:1405.0425 (2014)

    if Layers==1
        fprintf(2,"GetAverageGlobalOverlapping:ERROR! At least two layers required. Aborting process.\n");
        exit;
    endif

    NodesTensor = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes);

    O = min(NodesTensor{1},NodesTensor{2});
    NormTotal = sum(sum(NodesTensor{1}));
    
    %assuming that LayerTensor is an undirected clique
    for l = 2:Layers
        O = min(O,NodesTensor{l});
        NormTotal = NormTotal + sum(sum(NodesTensor{l}));
    end

    AvGlobOverl = Layers*sum(sum(O))/NormTotal;
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [AvGlobOverlMatrix] = GetAverageGlobalOverlappingMatrix(SupraAdjacencyMatrix,Layers,Nodes)
    %   References: 
    %   M. De Domenico, V. Nicosia, A. Arenas, V. Latora, arXiv:1405.0425 (2014)

    if Layers==1
        fprintf(2,"GetAverageGlobalOverlappingMatrix:ERROR! At least two layers required. Aborting process.\n");
        exit;
    endif

    NodesTensor = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes);

    for l1 = 1:Layers
        AvGlobOverlMatrix(l1,l1) = 1;
        Norm1 = sum(sum(NodesTensor{l1}));
        for l2 = (l1+1):Layers
            O = min(NodesTensor{l1},NodesTensor{l2});
            AvGlobOverlMatrix(l1,l2) = 2*sum(sum(O))/(Norm1 + sum(sum(NodesTensor{l2})));
            AvGlobOverlMatrix(l2,l1) = AvGlobOverlMatrix(l1,l2);
        end
    end
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [InterPearson,InterSpearman] = GetInterAssortativityTensor(SupraAdjacencyMatrix,Layers,Nodes,Flags,Type)

    if Layers==1
        fprintf(2,"GetInterAssortativityTensor: ERROR! At least two layers required. Aborting process.\n");
        exit;
    endif

    NodesTensor = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes);

    InterPearson = sparse(Layers,Layers);
    InterSpearman = sparse(Layers,Layers);

    if strcmp(Type,"IO") || strcmp(Type,"OI")
        InDegree = {};
        OutDegree = {};

        for l = 1:Layers
            InDegree{l} = GetMultiInDegree(NodesTensor{l},1,Nodes,Flags);
            OutDegree{l} = GetMultiOutDegree(NodesTensor{l},1,Nodes,Flags);
        end

        for l1 = 1:Layers
            for l2 = 1:Layers
                InterPearson(l1,l2) = cor(InDegree{l1},OutDegree{l2});
                InterSpearman(l1,l2) = spearman(InDegree{l1},OutDegree{l2});
            end
        end

        if strcmp(Type,"OI")
            InterPearson = InterPearson';
            InterSpearman = InterSpearman';
        endif
    else
        Degree = {};
        
        if strcmp(Type,"OO")
            for l = 1:Layers
                Degree{l} = GetMultiOutDegree(NodesTensor{l},1,Nodes,Flags);
            end
        endif
    
        if strcmp(Type,"II")
            for l = 1:Layers
                Degree{l} = GetMultiInDegree(NodesTensor{l},1,Nodes,Flags);
            end
        endif
        
        if strcmp(Type,"TT")
            for l = 1:Layers
                Degree{l} = GetMultiDegree(NodesTensor{l},1,Nodes,Flags);
            end
        endif
        
        
        for l1 = 1:Layers
            InterPearson(l1,l1) = 1;
            InterSpearman(l1,l1) = 1;
            for l2 = (l1+1):Layers
                InterPearson(l1,l2) = cor(Degree{l1},Degree{l2});
                InterSpearman(l1,l2) = spearman(Degree{l1},Degree{l2});
                
                InterPearson(l2,l1) = InterPearson(l1,l2);
                InterSpearman(l2,l1) = InterSpearman(l1,l2);
            end
        end
    endif

endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiDegreeVector = GetMultiDegree(SupraAdjacencyMatrix,Layers,Nodes,Flags)
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    
    MultiInDegreeVector = GetMultiInDegree(SupraAdjacencyMatrix,Layers,Nodes,Flags);
    MultiOutDegreeVector = GetMultiOutDegree(SupraAdjacencyMatrix,Layers,Nodes,Flags);
    
    if ismember("U",Flags)
        MultiDegreeVector = (MultiInDegreeVector + MultiOutDegreeVector)/2;
    else
        MultiDegreeVector = MultiInDegreeVector + MultiOutDegreeVector;
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiOutDegreeVector = GetMultiOutDegree(SupraAdjacencyMatrix,Layers,Nodes,Flags)    
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    
    if ismember("U",Flags)        
        %we proceed by considering the interlayers separately
        BlockTensor = SupraAdjacencyToBlockTensor(SupraAdjacencyMatrix,Layers,Nodes);
        
        MultiOutDegreeVector = sparse(Nodes,1);

        %with the matrix U we reweight interlinks corresponding to same replicas
        U = ones(Nodes,Nodes);
        U(logical(speye(size(U)))) = 1/2;

        for i = 1:Layers
            for j = 1:Layers
                if i==j
                    MultiOutDegreeVector += (BlockTensor{i,j}-diag(diag(BlockTensor{i,j})))*ones(Nodes,1) + diag(BlockTensor{i,j})*0.5;
                else
                    MultiOutDegreeVector += (BlockTensor{i,j} .* U)*ones(Nodes,1);
                endif
            endfor
        endfor
    else
        SupraDegree = SupraAdjacencyMatrix*ones(Nodes*Layers,1); 

        MultiOutDegreeVector = sum(reshape(SupraDegree,Nodes,Layers),2);
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiInDegreeVector = GetMultiInDegree(SupraAdjacencyMatrix,Layers,Nodes,Flags)
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    
    if ismember("U",Flags)
        %using the following would consider multiple times the interlinks
        %SupraDegree = (SupraAdjacencyMatrix*ones(Nodes*Layers,1) + (ones(1,Nodes*Layers)*SupraAdjacencyMatrix)')/2;
        
        %we proceed by considering the interlayers separately
        BlockTensor = SupraAdjacencyToBlockTensor(SupraAdjacencyMatrix,Layers,Nodes);
        
        MultiInDegreeVector = sparse(Nodes,1);

        %with the matrix U we reweight interlinks corresponding to same replicas
        U = ones(Nodes,Nodes);
        U(logical(speye(size(U)))) = 1/2;

        for i = 1:Layers
            for j = 1:Layers
                if i==j
                    MultiInDegreeVector += (ones(1,Nodes)*(BlockTensor{i,j}-diag(diag(BlockTensor{i,j}))))' + diag(BlockTensor{i,j})*0.5;
                else
                    MultiInDegreeVector += (ones(1,Nodes)*(BlockTensor{i,j} .* U))';
                endif
            endfor
        endfor
    else
        SupraDegree = (ones(1,Nodes*Layers)*SupraAdjacencyMatrix)';

        MultiInDegreeVector = sum(reshape(SupraDegree,Nodes,Layers),2);
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiDegreeVector = GetMultiDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,Flags)
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    MultiInDegreeVector = GetMultiInDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,Flags);
    MultiOutDegreeVector = GetMultiOutDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,Flags);
    
    if ismember("U",Flags)
        MultiDegreeVector = (MultiInDegreeVector + MultiOutDegreeVector)/2;
    else
        MultiDegreeVector = MultiInDegreeVector + MultiOutDegreeVector;
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiOutDegreeVector = GetMultiOutDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,Flags)    
    %this degree include multiple times the interlinks
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)


    SupraDegree = SupraAdjacencyMatrix*ones(Nodes*Layers,1);    
    MultiOutDegreeVector = sum(reshape(SupraDegree,Nodes,Layers),2);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function MultiInDegreeVector = GetMultiInDegreeSum(SupraAdjacencyMatrix,Layers,Nodes,Flags)
    %this degree include multiple times the interlinks
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)

    
    SupraDegree = (ones(1,Nodes*Layers)*SupraAdjacencyMatrix)';    
    MultiInDegreeVector = sum(reshape(SupraDegree,Nodes,Layers),2);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function CentralityVector = GetOverallKatzCentrality(SupraAdjacencyMatrix,Layers,Nodes)
    %we pass the transpose of the transition matrix to get the left eigenvectors
    %   References: 
    %   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, arXiv:1311.2906 (2013)
    
    [QMatrix,LMatrix] = GetLargestEigenv(SupraAdjacencyMatrix');
    LeadingEigenvalue = LMatrix;
   
    %Katz kernel tensor
    deltaTensor = kron(speye(Nodes,Nodes),speye(Layers,Layers));
    
    %this ensures convergence of the Katz kernel tensor
    a = 0.99999/abs(LeadingEigenvalue);

    KatzKernelTensor = inv(deltaTensor - a*SupraAdjacencyMatrix);

    KatzCentralitySupraVector = KatzKernelTensor * ones(Nodes*Layers,1);
    CentralityVector = sum(reshape(KatzCentralitySupraVector,Nodes,Layers),2);    
    CentralityVector = CentralityVector/max(CentralityVector);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function CentralityVector = GetOverallEigenvectorCentrality(SupraAdjacencyMatrix,Layers,Nodes)
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    %   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, arXiv:1311.2906 (2013)
    
    %we pass the transpose of the transition matrix to get the left eigenvectors
    [LeadingEigenvector,LeadingEigenvalue] = GetLargestEigenv(SupraAdjacencyMatrix');
        
    %LeadingEigenvector = LeadingEigenvector / sum(LeadingEigenvector);
    CentralityVector = sum(reshape(LeadingEigenvector,Nodes,Layers),2);
    CentralityVector = CentralityVector/max(CentralityVector);

endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function CentralityVector = GetOverallHubCentrality(SupraAdjacencyMatrix,Layers,Nodes)
    %see review http://arxiv.org/pdf/0805.3322v2.pdf
    %   References: 
    %   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, arXiv:1311.2906 (2013)
    
    %build the A A'
    SupraMatrix = SupraAdjacencyMatrix*(SupraAdjacencyMatrix');

    %we pass the matrix to get the right eigenvectors
    %to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    %this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    %leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    [LeadingEigenvector,LeadingEigenvalue] = GetLargestEigenv(SupraMatrix+eps);

    %LeadingEigenvector = LeadingEigenvector / sum(LeadingEigenvector);
    CentralityVector = sum(reshape(LeadingEigenvector,Nodes,Layers),2);
    CentralityVector = CentralityVector/max(CentralityVector);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function CentralityVector = GetOverallAuthCentrality(SupraAdjacencyMatrix,Layers,Nodes)
    %see review http://arxiv.org/pdf/0805.3322v2.pdf
    %   References: 
    %   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, arXiv:1311.2906 (2013)


    %build the A' A
    SupraMatrix = (SupraAdjacencyMatrix') * SupraAdjacencyMatrix;

    %we pass the matrix to get the right eigenvectors
    %to deal with the possible degeneracy of the leading eigenvalue, we add an eps to the matrix
    %this ensures that we can apply the Perron-Frobenius theorem to say that there is a unique
    %leading eigenvector. Here we add eps, a very very small number (<1e-8, generally)
    [LeadingEigenvector,LeadingEigenvalue] = GetLargestEigenv(SupraMatrix+eps);

    CentralityVector = sum(reshape(LeadingEigenvector,Nodes,Layers),2);
    CentralityVector = CentralityVector/max(CentralityVector);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [S,Q] = GetMultisliceCommunityGeneralizedLouvain(NodesTensor, Layers, Nodes, GammaParameter, OmegaParameter, Type)
    % This function is an interface between the genlouvain (see the corresponding function
    % for reference and license) and muxNet (see the above functions for reference and
    % license)
    
    % Type can be: "ordered" or "categorical"
    % See http://netwiki.amath.unc.edu/GenLouvain/GenLouvain
    
    % Ordered Multislice Matrix
    % Define the cell array A of square symmetric NxN matrices of equal size each representing 
    % one of the layers ordered, undirected network "slices". 
    
    % Categorical Multislice Matrix
    % The distinction between ordered slices and categorical slices manifests in the presence 
    % of all-to-all identity arcs between slices.
    
    if strcmp(Type,"ordered")
        B = spalloc(Nodes*Layers,Nodes*Layers,(Nodes+Layers)*Nodes*Layers);
    end
    if strcmp(Type, "categorical")
        B = spalloc(Nodes*Layers,Nodes*Layers,Nodes*Nodes*Layers+2*Nodes*Layers);
    end

    twomu = 0;
    
    for s = 1:Layers
        k = sum(NodesTensor{s});
        twom = sum(k);
        twomu = twomu + twom;
        indx = [1:Nodes] + (s-1)*Nodes;
        B(indx,indx) = NodesTensor{s} - GammaParameter* k' * k /twom;
    end

    if strcmp(Type,"ordered")
        twomu = twomu + Layers*OmegaParameter*Nodes*(Layers-1);
        all2all = Nodes*[(-Layers+1):-1,1:(Layers-1)];
        B = B + OmegaParameter*spdiags(ones(Nodes*Layers,2*Layers-2),all2all,Nodes*Layers,Nodes*Layers);
    end
    if strcmp(Type, "categorical")
        twomu = twomu + 2*OmegaParameter*Nodes*(Layers-1);
        B = B + OmegaParameter*spdiags(ones(Nodes*Layers,2),[-Nodes,Nodes],Nodes*Layers,Nodes*Layers);
    end
        
    [S,Q] = genlouvain(B);
    Q = Q/twomu;
    Q = full(Q);
    S = reshape(S,Nodes,Layers);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Random Walk in Multilayer Networks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function SupraTransitionMatrix = BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SupraAdjacencyMatrix,Layers,Nodes)
    %   References: 
    %   M. De Domenico et al, Phys. Rev. X 3, 041022 (2013)
    %   M. De Domenico, A. Sole-Ribalta, S. Gomez, A. Arenas, arXiv:1306.0519 (2013)

    
    Order = Layers*Nodes;
    #SupraUnitVector = ones(Order,1);

    SupraAdjacencyMatrix = sparse(SupraAdjacencyMatrix);
    SupraStrengthMatrix = sum(SupraAdjacencyMatrix,2);
    DisconnectedNodes = size(SupraStrengthMatrix(SupraStrengthMatrix==0),1);
    SupraStrengthMatrix = sparse(diag(SupraStrengthMatrix));
    
    if DisconnectedNodes>0
        fprintf(2,"#Trapping nodes (no outgoing-links): %d\n",DisconnectedNodes);
    endif
    
    SupraStrengthMatrix(SupraStrengthMatrix(:)~=0) = 1./SupraStrengthMatrix(SupraStrengthMatrix(:)~=0);
    SupraTransitionMatrix = SupraStrengthMatrix*SupraAdjacencyMatrix;

    alpha = 0.85;        
    %to normalize correctly in the case of nodes with no outgoing links:
    SupraTransitionMatrix(find(sum(SupraTransitionMatrix,2)==0),:) = 1/Order;
    SupraTransitionMatrix(find(sum(SupraTransitionMatrix,2)!=0),:) = alpha * SupraTransitionMatrix(find(sum(SupraTransitionMatrix,2)!=0),:) + (1-alpha)/Order;
    
    %no more disconnected nodes
    DisconnectedNodes = 0;
        
    %check
    if abs(sum(sum(SupraTransitionMatrix,2))-Order+DisconnectedNodes) > 1e-6
        error("  BuildSupraTransitionMatrixFromSupraAdjacencyMatrix: ERROR! Problems in building the supra-transition matrix -> %f. Aborting process.",abs(sum(sum(SupraTransitionMatrix,2))-Order+DisconnectedNodes));
        %pause;
    endif
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function CentralityVector = GetOverallPageRankCentrality(SupraTransitionMatrix,Layers,Nodes)
    %   References: 
    %   M. De Domenico, A. Sole-Ribalta, E. Omodei, S. Gomez, A. Arenas, arXiv:1311.2906 (2013)
    
    %we pass the transpose of the transition matrix to get the left eigenvectors
    [LeadingEigenvector,LeadingEigenvalue] = GetLargestEigenv(SupraTransitionMatrix');
    
    if abs(LeadingEigenvalue-1)>1e-6
        error("\tGetRWOverallOccupationProbability: ERROR! Expected leading eigenvalue equal to 1, obtained %f\n",LeadingEigenvalue);
        %exit
    endif
    
    LeadingEigenvector = LeadingEigenvector / sum(LeadingEigenvector);
    CentralityVector = sum(reshape(LeadingEigenvector,Nodes,Layers),2);
    CentralityVector = CentralityVector/max(CentralityVector);
endfunction

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Community Detection in Multislice Networks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%GENLOUVAIN  Louvain-like community detection, specified quality function.
%   Version 1.2 (July 2012)
%
%   [S,Q] = GENLOUVAIN(B) with matrix B implements a Louvain-like greedy
%   community detection method using the modularity/quality matrix B that
%   encodes the quality function Q, defined by summing over all elements
%   B(i,j) such that nodes i and j are placed in the same community.
%   Following Blondel et al. 2008, the algorithm proceeds in two phases
%   repeated iteratively: quality is optimized by moving one node at a time
%   until no such moves improve quality; the communities found to that
%   point are then aggregated to build a new network where each node
%   represents a community.  The output vector S encodes the obtained
%   community assignments, with S(i) identifying the community to which
%   node i has been assigned.  The output Q gives the quality of the
%   resulting partition of the network.
%
%   [S,Q] = GENLOUVAIN(B) with function handle B such that B(i) returns
%   the ith column of the modularity/quality matrix uses this function
%   handle (to reduce the memory footprint for large networks) until the
%   number of groups is less than 10000 and then builds the B matrix
%   corresponding to the new aggregated network in subsequent passes.  Use
%   [S,Q] = GENLOUVAIN(B,limit) to change this default=10000 limit.
%
%   [S,Q] = GENLOUVAIN(B,limit,0) suppresses displayed text output.
%
%   [S,Q] = GENLOUVAIN(B,limit,verbose,0) forces index-ordered (cf.
%   randperm-ordered) consideration of nodes, for deterministic results.
%
%   Example (using adjacency matrix A)
%         k = full(sum(A));
%         twom = sum(k); 
%         B = @(v) A(:,v) - k'*k(v)/twom;
%         [S,Q] = genlouvain(B); 
%         Q = Q/twom;
%     finds community assignments for the undirected network encoded by the
%     symmetric adjacency matrix A.  For small networks, one may obtain
%     reasonably efficient results even more simply by handling the full
%     modularity/quality matrix
%         B = A - k'*k/twom;
%     instead of the function handle.  Intended use also includes the
%     "multislice" network quality function of Mucha et al. 2010, where B
%     encodes the interactions as an equivalent matrix (see examples posted
%     online at http://netwiki.amath.unc.edu/GenLouvain).
%
%   Notes:
%     The matrix represented by B must be both symmetric and square.  This
%     condition is not checked thoroughly if B is a function handle, but is
%     essential to the proper use of this routine.
%
%     Under default options, this routine can return different results from
%     run to run because it considers nodes in pseudorandom (randperm)
%     order.  Because of the potentially large number of nearly-optimal
%     partitions (Good et al. 2010), one is encouraged to investigate
%     results of repeated applications of this code (and, if possible, of
%     other computational heuristics).  To force deterministic behavior,
%     ordering nodes by their index, pass zero as the fourth input:
%     GENLOUVAIN(B,limit,verbose,0).
%
%     This algorithm is only "Louvain-like" in the sense that the two
%     phases are used iteratively in the same manner as in the Louvain
%     algorithm (Blondel et al. 2008).  Because it operates on a general
%     quality/modularity matrix B, it does not include any analytical
%     formulas for quickly identifying the change in modularity from a
%     proposed move nor any improved efficiency obtained by their use.  If
%     your problem uses one of the well-used null models included in other
%     codes, those codes should be much faster for your task.
%
%     Past versions had a problem where accumulated subtraction error might
%     lead to an infinite loop with each pass oscillating between two or
%     more partitions yet incorrectly identifying increases in quality.  We
%     believe this problem has been corrected by the relative change checks
%     in lines 178 and 273.  If you encounter a similar problem, notify
%     Peter Mucha (<a href="mailto:mucha@unc.edu">mucha@unc.edu</a>).
%
%     The output Q provides the sum over the appropriate elements of B
%     without any rescaling.  As such, we have rescaled Q in the example
%     above by 2m = sum(k) so that Q <= 1.
%
%     The '~' for ignoring function returns (used for "max" below) are not
%     supported prior to R2009b.  Replace (e.g. 'dummy') for pre-2009b.
%
%     By using this code, the user implicitly acknowledges that the authors
%     accept no liability associated with that use.  (What are you doing
%     with it anyway that might cause there to be a potential liability?!?)
%
%   References:
%     Blondel, Vincent D., Jean-Loup Guillaume, Renaud Lambiotte, and
%     Etienne Lefebvre, "Fast unfolding of communities in large networks,"
%     Journal of Statistical Mechanics: Theory and Experiment, P10008
%     (2008).
%
%     Fortunato, Santo, "Community detection in graphs," Physics Reports
%     486, 75-174 (2010).
%
%     Mucha, Peter J., Thomas Richardson, Kevin Macon, Mason A. Porter, and
%     Jukka-Pekka Onnela. "Community Structure in Time-Dependent,
%     Multiscale, and Multiplex Networks," Science 328, 876-878 (2010).
%
%     Porter, M. A., J. P. Onnela, and P. J. Mucha, "Communities in
%     networks," Notices of the American Mathematical Society 56, 1082-1097
%     & 1164-1166 (2009).
%
%   Acknowledgments:
%     A special thank you to Stephen Reid, whose greedy.m code was the
%     original version that has over time developed into the present code.
%     Thank you also to Dani Bassett, Jesse Blocher, Mason Porter and Simi
%     Wang for inspiring improvements to the code.
%
%   Citation: If you use this code, please cite as
%        Inderjit S. Jutla, Lucas G. S. Jeub, and Peter J. Mucha,
%        "A generalized Louvain method for community detection implemented
%        in MATLAB," http://netwiki.amath.unc.edu/GenLouvain (2011-2012).

function [S,Q] = genlouvain(B,limit,verbose,randord)    
    %set default for maximum size of modularity matrix
    if nargin<2
        limit = 10000;
    end
    
    %set level of reported/displayed text output
    if nargin<3
        verbose = 1;
    end
    if verbose
        mydisp = @(s) disp(s);
    else
        mydisp = @(s) disp('');
    end
    
    %set randperm- v. index-ordered
    if nargin<4
        randord = 1;
    end
    if randord
        myord = @(n) randperm(n);
    else
        myord = @(n) 1:n;
    end
    
    %initialise variables and do symmetry check
    if isa(B,'function_handle')
        n=length(B(1));
        S=(1:n)';
        M=B;
        it(:,1)=M(1);
        ii=find(it(2:end)>0,3)+1;
        ii=[1,ii'];
        for i=2:length(ii),
            it(:,i)=M(ii(i));
        end
        it=it(ii,:);
        if nnz(it-it'),
            disp('WARNING: Function handle does not correspond to a symmetric matrix')
        end
    else
        n = length(B);
        S = (1:n)';
        M=B;
        if nnz(M-M'),
            B=(B+B')/2; disp('WARNING: Forced symmetric B matrix')
        end
    end
    
    dtot=0; %keeps track of total change in modularity
    
    %Run using function handle, if provided
    while (isa(M,'function_handle')) %loop around each "pass" (in language of Blondel et al) with B function handle
          
        y = unique(S); %unique also puts elements in ascending order
        Sb=S;  
        yb = [];
       
        clocktime=clock;
        mydisp(['Merging ',num2str(length(y)),' communities  ',num2str(clocktime(4:6))]);
        
        dstep=1;	%keeps track of change in modularity in pass
    
        while (~isequal(yb,y))&&(dstep/dtot>2*eps) %This is the loop around Blondel et al's "first phase"
    %        Q = 0;
    %        %improves performance considerably if one doesn't compute modularity
    %        %for the first pass (for display purposes only)
    %         P = sparse(y,1:length(y),1); %Modularity Calculation
    %         for i = 1:length(M(1))
    %             Q = Q + (P*M(i))'*P(:,i);
    %         end
    %         mydisp([num2str(length(unique(y))),' ',num2str(Q)])
            yb = y;
            G=sparse(1:length(y),y,1);      %no-mex version
            dstep=0;
            
            for i = myord(length(M(1)))     %loop over nodes in pseudorandom order     
    
                Mi = M(i);
              
                u = unique([y(i);y(Mi>0)]);
                
                dH=Mi'*G(:,u);              %no-mex version
                %dH=modchange_y(Mi,y,u);
                
                yi=find(u==y(i));
                dH(yi) = dH(yi) - Mi(i);
                
                [~, k] = max(dH);
                
                %only move to different group if it is more optimized than
                %staying in same group (up to error with double precision)
                if(dH(k)>(dH(yi)))
                	dtot=dtot+dH(k)-dH(yi);
                	dstep=dstep+dH(k)-dH(yi);
                    G(i,y(i))=0;            %no-mex version
                    G(i,u(k))=1;            %no-mex version
                    y(i) = u(k);            
                end
                
            end
            
            mydisp([num2str(length(unique(y))),' change: ',num2str(dstep),...
                ' total: ',num2str(dtot),' relative: ',num2str(dstep/dtot)]);
        end
            
        %[S,y] = tidyconfig_c(S,y);  %note tidyconfig reorders along node numbers
        y = tidyconfig(y);                  %no-mex version
        for i = 1:length(y)                 %no-mex version
            S(S==i) = y(i);                 %no-mex version
        end                                 %no-mex version
        
        %calculate modularity and return if converged
        if isequal(Sb,S)
            Q=0;
            P=sparse(y,1:length(y),1);
            for i=1:length(M(1))
                Q=Q+(P*M(i))'*P(:,i);
            end
            return
        end
        
        %check wether #groups < limit 
        t = length(unique(S));
        if (t>limit)
           M=@(i) metanetwork_i(B,S,t,i);   %use function handle if #groups>limit 
        else
            J = zeros(t);   %convert to matrix if #groups small enough
            for c=1:t
                J(:,c)=metanetwork_i(B,S,t,c);
            end
            B = J;
            M=B;
        end
        
    end
        
    
    S2 = (1:length(B))';
    Sb = [];
    while ~isequal(Sb,S2) %loop around each "pass" (in language of Blondel et al) with B matrix
        
        y = unique(S2);  %unique also puts elements in ascending order
        Sb = S2;
    
        clocktime=clock;
        mydisp(['Merging ',num2str(length(y)),' communities  ',num2str(clocktime(4:6))]);
    
        yb = [];
        
        G=sparse(1:length(y),y,1);
        
        dstep=1;
        
        % P = G';
        % Q = sum(sum((P*M).*(P)));
        % Qb = -inf;
        
        while (~isequal(yb,y)) && (dstep/dtot>2*eps) %This is the loop around Blondel et al's "first phase"
            
            % mydisp([num2str(length(unique(y))),' ',num2str(Q)])
            yb = y;
            % Qb=Q;
            
            dstep=0;
            
            for i = myord(length(M))
                u = unique([y(i);y(M(:,i)>0)]);
                % dH = modchange_y(M(:,i),y,u); %relative changes in modularities
                dH = (M(:,i)'*G(:,u));
                
                yi=find(u==y(i));
                dH(yi) = dH(yi) - M(i,i);
                [~, k] = max(dH);
                %%only move to different group if it is more optimized than
                %%staying in same group (up to error with double precision)
                if(dH(k)>(dH(yi)))
                	dtot=dtot+dH(k)-dH(yi);
                	dstep=dstep+dH(k)-dH(yi);
                	G(i,y(i))=0;
                	G(i,u(k))=1;
                    y(i) = u(k);
                end
            end
            
            % P=sparse(y,1:length(y),1);
            % Q = sum(sum((P*M).*(P)));
            
        end
        
        y = tidyconfig(y);  %note tidyconfig reorders along node numbers
        for i = 1:length(y)
            S(S==i) = y(i);
            S2(S2==i) = y(i);
        end
        
        if isequal(Sb,S2)
        	P=G';
        	Q=sum(sum((P*M).*P));
        	return
        end
       
        M = metanetwork(B,S2);    
    end
endfunction

%-----%
function M = metanetwork(J,S)
    %Computes new aggregated network (communities --> nodes)
    if(issparse(J))
        m=max(S);
        [i,j,v]=find(J);
        M = sparse(S(i),S(j),v,m,m);
    else
        PP = sparse(1:length(S),S,1);
        M = PP'*J*PP;
    end
endfunction

%-----%
function Mi = metanetwork_i(J,S,t,i) 
    %ith column of metanetwork (used to create function handle)
    %J is a function handle
      
        Mi=sparse([],[],[],t,1);
        for j=find(S==i)'
            Jj=J(j);
            [ii,k,v]=find(Jj);
            Mi=Mi+sparse(S(ii),k,v,t,1);
        end
endfunction

%-----%
function S = tidyconfig(S)
    %This function remains almost identical to that originally written by
    %Stephen Reid for his greedy.m code.
    %   tidy up S i.e.  S = [2 4 2 6] -> S = [1 2 1 3]
    T = zeros(length(S),1);
    for i = 1:length(S)
        if T(i) == 0
            T(S==S(i)) = max(T) + 1;
        end
    end
    S = T;
endfunction
