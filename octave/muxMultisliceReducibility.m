addpath("octave");

%import the muxOctaveLib
muxOctaveLib;

%import the configuration file
LayersList = {};
muxOctaveConfig;

NodesTensor = {}; 
Layers = 0;

if isExtendedEdgesList
    [SupraAdjacencyMatrix,Layers,Nodes] = BuildSupraAdjacencyMatrixFromFile(MultiLayerEdgesListFile,Flags,MaxNodes,FirstNodeLabel);
    NodesTensor = SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix,Layers,Nodes);
else
    [NodesTensor,Layers,Nodes] = BuildMultiplexFromFileList(LayersList,Flags,MaxNodes,FirstNodeLabel);         
endif


if Layers > 1    
    %%%%%%%%%%%%%%%%%
    %VN Entropies
    %%%%%%%%%%%%%%%%%
    %single layer entropy
    SingleLayerEntropy = {};
    
    for i = 1:Layers
        SingleLayerEntropy{i} = GetRenyiEntropyFromAdjacencyMatrix(NodesTensor{i},1);
    end
    
    %%%%%%%%%%%%%%%%%
    %JSD
    %%%%%%%%%%%%%%%%%
    JSD = {};
    
    for i = 1:Layers
        JSD{i,i} = 0;
        for j = (i+1):Layers
            JSD{i,j} = GetJensenShannonDivergence(NodesTensor{i},NodesTensor{j},SingleLayerEntropy{i},SingleLayerEntropy{j});
            JSD{j,i} = JSD{i,j};
        end
    end
        
    reducibilityFile = "jsd_distance_matrix.txt";
    fidred = fopen (reducibilityFile, "w");
    for i = 1:Layers
        for j = 1:Layers
            fprintf(fidred,"%d %d %e\n",i,j,sqrt(JSD{i,j}));
        end
    end
    fclose (fidred);

    %re-output for muxViz
    reducibilityFile = strcat(AnalysisName,"_reducibility_jsd.txt");
    fidred = fopen (reducibilityFile, "w");
    for i = 1:Layers
        for j = 1:Layers
            fprintf(fidred,"%e ",sqrt(JSD{i,j}));
        end
        fprintf(fidred,"\n");
    end
    fclose (fidred);    
    printf("Multislice reducibility output (quality function) to: %s\n",reducibilityFile);

    JSD = sqrt(cell2mat(JSD));
    
    if system("R CMD BATCH muxMultisliceReducibility.R") == 0
        MergeMatrix = load("hclust_merge.txt");
        
        %aggregate: I dont use the function in the library to avoid calculating again the nodesTensor
        AggregateMatrix = NodesTensor{1};
        for i = 2:Layers
            AggregateMatrix += NodesTensor{i};
        end
        
        %aggregate entropy
        AggregateEntropy = GetRenyiEntropyFromAdjacencyMatrix(AggregateMatrix,1);
            
        %step zero: full colored-edge graph against fully aggregated network
        cntCurrentLayers = 0;
        gQualityFunction = 0;
        for i = 1:Layers
            if SingleLayerEntropy{i}>0
                gQualityFunction = gQualityFunction + SingleLayerEntropy{i};
                cntCurrentLayers = cntCurrentLayers + 1;
            end
        end
        gQualityFunction = cntCurrentLayers*AggregateEntropy - gQualityFunction;
        relgQualityFunction = gQualityFunction/(cntCurrentLayers*AggregateEntropy);
        
        qualityFile = strcat(AnalysisName,"_reducibility_quality.txt");
        fidred = fopen (qualityFile, "w");
        
        fprintf(fidred,"#step g(m) relg(m) diffEntropy reldiffEntropy\n")
        fprintf(fidred,"%d %e %e %d %d\n",0,gQualityFunction,relgQualityFunction,0,0)
    
        MergedTensor = {};
        MergedEntropy = {};
        for m = 1:(Layers-1)
            if MergeMatrix(m,1)<0
                %we must use the network in NodesTensor
                A = NodesTensor{-MergeMatrix(m,1)};
                entropyA = SingleLayerEntropy{-MergeMatrix(m,1)};
                SingleLayerEntropy{-MergeMatrix(m,1)} = 0;
            else
                %we must use the network stored in MergedTensor
                A = MergedTensor{MergeMatrix(m,1)};
                entropyA = MergedEntropy{MergeMatrix(m,1)};
                MergedEntropy{MergeMatrix(m,1)} = 0;
            end
    
            if MergeMatrix(m,2)<0
                %we must use the network in NodesTensor
                B = NodesTensor{-MergeMatrix(m,2)};
                entropyB = SingleLayerEntropy{-MergeMatrix(m,2)};
                SingleLayerEntropy{-MergeMatrix(m,2)} = 0;
            else
                %we must use the network stored in MergedTensor
                B = MergedTensor{MergeMatrix(m,2)};
                entropyB = MergedEntropy{MergeMatrix(m,2)};
                MergedEntropy{MergeMatrix(m,2)} = 0;
            end
                        
            MergedTensor{m} = A + B;
            tmpLayerEntropy = GetRenyiEntropyFromAdjacencyMatrix(MergedTensor{m},1);
            MergedEntropy{m} = tmpLayerEntropy;
            diffEntropy = 2*tmpLayerEntropy - (entropyA + entropyB);
            reldiffEntropy = diffEntropy/(2*tmpLayerEntropy);
            
            gQualityFunction = 0;
            cntCurrentLayers = Layers - m;
            for i = 1:Layers
                if SingleLayerEntropy{i}>0
                    %resetting the values as above, will guarantee that we consider only layers
                    %that are still to be merged
                    gQualityFunction = gQualityFunction + SingleLayerEntropy{i};
                end
            end
            for i = 1:m
                % the current merge is accounted by position m
                if MergedEntropy{i}>0
                    %resetting the values as above, will guarantee that we consider only layers
                    %that are still to be merged
                    gQualityFunction = gQualityFunction + MergedEntropy{i};
                end
            end
            gQualityFunction = cntCurrentLayers*AggregateEntropy - gQualityFunction;
            relgQualityFunction = gQualityFunction/(cntCurrentLayers*AggregateEntropy);
            
            fprintf(fidred,"%d %e %e %e %e\n",m,gQualityFunction,relgQualityFunction,diffEntropy,reldiffEntropy)
        end
    else
        fprintf(2,"Error while calling R... Aborting process.\n");
        exit
    end
    fclose(fidred);
    printf("Multislice reducibility output (quality function) to: %s\n",qualityFile);
end    
