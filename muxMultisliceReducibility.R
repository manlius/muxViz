library(gplots)

res <- read.table("jsd_distance_matrix.txt")
N <- max(max(res[,1]),max(res[,2]))
distanceMatrix <- matrix(0, nrow=N, ncol=N)
for(i in 1:nrow(res)){
    distanceMatrix[ res[i,1], res[i,2] ] <- res[i,3]
}

#calculate the clustering with our favorite method
user_method <- as.character(read.table("hclust_method.tmp")$V1)
hc <- hclust(as.dist(distanceMatrix), method=user_method)

## Output the list of merging operations
#see http://stackoverflow.com/questions/18215184/how-to-print-the-order-of-hierarchical-clustering-in-r
write.table(hc$merge, file = "hclust_merge.txt", sep = " ",append=F, col.names = F, row.names = F)
