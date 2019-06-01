# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

#### make a 245M row data frame 
load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", 1), ".Rdata"))
forLinks245M <- myPairs[, c("hash1", "hash2", "PGPmatched", "preds10M")]

# 1:41pm
for (i in 2:246) {
    cat(i, ", ")
    print(Sys.time())
    load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
    forLinks245M <- rbind(forLinks245M, myPairs[, c("hash1", "hash2", "PGPmatched", "preds10M")])
    gc()
}

save(forLinks245M, file = paste0("./245milPairs/redoInventory/forLinks245M.Rdata"))



######################## now do clustering ######################## 
library(heisenbrgr)
load("./245milPairs/redoInventory/forLinks245M.Rdata")

linkage <- "minimax" # repeat with "single", "complete", "average"
testThis <- getHcluster(forLinks245M, pairColNums = 1:2, distSimCol = "preds10M", linkage = linkage, myDist = FALSE) # do clustering once

# vary cutoffs
cutoffs <- rev(seq(from = 0.02, to = 1, by = .02))

for (i in 1:length(cutoffs)) {
  cat(args[1], ": cutoff", cutoffs[i], ", ")
  myLinksPart <- linksAnalysis(forLinks245M, pairColNums = 1:2, testThis, cutoffs[i], linkage = linkage)
  save(myLinksPart, file = paste0("./245milPairs/redoInventory/linkages/myLinks_", paste0(linkage, cutoffs[i]),".Rdata"))
  gc()
}


########### one specification for final clusters: minimax 0.74
load("./245milPairs/redoInventory/forLinks245M.Rdata")
load("./245milPairs/redoInventory/linkages/myLinks_minimax0.74.Rdata")
forLinks245M <- cbind(forLinks245M, myLinks = myLinksPart)
myClusters <- getClust(forLinks245M, linkCol = "myLinks", pairColNums = 1:2)

save(myClusters, file = paste0("./245milPairs/redoInventory/myClusters_minimax0.74.Rdata"))

# this has item (vendor_hash) and cluster (cluster number)

### write out more informative csv: vendor_hash, id, marketplace, cluster, clusterSize, PGP1-9 containing PGP-IDs --- PGP mapping: redo outStep2$PGPlist into just numerical idenfitiers for PGPs

load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2Add.Rdata")
PGPs <- unique(unlist(outStep2$PGPlist))
PGPmapped <- outStep2$PGPlist
for (i in 1:length(PGPmapped)) {
  if (i %% 1000 == 0) cat(i, ", ")
  if (length(PGPmapped[[i]]) == 0) next
  PGPmapped[[i]] <- which(PGPs %in% PGPmapped[[i]])
}

library(magrittr)
  
load(paste0("/home/xtai/Desktop/tmp_9-5/update_1-28/myClusters_minimax0.74.Rdata")) # load clusters

clusterSizes <- myClusters %>% dplyr::group_by(cluster) %>% dplyr::summarize(clusterSize = length(cluster))

myClusters <- dplyr::left_join(myClusters, clusterSizes)

finalClusters <- dplyr::left_join(outStep2$final[, c("vendor_hash", "id", "marketplace")], myClusters, by = c("vendor_hash" = "item"))
finalClusters <- finalClusters[order(-finalClusters$clusterSize, finalClusters$cluster, finalClusters$id), ]

finalClusters$PGP1 <- NA
finalClusters$PGP2 <- NA
finalClusters$PGP3 <- NA
finalClusters$PGP4 <- NA
finalClusters$PGP5 <- NA
finalClusters$PGP6 <- NA
finalClusters$PGP7 <- NA
finalClusters$PGP8 <- NA
finalClusters$PGP9 <- NA

for (i in 1:nrow(finalClusters)) {
  if (i %% 1000 == 0) cat(i, ", ")    
  tmp <- PGPmapped[[which(names(PGPmapped) == finalClusters$vendor_hash[i])]]
  if (length(tmp) == 0) next
  for (j in 1:length(tmp)) {
    finalClusters[i, 5 + j] <- tmp[j]
  }
}
write.csv(finalClusters, file = paste0("/home/xtai/Desktop/tmp_9-5/update_1-28/finalClusters_minimax0.74.csv"))

finalClusters$clusterSize[is.na(finalClusters$clusterSize)] <- 1
table(finalClusters$clusterSize)/1:11
#     1     2     3     4     5     6     7     8     9    10    11 
# 12155  1909   882   358   151    93    56    31    13     2     2 
