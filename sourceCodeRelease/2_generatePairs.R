# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

#################### Step 3
library(heisenbrgr)
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2Add.Rdata")

# make all pairs ----- make dataframes of 1 million each
allPossiblePairwise <- t(combn(outStep2$final$vendor_hash, 2))

for (i in 1:246) {
  if (i %% 10 == 0) cat(i, ", ")
  if (i < 246) {
    myPairs <- allPossiblePairwise[((i - 1)*1000000 + 1):(i*1000000), ]
  } else {
    myPairs <- allPossiblePairwise[((i - 1)*1000000 + 1):(nrow(allPossiblePairwise)), ]
  }
  save(myPairs, file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
}
# just under 15 minutes


getPairsSubset <- function(dataframeName, myPairs) {
  # allPairwise <- t(combn(dataframeName$vendor_hash, 2))
  myPairs <- data.frame(hash1 = myPairs[, 1], hash2 = myPairs[, 2], stringsAsFactors = FALSE)
  myPairs$order1 <- match(myPairs$hash1, dataframeName$vendor_hash)
  myPairs$order2 <- match(myPairs$hash2, dataframeName$vendor_hash)
  
  info1 <- dataframeName[myPairs$order1, c("id", "marketplace")]
  info2 <- dataframeName[myPairs$order2, c("id", "marketplace")]
  
  myPairs$id1 <- info1$id
  myPairs$id2 <- info2$id
  
  myPairs$marketplace1 <- info1$marketplace
  myPairs$marketplace2 <- info2$marketplace
  
  myPairs$idDist <- apply(myPairs[, c("id1", "id2")], MARGIN = 1, FUN = function(x) adist(x[1], x[2]))
  
  myPairs$sameMarket <- ifelse(info1$marketplace == info2$marketplace, 1, 0)
  
  return(myPairs)
}


for (i in 1:246) {
  if (i %% 10 == 0) cat(i, ", ")
  load(file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  myPairs <- getPairsSubset(outStep2$final, myPairs)
  save(myPairs, file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
}
# 4:02pm (40 seconds for 1)
# this is going to take 2 hours

#########################################################################
library(heisenbrgr)
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2Add.Rdata")
# 10:15
for (i in 1:246) {
  cat(i, ", ")
  load(file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  
  profileJaccard <- fromList(myPairs, outStep2$profileTokens, heisenbrgr:::jaccardSimilarity)
  titleJaccard <- fromList(myPairs, outStep2$titleTokens, heisenbrgr:::jaccardSimilarity)
  descriptionJaccard <- fromList(myPairs, outStep2$descriptionTokens, heisenbrgr:::jaccardSimilarity)
  
  cat("categories, ")
  catJaccard <- fromList(myPairs, lapply(outStep2$inventory, function(x) x$category), heisenbrgr:::jaccardSimilarity)
  catDosageJaccard <- fromList(myPairs, lapply(outStep2$inventory, function(x) x$catDosage), heisenbrgr:::jaccardSimilarity)
  catUnitJaccard <- fromList(myPairs, lapply(outStep2$inventory, function(x) x$catUnit), heisenbrgr:::jaccardSimilarity)
  catDosageUnitJaccard <- fromList(myPairs, lapply(outStep2$inventory, function(x) x$catDosageUnit), heisenbrgr:::jaccardSimilarity)
  
  cat("abs, ")
  absDifferences <- absFromDataframe(myPairs, outStep2$final, c("numListingsWithFeedback", "totalFeedback", "dailyFrac", "daysActive", "diversity", "meanPriceSold", "medianPriceSold", "minPriceSold", "maxPriceSold", "priceRange", "numDescriptionTokens", "numTitleTokens", "numProfileTokens"), c(rep(0, 10), rep(1, 3)))
  
  myPairs <- cbind(myPairs, profileJaccard, titleJaccard, descriptionJaccard, catJaccard, catDosageJaccard, catUnitJaccard, catDosageUnitJaccard, absDifferences)
  
  save(myPairs, file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  gc()
}

# sales days
for (i in 1:246) {
  cat(i, ", ")
  load(file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  
  colNums <- which(substr(names(outStep2$final), 1, 2) == "20")
  salesDiffs1 <- fromSalesDays(myPairs[1:250000, ], outStep2$final[, colNums])
  gc()
  salesDiffs2 <- fromSalesDays(myPairs[250001:500000, ], outStep2$final[, colNums])
  gc()
  if (i != 246) {
    salesDiffs3 <- fromSalesDays(myPairs[500001:750000, ], outStep2$final[, colNums])
    gc()
    salesDiffs4 <- fromSalesDays(myPairs[750001:nrow(myPairs), ], outStep2$final[, colNums])
    gc()
    salesDiffs <- rbind(salesDiffs1, salesDiffs2, salesDiffs3, salesDiffs4)
  } else {
    salesDiffs3 <- fromSalesDays(myPairs[500001:nrow(myPairs), ], outStep2$final[, colNums])
    gc()
    salesDiffs <- rbind(salesDiffs1, salesDiffs2, salesDiffs3)
  }
  
  myPairs <- cbind(myPairs, salesDiffs)
  
  save(myPairs, file = paste0("/media/xtai/4AF561E807105059/markets/data/245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  gc()
}

####################################### PGP LABELS #######################################
library(heisenbrgr)
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2Add.Rdata")
numPGPs <- unlist(lapply(outStep2$PGPlist, length), use.names = FALSE)

tmp <- outStep2$final[numPGPs >= 1, ]
tmpPGPlist <- outStep2$PGPlist[numPGPs >= 1]

allPairwisePGPonly <- t(combn(tmp$vendor_hash, 2))
allPairwisePGPonly <- data.frame(hash1 = allPairwisePGPonly[, 1], hash2 = allPairwisePGPonly[, 2], stringsAsFactors = FALSE)
allPairwisePGPonly$order1 <- match(allPairwisePGPonly$hash1, tmp$vendor_hash)
allPairwisePGPonly$order2 <- match(allPairwisePGPonly$hash2, tmp$vendor_hash)

list1 <- tmpPGPlist[allPairwisePGPonly$order1]
list2 <- tmpPGPlist[allPairwisePGPonly$order2]

PGPmatched <- rep(NA, nrow(allPairwisePGPonly))
Sys.time()
for (i in 1:nrow(allPairwisePGPonly)) {
  if (i %% 1000000 == 0) cat(i, ", ")
  PGPmatched[i] <- heisenbrgr:::PGPmatch(list1[[i]], list2[[i]])
}
Sys.time()

allPairwisePGPonly <- cbind(allPairwisePGPonly, PGPmatched)

for (i in 1:246) {
  if (i %% 10 == 0) cat(i, ", ")
  load(file = paste0("./245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
  myPairs <- dplyr::left_join(myPairs, allPairwisePGPonly[, c(1, 2, 5)], by = c("hash1" = "hash1", "hash2" = "hash2"))
  save(myPairs, file = paste0("./245milPairs/myPairs_", sprintf("%03d", i), ".Rdata"))
}


######################################################################################## 
# myPairs 1 to 245: all features + PGP labels



