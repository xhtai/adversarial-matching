# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

# cbind all 246 myPairs into evalPairs, and add grams labels and edit distance
# also forPairsGramsTCall and allPairwise

############################## grams labels

# very non-straightforward way of doing things, but memory issues...
############ make eval set
# start <- 1
# start <- 61
# start <- 121
start <- 181

load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", start), ".Rdata"))
evalPairsPart <- myPairs[, c("hash1", "hash2", "PGPmatched", "preds10M")]

for (i in (start + 1):(start + 59)) {
  cat(i, ", ")
  print(Sys.time())
  load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
  
  evalPairsPart <- rbind(evalPairsPart, myPairs[, c("hash1", "hash2", "PGPmatched", "preds10M")])
  
  gc()
  if (i == 240) {
    for (j in 241:246) {
      load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", j), ".Rdata"))
      evalPairsPart <- rbind(evalPairsPart, myPairs[, c("hash1", "hash2", "PGPmatched", "preds10M")])
    }
  }
}

save(evalPairsPart, file = paste0("./245milPairs/redoInventory/reorg/eval", start, ".Rdata"))


load("./245milPairs/redoInventory/reorg/eval1.Rdata")
evalPairs <- evalPairsPart
load("./245milPairs/redoInventory/reorg/eval61.Rdata")
evalPairs <- rbind(evalPairs, evalPairsPart)
load("./245milPairs/redoInventory/reorg/eval121.Rdata")
evalPairs <- rbind(evalPairs, evalPairsPart)
load("./245milPairs/redoInventory/reorg/eval181.Rdata")
evalPairs <- rbind(evalPairs, evalPairsPart)

save(evalPairs, file = paste0("./245milPairs/redoInventory/reorg/evalPairs.Rdata"))

################### now put the grams labels in
# group_by_key_grams_tc has singletons ---- use this to get all hashes with any info
# pairs_grams_tc.csv has all matched pairs. some of the singletons in group_by_key_grams_tc appear multiple times and have matched pairs. matched versions supersede singletons

### first get hashes with any grams/PGP info (these will be labeled 0 or 1, not NA)
group_by_key_grams_tc <- read.csv("/home/xtai/Desktop/markets/code/marketplaces/kyle_scripts/adj_matrices/group_by_key_grams_tc.csv", header = FALSE, stringsAsFactors = FALSE)
singletons <- group_by_key_grams_tc[group_by_key_grams_tc$V3 == "", ]
singletonHashes <- singletons$V2[singletons$V2 %in% outStep2$final$vendor_hash]
singletonHashes <- unique(singletonHashes) # 9698

pairs_grams_tc <- read.csv("/home/xtai/Desktop/markets/code/marketplaces/kyle_scripts/adj_matrices/pairs_grams_tc.csv", header = FALSE, stringsAsFactors = FALSE)
# save(pairs_grams_tc, file = "../pairs_grams_tc.Rdata")

hashes <- unique(c(pairs_grams_tc$V2, pairs_grams_tc$V3)) # 21890
myHashes <- hashes[hashes %in% outStep2$final$vendor_hash] # 10769
# some singleton hashes also appear in groups --- groups supersede those

noInfoHashes <- outStep2$final$vendor_hash[!(outStep2$final$vendor_hash %in% c(singletonHashes, myHashes))] # 4140 --- these are hashes with no grams or PGP info

# so we have choose(18023, 2) = 162405253 pairs

#### now make subset of 245M pairs that have info (162405253 pairs)
load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", 1), ".Rdata"))
removeThese <- which(myPairs$hash1 %in% noInfoHashes | myPairs$hash2 %in% noInfoHashes)
forPairsGramsTC <- myPairs[-removeThese, c("hash1", "hash2", "PGPmatched")]

for (i in 2:60) {
  cat(i, ", ")
  print(Sys.time())
  load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
  
  removeThese <- which(myPairs$hash1 %in% removeHashes | myPairs$hash2 %in% removeHashes)
  forPairsGramsTC <- rbind(forPairsGramsTC, myPairs[-removeThese, c("hash1", "hash2", "PGPmatched", "preds10M")])
  
  gc()
}

save(forPairsGramsTC, file = paste0("./245milPairs/redoInventory/forPairsGramsTC1.Rdata"))


load("./245milPairs/redoInventory/forPairsGramsTC1.Rdata")
forPairsGramsTCall <- forPairsGramsTC
load("./245milPairs/redoInventory/forPairsGramsTC61.Rdata")
forPairsGramsTCall <- rbind(forPairsGramsTCall, forPairsGramsTC)
load("./245milPairs/redoInventory/forPairsGramsTC121.Rdata")
forPairsGramsTCall <- rbind(forPairsGramsTCall, forPairsGramsTC)
load("./245milPairs/redoInventory/forPairsGramsTC181.Rdata")
forPairsGramsTCall <- rbind(forPairsGramsTCall, forPairsGramsTC)

save(forPairsGramsTCall, file = paste0("./245milPairs/redoInventory/forPairsGramsTCall.Rdata"))
# this has 162405253 pairs


############ now make evalPairs
load("./245milPairs/redoInventory/forPairsGramsTCall.Rdata") # pairs with non-NA label
load("./245milPairs/redoInventory/reorg/evalPairs.Rdata")
# first put in zeros
evalPairs <- dplyr::left_join(evalPairs, cbind(forPairsGramsTCall[, c("hash1", "hash2")], 0), by = c("hash1", "hash2"))
# check what name is ---- it's 0
names(evalPairs)[5] <- "gramsTCmatched"

# now put in 1's
# now left join with kyle's list
pairs_grams_tc <- read.csv("/home/xtai/Desktop/markets/code/marketplaces/kyle_scripts/adj_matrices/pairs_grams_tc.csv", header = FALSE, stringsAsFactors = FALSE)

evalPairs <- dplyr::left_join(evalPairs, cbind(pairs_grams_tc[, -1], 1), by = c("hash1" = "V2", "hash2" = "V3"))

evalPairs[which(evalPairs[, 6] == 1), "gramsTCmatched"] <- 1
evalPairs <- evalPairs[, 1:5]

save(evalPairs, file = paste0("./245milPairs/redoInventory/reorg/evalPairs.Rdata"))



########################## baseline: idDist ###########################
idDist245 <- c()

for (i in 1:246) {
  cat(i, ", ")
  print(Sys.time())
  load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
  
  idDist245 <- c(idDist245, myPairs$idDist)
  gc()
}

load("./245milPairs/redoInventory/reorg/evalPairs.Rdata")
evalPairs$idDist <- idDist245
save(evalPairs, file = "./245milPairs/redoInventory/reorg/evalPairs.Rdata")
