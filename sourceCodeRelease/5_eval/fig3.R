# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

# get 162M pairs (can also use evalPairs.Rdata, but here I redid it)

load("./245milPairs/redoInventory/forPairsGramsTCall.Rdata") # pairs with non-NA label (162M): this has hash1, hash2, PGPmatched

# now left join forPairsGramsTCall with kyle's list
pairs_grams_tc <- read.csv("/home/xtai/Desktop/markets/code/marketplaces/kyle_scripts/adj_matrices/pairs_grams_tc.csv", header = FALSE, stringsAsFactors = FALSE)
forPrecRec <- dplyr::left_join(forPairsGramsTCall, pairs_grams_tc, by = c("hash1" = "V2", "hash2" = "V3"))
forPrecRec$pairsGramsTCmatched <- ifelse(forPrecRec$V1 == 0, 1, 0) # check to make sure there are 4971 ones
forPrecRec$pairsGramsTCmatched[is.na(forPrecRec$pairsGramsTCmatched)] <- 0

predObj <- ROCR::prediction(forPrecRec$preds10M, forPrecRec$pairsGramsTCmatched)
RP.perf <- ROCR::performance(predObj, "prec", "rec")

out <- data.frame(cutoffs = RP.perf@alpha.values[[1]], recall = RP.perf@x.values[[1]], precision = RP.perf@y.values[[1]])

save(out, file = paste0("./245milPairs/redoInventory/precisionRecall/out", sprintf("%d", 10000000), "-50trees-pairsGramsTC.Rdata"))
