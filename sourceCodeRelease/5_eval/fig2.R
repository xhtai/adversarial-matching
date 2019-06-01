# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

# evalPairs has 245M rows
# allPairwise has 73M with PGP labels

########################### 1. precision-recall graph ############################
########## edit baseline
load("./245milPairs/redoInventory/reorg/evalPairs.Rdata")
predObj <- ROCR::prediction(-evalPairs[!is.na(evalPairs$PGPmatched), "idDist"], evalPairs$PGPmatched[!is.na(evalPairs$PGPmatched)])
RP.perf <- ROCR::performance(predObj, "prec", "rec")

out <- data.frame(cutoffs = RP.perf@alpha.values[[1]], recall = RP.perf@x.values[[1]], precision = RP.perf@y.values[[1]])

save(out, file = paste0("./245milPairs/redoInventory/reorg/precisionRecall/out-idDist-PGP.Rdata"))


zerosSampled <- c(3653, 40000, 300000, 1000000, 5000000, 10000000)
# files are paste0("preds", sprintf("%d", zerosSampled))

# on server
load("./allPairwise.Rdata")

for (i in 1:length(zerosSampled)) {
    cat(i, ", ")
    load(paste0("./245milPairs/redoInventory/preds", sprintf("%d", zerosSampled[i]), ".Rdata"))
    predObj <- ROCR::prediction(myPreds, allPairwise$PGPmatched)
    RP.perf <- ROCR::performance(predObj, "prec", "rec")
    
    out <- data.frame(cutoffs = RP.perf@alpha.values[[1]], recall = RP.perf@x.values[[1]], precision = RP.perf@y.values[[1]])
    
    save(out, file = paste0("./245milPairs/redoInventory/precisionRecall/out", sprintf("%d", zerosSampled[i]), "-50trees.Rdata"))
}


######################## Plot2: total sales
# first get sales quantiles
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep1.Rdata")
feedbackSubset <- outStep1$feedback[outStep1$feedback$date >= "2011-05-22" & outStep1$feedback$date < "2018-08-22", ] # 5 million

library(magrittr)
out <- feedbackSubset %>%
  dplyr::group_by(vendor_hash) %>%
  dplyr::summarize(totalSales = sum(order_amount_usd))

save(out, file = "/home/xtai/Desktop/tmp_9-5/update_1-21/totalSales.Rdata")

# use this to subset the hashes, then only take pairs where both fulfill criteria
load("./allPairwise.Rdata")
load("./totalSales.Rdata")
totalSales <- out
salesQuantiles <- quantile(totalSales$totalSales, probs = seq(.1, .9, by = .1))

zerosSampled <- c(3653, 40000, 300000, 1000000, 5000000, 10000000)
# files are paste0("preds", sprintf("%d", zerosSampled))

for (i in 1:length(zerosSampled)) {
    cat(i, ", ")
    load(paste0("./245milPairs/redoInventory/preds", sprintf("%d", zerosSampled[i]), ".Rdata"))

    j <- 7 # only do 7th decile
    # for (j in 1:length(salesQuantiles)) {
        # cat(j, ", ")
        selectedHashes <- totalSales$vendor_hash[totalSales$totalSales >= salesQuantiles[j]]
        selectedRows <- which(allPairwise$hash1 %in% selectedHashes & allPairwise$hash2 %in% selectedHashes)
        tmpPreds <- myPreds[selectedRows]
        tmpMatched <- allPairwise$PGPmatched[selectedRows]
        predObj <- ROCR::prediction(tmpPreds, tmpMatched)
        RP.perf <- ROCR::performance(predObj, "prec", "rec")
        
        out <- data.frame(cutoffs = RP.perf@alpha.values[[1]], recall = RP.perf@x.values[[1]], precision = RP.perf@y.values[[1]])
    
        save(out, file = paste0("./245milPairs/redoInventory/precisionRecall/out", sprintf("%d", zerosSampled[i]), "-50trees-", j, "decile.Rdata"))
    # }
}

######################## Plot3: weighted by min(A, B)
# first generate weights
load("./totalSales.Rdata") # named 'out', length 22,163
load("./245milPairs/redoInventory/allPairwise.Rdata")

sales1 <- out[allPairwise$order1, "totalSales"]
sales2 <- out[allPairwise$order2, "totalSales"]
myWeight <- pmin(sales1, sales2)

forWeightPlot <- data.frame(hash1 = allPairwise$hash1, hash2 = allPairwise$hash2, PGPmatched = allPairwise$PGPmatched, myWeight = myWeight, stringsAsFactors = FALSE)

zerosSampled <- c(3653, 40000, 300000, 1000000, 5000000, 10000000)
cutoffs <- rev(seq(from = 0, to = 1, by = .02))

for (i in 1:length(zerosSampled)) {
    cat(i, ", ")
    load(paste0("./245milPairs/redoInventory/preds", sprintf("%d", zerosSampled[i]), ".Rdata"))
    out <- data.frame(cutoffs = cutoffs, recall = NA, precision = NA)
    
    for (c in 1:length(cutoffs)) {
        numerator <- sum((myPreds >= cutoffs[c] & forWeightPlot$PGPmatched == 1) * forWeightPlot$myWeight)
        denom <- sum((myPreds >= cutoffs[c]) * forWeightPlot$myWeight)
        
        if (denom == 0) {
            out$precision[c] <- 1
        } else {
            out$precision[c] <- numerator/denom
        }
        out$recall[c] <- numerator/sum((forWeightPlot$PGPmatched == 1) * forWeightPlot$myWeight)
    }
    save(out, file = paste0("./245milPairs/redoInventory/precisionRecall/out", sprintf("%d", zerosSampled[i]), "-50trees-weightSales.Rdata"))

}

