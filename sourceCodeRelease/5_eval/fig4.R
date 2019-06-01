# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

##################################################################################
# make one out dataframe for each linkage
load("./245milPairs/redoInventory/forLinks245M.Rdata")

cutoffs <- rev(seq(from = 0.02, to = 1, by = .02))
out <- data.frame(cutoffs = cutoffs, recall = NA, precision = NA)
linkage <- "minimax"
# linkage <- "single" # done
# linkage <- "complete" # done
# linkage <- "average" # done

for (c in 1:length(cutoffs)) {
  cat(c, ", ")
  # if (cutoffs[c] == 0 || (cutoffs[c] == .02 & linkage == "average")) next
  load(paste0("./245milPairs/redoInventory/linkages/myLinks_", linkage, cutoffs[c], ".Rdata"))
  
  numerator <- sum(myLinksPart == 1 & forLinks245M$PGPmatched == 1 & !is.na(forLinks245M$PGPmatched))
  denom <- sum(myLinksPart == 1 & !is.na(forLinks245M$PGPmatched))
  
  if (denom == 0) {
    out$precision[c] <- 1
  } else {
    out$precision[c] <- numerator/denom
  }
  out$recall[c] <- numerator/sum(forLinks245M$PGPmatched == 1 & !is.na(forLinks245M$PGPmatched))
}

save(out, file = paste0("./245milPairs/redoInventory/precisionRecall/out-50trees-10M-", linkage, "-22k.Rdata"))
