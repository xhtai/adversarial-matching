# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

# first get zeros, ones and NAs from 245M
load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", 1), ".Rdata"))
allPairwise <- myPairs[which(myPairs$PGPmatched %in% c(0, 1)), ] # remove the NAs

for (i in 2:246) {
    if (i %% 10 == 0) cat(i, ", ")
    load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
    
    allPairwise <- rbind(allPairwise, myPairs[which(myPairs$PGPmatched %in% c(0, 1)), ])
    gc()
}

allPairwise <- allPairwise[order(allPairwise$PGPmatched, decreasing = TRUE), ] # put the 3653 ones on top
save(allPairwise, file = "./245milPairs/redoInventory/allPairwise.Rdata") # this should have 73M pairs

########### now run model 
sampledRFnowt <- function(allPairwise, similarityCols, sampleSize, mySeed) {
  sampled <- 1:3653
  sampleFrom <- 3653:nrow(allPairwise)
  set.seed(mySeed)
  sampled <- c(sampled, sample(sampleFrom, sampleSize))
  sampled <- sampled[order(sampled)]
  
  sampledPairwise <- allPairwise[sampled, ]
  
  trainX <- sampledPairwise[ , similarityCols]
  trainY <- sampledPairwise$PGPmatched
  
  K = 10
  n <- nrow(sampledPairwise)
  d = ceiling(n/K)
  set.seed(0)
  i.mix = sample(1:n)
  folds = vector(mode = "list", length = K)
  
  for (i in 1:K) {
    folds[[i]] <- i.mix[((i - 1)*d + 1):(i*d)]
  }
  
  preds <- rep(NA, nrow(trainX))
  
  for (k in 1:K) {
    cat("Fold", k, "\n")
    i.tr <- unlist(folds[-k])
    i.tr <- i.tr[!is.na(i.tr)]
    i.val <- folds[[k]]
    i.val <- i.val[!is.na(i.val)]
    
    x.tr <- trainX[i.tr, ]
    y.tr <- trainY[i.tr]
    
    set.seed(1)
    x.val <- trainX[i.val, ]
    
    tmpFit <- randomForest::randomForest(x = x.tr, y = as.factor(y.tr), ntree = 50)
    
    preds[i.val] <- predict(tmpFit, x.val, type = "vote")[, 2]
    gc()
  }
  
  set.seed(1)
  fit <- randomForest::randomForest(x = trainX, y = as.factor(trainY), ntree = 50)
  
  allPreds <- predict(fit, allPairwise[ , similarityCols], type = "vote")[, 2]
  allPreds[sampled] <- preds
  ret <- list(allPreds = allPreds, fit = fit)
  return(ret)
}

load("./245milPairs/redoInventory/allPairwise.Rdata")
out <- sampledRFnowt(allPairwise, 9:33, 3653, 0)
myPreds <- out$allPreds
save(myPreds, file = paste0("./245milPairs/redoInventory/preds3653.Rdata"))
fit <- out$fit
save(fit, file = paste0("./245milPairs/redoInventory/fit3653.Rdata"))

# repeat for 40000, 300000, 1000000, 5000000, 10000000



############### predict on all 245M for 10M model: put preds10M in myPairs
load("./245milPairs/redoInventory/preds10000000.Rdata")
load("./245milPairs/redoInventory/fit10000000.Rdata")
load("./245milPairs/redoInventory/allPairwise.Rdata")
allPairwise <- cbind(allPairwise[, c("hash1", "hash2", "PGPmatched")], myPreds)

for (i in 1:246) { 
  cat(i, ", ")
  print(Sys.time())
  load(file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
  tmpPreds <- predict(fit, myPairs[ , 9:33], type = "vote")[, 2]
  myPairs$preds10M <- tmpPreds
  
  # replace those that were predicted using folds initially
  tmp <- myPairs[!is.na(myPairs$PGPmatched), c("hash1", "hash2", "PGPmatched")]
  tmp <- dplyr::left_join(tmp, allPairwise[, c("hash1", "hash2", "myPreds")])
  myPairs$preds10M[!is.na(myPairs$PGPmatched)] <- tmp$myPreds # make sure order isn't changed from tmp and tmp2
  
  save(myPairs, file = paste0("./245milPairs/redoInventory/myPairs_", sprintf("%03d", i), ".Rdata"))
  gc()
}
