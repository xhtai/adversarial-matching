# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

load("./245milPairs/redoInventory/reorg/evalPairs.Rdata")

load("./245milPairs/redoInventory/linkages/myLinks_minimax0.74.Rdata")
#### distribution for minimax0.74 links:
#         0         1 
# 245575883     12320 

# 12320 obs
  # 8653 pairs have missing key (is.na(PGPmatched))
  # 757 pairs have different key (PGPmatched == 0)
  # 2910 are PGP matches

out <- aggregate(evalPairs$idDist[myLinksPart == 1], list(idDist = evalPairs$idDist[myLinksPart == 1]), length)
#   idDist    x  ----- count
# 1       0 8233
# 2       1 1764
# 3       2  909
# 4       3  415
# 5       4  215
# 6       5  104
# 7       6  107
# 8       7   89
# 9       8  108
# 10      9   71
# 11     10   59
# 12     11   83
# 13     12   86
# 14     13   44
# 15     14   16
# 16     15    9
# 17     16    7
# 18     17    1

# also for myLinksPart == 1 & evalPairs$PGPmatched == 0 & !is.na(evalPairs$PGPmatched)
# 757 pairs --- these explicitly have non-overlapping PGPs and the model found them to be links, many of them have different IDs too and might be adversarial
aggregate(evalPairs$idDist[myLinksPart == 1 & evalPairs$PGPmatched == 0 & !is.na(evalPairs$PGPmatched)], list(idDist = evalPairs$idDist[myLinksPart == 1 & evalPairs$PGPmatched == 0 & !is.na(evalPairs$PGPmatched)]), length)
#   idDist   x
# 1       0 391
# 2       1 145
# 3       2 100
# 4       3  30
# 5       4  14
# 6       5   8
# 7       6   8
# 8       7  12
# 9       8  12
# 10      9   9
# 11     10   8
# 12     11  12
# 13     12   2
# 14     13   2
# 15     14   2
# 16     15   2

# 8653 pairs: these just did not post a key --- not explicitly adversarial
# also for myLinksPart == 1 & is.na(evalPairs$PGPmatched)
out <- aggregate(evalPairs$idDist[myLinksPart == 1 & is.na(evalPairs$PGPmatched)], list(idDist = evalPairs$idDist[myLinksPart == 1 & is.na(evalPairs$PGPmatched)]), length)
#     idDist    x
# 1       0 5841
# 2       1 1232
# 3       2  598
# 4       3  273
# 5       4  154
# 6       5   78
# 7       6   80
# 8       7   64
# 9       8   71
# 10      9   55
# 11     10   34
# 12     11   53
# 13     12   68
# 14     13   26
# 15     14   14
# 16     15    6
# 17     16    5
# 18     17    1

aggregate(evalPairs$idDist[myLinksPart == 1 & evalPairs$PGPmatched == 1 & !is.na(evalPairs$PGPmatched)], list(idDist = evalPairs$idDist[myLinksPart == 1 & evalPairs$PGPmatched == 1 & !is.na(evalPairs$PGPmatched)]), length)
# idDist    x
# 1       0 2001
# 2       1  387
# 3       2  211
# 4       3  112
# 5       4   47
# 6       5   18
# 7       6   19
# 8       7   13
# 9       8   25
# 10      9    7
# 11     10   17
# 12     11   18
# 13     12   16
# 14     13   16
# 15     15    1
# 16     16    2

# save above output as text files

################################################################################
predictedMatch <- read.table("/Users/xtai/Desktop/kddtodo/idDistEval/predictedMatch.txt")
predictedMatch <- predictedMatch[, 2:3]
colnames(predictedMatch) <- c("idDist", "count")

PGP0 <- read.table("/Users/xtai/Desktop/kddtodo/idDistEval/predictedMatchPGP0.txt")
PGP0 <- PGP0[, 2:3]
colnames(PGP0) <- c("idDist", "PGP0count")

PGP1 <- read.table("/Users/xtai/Desktop/kddtodo/idDistEval/predictedMatchPGP1.txt")
PGP1 <- PGP1[, 2:3]
colnames(PGP1) <- c("idDist", "PGP1count")


PGPNA <- read.table("/Users/xtai/Desktop/kddtodo/idDistEval/predictedMatchPGPNA.txt")
PGPNA <- PGPNA[, 2:3]
colnames(PGPNA) <- c("idDist", "PGPNAcount")


predictedMatch <- dplyr::left_join(predictedMatch, PGP0)
predictedMatch <- dplyr::left_join(predictedMatch, PGP1)
predictedMatch <- dplyr::left_join(predictedMatch, PGPNA)

predictedMatch[is.na(predictedMatch)] <- 0

predictedMatch$PGP0countFrac <- predictedMatch$PGP0count/sum(predictedMatch$PGP0count)
predictedMatch$PGP1countFrac <- predictedMatch$PGP1count/sum(predictedMatch$PGP1count)
predictedMatch$PGPNAcountFrac <- predictedMatch$PGPNAcount/sum(predictedMatch$PGPNAcount)
save(predictedMatch, file = "/Users/xtai/Desktop/kddtodo/idDistEval/predictedMatch.Rdata")


############# PLOT
matlab_colors <- c(rgb(0.9290, 0.6940, 0.1250),
                   rgb(0.4940, 0.1840, 0.5560),
                   rgb(0.4660, 0.6740, 0.1880),
                   rgb(0.3010, 0.7450, 0.9330),
                   rgb(0.6350, 0.0780, 0.1840))


png(file = paste0("/Users/xtai/Desktop/kddtodo/idDistEval/idDists.png"), width = 5, height = 4, units = "in", res = 300)
# tikz(file = paste0("/Users/xtai/Desktop/kddtodo/idDistEval/idDists.tex"), width = 5, height = 4, standAlone = TRUE)
par(mar = c(5.1, 5.1, 4.1, 2.1))
plot(predictedMatch$idDist, predictedMatch$PGP0countFrac, 
     type = "l", 
     ylim = c(0, .7), 
     main = "ID Distance for Predicted Matches", 
     xlab = "ID Distance", 
     ylab = "Fraction", 
     lwd = 2, 
     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, col = matlab_colors[1])

lines(predictedMatch$idDist, predictedMatch$PGP1countFrac, type = "l", 
      col = matlab_colors[2], lwd = 2)
lines(predictedMatch$idDist, predictedMatch$PGPNAcountFrac, type = "l", 
      col = matlab_colors[3], lwd = 2)

legend("topright", legend = c(paste0("PGP non-match (N = ", sum(predictedMatch$PGP0count), ")"),
                              paste0("PGP match (N = ", sum(predictedMatch$PGP1count), ")"),
                              paste0("PGP NA (N = ", sum(predictedMatch$PGPNAcount), ")")), 
       col = matlab_colors[1:3], lty = 1, lwd = 2, cex = 1)


dev.off()


