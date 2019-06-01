# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

library(randomForest)
load("./245milPairs/redoInventory/fit10000000.Rdata")
cbind(importance(fit)[order(importance(fit), decreasing = TRUE), ])
# [,1]
# idDist                       3690.23160
# titleJaccard                 1108.76982
# descriptionJaccard            697.31378
# profileJaccard                205.65588
# sameMarket                    160.07866
# diff_numTitleTokens           108.50909
# diff_numDescriptionTokens      92.54797
# diff_dailyFrac                 89.22076
# diff_meanPriceSold             88.36706
# diffSalesDates                 87.83630
# diff_medianPriceSold           86.46170
# diff_minPriceSold              86.26376
# diff_maxPriceSold              82.70426
# diff_priceRange                77.24157
# diff_numProfileTokens          76.60745
# totalSalesDays                 75.84752
# diff_numListingsWithFeedback   64.17463
# diff_totalFeedback             62.04777
# diff_daysActive                61.44199
# catUnitJaccard                 56.40430
# salesOverlap                   55.46365
# catJaccard                     54.86442
# diff_diversity                 51.32814
# catDosageUnitJaccard           38.82498
# catDosageJaccard               29.02738
# > 
#   