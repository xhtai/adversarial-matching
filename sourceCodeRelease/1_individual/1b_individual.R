# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

#### Step 2
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep1.Rdata")

min(outStep1$feedback$date)
# [1] "1970-01-01" --------------- 76 of this
min(outStep1$feedback$date[outStep1$feedback$date != "1970-01-01"])
# [1] "2011-05-22"
max(outStep1$feedback$date)
# [1] "2018-08-21"

system.time(outStep2 <- runStep2(outStep1$feedback, outStep1$items, outStep1$users, outStep1$profileClean, outStep1$descriptionClean, outStep1$PGPclean, "2011-05-22", "2018-08-22"))
# 1-2 hours

save(outStep2, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2.Rdata")

#########################  Addendum: step2_addendum.R in heisenbrgr #########################  
# add to account-level information using users$profile and items$description (previously only use parse databases, but not all have parsed data)

library(heisenbrgr)
users <- read.csv("/media/xtai/4AF561E807105059/markets/data/users_alphabay.csv", colClasses = c("character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character"), na.strings = NULL) # only read hash_str and profile
users_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_emcdda_2018_users.csv", colClasses = c("character", "NULL", "NULL", "NULL", "NULL", "NULL", rep("NULL", 3), "NULL", "NULL", "NULL", "NULL", "character", rep("NULL", 39)), na.strings = NULL) # 8238 obs
users <- rbind(users, users_new)
users <- users[duplicated(users$hash) == FALSE, ]

# ITEMS data
items <- read.csv("/media/xtai/4AF561E807105059/markets/data/items_alphabay.csv", colClasses = c("character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL"), na.strings = NULL) # 230643 obs -- only read in the description
items_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_emcdda_2018_items.csv", colClasses = c("character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL"), na.strings = NULL) # 130897 obs
items <- rbind(items, items_new)
items <- items[duplicated(items$hash) == FALSE, ]

#####
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2.Rdata")
# there are 22163 users (out of 22287 users in users.Rdata, 22163 have some feedback)

out1 <- cleanProfile(users) # out1$retrievedPGPs (3386 rows and 3372 unique vendor_hashes, i.e. some had more than one PGP in profile) and out1$users
out2 <- cleanDescriptions(items) # out2$retrievedPGPs has 918 entries, with 822 unique vendor_hashes, i.e. 822 more with posted PGP key

outStep2$profileTokens <- profileFromUsers(out1$users, outStep2$profileTokens)
outStep2$descriptionTokens <- descriptionFromItems(out2$items, outStep2$descriptionTokens)

PGPclean <- rbind(out1$retrievedPGPs, out2$retrievedPGPs)
PGPclean <- PGPclean[duplicated(PGPclean) == FALSE, ]

outStep2$PGPlist <- PGPfromUsersItems(PGPclean, outStep2$PGPlist)

outStep2$final$numProfileTokens <- unlist(lapply(outStep2$profileTokens, length), use.names = FALSE)
outStep2$final$numDescriptionTokens <- unlist(lapply(outStep2$descriptionTokens, length), use.names = FALSE)

save(outStep2, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2Add.Rdata")


