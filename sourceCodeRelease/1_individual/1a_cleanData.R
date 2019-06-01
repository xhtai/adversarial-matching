# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
####################### data: ####################### 
#### users, items and feedback --- just one snapshot
# 1. users_alphabay.csv: analysis_alphabay.db 
# 2. analysis_2018_users.csv: analysis_2018.db (Traderoute, Valhalla, Dream)
# 3. items_alphabay.csv
# 4. analysis_2018_items.csv
# 5. feedbacks_alphabay.csv
# 6. analysis_2018_feedbacks.csv

# 7. parse_users.csv
# 8. parse_users_alphabay.csv: alphabay-monero-clean.db
# 9. parse_items.csv
# 10. parse_items_alphabay.csv: alphabay-monero-clean.db
# no parsed databases for Traderoute, Valhalla, Dream

################## issues to fix with data before starting: ##################
# - cat users, items, feedbacks_alphabay.csv with new users, items, feedbacks
# - fix SR2 item titles in items (there could be 1:many matches; just pick one randomly)

# - in parsedUsers and parsedItems fix case for Alphabay (AlphaBay -> Alphabay)
# - parsedUsers -- cat Alphabay's PGP to profile col,
# there are 19 cases of this in PGP col:  -----BEGIN PGP M
# - parsedItems -- special processing for SR2 and Agora
# 	- SR2:  "Overall Average ... description" -- remove these.
# 	- Agora: gets this BTC \n \n sequence -- remove this
# - cat parsedUsers and parsedUsersAlphabay, parsedItems and parsedItemsAlphabay
####################################################################################


#################################### 1. USERS data ################################### 
users <- read.csv("/home/xtai/Desktop/markets/data/users_alphabay.csv", colClasses = c("character", "character", "character", "numeric", "numeric", "numeric", "character", "character", "character", "numeric", "NULL"), na.strings = NULL) # NULL for profile description
# hash_str,marketplace,id,total_sales,percentile,sales_per_day,first_observed,last_observed,category,diversity,profile

users_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_2018_users.csv", colClasses = c("character", "character", "character", "numeric", "numeric", "numeric", rep("NULL", 3), "character", "character", "character", "numeric", rep("NULL", 40)), na.strings = NULL) # 8238 obs
# hash_str,marketplace,id,total_sales,percentile,sales_per_day,last_90,last_30,last_7,first_observed,last_observed,category,diversity,profile,benzos_total,benzos_30,cannabis_total,cannabis_30,cocaine_total,cocaine_30,digital_goods_total,digital_goods_30,dissociatives_total,dissociatives_30,hallucinogens_total,hallucinogens_30,misc_total,misc_30,nps_cannabis_total,nps_cannabis_30,nps_dissociatives_total,nps_dissociatives_30,nps_hallucinogens_total,nps_hallucinogens_30,nps_opioids_total,nps_opioids_30,nps_other_total,nps_other_30,nps_stimulants_total,nps_stimulants_30,opioids_total,opioids_30,prescription_total,prescription_30,psychedelics_total,psychedelics_30,stimulants_total,stimulants_30,steroids_total,steroids_30,other_total,other_30,csv

users <- rbind(users, users_new)
users <- users[duplicated(users$hash_str) == FALSE, ] # 1638 are duplicated, and theonlinesource and color have different percentiles

save(users, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/users.Rdata")

#################################### 2. ITEMS data ################################### 
items <- read.csv("/home/xtai/Desktop/markets/data/items_alphabay.csv", colClasses = c("character", "character", "character", "character", "character", "numeric", "character", "character", "character", "numeric", "character", "character", "NULL", "character"), na.strings = NULL) # 230643 obs -- don't read in the description
# hash_str,marketplace,title,vendor,vendor_hash,total_sales,first_observed,last_observed,prediction,prediction_number,ships_to,ships_from,description,source

items_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_2018_items.csv", colClasses = c("character", "character", "character", "character", "character", "numeric", "character", "character", "character", "numeric", "character", "character", "NULL", "character", "NULL"), na.strings = NULL) # 130897 obs
# hash_str,marketplace,title,vendor,vendor_hash,total_sales,first_observed,last_observed,prediction,prediction_number,ships_to,ships_from,description,source,sales

items <- rbind(items, items_new)
items <- items[duplicated(items) == FALSE, ]
# 13140 valhalla duplicated -- entire rows

############# fix items: SR2 item titles
parsedItems <- read.csv("/home/xtai/Desktop/markets/data/parse_items.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "character", rep("NULL", 8), "character", rep("NULL", 4), "character", "character")) # 9200943 rows
parsedItemsSR <- parsedItems[parsedItems$marketplace == "Silk Road 2", ] # 4655647

# fix the SR2 titles: match item$title to title from parsedItems$source
items$marketplaceIDtitle <- paste0(items$marketplace, items$vendor, items$title)

# need to add parsedItems$item_hash by matching on ID-itemtitle, where item title is from source
parsedItemsSR$processedTitle <- unlist(lapply(lapply(parsedItemsSR$source, FUN = function(x) strsplit(x, split = "/items/", fixed = TRUE)[[1]][2]), FUN = function(x) strsplit(x, split = "?", fixed = TRUE)[[1]][1]), use.names = FALSE) # get the processed ones instead of the ones in "title"

parsedItemsSR <- parsedItemsSR[, c("marketplace", "seller_id", "title", "processedTitle")]
# processedTitles might match to > 1 title because of case differences

# in items there is only 1 entry for this and one item hash -- just ignore the different case and randomly choose one
parsedItemsSR <- parsedItemsSR[duplicated(parsedItemsSR[, c("marketplace", "seller_id", "title", "processedTitle")]) == FALSE, ] # 45651 rows

parsedItemsSR <- parsedItemsSR[duplicated(parsedItemsSR[, c("marketplace", "seller_id", "processedTitle")]) == FALSE, ] # 45565 rows (86 are duplicated)

parsedItemsSR$marketplaceIDtitle <- paste0(parsedItemsSR$marketplace, parsedItemsSR$seller_id, parsedItemsSR$processedTitle)

parsedItemsSR$item_hash <- items$hash_str[match(parsedItemsSR$marketplaceIDtitle, items$marketplaceIDtitle)]

tmpTitles <- parsedItemsSR$title[match(items$hash_str, parsedItemsSR$item_hash)]

items$title[items$marketplace == "Silk Road 2"] <- tmpTitles[items$marketplace == "Silk Road 2"]

save(items, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/itemsFixed.Rdata")



################################### 3. FEEDBACK ##################################
feedback <- read.csv("/home/xtai/Desktop/markets/data/feedbacks_alphabay.csv", colClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "character"), na.strings = NULL)
# 4203382 rows
# hash_str,marketplace,item_hash,date,giver,reciever,message,order_title,feedback_value,order_amount,order_amount_usd,source

# feedback_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_2018_feedbacks.csv", colClasses = c("character", "character", "character", "character", "character", "character", "NULL", "character", "numeric", "numeric", "numeric", "character"), na.strings = NULL, skip = 499999, nrow = 100000) # format is exact same. Don't read in feedback message

feedback_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_2018_feedbacks.csv", colClasses = c("character", "character", "character", "character", "character", "character", "NULL", "character", "NULL", "numeric", "numeric", "character"), na.strings = NULL) # format is exact same. Don't read in feedback message, feedback_value
# 1918971
# problem with feedback_value --- some are missing so drop this column (we are not using it anyway)

# scan("/home/xtai/Desktop/markets/data/analysis_2018_feedbacks.csv", skip = 220000, nlines = 1, sep = "x", allowEscapes = FALSE, what = "character")

feedback <- rbind(subset(feedback, select = -(c(message, feedback_value))), feedback_new) # 6122353

### first do this
dupAll <- duplicated(feedback)
feedback <- feedback[dupAll == FALSE, ] # 5768829

# the cols we care about are: marketplace, item_hash, date,

feedback <- feedback[, c("hash_str", "date", "item_hash", "marketplace", "order_amount_usd")]
newDups <- duplicated(feedback)
feedback <- feedback[newDups == FALSE, ]

diffDate <- duplicated(subset(feedback, select = -date))

# just choose any random one
feedback <- feedback[diffDate == FALSE, ]

save(feedback, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/feedbackFixed.Rdata")

################################## 4. Parsed users ######################################
parsedUsers <- read.csv("/home/xtai/Desktop/markets/data/parse_users.csv", stringsAsFactors = FALSE) # 865323 rows
parsedUsers$profile <- paste(parsedUsers$profile, parsedUsers$PGP) # there are some alphabay in here

parsedUsersAB <- read.csv("/home/xtai/Desktop/markets/data/parse_users_alphabay.csv", stringsAsFactors = FALSE) # 1156854 rows
parsedUsersAB$profile <- paste(parsedUsersAB$profile, parsedUsersAB$PGP)
# parsedUsersAB <- parsedUsersAB[, -which(names(parsedUsersAB) == "PGP")]

parsedUsers <- rbind(parsedUsers[, c("marketplace", "date", "id", "profile")], parsedUsersAB[, c("marketplace", "date", "id", "profile")]) # 2022177 rows

parsedUsers$marketplace[parsedUsers$marketplace == "AlphaBay"] <- "Alphabay" # all lower case in the non-parse databases

parsedUsers <- parsedUsers[parsedUsers$profile != "" & parsedUsers$id != "", ]
parsedUsers <- parsedUsers[duplicated(parsedUsers) == FALSE, ]
# got rid of 1290676 rows --- 731501 left

save(parsedUsers, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/parsedUsers.Rdata")

################################# 5. Parsed items #######################################
parsedItemsAB <- read.csv("/home/xtai/Desktop/markets/data/parse_items_alphabay.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "character", rep("NULL", 8), "character", rep("NULL", 4), "character", "character", rep("NULL", 7))) # 2431636 rows

parsedItems <- read.csv("/home/xtai/Desktop/markets/data/parse_items.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "character", rep("NULL", 8), "character", rep("NULL", 4), "character", "character")) # 9200943 rows

parsedItems <- rbind(parsedItems[, c("marketplace", "date", "seller_id", "listing_description", "title")], parsedItemsAB[, c("marketplace", "date", "seller_id", "listing_description", "title")])

parsedItems$marketplace[parsedItems$marketplace == "AlphaBay"] <- "Alphabay" # all lower case in the non-parse databases

parsedItems <- parsedItems[parsedItems$listing_description != "" & parsedItems$seller_id != "" & parsedItems$title != "", ]
parsedItems <- parsedItems[duplicated(parsedItems) == FALSE, ] # 4.7 million rows

####################### remaining issues ######################
# - parsedItems -- special processing for SR2 and Agora
# 	- SR2:  "Overall Average ... description" -- remove these.
# 	- Agora: gets this BTC \n \n sequence -- remove this

parsedItems <- parsedItems[!is.na(parsedItems$marketplace), ]

#### Agora
parsedItems[parsedItems$marketplace == "Agora", "listing_description"] <- unlist(lapply(strsplit(parsedItems[parsedItems$marketplace == "Agora", "listing_description"], split = "\n    \n    Brought to you by:", fixed = TRUE), FUN = function(x) x[1]))
parsedItems[parsedItems$marketplace == "Agora", "listing_description"] <- unlist(lapply(strsplit(parsedItems[parsedItems$marketplace == "Agora", "listing_description"], split = "\n\n    \n    ", fixed = TRUE), FUN = function(x) x[2]))

parsedItems[parsedItems$marketplace == "Agora" & nchar(parsedItems$listing_description) == 1 & !(parsedItems$listing_description %in% c("3", "O", ".", "X")), "listing_description"] <- ""

######## now do SR2
## remove empty descriptions -- in SR2 if it's empty it becomes
# 30 day average: 4.93\n    60 day average: 4.94\n    Overall average: 4.94
# some are just "Overall average: " -- probably been up for less than 30 days
parsedItems <- parsedItems[!(parsedItems$marketplace == "Silk Road 2" & grepl("Overall average: ([0-9.]+)$", parsedItems$listing_description) == 1), ]
# 4722663 --> 4533246

save(parsedItems, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/parsedItemsFixed.Rdata")




########################### STEP 1 ########################### 
load("/home/xtai/Desktop/tmp_9-5/data_10-15/feedbackFixed.Rdata") # 5704913 million rows
load("/home/xtai/Desktop/tmp_9-5/data_10-15/itemsFixed.Rdata") # 348400
load("/home/xtai/Desktop/tmp_9-5/data_10-15/users.Rdata") # 22287
load("/home/xtai/Desktop/tmp_9-5/data_10-15/parsedUsers.Rdata") # 730,000 rows
load("/home/xtai/Desktop/tmp_9-5/data_10-15/parsedItemsFixed.Rdata")

library(heisenbrgr)
#### Step 1
system.time(outStep1 <- runStep1_2(feedback, items, users, parsedUsers, parsedItems)) # use runStep1_2 instead of runStep1 --- for inventory use Nicolas's code instead
save(outStep1, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/outStep1.Rdata")
# takes something like 6 hours; 43 warnings
# 1: In extractPGPs(parsedUsers[i, "profile"]) :
#   Invalid PGP -----BEGIN PGP PUBLIC KEY BLOCK-----
# and -----END PGP PUBLIC KEY BLOCK
# Invalid PGP -----BEGIN PGP PUBLIC KEY BLOCK----- och slutar med -----END PGP PUBLIC KEY BLOCK
# Invalid PGP -----BEGIN PGP PUBLIC KEY BLOCK----- ALL THE **** IN BETWEEN -----END PGP PUBLIC KEY BLOCK
# Invalid PGP -----BEGIN PGP PUBLIC KEY BLOCK-----" and here too :  "- -----END PGP PUBLIC KEY BLOCK

##### inventory
load("/home/xtai/Desktop/tmp_9-5/data_10-15/feedbackFixed.Rdata") # 5704913 million rows
load("/home/xtai/Desktop/tmp_9-5/data_10-15/itemsFixed.Rdata") # 348400

items <- items[items$hash_str %in% feedback$item_hash, c("hash_str", "marketplace", "title", "vendor", "vendor_hash", "prediction")]
# then extract inventories from here

write.csv(items[, c("hash_str", "title")], "/home/xtai/Desktop/tmp_9-5/1-27pythonInventories/items.csv", row.names = FALSE)

#### THEN DO IN PYTHON
# 1-27redoInventory.py

############################## HERE: Step 1 #############################
# items needs hash_str, vendor_hash, prediction, dosage, unit
itemsOut <- read.csv("/home/xtai/Desktop/tmp_9-5/1-27pythonInventories/itemsOut.csv", stringsAsFactors = FALSE) 
names(itemsOut)[3:4] <- c("dosage", "unit")

items <- cbind(items, itemsOut[, 3:4])
outStep1$items <- items
save(outStep1, file = "/home/xtai/Desktop/tmp_9-5/data_10-15/outStep1.Rdata")

