# Validate the SLE index in AddHealth.

# 1) create proto_SLE = (# endorsed item) / (total number of items for that subject)

# 2) standardize proto_SLE by wave: SLE_index = (proto_SLE - (wave-specific mean 
# of proto_SLE)) / (SD of proto_SLE)

#3) assess association of SLE_index (and each SL item separately) to the depression 
# measures in "*Dropbox\adhealth_sexuality\dep9_good.dta"

source("CodeSLEs.R")

require(readstata13)
require(stats)

depress <- read.dta13("~/Dropbox/adhealth_sexuality/dep9_good.dta", convert.factors = FALSE)

depress <- depress[match(sleCodes$AID, depress$aid),]
depress <- depress[which(WaveI %in% depress$aid), ]
rownames(depress) <- NULL

#sleCodes <- read.csv("~/Dropbox/adhealth_sexuality/sleCodes.csv")[,-1]
#colnames(sleCodes)[1] <- "AID"
#sleCodes[is.na(sleCodes)] <- 0
# Convert all missing values codes (".") to NA and all factors to numeric
sleCodes <- data.frame(apply(sleCodes, 2, function(x) as.numeric(x)))

indexSLEs <- data.frame(sleCodes$AID)
colnames(indexSLEs)[1] <- "AID"

# proto_SLEs
indexSLEs$proto_SLE <- rowMeans(sleCodes[ ,-1], na.rm = TRUE)

# Wave specific means
indexSLEs$proto_SLE_W1 <- ifelse(!Waves$AID %in% WaveI, NA,
                                 rowMeans(sleCodes[ ,grep("_W1", names(sleCodes))], na.rm = TRUE))
indexSLEs$proto_SLE_W2 <- ifelse(!Waves$AID %in% WaveII, NA,
                                 rowMeans(sleCodes[ ,grep("_W2", names(sleCodes))], na.rm = TRUE))
#indexSLEs$proto_SLE_W2[!WaveII %in% Waves$AID] <- NA
indexSLEs$proto_SLE_W3 <- ifelse(!Waves$AID %in% WaveIII, NA,
                                 rowMeans(sleCodes[ ,grep("_W3", names(sleCodes))], na.rm = TRUE))
#indexSLEs$proto_SLE_W3[!WaveIII %in% Waves$AID] <- NA
indexSLEs$proto_SLE_W4 <- ifelse(!Waves$AID %in% WaveIV, NA,
                                 rowMeans(sleCodes[ ,grep("_W4", names(sleCodes))], na.rm = TRUE))

#indexSLEs$proto_SLE_W2 <- rowMeans(sleCodes[ ,grep("_W2", names(sleCodes))], na.rm = TRUE)
#indexSLEs$proto_SLE_W1[!Waves$AID %in% WaveI] <- NA

#w4 <- grep("_W2", names(sleCodes))
#sleCodes[apply(is.na(sleCodes[, w4]), 1, all), ]

# Standardized proto_SLEs by Wave
# SLE_index = (proto_SLE - (wave-specific mean of proto_SLE)) / (SD of proto_SLE)
indexSLEs$SLE_index_W1 <- (indexSLEs$proto_SLE - indexSLEs$proto_SLE_W1) / sd(indexSLEs$proto_SLE)
indexSLEs$SLE_index_W2 <- (indexSLEs$proto_SLE - indexSLEs$proto_SLE_W2) / sd(indexSLEs$proto_SLE)
indexSLEs$SLE_index_W3 <- (indexSLEs$proto_SLE - indexSLEs$proto_SLE_W3) / sd(indexSLEs$proto_SLE)
indexSLEs$SLE_index_W4 <- (indexSLEs$proto_SLE - indexSLEs$proto_SLE_W4) / sd(indexSLEs$proto_SLE)

# For time being, drop found additions in Wave II & III from indexSLEs
# Circle back to this. Waves reconstructed to include for throughness
# Will not affect calculations if using original Waves df
table(!sleCodes$AID %in% depress$aid)
indexSLEs <- indexSLEs[which(depress$aid %in% indexSLEs$AID), ]



#3) Association of standardized SLE_indexs with depression inicators

par(mfrow=c(2, 2))
hist(indexSLEs$SLE_index_W1, probability = TRUE, xlim = c(-6, 6), ylim = c(0, .75))
lines(density(indexSLEs$SLE_index_W1, adjust=3, na.rm = TRUE), col="red")
hist(indexSLEs$SLE_index_W2, probability = TRUE, xlim = c(-6, 6), ylim = c(0, .75))
lines(density(indexSLEs$SLE_index_W2, adjust=3, na.rm = TRUE), col="red")
hist(indexSLEs$SLE_index_W3, probability = TRUE, xlim = c(-6, 6), ylim = c(0, .75))
lines(density(indexSLEs$SLE_index_W2, adjust=3, na.rm = TRUE), col="red")
hist(indexSLEs$SLE_index_W4, probability = TRUE, xlim = c(-6, 6), ylim = c(0, .75))
lines(density(indexSLEs$SLE_index_W4, adjust=3, na.rm = TRUE), col="red")


cor(indexSLEs$proto_SLE, depress$dep9, use = "complete.obs")
cor(indexSLEs$proto_SLE, depress$newvar, use = "complete.obs")

cor(indexSLEs$SLE_index_W1, scale(depress$dep9, scale = sd(depress$dep9)), use = "complete.obs")

par(mfrow=c(2, 1))
plot(indexSLEs$proto_SLE, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$proto_SLE))
plot(indexSLEs$SLE_index_W1, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$SLE_index_W1))


cor(indexSLEs[ ,7:10], depress$dep9, use = "complete.obs")
cor(indexSLEs[ ,7:10], depress$newvar, use = "complete.obs")

par(mfrow=c(2, 2))
plot(indexSLEs$SLE_index_W1, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$SLE_index_W1))
plot(indexSLEs$SLE_index_W2, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$SLE_index_W2))
plot(indexSLEs$SLE_index_W3, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$SLE_index_W3))
plot(indexSLEs$SLE_index_W4, depress$dep9)
abline(lm(depress$dep9 ~ indexSLEs$SLE_index_W4))



#cor(sleCodes[ ,grep("_W1", names(sleCodes))], as.numeric(depress[ ,10]), use = "complete.obs")
#cor(sleCodes[ ,grep("_W1", names(sleCodes))], as.numeric(depress[ ,11]), use = "complete.obs")


# For time being, drop found additions in Wave II & III from sleCodes
# Circle back to this. Waves reconstructed to include for throughness
# Will not affect calculations if using original Waves df
table(!sleCodes$AID %in% depress$aid)
sleCodes <- sleCodes[which(depress$aid %in% sleCodes$AID), ]


cor(sleCodes[ ,2:174], as.numeric(depress[ ,10]), use = "pairwise.complete.obs")
cor(sleCodes[ ,2:174], as.numeric(depress[ ,11]), use = "pairwise.complete.obs")





cor(indexSLEs[ ,7:10], as.numeric(depress[ ,2]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,3]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,4]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,5]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,6]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,7]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,8]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,9]), use = "complete.obs")
cor(indexSLEs[ ,7:10], as.numeric(depress[ ,10]), use = "complete.obs")













#########################################

# Repondents in Waves II and III not in any of other 3 waves
m2 <- WaveII$AID[which(!WaveII$AID %in% Waves$AID)]
m3 <- WaveIII[which(!WaveIII %in% Waves$AID)]

length(sleCodes$AID[!(WaveII)])


table(!Waves$AID %in% WaveII)
table(!WaveII %in% Waves$AID)

Waves <- full_join(WaveI, WaveII)
Waves <- full_join(Waves, WaveIII)
Waves <- full_join(Waves, WaveIV)

which(!names(Waves) %in% names(Waves2))



# Test
test <- as.data.frame(cbind(c(1, 2, 3, 4 ,5), c(1, 3, 4, NA, 4), c(4, NA, 3, NA, NA), 
              c(NA, NA, NA, NA, 1), c(4, 1, 2, ".", NA)))
# Make one a factor
test[5] <- lapply(test[5], factor)
# Index
t <- c(2, 4:5)
# Convert factor colums to numeric (or integer)
test <- apply(test, 2, function(x) as.numeric(x))
# Find subset of colums who rows are all NAs
test[apply(is.na(test[, t]), 1, all), ]


