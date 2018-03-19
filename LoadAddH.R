# Load Data

library(readstata13)
library(foreign)
library(dplyr)
library(tidyr)

# Find all .dta files in existing structure and subset relevent sets into list
if(Sys.info()["nodename"] == "CLT-0716-1549")  {
    
    files <- list.files("C:/Users/u6006921/Dropbox/adhealth_sexuality/ad_helf_selected/Add_health_restricted_use/ICPSR_27021", recursive=TRUE, full.names=T, pattern="\\.dta$")
    setwd('C:/Users/u6006921/Dropbox/adhealth_sexuality')
} else {
    
    # Find all .dta files in existing structure and subset relevent sets into list
    files <- dir("./ICPSR_27021", recursive=TRUE, full.names=TRUE, pattern="\\.dta$")
}

files <- files[1:19]

 
# Function to add factor level "." to factor variables with NAs in order to maintain 
# existing code base after R 3.4 premenantly depreciated handling of factor levels
# missing explicit codes in Sata data files imported via foreign::read.dta()
StataMissing <- function(x){
    if(is.factor(x) && any(is.na(x))) return(factor(x, levels=c(levels(x), ".")))
    return(x)
}

# Codebase changed to use readstata13. Settings maintain existing read output structure.
WaveI <- read.dta13(files[1], convert.factors = TRUE, generate.factors = FALSE)
WaveI[] <- lapply(WaveI, StataMissing)
WaveI[is.na(WaveI)] <- "."
WaveII <- read.dta13(files[2], convert.factors = TRUE, generate.factors = FALSE)
WaveII[] <- lapply(WaveII, StataMissing)
WaveII[is.na(WaveII)] <- "."
WaveIII <- read.dta13(files[3], convert.factors = TRUE, generate.factors = FALSE)
WaveIII[] <- lapply(WaveIII, StataMissing)
WaveIII[is.na(WaveIII)] <- "."
WaveIV  <- read.dta13(files[12], convert.factors = TRUE, generate.factors = FALSE)
WaveIV[] <- lapply(WaveIV, StataMissing)
WaveIV[is.na(WaveIV)] <- "."

# Waves as one file. 
Waves <- full_join(WaveI, WaveII)
Waves <- full_join(Waves, WaveIII)
Waves <- full_join(Waves, WaveIV)

# WaveI   = 20745
# WaveII  = 14738
# WaveIII = 15197
# WaveIV  = 15701

length(unique(Waves$AID))

# Reduce individual Wave files to index lists of unique IDs 
WaveI <- WaveI$AID
WaveII <- WaveII$AID
WaveIII <- WaveIII$AID
WaveIV <- WaveIV$AID

# Wave III & IV realationship data - data for each Wave spans multiple files
# Relevant vars contained in first addendum for each. Structed and coded differnently
# Files are tall (and poorly coded), file must be flattened or scores summed by AID
WaveIII_rels <- read.dta(files[4])
WaveIV_rels <- read.dta(files[13])
#InSchool <- read.dta(files[19])

# Extract relevent relationship variables from relationship addendem files
# Flatten so each row is a unique case (AID)
WaveIII_rels <- read.dta(files[4])
WaveIV_rels <- read.dta(files[13])
# Added H3TR8 (had sex with partner), subsetted for cases who have and removed H3TR8
WaveIII_rels <- WaveIII_rels[c("AID", "RRELNO", "H3TR3", "H3TR8")]
WaveIII_rels <- spread(WaveIII_rels, RRELNO, H3TR3, sep = "_")
WaveIII_rels <- WaveIII_rels[WaveIII_rels$H3TR8 == "Yes, we have had sexual relations", ]
WaveIII_rels <- WaveIII_rels[-2]
WaveIV_rels <- WaveIV_rels[c("AID", "PTNR_ID", "H4TR19")]
WaveIV_rels <- spread(WaveIV_rels, PTNR_ID, H4TR19, sep = "_")

# Construct Sexuality dataset for reported sexual attaction across waves
sexVars <- as.character(read.csv("Sexual_Preference_Vars.csv", header = FALSE)[,1])
sexuality <- Waves[,sexVars[1:13]]
#sexuality <- Waves[,sexVars[c(1:28, seq(31, 41, by=2))]]
# Create sexuality2 df to test against reported sexual activities over waves
sexuality2 <- Waves[,c("AID", sexVars[c(14:28, seq(31, 41, by=2))])]

# Join full rels dfs to AID vector for last sexuality test matrix of partners sex
fullTest <- data.frame(Waves$AID)
colnames(fullTest) <- "AID" 
fullTest$AID <- as.character(fullTest$AID)
fullTest <- left_join(fullTest, WaveIII_rels)
fullTest <- left_join(fullTest, WaveIV_rels)

# Remove intermediate DFs
rm(WaveIII_rels, WaveIV_rels)
