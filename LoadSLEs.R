# Load AddHealth data loader
source("LoadAddH.R")

# Create dfs for SLEs in Wave III & IV addendum files
# Codebase changed to use readstata13. Settings maintain existing read output structure.
WA1 <- read.dta13(files[4], convert.factors = TRUE, generate.factors = FALSE)
WA1[] <- lapply(WA1, StataMissing)
WA1[is.na(WA1)] <- "."
WA2 <- read.dta13(files[5], convert.factors = TRUE, generate.factors = FALSE)
WA2[] <- lapply(WA2, StataMissing)
WA2[is.na(WA2)] <- "."
WA3 <- read.dta13(files[6], convert.factors = TRUE, generate.factors = FALSE)
WA3[] <- lapply(WA3, StataMissing)
WA3[is.na(WA3)] <- "."
WA4 <- read.dta13(files[7], convert.factors = TRUE, generate.factors = FALSE)
WA4[] <- lapply(WA4, StataMissing)
WA4[is.na(WA4)] <- "."
WA5 <- read.dta13(files[8], convert.factors = TRUE, generate.factors = FALSE)
WA5[] <- lapply(WA5, StataMissing)
WA5[is.na(WA5)] <- "."
WA6 <- read.dta13(files[9], convert.factors = TRUE, generate.factors = FALSE)
WA6[] <- lapply(WA6, StataMissing)
WA6[is.na(WA6)] <- "."

WA7 <- read.dta13(files[13], convert.factors = TRUE, generate.factors = FALSE)
WA7[] <- lapply(WA7, StataMissing)
WA7[is.na(WA7)] <- "."
WA8 <- read.dta13(files[14], convert.factors = TRUE, generate.factors = FALSE)
WA8[] <- lapply(WA8, StataMissing)
WA8[is.na(WA8)] <- "."
WA9 <- read.dta13(files[15], convert.factors = TRUE, generate.factors = FALSE)
WA9[] <- lapply(WA9, StataMissing)
WA9[is.na(WA9)] <- "."
WA10 <- read.dta13(files[16], convert.factors = TRUE, generate.factors = FALSE)
WA10[] <- lapply(WA10, StataMissing)
WA10[is.na(WA10)] <- "."


WaveIII_adds <- left_join(WA1, WA2)
WaveIII_adds <- left_join(WaveIII_adds, WA3)
WaveIII_adds <- left_join(WaveIII_adds, WA4)
WaveIII_adds <- left_join(WaveIII_adds, WA5)
WaveIII_adds <- left_join(WaveIII_adds, WA6)

WaveIV_adds <- left_join(WA7, WA8)
WaveIV_adds <- left_join(WaveIV_adds, WA9)
WaveIV_adds <- left_join(WaveIV_adds, WA10)
#WaveIV_adds <- left_join(WaveIV_adds, read.dta(files[17]))

# Remove intermediat addendum files
rm(list=ls(pattern = "WA"))

# Load SLEs
sles <- read.csv("./SLEs Temp/SLEs_Master.csv", stringsAsFactors = FALSE)[,2]
sles <- sles[sles != ""]
sles <- unique(sles)
# Create vector for all SLE vars in Waves df (includes In-School and Parental Q vars)
waveSles <- sles[(sles %in% names(Waves))]
# Create vectors for all SLE vars contained in addendum data sets for Waves III & IV
waveAdds <- sles[!(sles %in% names(Waves))]
# Create vector for all SLE vars contained in WaveIII_adds df
waveAddsIII <- unique(sort(waveAdds[waveAdds %in% names(WaveIII_adds)]))
# Create vector for all SLE vars contained in WaveIV_adds df
waveAddsIV <-unique(sort(waveAdds[waveAdds %in% names(WaveIV_adds)]))

# Test for completeness 
sles[!(sles %in% waveSles) & !(sles %in% waveAdds)]
waveAdds[!(waveAdds %in% waveAddsIII) & !(waveAdds %in% waveAddsIV)]

# Subet addendum dfs to relevent SLE vars
WaveIII_adds <- WaveIII_adds[c("AID", "RRELNO", "RPREGNO", waveAddsIII)] 
#WaveIII_adds <- spread_(WaveIII_adds, c("RRELNO", "RPREGNO"), waveAddsIII, sep = "_")
WaveIV_adds <- WaveIV_adds[c("AID", "PTNR_ID", "PRGNO", waveAddsIV)] 
#WaveIV_adds <- spread(WaveIV_rels, PTNR_ID, H4TR19, sep = "_")

# Test for completeness 
waveAdds[!(waveAdds %in% names(WaveIII_adds)) & !(waveAdds %in% names(WaveIV_adds))]

#rm(list=ls(pattern="OG"))



