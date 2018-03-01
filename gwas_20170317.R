#### Load Data

library(foreign)

# Find all .dta files in existing structure and subset relevent sets into list
files <- dir("./ICPSR_27021", recursive=TRUE, full.names=TRUE, pattern="\\.dta$")
files <- files[1:19]

WaveIV  <- read.dta(files[12])

gwas <- as.data.frame(WaveIV$AID)
colnames(gwas) <- "AID" 


#####

# Transform codes and create risck score from selected variables (below)

# H4TO51 = 1
# H4TO52 = 1
# H4TO53 = 1
# H4TO55 = 1
# H4TO56 = 0
# H4TO58 = 1
# H4TO59 = 1 
# H4TO60 = 1

gwas$H4TO51[WaveIV$H4TO51 != "Yes"] <- 0
gwas$H4TO51[WaveIV$H4TO51 == "Yes"] <- 1
gwas$H4TO52[WaveIV$H4TO52 != "Yes"] <- 0
gwas$H4TO52[WaveIV$H4TO52 == "Yes"] <- 1
gwas$H4TO53[WaveIV$H4TO53 != "Yes"] <- 0
gwas$H4TO53[WaveIV$H4TO53 == "Yes"] <- 1
gwas$H4TO55[WaveIV$H4TO55 != "Yes"] <- 0
gwas$H4TO55[WaveIV$H4TO55 == "Yes"] <- 1
gwas$H4TO56[WaveIV$H4TO56 != "No"] <- 0
gwas$H4TO56[WaveIV$H4TO56 == "No"] <- 1
gwas$H4TO58[WaveIV$H4TO58 != "Yes"] <- 0
gwas$H4TO58[WaveIV$H4TO58 == "Yes"] <- 1
gwas$H4TO59[WaveIV$H4TO59 != "Yes"] <- 0
gwas$H4TO59[WaveIV$H4TO59 == "Yes"] <- 1
gwas$H4TO60[WaveIV$H4TO60 != "Yes"] <- 0
gwas$H4TO60[WaveIV$H4TO60 == "Yes"] <- 1


gwas$risk_sum <- rowSums(gwas[ ,2:9])

# Create drinking experience code 

# If H4TO33 = "no", then dependence = "."
# If H4TO61 = "yes", "no", or "don't know", then dependence = 1 
# If H4TO61 = "legitimate skip", then dependence = 0

gwas$drinking_exp[WaveIV$H4TO61 == "No" | WaveIV$H4TO61 == "Yes" | 
                      WaveIV$H4TO61 == "Don't know"] <- 1
gwas$drinking_exp[WaveIV$H4TO61 == "Legitimate skip"] <- 0
gwas$drinking_exp[WaveIV$H4TO61 == "Not asked on pretest"] <- ""
gwas$drinking_exp[WaveIV$H4TO33 == "No"] <- "."

table(gwas$drinking_exp)

# Cross tabulation of risk score and drinking experience 

gwas$cross_tab[gwas$risk_sum < 3 & gwas$drinking_exp == 0] <- 0
gwas$cross_tab[gwas$risk_sum >= 3 & gwas$drinking_exp == 1] <- 1

table(gwas$cross_tab)

CrossTable(gwas$risk_sum < 3, gwas$drinking_exp)


write.csv(gwas, "gwas_20170317.csv")