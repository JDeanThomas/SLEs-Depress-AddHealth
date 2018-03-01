#### Load Data

library(foreign)
library(dplyr)
library(tidyr)

# Find all .dta files in existing structure and subset relevent sets into list
files <- dir("./ICPSR_27021", recursive=TRUE, full.names=TRUE, pattern="\\.dta$")
files <- files[1:19]

WaveI <-read.dta(files[1])
WaveII <-read.dta(files[2])
WaveIII <- read.dta(files[3])
WaveIV  <- read.dta(files[12])

# Waves as one file. 
Waves <- left_join(WaveI, WaveII)
Waves <- left_join(Waves, WaveIII)
Waves <- left_join(Waves, WaveIV)

rm(WaveI, WaveII, WaveIII, WaveIV)

#### Construct dataset

# Construct Sexuality dataset for reported sexual attaction across waves
sexRisk <- as.data.frame(Waves$AID)
colnames(sexRisk) <- "AID" 



##### Code sex risk variables

# Number of Partners
# Use of Contraception
# Casual Sex


##### Number of Sexual Partners

## Number of partners WaveI (Waves$H1NR6)

# 1-900 - range 1 to 900 partners
# 996 -   refused
# 997 -	  legitimate skip
# 998 -	  don't know
# 999 -	  missing

sexRisk$num_partners_WaveI <- Waves$H1NR6

## Number of partners WaveII (Waves$H2NR8)
# Wave I and II may not be appropriate match, though listed as same question.
# See diffence in workding and distrobution of responces:
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/variablecollection?VariableCollectionId=747

# 1-987 - range 1 to 987 partners
# 996 -   refused	
# 997 -   legitimate skip
# 998 -   don't know

sexRisk$num_partners_WaveII <- Waves$H2NR8

## Number of partners WaveIII (Waves$H3SE3)

# -  -   missing (one case)
# 1-50 - partners range 1 to 50
# 96 -   refused
# 97 -	 legitimate skip
# 98 - 	 don't know
# 99 -	 not applicable

sexRisk$num_partners_WaveIII <- Waves$H3SE3

## Number of partners WaveIV (Waves$H4SE8)

# 1-800 - range (1-800) partners
# 995 - 995 partners
# 9995 - not asked in pretest
# 9996 - refused	
# 9997 - legitimate skip	
# 9998 - don't know

sexRisk$num_partners_WaveIV <- Waves$H4SE8


##### Casual sex


## Casual sex WaveI (Waves$H1NR5)

# 0 - no
# 1 - yes
# 6 - refused
# 8 - don't know
# 9 - not applicable

sexRisk$casual_WaveI[Waves$H1NR5 == "No"] <- 0
sexRisk$casual_WaveI[Waves$H1NR5 == "Yes"] <- 1
sexRisk$casual_WaveI[Waves$H1NR5 == "Refused"] <- 6
sexRisk$casual_WaveI[Waves$H1NR5 == "Don'y know"] <- 8
sexRisk$casual_WaveI[Waves$H1NR5 == "Not applicable"] <- 9

## Number of casual sex partners WaveI (Waves$H1NR45)

# 1-90 - range 1 to 90 people
# 96   - refused
# 97   - legitimate skip
# 98   - don't know
# 99   - not applicable

sexRisk$num_casual_WaveI <- Waves$H1NR45


## Casual sex WaveII (Waves$H2NR5)

# 0 - no
# 1 - yes
# 6 - refused
# 8 - don't know

sexRisk$casual_WaveII <- Waves$H2NR5

## Number of casual sex partners WaveII (Waves$H2NR80)

# 1-89 - range 1 to 89 people
# 96   - refused
# 97   - legitimate skip
# 98   - don't know

sexRisk$num_casual_WaveII <- Waves$H2NR80



# Casual sex WaveIII

# Casual sex WaveIV


##### Condom use

## Condom use WaveI (Waves$H1CO9)

# 1 - none of the time
# 2 - some of the time
# 3 - half of the time
# 4 - most of the time
# 5 - all of the time
# 6 - refused
# 7 - legitimate skip
# 8 - don't know

sexRisk$condoms_WaveI[Waves$H1CO8 == "None of the time"] <- 1
sexRisk$condoms_WaveI[Waves$H1CO8 == "Some of the time"] <- 2
sexRisk$condoms_WaveI[Waves$H1CO8 == "Half of the time"] <- 3
sexRisk$condoms_WaveI[Waves$H1CO8 == "Most of the time"] <- 4
sexRisk$condoms_WaveI[Waves$H1CO8 == "All of the time"] <- 5
sexRisk$condoms_WaveI[Waves$H1CO8 == "Refused"] <- 6
sexRisk$condoms_WaveI[Waves$H1CO8 == "Legitimate skip"] <- 7
sexRisk$condoms_WaveI[Waves$H1CO8 == "Don't know"] <- 8


# Condom use WaveII (Waves$H2CO10)

# 2 - some of the time
# 3 - half of the time
# 4 - most of the time
# 5 - all of the time
# 6 - refused
# 7 - legitimate skip
# 8 - don't know

sexRisk$condoms_WaveII <- Waves$H2CO10


write.csv(sexRisk, "SexRisk.csv")
