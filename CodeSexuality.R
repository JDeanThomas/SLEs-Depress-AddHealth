source("LoadAddH.R")

######################################

# 1) Create a small dataset containing only these variables. 
# 2) Code a couple of specifications of sexuality. 
# Current (using the "are you attracted to fe/male" question).
# Ever. 
# And a stringent coding flagging individuals who give conflicting info 
# across the various measures (for instance those who have never had same sex 
# attraction, but list >0 same sex partners.    


# Two respondents gave answers "Don't Know" or "Refused" for WavesI-III
# Both supplied response of "Male" in WaveIV
# Recoding both to "Male"
dn <- which(sexuality$BIO_SEX != "Male" & sexuality$BIO_SEX != "Female")
sexuality$BIO_SEX[dn] <- "Male"
rm(dn)


# Change of gender ID across waves
# 18 males and 6 females reported differnt gender ID in follow up Waves than in WaveI

# 0 - No self-reported change in gender ID across Waves
# 1 - Self-reported change in gender ID after intital declaration in WaveI

sexuality$Gender_ID_Switch[sexuality$BIO_SEX == "Male" & (sexuality$BIO_SEX2 == 2 | 
                           sexuality$BIO_SEX3 == "Female" | sexuality$BIO_SEX4 == "Female")] <- 1
sexuality$Gender_ID_Switch[sexuality$BIO_SEX == "Female" & (sexuality$BIO_SEX2 == 1 |
                           sexuality$BIO_SEX3 == "Male" | sexuality$BIO_SEX4 == "Male")] <- 1



sexuality$Gender_ID_Switch[is.na(sexuality$Gender_ID_Switch)] <- 0


# Reported change BIO_SEX 

# 20 male to female changes. Wave II code book sates there were 7 BIO_SEX
# miscodes in Wave I and that BIO_SEX2 in Wave II reflects correct sex.
# All 20 cases switched to Female in Wave II. Only 3 case reported male again
# after switching. All 3 cases only reported female once. All 3 possible miscodes
# Will probably need to swtich base bio sex case for codes to BIO_SEX2

switch <- which(sexuality$BIO_SEX == "Male" & (sexuality$BIO_SEX2 == 2 | 
                exuality$BIO_SEX3 == "Female" | sexuality$BIO_SEX4 == "Female"))

maleSwitch <- sexuality[switch, 1:5]

# Female switch appears fine. Wave II code book sates there were 7 BIO_SEX
# miscodes in Wave I and that BIO_SEX2 in Wave II reflects correct sex.
# All 7 cases continued to report as Male for durration of Waves
# Will definitely swtich base bio sex case for codes to BIO_SEX2

switch <- which(sexuality$BIO_SEX == "Female" & (sexuality$BIO_SEX2 == 1 |
                sexuality$BIO_SEX3 == "Male" | sexuality$BIO_SEX4 == "Male"))

femaleSwitch <- sexuality[switch, 1:5]



#### Current (at time of interview) self reported sexuality

# 0 - No attraction to same sex
# 1 - Attracted opposite sex but no sure about same sex
# 2 - Attracted to same sex only
# 3 - Attracted to same sex but not sure about opposite sex
# 4 - Attracted to both sexes
# 5 - Attracted to neither sex
# 6 - Refuse to answer about opposite sex
# 7 - Refused to answer


# Wave I

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 != "Yes"] <- 0
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 != "Yes"] <- 0

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Don't know" & sexuality$H1NR2 == "Yes"] <- 1
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Don't know" & sexuality$H1NR1 == "Yes"] <- 1

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Yes" & sexuality$H1NR2 != "Yes"] <- 2
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Yes" & sexuality$H1NR1 != "Yes"] <- 2

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Yes" & sexuality$H1NR2 == "Don't know"] <- 3
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Yes" & sexuality$H1NR1 == "Don't know"] <- 3

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Yes" & sexuality$H1NR2 == "Yes"] <- 4
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Yes" & sexuality$H1NR1 == "Yes"] <- 4

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "No" & sexuality$H1NR2 == "No"] <- 5
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "No" & sexuality$H1NR1 == "No"] <- 5

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Refused" & sexuality$H1NR2 != "Refused"] <- 6
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Refused" & sexuality$H1NR1 != "Refused"] <- 6

sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Female" & sexuality$H1NR1 
                                  == "Refused" & sexuality$H1NR2 == "Refused"] <- 7
sexuality$Current_sexuality_WaveI[sexuality$BIO_SEX == "Male" & sexuality$H1NR2 
                                  == "Refused" & sexuality$H1NR1 == "Refused"] <- 7


# 20745 respondents in WaveI (1 "refused" and 1 "Don't know" for BIO_SEX)
sum(table(sexuality$Current_sexuality_WaveI))
table(sexuality$Current_sexuality_WaveI)


# WaveII 

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 != 1] <- 0
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 != 1] <- 0

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 8 & sexuality$H2NR2 == 1] <- 1
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 8 & sexuality$H2NR1 == 1] <- 1

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 1 & sexuality$H2NR2 != 1] <- 2
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 1 & sexuality$H2NR1 != 1] <- 2

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 1 & sexuality$H2NR2 == 8] <- 3
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 1 & sexuality$H2NR1 == 8] <- 3

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 1 & sexuality$H2NR2 == 1] <- 4
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 1 & sexuality$H2NR1 == 1] <- 4

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 0 & sexuality$H2NR2 == 0] <- 5
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 0 & sexuality$H2NR1 == 0] <- 5

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 == 6] <- 6
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 == 6] <- 6

sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Female" & sexuality$H2NR1 
                                   == 6 & sexuality$H2NR2 == 6] <- 7
sexuality$Current_sexuality_WaveII[sexuality$BIO_SEX == "Male" & sexuality$H2NR2 
                                   == 6 & sexuality$H2NR1 == 6] <- 7


# 1438 respondents in WaveII
sum(table(sexuality$Current_sexuality_WaveII))
table(sexuality$Current_sexuality_WaveII)


# Verification of 2 respondents in WaveII only
# There are 2 respondents included in WaveIII not included in any other wave.
# Code verifying below. They are excluded unless othewise instructed.
w2 <- sexuality$AID[!is.na(sexuality$BIO_SEX2)]
w2n <- WaveII[!WaveII$AID %in% w2, ]
which(w2n$AID %in% WaveI$AID)
which(w2n$AID %in% WaveII$AID)
which(w2n$AID %in% WaveIII$AID)
which(w2n$AID %in% WaveIV$AID)
rm(w2, w2n)


# WaveIII

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 != "Yes"] <- 0
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 != "Yes"] <- 0

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "Don't know" & sexuality$H3SE12 == "Yes"] <- 1
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "Don't know" & sexuality$H3SE11 == "Yes"] <- 1

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "Yes" & sexuality$H3SE12 != "Yes"] <- 2
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "Yes" & sexuality$H3SE11 != "Yes"] <- 2

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "Yes" & sexuality$H3SE12 == "Don't know"] <- 3
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "Yes" & sexuality$H3SE11 == "Don't know"] <- 3

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "Yes" & sexuality$H3SE12 == "Yes"] <- 4
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "Yes" & sexuality$H3SE11 == "Yes"] <- 4

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "No" & sexuality$H3SE12 == "No"] <- 5
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "No" & sexuality$H3SE11 == "No"] <- 5

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 == "Refused"] <- 6
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 == "Refused"] <- 6

sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Female" & sexuality$H3SE11 
                                    == "Refused" & sexuality$H3SE12 == "Refused"] <- 7
sexuality$Current_sexuality_WaveIII[sexuality$BIO_SEX == "Male" & sexuality$H3SE12 
                                    == "Refused" & sexuality$H3SE11 == "Refused"] <- 7


# 15197 respondents in WaveIII
# There are 25 respondents included in WaveIII not included in any other wave.
# Code verifying below. They are excluded unless othewise instructed. 
sum(table(sexuality$Current_sexuality_WaveIII))
table(sexuality$Current_sexuality_WaveIII)


# Verification of 27 respondents in WaveIII only
w3 <- sexuality$AID[!is.na(sexuality$BIO_SEX3)]
w3n <- WaveIII[!WaveIII$AID %in% w3, ]
which(w3n$AID %in% WaveI$AID)
which(w3n$AID %in% WaveII$AID)
which(w3n$AID %in% WaveIII$AID)
which(w3n$AID %in% WaveIV$AID)
rm(w3, w3n, WaveI, WaveII, WaveIII, WaveIV)


# Wave IV

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 != "Yes"] <- 0
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 != "Yes"] <- 0

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "Don't know" & sexuality$H4SE30 == "Yes"] <- 1
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "Don't know" & sexuality$H4SE29 == "Yes"] <- 1

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "Yes" & sexuality$H4SE30 != "Yes"] <- 2
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "Yes" & sexuality$H4SE29 != "Yes"] <- 2

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "Yes" & sexuality$H4SE30 == "Don't know"] <- 3
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "Yes" & sexuality$H4SE29 == "Don't know"] <- 3

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "Yes" & sexuality$H4SE30 == "Yes"] <- 4
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "Yes" & sexuality$H4SE29 == "Yes"] <- 4

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "No" & sexuality$H4SE30 == "No"] <- 5
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "No" & sexuality$H4SE29 == "No"] <- 5

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 == "Refused"] <- 6
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 == "Refused"] <- 6

sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Female" & sexuality$H4SE29 
                                   == "Refused" & sexuality$H4SE30 == "Refused"] <- 7
sexuality$Current_sexuality_WaveIV[sexuality$BIO_SEX == "Male" & sexuality$H4SE30 
                                   == "Refused" & sexuality$H4SE29 == "Refused"] <- 7

# 15701 respondents in WaveIV
sum(table(sexuality$Current_sexuality_WaveIV))
table(sexuality$Current_sexuality_WaveIV)


# Tables of distrobutions for all 4 Waves for comparison
table(sexuality$Current_sexuality_WaveI)
table(sexuality$Current_sexuality_WaveII)
table(sexuality$Current_sexuality_WaveIII)
table(sexuality$Current_sexuality_WaveIV)


#### Ever reported attracted to same sex after reporting no in WaveI

# 0 - Male   - No
# 1 - Male   - Yes
# 2 - Female - No
# 3 - Female - Yes

sexuality$sexuality_change[sexuality$BIO_SEX == "Male" & 
                               sexuality$H1NR2 != "Yes" & 
                               (sexuality$H2NR2 != 1      | is.na(sexuality$H2NR2)) & 
                               (sexuality$H3SE12 != "Yes" | is.na(sexuality$H3SE12))  & 
                               (sexuality$H4SE30 != "Yes" | is.na(sexuality$H4SE3))] <- 0

sexuality$sexuality_change[sexuality$BIO_SEX == "Male" & 
                               (sexuality$H1NR2 == "Yes"  | sexuality$H2NR2 == 1 | 
                                sexuality$H3SE12 == "Yes" | sexuality$H4SE30 == "Yes")] <- 1

sexuality$sexuality_change[sexuality$BIO_SEX == "Female" & 
                               sexuality$H1NR1 != "Yes" & 
                               (sexuality$H2NR1 != 1      | is.na(sexuality$H2NR1)) &
                               (sexuality$H3SE11 != "Yes" | is.na(sexuality$H3SE11)) & 
                               (sexuality$H4SE29 != "Yes" | is.na(sexuality$H4SE29))] <- 2

sexuality$sexuality_change[sexuality$BIO_SEX == "Female" & 
                               (sexuality$H1NR1 == "Yes"  | sexuality$H2NR1 == 1 |
                                sexuality$H3SE11 == "Yes" | sexuality$H4SE29 == "Yes")] <-3

# Table of distrobution
sum(table(sexuality$sexuality_change))
table(sexuality$sexuality_change)


#### Presented info confliting info on sexuality (lots of vars, esp II & IV relationship data, tall and messy)

#### Conclicting info codings 

# 0 Males never reported same sex attraction, continued to report no conflicting sexual behavior
# 1 Males never reported same sex attraction, but reported conflicting sexual behavior
# 2 Femaels never reported same sex attraction, continued to report no conflicting sexual behavior
# 3 Females never reported same sex attraction, but reported conflicting sexual behavior

sexuality$conflicting_info[sexuality$sexuality_change == 0 & 
                           (# Wave I romantic relationships (no intercourse test)
                           apply(sexuality2[4:6] == "Male", 1, any) | 
                           # Wave I Non-romantic partners (includes intercourse test)
                           ((Waves$H1NR15_1 == "Male" & Waves$H1NR22_1 == "Yes") | 
                            (Waves$H1NR15_2 == "Male" & Waves$H1NR22_2 == "Yes") |
                            (Waves$H1NR15_3 == "Male" & Waves$H1NR22_3 == "Yes")) |
                           # Wave I Non-romantic partners RX data (includes intercourse test)
                           ((Waves$H1RX20_1 == "Male" & 
                            (Waves$H1RX21O1 == "Card kept" | Waves$H1RX24A1 == "Yes")) | 
                            (Waves$H1RX20_2 == "Male" & 
                            (Waves$H1RX21O2 == "Card kept" | Waves$H1RX24A2 == "Yes")) |
                            (Waves$H1RX20_1 == "Male" & 
                            (Waves$H1RX21O3 == "Card kept" | Waves$H1RX24A3 == "Yes"))) |
                           # Wave I additonal non-romantic sexual parners
                            (Waves$H1NR47 == "Male" |
                            (sexuality2[2] == "All male" | sexuality2[2] == "Some male/some female")) |
                           # Wave II romantic relationships (no intercourse test)
                             apply(sexuality2[7:9] == 1, 1, any) |
                           # Wave II Non-romantic partners
                           ((Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1) |
                            (Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1) |
                            (Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1)) |
                           # Wave II Non-romantic partners RX data (includes intercourse test)
                           ((Waves$H2RX32_1 == 1 & (Waves$H2RX33M1 == 1 |
                             Waves$H2RX35_1 == 1 | Waves$H2RX36_1 == 1)) |
                            (Waves$H2RX32_2 == 1 & (Waves$H2RX33M2 == 1 |
                             Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1)) |
                            (Waves$H2RX32_3 == 1 & (Waves$H2RX33M3 == 1 |
                             Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1))) |
                           # Wave II additonal non-romantic sexual parners
                           (Waves$H2NR82 == 1 |
                           (sexuality2[3] == 1 | sexuality2[3] == 3)) |
                           # Wave III & IV  partner data (includes intercourse test
                           # for Wave III, can remove, no such variable for Wave IV)  
                           apply(fullTest[2:75] == "Male", 1, any) |
                           # Wave IV numbers of sexual partners by sex (ever, before 18, last year)    
                           apply(sexuality2[20:21] >= 1 & sexuality2[20:21] < 996, 1, any) |
                                                (sexuality2[22] > 0 & sexuality2[22] < 96))] <- 1

sexuality$conflicting_info[sexuality$sexuality_change == 2 & 
                           (# Wave I romantic relationships (no intercourse test)
                           apply(sexuality2[4:6] == "Female", 1, any) | 
                           # Wave I Non-romantic partners (includes intercourse test)
                           ((Waves$H1NR15_1 == "Female" & Waves$H1NR22_1 == "Yes") |
                            (Waves$H1NR15_2 == "Female" & Waves$H1NR22_2 == "Yes") |
                            (Waves$H1NR15_3 == "Female" & Waves$H1NR22_3 == "Yes")) |
                           # Wave I Non-romantic partners RX data (includes intercourse test)
                           ((Waves$H1RX20_1 == "Female" & 
                            (Waves$H1RX21O1 == "Card kept" | Waves$H1RX24A1 == "Yes")) | 
                            (Waves$H1RX20_2 == "Female" & 
                            (Waves$H1RX21O2 == "Card kept" | Waves$H1RX24A2 == "Yes")) |
                            (Waves$H1RX20_1 == "Female" & 
                            (Waves$H1RX21O3 == "Card kept" | Waves$H1RX24A3 == "Yes"))) |
                           # Wave I additonal non-romantic sexual parners 
                            (Waves$H1NR47 == "Female" |
                            (sexuality2[2] == "All female" | sexuality2[2] == "Some male/some female")) |
                           # Wave II romantic relationships (no intercourse test)
                             apply(sexuality2[7:9] == 2, 1, any) |
                           # Wave II Non-romantic partners (includes intercourse test)
                           ((Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1) |
                            (Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1) |
                            (Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1)) |
                           # Wave II Non-romantic partners RX data (includes intercourse test)
                           ((Waves$H2RX32_1 == 2 & (Waves$H2RX33M1 == 1 |
                             Waves$H2RX35_1 == 2 | Waves$H2RX36_1 == 1)) |
                            (Waves$H2RX32_2 == 2 & (Waves$H2RX33M2 == 1 |
                             Waves$H2RX35_2 == 2 | Waves$H2RX36_2 == 1)) |
                            (Waves$H2RX32_3 == 2 & (Waves$H2RX33M3 == 1 |
                             Waves$H2RX35_3 == 2 | Waves$H2RX36_3 == 1))) |
                           # Wave II additonal non-romantic sexual parners  
                           (Waves$H2NR82 == 2 |
                           (sexuality2[3] == 2 | sexuality2[3] == 3)) |
                           # Wave III & IV  partner data (includes intercourse test
                           # for Wave III, can remove, no such variable for Wave IV)  
                           apply(fullTest[2:75] == "Female", 1, any) |
                           # Wave IV numbers of sexual partners by sex (ever, before 18, last year)    
                           apply(sexuality2[17:18] >= 1 & sexuality2[17:18] < 996, 1, any) |
                                                (sexuality2[19] > 0 & sexuality2[19] < 96))] <- 3

sexuality$conflicting_info[sexuality$sexuality_change == 0 & is.na(sexuality$conflicting_info)] <- 0
sexuality$conflicting_info[sexuality$sexuality_change == 2 & is.na(sexuality$conflicting_info)] <- 2


sum(table(sexuality$conflicting_info))
table(sexuality$conflicting_info)

# 0 should equal ~5087

# As of yet left out, left to "PI's" discretion
#sexuality2[9:15]  # NR Parner sexes Wavee I&II - left out until specified
#sexuality2[15]    # Ever touched a dudes dick -  No   Yes

write.csv(sexuality, "sexuality_codes_new.csv")

