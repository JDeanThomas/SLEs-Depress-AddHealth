source("LoadSLEs.R")

######## For all indicators

# 0 = NO
# 1 = Yes
# . = Missing

# Create SLE dateframe
sleCodes <-data.frame(Waves$AID)
colnames(sleCodes) <- "AID" 

# Create dummy dataframes to hold transformed columns, leving orginial 
# (and missing) intact
WavesTrans <-data.frame(Waves$AID)
colnames(WavesTrans) <- "AID" 
WaveIIITrans <-data.frame(WaveIII_adds$AID)
colnames(WaveIIITrans) <- "AID" 
WaveIVTrans <-data.frame(WaveIV_adds$AID)
colnames(WaveIVTrans) <- "AID" 

### Death of parent

sleCodes$DeathParent_W1[Waves$H1NF2 == "Yes" | Waves$H1NM2 == "Yes"] <- 0
sleCodes$DeathParent_W1[Waves$H1NF2 == "No (skip to Q.4)" | Waves$H1NM2 == "No"] <- 1
sleCodes$DeathParent_W2[Waves$H2NF4 == 1 | Waves$H2NM4 == 1] <- 0
sleCodes$DeathParent_W2[Waves$H2NF4 == 0 | Waves$H2NM4 == 0] <- 1
sleCodes$DeathParent_W3[Waves$H3WP12 == "Yes (skip to Q.15)" | Waves$H3WP8 == "Yes (skip to Q.15)"] <- 0
sleCodes$DeathParent_W3[Waves$H3WP12 == "No" | Waves$H3WP8 == "No"] <- 1
sleCodes$DeathParent_W4[Waves$H4WP7 == "Yes" | Waves$H4WP1 == "Yes"] <- 0
sleCodes$DeathParent_W4[Waves$H4WP7 == "No" | Waves$H4WP1 == "No"] <- 1


### Suicide attempt resulting in injury

# Missing in 3 & 4, 3 differs from online codebook
# Missing values not actually in data

sleCodes$SuicideInjury_W1[Waves$H1SU3 == "No"] <- 0
sleCodes$SuicideInjury_W1[Waves$H1SU3 == "Yes"] <- 1
sleCodes$SuicideInjury_W2[Waves$H2SU3 == 0] <- 0
sleCodes$SuicideInjury_W2[Waves$H2SU3 == 1] <- 1
sleCodes$SuicideInjury_W3[Waves$H3TO132 == "."] <- "."
sleCodes$SuicideInjury_W3[Waves$H3TO132 == "No"] <- 0
sleCodes$SuicideInjury_W3[Waves$H3TO132 == "Yes"] <- 1
sleCodes$SuicideInjury_W4[Waves$H4SE3 == "."] <- "."
sleCodes$SuicideInjury_W4[Waves$H4SE3 == "No"] <- 0
sleCodes$SuicideInjury_W4[Waves$H4SE3 == "Yes"] <- 1


### Friend committed suicide

# Missing in 3 & 4, 3 differs from online codebook
# Missing values not actually in data

sleCodes$SuicideFriend_W1[Waves$H1SU5 == "No"] <- 0
sleCodes$SuicideFriend_W1[Waves$H1SU5 == "Yes"] <- 1
sleCodes$SuicideFriend_W2[Waves$H2SU5 == 0] <- 0
sleCodes$SuicideFriend_W2[Waves$H2SU5 == 1] <- 1
sleCodes$SuicideFriend_W3[Waves$H3TO134 == "."] <- "."
sleCodes$SuicideFriend_W3[Waves$H3TO134 == "No"] <- 0
sleCodes$SuicideFriend_W3[Waves$H3TO134 == "Yes"] <- 1
sleCodes$SuicideFriend_W4[Waves$H4SE5 == "."] <- "."
sleCodes$SuicideFriend_W4[Waves$H4SE5 == "No"] <- 0
sleCodes$SuicideFriend_W4[Waves$H4SE5 == "Yes"] <- 1


### Relative committed suicide 

sleCodes$SuicideRelative_W1[Waves$H1SU7 == "No"] <- 0
sleCodes$SuicideRelative_W1[Waves$H1SU7 == "Yes"] <- 1
sleCodes$SuicideRelative_W2[Waves$H2SU7 == 0] <- 0
sleCodes$SuicideRelative_W2[Waves$H2SU7 == 1] <- 1
sleCodes$SuicideRelative_W3[Waves$H3TO136 == "."] <- "."
sleCodes$SuicideRelative_W3[Waves$H3TO136 == "No"] <- 0
sleCodes$SuicideRelative_W3[Waves$H3TO136 == "Yes"] <- 1
sleCodes$SuicideRelative_W4[Waves$H4SE5 == "."] <- "."
sleCodes$SuicideRelative_W4[Waves$H4SE5 == "No"] <- 0
sleCodes$SuicideRelative_W4[Waves$H4SE5 == "Yes"] <- 1


### Saw violence

sleCodes$SawViolence_W1[Waves$H1FV1 == "Never"] <- 0
sleCodes$SawViolence_W1[Waves$H1FV1 == "Once" | Waves$H1FV1 == "More than Once"] <- 1
sleCodes$SawViolence_W2[Waves$H2FV1 == 0] <- 0
sleCodes$SawViolence_W2[Waves$H2FV1 == 1 | Waves$H2FV1 == 2] <- 1
sleCodes$SawViolence_W3[Waves$H3DS18A == "."] <- "."
sleCodes$SawViolence_W3[Waves$H3DS18A == "Not marked"] <- 0
sleCodes$SawViolence_W3[Waves$H3DS18A == "Marked"] <- 1
sleCodes$SawViolence_W4[Waves$H4DS14 == "No"] <- 0
sleCodes$SawViolence_W4[Waves$H4DS14 == "Yes"] <- 1


### Was shot

sleCodes$WasShot_W1[Waves$H1FV3 == "Never"] <- 0
sleCodes$WasShot_W1[Waves$H1FV3 == "Once" | Waves$H1FV3 == "More than Once"] <- 1
sleCodes$WasShot_W2[Waves$H2FV3 == 0] <- 0
sleCodes$WasShot_W2[Waves$H2FV3 == 1 | Waves$H2FV3 == 2] <- 1
sleCodes$WasShot_W3[Waves$H3DS18D == "."] <- "."
sleCodes$WasShot_W3[Waves$H3DS18D == "Not marked"] <- 0
sleCodes$WasShot_W3[Waves$H3DS18D == "Marked"] <- 1
# Has to be an error. Not concordant with code book.
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/codebookssearch?field=varname&match=contains&text=H4DS16
sleCodes$WasShot_W4[Waves$H4DS16 == "No"] <- 0
sleCodes$WasShot_W4[Waves$H4DS16 == "Yes"] <- 1


### Was stabbed

sleCodes$WasStabbed_W1[Waves$H1FV4 == "Never"] <- 0
sleCodes$WasStabbed_W1[Waves$H1FV4 == "Once" | Waves$H1FV4 == "More than Once"] <- 1
sleCodes$WasStabbed_W2[Waves$H2FV4 == 0] <- 0
sleCodes$WasStabbed_W2[Waves$H2FV4 == 1 | Waves$H2FV4 == 2] <- 1
sleCodes$WasStabbed_W3[Waves$H3DS18E == "."] <- "."
sleCodes$WasStabbed_W3[Waves$H3DS18E == "Not marked"] <- 0
sleCodes$WasStabbed_W3[Waves$H3DS18E == "Marked"] <- 1
# Has to be an error. Not concordant with code book.
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/codebookssearch?field=varname&match=contains&text=H4DS16
sleCodes$WasStabbed_W4[Waves$H4DS16 == "No"] <- 0
sleCodes$WasStabbed_W4[Waves$H4DS16 == "Yes"] <- 1


### Was jumped

sleCodes$WasJumped_W1[Waves$H1FV6 == "Never"] <- 0
sleCodes$WasJumped_W1[Waves$H1FV6 == "Once" | Waves$H1FV6 == "More than Once"] <- 1
sleCodes$WasJumped_W2[Waves$H2FV5 == 0] <- 0
sleCodes$WasJumped_W2[Waves$H2FV5 == 1 | Waves$H2FV5 == 2] <- 1
sleCodes$WasJumped_W3[Waves$H3DS18F == "."] <- "."
sleCodes$WasJumped_W3[Waves$H3DS18F == "Not marked"] <- 0
sleCodes$WasJumped_W3[Waves$H3DS18F == "Marked"] <- 1
# Has to be an error. Not concordant with code book.
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/codebookssearch?field=varname&match=contains&text=H4DS18
sleCodes$WasJumped_W4[Waves$H4DS18 == "No"] <- 0
sleCodes$WasJumped_W4[Waves$H4DS18 == "Yes"] <- 1


### Threatened someone with knife or gun

sleCodes$ThreatenedSomeone_W1[Waves$H1FV7 == "Never"] <- 0
sleCodes$ThreatenedSomeone_W1[Waves$H1FV7 == "Once" | Waves$H1FV7 == "More than Once"] <- 1
sleCodes$ThreatenedSomeone_W2[Waves$H2FV6 == 0] <- 0
sleCodes$ThreatenedSomeone_W2[Waves$H2FV6 == 1 | Waves$H2FV6 == 2] <- 1
sleCodes$ThreatenedSomeone_W3[Waves$H3DS18H == "."] <- "."
sleCodes$ThreatenedSomeone_W3[Waves$H3DS18H == "Not marked"] <- 0
sleCodes$ThreatenedSomeone_W3[Waves$H3DS18H == "Marked"] <- 1
sleCodes$ThreatenedSomeone_W4[Waves$H4DS19 == "No"] <- 0
sleCodes$ThreatenedSomeone_W4[Waves$H4DS19 == "Yes"] <- 1


### Shot / Stabbed someone

sleCodes$ShotStabbed_W1[Waves$H1FV8 == "Never"] <- 0
sleCodes$ShotStabbed_W1[Waves$H1FV8 == "Once" | Waves$H1FV8 == "More than Once"] <- 1
sleCodes$ShotStabbed_W2[Waves$H2FV7 == 0] <- 0
sleCodes$ShotStabbed_W2[Waves$H2FV7 == 1 | Waves$H2FV7 == 2] <- 1
sleCodes$ShotStabbed_W3[Waves$H3DS18I == "."] <- "."
sleCodes$ShotStabbed_W3[Waves$H3DS18I == "Not marked"] <- 0
sleCodes$ShotStabbed_W3[Waves$H3DS18I == "Marked"] <- 1
# Has to be an error. Not concordant with code book.
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/codebookssearch?field=varname&match=contains&text=H4DS20
sleCodes$ShotStabbed_W3[Waves$H4DS20 == "."] <- "."
sleCodes$ShotStabbed_W4[Waves$H4DS20 == "No"] <- 0
sleCodes$ShotStabbed_W4[Waves$H4DS20 == "Yes"] <- 1


### Was injured in a physical fight

WavesTrans$H4DS11 <- as.integer((Waves$H4DS11))
WavesTrans$H4DS12 <- as.integer((Waves$H4DS12))

sleCodes$InjuredInFight_W1[Waves$H1FV13 == "."] <- "."
sleCodes$InjuredInFight_W1[Waves$H1FV13 == 0] <- 0
sleCodes$InjuredInFight_W1[Waves$H1FV13 > 0 & Waves$H1FV13 <= 365] <- 1
sleCodes$InjuredInFight_W2[Waves$H2FV20 == 0] <- 0
sleCodes$InjuredInFight_W2[Waves$H2FV20 > 0 & Waves$H2FV20 <= 333] <- 1
sleCodes$InjuredInFight_W3[Waves$H3DS16 == "."] <- "."
sleCodes$InjuredInFight_W3[Waves$H3DS16 == 0] <- 0
sleCodes$InjuredInFight_W3[Waves$H3DS16 > 0 & Waves$H3DS16 <= 56] <- 1
sleCodes$InjuredInFight_W4[WavesTrans$H4DS11 == 1] <- 0
sleCodes$InjuredInFight_W4[WavesTrans$H4DS11 > 1 & WavesTrans$H4DS11 < 5] <- 1

### Hurt someone in a physical fight

WavesTrans$H1DS6 <- as.integer((Waves$H1DS6))

sleCodes$InjuredSomeoneInFight_W1[WavesTrans$H1DS6 == 1] <- 0
sleCodes$InjuredSomeoneInFight_W1[WavesTrans$H1DS6 > 1 & WavesTrans$H1DS6 < 5] <- 1
sleCodes$InjuredSomeoneInFight_W2[Waves$H2FV22 == 0] <- 0
sleCodes$InjuredSomeoneInFight_W2[Waves$H2FV22 > 0 & Waves$H2FV22 <= 3] <- 1
sleCodes$InjuredSomeoneInFight_W3[Waves$H3DS17 == "."] <- "."
sleCodes$InjuredSomeoneInFight_W3[Waves$H3DS17 == 0] <- 0
sleCodes$InjuredSomeoneInFight_W3[Waves$H3DS17 > 0 & Waves$H3DS17 <= 67] <- 1
sleCodes$InjuredSomeoneInFight_W4[WavesTrans$H4DS12 == 1] <- 0
sleCodes$InjuredSomeoneInFight_W4[WavesTrans$H4DS12 > 1 & WavesTrans$H4DS12 < 5] <- 1

### Unwanted Pregnancy

sleCodes$UnwantedPregnancy_W1[Waves$H1FP15_1 == "Definitely yes" | 
                                  Waves$H1FP15_1 == "Probably yes" | Waves$H1FP15_2 == "Definitely yes" |
                                  Waves$H1FP15_2 == "Probably yes" | Waves$H1FP15_3 == "Definitely yes" |
                                  Waves$H1FP15_3 == "Probably yes" | Waves$H1FP15_4 == "Definitely yes" |
                                  Waves$H1FP15_3 == "Probably yes" | Waves$H1FP15_5 == "Definitely yes" |
                                  Waves$H1FP15_5 == "Probably yes"] <- 0
sleCodes$UnwantedPregnancy_W1[Waves$H1FP15_1 == "Definitely no" | 
                                  Waves$H1FP15_1 == "Probably no" | Waves$H1FP15_2 == "Definitely no" |
                                  Waves$H1FP15_2 == "Probably no" | Waves$H1FP15_3 == "Definitely no" |
                                  Waves$H1FP15_3 == "Probably no" | Waves$H1FP15_4 == "Definitely no" |
                                  Waves$H1FP15_3 == "Probably no" | Waves$H1FP15_5 == "Definitely no" |
                                  Waves$H1FP15_5 == "Probably no"] <- 1
sleCodes$UnwantedPregnancy_W2[(Waves$H2FP19_1 > 2 & Waves$H2FP19_1 < 6) | (Waves$H2FP19_2 > 2 &
                                                                               Waves$H2FP19_2 < 6) | (Waves$H2FP19_3 > 2 & Waves$H2FP19_3 < 6)] <- 0
sleCodes$UnwantedPregnancy_W2[Waves$H2FP19_1 <= 2 | Waves$H2FP19_2 <= 2 |
                                  Waves$H2FP19_3 <= 2] <- 1
sleCodes$UnwantedPregnancy_W2[Waves$H2FP19_1 == "."] <- "."
sleCodes$UnwantedPregnancy_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3PG8 == "."]] <- "."
sleCodes$UnwantedPregnancy_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3PG8 == "Yes (skip to Q.24)"] | 
                                  Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3PC22 == "Yes (skip to Q.24)"]] <- 0
sleCodes$UnwantedPregnancy_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3PG8 == "No"] | 
                                  Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3PC22 == "No"]] <- 1
sleCodes$UnwantedPregnancy_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4PG8 == "Yes"]] <- 0
sleCodes$UnwantedPregnancy_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4PG8 == "No"]] <- 1


### Abbortion, still birth or miscarriage


WaveIIITrans$H3TP1 <- as.integer(WaveIII_adds$H3TP1)
WaveIVTrans$H4PG1 <- as.integer(WaveIV_adds$H4PG1)

sleCodes$AbortionMiscarriage_W1[Waves$H1FP21_1 == 
                                    "Still hasn't ended/still pregnant (skip to next section)" |
                                    Waves$H1FP21_1 == "A live birt" |
                                    Waves$H1FP21_2 == "It hasn't ended/still pregnant (skip to next section)" |
                                    Waves$H1FP21_2 == "A live birt" |
                                    Waves$H1FP21_3 == "It hasn't ended/still pregnant (skip to next section)" |
                                    Waves$H1FP21_3 == "A live birt" |
                                    Waves$H1FP21_4 == "It hasn't ended/still pregnant (skip to next section)" |
                                    Waves$H1FP21_4 == "A live birt" |
                                    Waves$H1FP21_5 == "It hasn't ended/still pregnant (skip to next section)" |
                                    Waves$H1FP21_5 == "A live birt"] <- 0
sleCodes$AbortionMiscarriage_W1[Waves$H1FP21_1 == 
                                    "Still birth/miscarriage (skip to next pregnancy/section)" |
                                    Waves$H1FP21_1 == "An abortion (skip to next pregnancy/section)" |
                                    Waves$H1FP21_2 == "Still birth/miscarriage (skip to next pregnancy/section)" |
                                    Waves$H1FP21_2 == "An abortion (skip to next pregnancy/section)" |
                                    Waves$H1FP21_3 == "Still birth/miscarriage (skip to next pregnancy/section)" |
                                    Waves$H1FP21_3 == "An abortion (skip to next pregnancy/section)" |
                                    Waves$H1FP21_4 == "Still birth/miscarriage (skip to next pregnancy/section)" |
                                    Waves$H1FP21_4 == "An abortion (skip to next pregnancy/section)" |
                                    Waves$H1FP21_5 == "Still birth/miscarriage (skip to next pregnancy/section)" |
                                    Waves$H1FP21_5 == "An abortion (skip to next pregnancy/section)"] <- 1
sleCodes$AbortionMiscarriage_W2[Waves$H2FP25_1 == "."] <- "."
sleCodes$AbortionMiscarriage_W2[Waves$H2FP25_1 < 3 | Waves$H2FP25_2 < 3 | Waves$H2FP25_3 < 3] <- 0
sleCodes$AbortionMiscarriage_W2[(Waves$H2FP25_1 >= 3 & Waves$H2FP25_1 <=5) |
                                    (Waves$H2FP25_2 >= 3 & Waves$H2FP25_2 <=5) |
                                    (Waves$H2FP25_3 >= 3 & Waves$H2FP25_3 <=5)] <- 1
sleCodes$AbortionMiscarriage_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3TP1 == "."]] <- "."
sleCodes$AbortionMiscarriage_W3[Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3TP1 == 4] | 
                                    Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3TP1 == 5]] <- 0
sleCodes$AbortionMiscarriage_W3[Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3TP1 <= 3] | 
                                    Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3TP1 == 6] |
                                    Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3TP1 == 7]] <-1
sleCodes$AbortionMiscarriage_W4[Waves$AID %in% WaveIV_adds$AID[WaveIVTrans$H4PG1 > 4] &
                                    Waves$AID %in% WaveIV_adds$AID[WaveIVTrans$H4PG1 < 8]] <- 0
sleCodes$AbortionMiscarriage_W4[Waves$AID %in% WaveIV_adds$AID[WaveIVTrans$H4PG1 <= 4]] <- 1
                                
                                    
#### Had a child adopted      

WaveIIITrans$H3LB2 <- as.numeric(WaveIII_adds$H3LB2)
WaveIVTrans$H4LB5 <- as.numeric(WaveIV_adds$H4LB5)

sleCodes$ChildAdopted_W1[Waves$H1FP23A1 == "No" | Waves$H1FP23A2 == "No" |
                             Waves$H1FP23A3 == "No" | Waves$H1FP23A4 == "No" |
                             Waves$H1FP23A5 == "No"] <- 0
sleCodes$ChildAdopted_W1[Waves$H1FP23A1 == "Yes" | Waves$H1FP23A2 == "Yes" |
                             Waves$H1FP23A3 == "Yes" | Waves$H1FP23A4 == "Yes" |
                             Waves$H1FP23A5 == "Yes"] <- 1 
sleCodes$ChildAdopted_W2[Waves$H2FP27A1 == "."] <- "."
sleCodes$ChildAdopted_W2[Waves$H2FP27A1 == 0 | Waves$H2FP27A2 == 0 |
                             Waves$H2FP27A3 == 0] <- 0
sleCodes$ChildAdopted_W2[Waves$H2FP27A1 == 1 | Waves$H2FP27A2 == 1 |
                             Waves$H2FP27A3 == 1] <- 1
# "Went to live with another relative" included for now
sleCodes$ChildAdopted_W3[Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB2 != 7 & !is.na(WaveIIITrans$H3LB2)]] <- 0
sleCodes$ChildAdopted_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB2 == "."]] <- "."
sleCodes$ChildAdopted_W3[Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB2 == 1] | 
                             Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB2 == 4]] <- 1
# "Went to live with another relative" included for now
sleCodes$ChildAdopted_W4[Waves$AID %in% WaveIV_adds$AID[WaveIVTrans$H4LB5 != 8 & !is.na(WaveIVTrans$H4LB5)]] <- 0
sleCodes$ChildAdopted_W4[Waves$AID %in% WaveIV_adds$AID[WaveIVTrans$H4LB5 == 1] | 
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB5 == 4]] <- 1


### Death of child
## Needs a sanity check

# Convert WaveIII death year codes to type int
WaveIIITrans$H3LB12Y <- as.integer(WaveIII_adds$H3LB12Y)

sleCodes$ChildDeath_W1[Waves$H1FP24A1 == "Yes (skip to Q.26)" | Waves$H1FP24A2 == "Yes (skip to Q.26)" |
                           Waves$H1FP24A3 == "Yes (skip to Q.26)" | Waves$H1FP24A4 == "Yes (skip to Q.26)" |
                           Waves$H1FP24A5 == "Yes (skip to Q.26)"] <- 0 
sleCodes$ChildDeath_W1[Waves$H1FP24A1 == "No" | Waves$H1FP24A2 == "No" |
                           Waves$H1FP24A3 == "No" | Waves$H1FP24A4 == "No" |
                           Waves$H1FP24A5 == "No"] <- 1
sleCodes$ChildDeath_W2[Waves$H2FP28A1 == 1 | Waves$H2FP28A2 == 1 | Waves$H2FP28A3 == 1] <- 0
sleCodes$ChildDeath_W2[Waves$H2FP28A1 == 0 | Waves$H2FP28A2 == 0 | Waves$H2FP28A3 == 0] <- 1
sleCodes$ChildDeath_W3[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y == "."]] <- "."
sleCodes$ChildDeath_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "Yes (skip to Q.13)"]] <- 0
sleCodes$ChildDeath_W3[(Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "No" & 
                       (WaveIIITrans$H3LB12Y >= 1996 & WaveIIITrans$H3LB12Y <= 2002)]) |
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "No" &
                       (WaveIV_adds$H4LB11Y >= 1996 & WaveIV_adds$H4LB11Y <= 2002)])] <- 1
sleCodes$ChildDeath_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y == "."]] <- "."
sleCodes$ChildDeath_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "Yes"]] <- 0
sleCodes$ChildDeath_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "No" &
                       (WaveIV_adds$H4LB11Y > 2002 & WaveIV_adds$H4LB11Y <= 2008)]] <- 1

# WaveIII
# Nubers of reported child death by year for WaveIII and IV don't sum
table(WaveIII_adds$H3LB12Y)
table(WaveIV_adds$H4LB11Y)
# Needs furthr examination / can improve indicator for Wave III
table(WaveIII_adds$H3LB11)
table(WaveIII_adds$H3LB11 == "No" & WaveIII_adds$H3LB12Y >= 1996)
# Something like a combination of
table(WaveIII_adds$H3LB11 == "No" & WaveIII_adds$H3LB12Y >= 1996)
table(WaveIV_adds$H4LB10 == "No" & (WaveIV_adds$H4LB11Y >= 1996 & WaveIV_adds$H4LB11Y <= 2002))
## *Now implimented above

# WaveIV
# Sums for H4LB10 don't match codebook
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/codebookssearch?field=varname&match=contains&text=H4LB10
table(WaveIV_adds$H4LB10)
# Rather than full test with H4LB10
table(WaveIV_adds$H4LB10 == "No")
table(WaveIV_adds$H4LB10 == "No" & (WaveIV_adds$H4LB11Y >= 2002 & WaveIV_adds$H4LB11Y <= 2008))
# Solution might be to construct indicator using reported dates alone
table(WaveIV_adds$H4LB11Y >= 2002 & WaveIV_adds$H4LB11Y <= 2008)
# Though this still doesn't seem right (alt test with OR below) and the below line may
# be the test that is most robust (reported baby not alive and reported death date)
table(WaveIV_adds$H4LB10 == "No" | (WaveIV_adds$H4LB11Y >= 2002 & WaveIV_adds$H4LB11Y <= 2008))
# Proof is that noone reported baby still alive AND a death date.
table(WaveIV_adds$H4LB10 == "Yes" & (WaveIV_adds$H4LB11Y >= 2002 & WaveIV_adds$H4LB11Y <= 2008))
## *Now implimented above

### Romantic relationship ended

# This probably needs to be reworked

# There are TWO variables for year relationship ended for all 3 partners
# in Wave I, they do not sum, and are imprecise (large % report not knowing)
sleCodes$RomanticRelEnd_W1[Waves$H1RI10_1 == "."] <- "."
sleCodes$RomanticRelEnd_W1[Waves$H1RI10_1 == "Yes" | Waves$H1RI10_2 == "Yes" |
                           Waves$H1RI10_3 == "Yes"] <- 0
sleCodes$RomanticRelEnd_W1[Waves$H1RI10_1 == "No" | Waves$H1RI10_2 == "No" |
                           Waves$H1RI10_3 == "No"] <- 1 
# There are time indicators. Not super precise, but interval is short. 
sleCodes$RomanticRelEnd_W2[Waves$H2RI20Y1 == "."] <- "."
sleCodes$RomanticRelEnd_W2[Waves$H2RI19_1 == 1 | Waves$H2RI19_2 == 1 |
                           Waves$H2RI19_3 == 1] <- 0
sleCodes$RomanticRelEnd_W2[(Waves$H2RI19_1 == 0 & Waves$H2RI20Y1 == 96) | 
                           (Waves$H2RI19_2 == 0 & Waves$H2RI20Y2 == 96) |
                           (Waves$H2RI19_3 == 0 & Waves$H2RI20Y3 == 96)] <- 1 
# Wave III & IV indicatora are probably inadequate and imprecise.
# Both only ask about 1 relationship, Wave III has no time indicator
# Narrowed to realtionships lasting longer than 3 months with H3TR1
sleCodes$RomanticRelEnd_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3TR1 == "."]] <- "."
sleCodes$RomanticRelEnd_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3TR1 == 
                                                               "Yes, this is a current relationship"]] <- 0
sleCodes$RomanticRelEnd_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3TR1 == 
                                                               "No, this isn't a current relationship"] & 
                          Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3TR2 == 
                                                               "Yes, this relationship lasted three months"]] <- 1
sleCodes$RomanticRelEnd_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "."]] <- "."
sleCodes$RomanticRelEnd_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Yes"]] <- 0
sleCodes$RomanticRelEnd_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No" &
                          (WaveIV_adds$H4TR28Y >= 2002 & WaveIV_adds$H4TR28Y <= 2009)]] <- 1                       


### Had sex for money

## Needs surther evaluation
sleCodes$SexForMoney_W1[Waves$H1NR3 == "No" | Waves$H1NR49 == "No"] <- 0
# table((Waves$H1NR3 == "Yes" | Waves$H1NR4 > 1) & Waves$H1NR49 == ".")
sleCodes$SexForMoney_W1[Waves$H1NR3 == "Yes" | Waves$H1NR49 == "Yes" |
                        Waves$H1NR4 <= 200 | Waves$H1NR4 == 998] <- 1
sleCodes$SexForMoney_W2[Waves$H2NR4 == "."] <- "."
sleCodes$SexForMoney_W2[Waves$H2NR4 == 0] <- 0
sleCodes$SexForMoney_W2[Waves$H2NR4 >= 1 & Waves$H2NR4 <= 834] <- 1
# Need way to test if SLE happend post-WaveII
sleCodes$SexForMoney_W3[Waves$H3SE17 == "No (skip to Q.19)"] <- 0
sleCodes$SexForMoney_W3[Waves$H3SE17 == "Yes"] <- 1


### Contracted an STD
# Includes tests for each disese across Waves and a Wave IV indicator asking "ever"
# Do not perfectly sum. Waves II and IV (longest intervals) asked "in the last 12 months"
# Yet the "ever" question is usually less than sums across waves.

# Bacterial Vaginosis
# No Wave IV
table((Waves$H4WP7 == "No" | Waves$H4WP1 == "No") |
          (Waves$H4WP7 == "Yes" | Waves$H4WP1 == "Yes"))

table(Waves$H1CO16I == "Yes" | Waves$H2CO19I == 1 | Waves$H3SE21H == "Yes") 
#Chlamydia
table(Waves$H1CO16A == "Yes" | Waves$H2CO19A == 1 | Waves$H3SE21A == "Yes" | Waves$H4SE37A == "Selected")
table(Waves$H4SE36A)
# Genital Herpes
table(Waves$H1CO16E == "Yes" | Waves$H2CO19E == 1 | Waves$H3SE21E == "Yes" | Waves$H4SE37E == "Selected")
table(Waves$H4SE36E)
# Genital Warts
table(Waves$H1CO16F == "Yes" | Waves$H2CO19F == 1 | Waves$H3SE21F == "Yes" | Waves$H4SE37F == "Selected")
table(Waves$H4SE36F)
# Gonorrhea
table(Waves$H1CO16C == "Yes" | Waves$H2CO19C == 1 | Waves$H3SE21B == "Yes" | Waves$H4SE37B == "Selected")
table(Waves$H4SE36B)
# Hapatitus B
# No Wave III
table(Waves$H1CO16H == "Yes" | Waves$H2CO19H ==1 | Waves$H4SE37G == "Selected")
table(Waves$H4SE36G)
# HIV / AIDS
table(Waves$H1CO16D == "Yes" | Waves$H2CO19D == 1 | Waves$H3SE21M == "Yes" | Waves$H4SE37M == "Selected")
table(Waves$H4SE36M)
table(Waves$H4ID5M)
# HPV
# No Waves I or II
table(Waves$H3SE21G == "Yes" | Waves$H4SE37H == "Selected")
table(Waves$H4SE36H)
# Syphilis
table(Waves$H1CO16B == "Yes" | Waves$H2CO19B == 1 | Waves$H3SE21D == "Yes" | Waves$H4SE37D == "Selected")
table(Waves$H4SE36D)
# Trichomoniasis
table(Waves$H1CO16G == "Yes" | Waves$H2CO19G == 1 | Waves$H3SE21C == "Yes" | Waves$H4SE37C == "Selected")
table(Waves$H4SE36C)
# Vaginitis
table(Waves$H1CO16J == "Yes" | Waves$H2CO19J == 1 | Waves$H3SE21L == "Yes" | Waves$H4SE37L == "Selected")
table(Waves$H4SE36L)
# Urethritis
# No Waves I or II
table(Waves$H3SE21K == "Yes" | Waves$H4SE37K == "Selected")
table(Waves$H4SE36K)
# Pelvic Inflammitory Disease
# No Waves I or II
table(Waves$H3SE21I == "Yes" | Waves$H4SE37I == "Selected")
table(Waves$H4SE36I)
# Cervicitis or Mucopurulent 
# No Waves I or II
table(Waves$H3SE21J == "Yes" | Waves$H4SE37J == "Selected")
table(Waves$H4SE36J)
# Other
# No Waves I or II
table(Waves$H3SE21N == "Yes" | Waves$H4SE37N == "Selected")


sleCodes$STD_W1[Waves$H1CO16I == "No" | Waves$H1CO16A == "No" | Waves$H1CO16E == "No" | 
                    Waves$H1CO16F == "No" | Waves$H1CO16C == "No" | Waves$H1CO16H == "No" | 
                    Waves$H1CO16D == "No" | Waves$H1CO16B == "No" | Waves$H1CO16G == "No" | 
                    Waves$H1CO16J == "No"] <- 0
sleCodes$STD_W1[Waves$H1CO16I == "Yes" | Waves$H1CO16A == "Yes" | Waves$H1CO16E == "Yes" | 
                    Waves$H1CO16F == "Yes" | Waves$H1CO16C == "Yes" | Waves$H1CO16H == "Yes" | 
                    Waves$H1CO16D == "Yes" | Waves$H1CO16B == "Yes" | Waves$H1CO16G == "Yes" | 
                    Waves$H1CO16J == "Yes"] <- 1
sleCodes$STD_W2[Waves$H2CO19I == 0 | Waves$H2CO19A == 0 | Waves$H2CO19E == 0 | 
                    Waves$H2CO19F == 0 | Waves$H2CO19C == 0 | Waves$H2CO19H == 0 | 
                    Waves$H2CO19D == 0 | Waves$H2CO19B == 0 | Waves$H2CO19G == 0 | 
                    Waves$H2CO19J == 0] <- 0
sleCodes$STD_W2[Waves$H2CO19I == 1 | Waves$H2CO19A == 1 | Waves$H2CO19E == 1 | 
                    Waves$H2CO19F == 1 | Waves$H2CO19C == 1 | Waves$H2CO19H == 1 | 
                    Waves$H2CO19D == 1 | Waves$H2CO19B == 1 | Waves$H2CO19G == 1 | 
                    Waves$H2CO19J == 1] <- 1
sleCodes$STD_W3[Waves$H3SE21H == "No" | Waves$H3SE21A == "No" | Waves$H3SE21E == "No" | 
                    Waves$H3SE21F == "No" | Waves$H3SE21B == "No" | Waves$H3SE21M == "No" | 
                    Waves$H3SE21G == "No" | Waves$H3SE21D == "No" | Waves$H3SE21C == "No" | 
                    Waves$H3SE21L == "No" | Waves$H3SE21K == "No" | Waves$H3SE21I == "No" |
                    Waves$H3SE21J == "No" | Waves$H3SE21N == "No"] <- 0
sleCodes$STD_W3[Waves$H3SE21H == "Yes" | Waves$H3SE21A == "Yes" | Waves$H3SE21E == "Yes" | 
                    Waves$H3SE21F == "Yes" | Waves$H3SE21B == "Yes" | Waves$H3SE21M == "Yes" | 
                    Waves$H3SE21G == "Yes" | Waves$H3SE21D == "Yes" | Waves$H3SE21C == "Yes" | 
                    Waves$H3SE21L == "Yes" | Waves$H3SE21K == "Yes" | Waves$H3SE21I == "Yes" |
                    Waves$H3SE21J == "Yes" | Waves$H3SE21N == "Yes"] <- 1
sleCodes$STD_W4[Waves$H4SE37A == "." | Waves$H4SE37E == "." | 
                    Waves$H4SE37F == "." | Waves$H4SE37B == "." |
                    Waves$H4SE37G == "." | Waves$H4SE37M == "." |
                    Waves$H4SE37H == "." | Waves$H4SE37D == "." |
                    Waves$H4SE37C == "." | Waves$H4SE37L == "." |
                    Waves$H4SE37K == "." | Waves$H4SE37I == "." |
                    Waves$H4SE37J == "." | Waves$H4SE37N == "."] <- "." 
sleCodes$STD_W4[Waves$H4SE37A == "Not selected" | Waves$H4SE37E == "Not selected" | 
                    Waves$H4SE37F == "Not selected" | Waves$H4SE37B == "Not selected" |
                    Waves$H4SE37G == "Not selected" | Waves$H4SE37M == "Not selected" |
                    Waves$H4SE37H == "Not selected" | Waves$H4SE37D == "Not selected" |
                    Waves$H4SE37C == "Not selected" | Waves$H4SE37L == "Not selected" |
                    Waves$H4SE37K == "Not selected" | Waves$H4SE37I == "Not selected" |
                    Waves$H4SE37J == "Not selected" | Waves$H4SE37N == "Not selected"] <- 0
sleCodes$STD_W4[Waves$H4SE37A == "Selected" | Waves$H4SE37E == "Selected" | 
                    Waves$H4SE37F == "Selected" | Waves$H4SE37B == "Selected" |
                    Waves$H4SE37G == "Selected" | Waves$H4SE37M == "Selected" |
                    Waves$H4SE37H == "Selected" | Waves$H4SE37D == "Selected" |
                    Waves$H4SE37C == "Selected" | Waves$H4SE37L == "Selected" |
                    Waves$H4SE37K == "Selected" | Waves$H4SE37I == "Selected" |
                    Waves$H4SE37J == "Selected" | Waves$H4SE37N == "Selected"] <- 1 


### Skipped necessary medical care

sleCodes$SkippedMedicalCare_W1[Waves$H1GH26 == "No"] <- 0
sleCodes$SkippedMedicalCare_W1[Waves$H1GH26 == "Yes"] <- 1
sleCodes$SkippedMedicalCare_W2[Waves$H2GH28 == 0] <- 0
sleCodes$SkippedMedicalCare_W2[Waves$H2GH28 == 1] <- 1
sleCodes$SkippedMedicalCare_W3[Waves$H3HS6 == "No (skip to Q.10)"] <- 0
sleCodes$SkippedMedicalCare_W3[Waves$H3HS6 == "Yes"] <- 1
sleCodes$SkippedMedicalCare_W4[Waves$H4HS4 == "No"] <- 0
sleCodes$SkippedMedicalCare_W4[Waves$H4HS4 == "Yes"] <- 1


### Juvenile conviction
## Variable constructed from time varient indicator

sleCodes$JuvenileConviction_W1[Waves$H3CJ25Y > 1995 ] <- 0
sleCodes$JuvenileConviction_W1[Waves$H3CJ25Y <= 1995] <- 1
sleCodes$JuvenileConviction_W1[Waves$H3CJ25Y == "."] <- "."
sleCodes$JuvenileConviction_W2[Waves$H3CJ25Y != 1996] <- 0
sleCodes$JuvenileConviction_W2[Waves$H3CJ25Y == 1996] <- 1
sleCodes$JuvenileConviction_W2[Waves$H3CJ25Y == "."] <- "."
sleCodes$JuvenileConviction_W3[Waves$H3CJ25Y < 1997 | Waves$H3CJ25Y > 2001] <- 0
sleCodes$JuvenileConviction_W3[Waves$H3CJ25Y == "."] <- "."
sleCodes$JuvenileConviction_W3[Waves$H3CJ25Y > 1996 & Waves$H3CJ25Y <= 2001] <- 1


### Adult conviction

## Variable constructed from time varient indicator
# More respondents reporting adult conviction than reporting time of conviction
table(Waves$H3CJ86 == "Yes" & Waves$H3CJ89Y <= 2001)
## Could additionally use age to calculate if was adult rather than reported adult conviction

sleCodes$AdultConviction_W1[Waves$H3CJ89Y > 1995] <- 0
sleCodes$AdultConviction_W1[Waves$H3CJ89Y <= 1995] <- 1
sleCodes$AdultConviction_W1[Waves$H3CJ89Y == "."] <- "."
sleCodes$AdultConviction_W2[Waves$H3CJ89Y != 1996] <- 0
sleCodes$AdultConviction_W2[Waves$H3CJ89Y == 1996] <- 1
sleCodes$AdultConviction_W2[Waves$H3CJ89Y == "."] <- "."
sleCodes$AdultConviction_W3[Waves$H3CJ89Y < 1997 | Waves$H3CJ89Y > 2002] <- 0
sleCodes$AdultConviction_W3[Waves$H3CJ89Y >= 1997 & Waves$H3CJ89Y <= 2002] <- 1
sleCodes$AdultConviction_W3[Waves$H3CJ89Y == "."] <- "."
## No straightforward way to code for all respondents with adult convictions 
## in the time interval between Waves III & IV
## http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/topic?TopicId=168
## Could code for those reporting yes to Waves$H4CJ10 and != yes in previous Waves


### Served time in jail

# Able to construct indicator for Waves I - III using Wave III data.
# No straightforward way to derive acurate indicator for Wave IV due to
# imprecision of factor levels. Possible to use age to construct partial 
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/variablecollection?VariableCollectionId=1954

WavesTrans$H3CJ108A <- as.integer(Waves$H3CJ108A) #17
WavesTrans$H3CJ108B <- as.integer(Waves$H3CJ108B) #58
WavesTrans$H3CJ131A <- as.integer(Waves$H3CJ131A) #15
WavesTrans$H3CJ131B <- as.integer(Waves$H3CJ131B) #17

WavesTrans$H3CJ108B <- StataMissing(WavesTrans$H3CJ108B)

# More respondents reporting adult conviction than reporting time of conviction
table(Waves$H3CJ86 == "Yes")
table(Waves$H3CJ86 == "Yes" & Waves$H3CJ89Y <= 2002)
table(Waves$H3CJ130 == "Jail")
table(Waves$H3CJ130 == "Jail" & Waves$H3CJ112Y <= 2002)

sleCodes$JailTime_W1[Waves$H3CJ130 != "Jail" & Waves$H3CJ130 != "Prison"] <- 0
sleCodes$JailTime_W1[WavesTrans$H3CJ108B == 23] <- "."
sleCodes$JailTime_W1[(Waves$H3CJ86 == "Yes" & Waves$H3CJ89Y >= 1993 & 
                      Waves$H3CJ89Y <= 1995 &
                     (WavesTrans$H3CJ108A <= 12 | WavesTrans$H3CJ108B <= 18)) | 
                     (Waves$H3CJ130 == "Jail" & Waves$H3CJ112Y >= 1993 &
                      Waves$H3CJ112Y <= 1995 &
                     (WavesTrans$H3CJ131A <= 10 | WavesTrans$H3CJ131B <= 14))] <- 1
sleCodes$JailTime_W2[Waves$H3CJ130 != "Jail" & Waves$H3CJ130 != "Prison"] <- 0
sleCodes$JailTime_W2[WavesTrans$H3CJ108B == 23] <- "."
sleCodes$JailTime_W2[Waves$H3CJ86 == "Yes" & Waves$H3CJ89Y == 1996 &
                    (WavesTrans$H3CJ108A <= 12 | WavesTrans$H3CJ108B <= 18) |
                    (Waves$H3CJ130 == "Jail" & Waves$H3CJ112Y == 1996 &
                    (WavesTrans$H3CJ131A <= 10 | WavesTrans$H3CJ131B <= 14))] <- 1
sleCodes$JailTime_W3[Waves$H3CJ130 != "Jail" & Waves$H3CJ130 != "Prison"] <- 0
sleCodes$JailTime_W3[WavesTrans$H3CJ108B == 23] <- "."
sleCodes$JailTime_W3[Waves$H3CJ86 == "Yes" & 
                    (Waves$H3CJ89Y >= 1997 & Waves$H3CJ89Y <= 2002) &
                    (WavesTrans$H3CJ108A <= 12 | WavesTrans$H3CJ108B <= 18) | 
                    (Waves$H3CJ130 == "Jail" & 
                    (Waves$H3CJ112Y >= 1997 & Waves$H3CJ112Y <= 2002) &
                    (WavesTrans$H3CJ131A <= 10 | WavesTrans$H3CJ131B <= 14))] <- 1


### Was expelled from school

sleCodes$ExpelledFromShool_W1[Waves$H1ED9 == "."] <- "."
sleCodes$ExpelledFromShool_W1[Waves$H1ED9 == "No"] <- 0
sleCodes$ExpelledFromShool_W1[Waves$H1ED9 == "Yes"] <- 1
sleCodes$ExpelledFromShool_W2[Waves$H2ED5 == 0] <- 0
sleCodes$ExpelledFromShool_W2[Waves$H2ED5 == 1] <- 1
## Wave III H3ED33 aks if ever explelled. May be possible to construct indicator with:
## http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/variablecollection?VariableCollectionId=1875
## Less than 0.37% were potentially 18 or younger at Wave III


### Suffered a serious injury 
# Not perfect. Wavs I II & IV cover 12 month prior period. 


sleCodes$SeriousInjury_W1[Waves$H1GH54 == "Very minor" | Waves$H1GH54 == "Minor" |
                              Waves$H1GH54 == "Not applicable"] <- 0
sleCodes$SeriousInjury_W1[Waves$H1GH54 == "Serious" | Waves$H1GH54 == "Very Serious" |
                              Waves$H1GH54 == "Extremely serious"] <- 1
sleCodes$SeriousInjury_W2[Waves$H2GH47 < 3] <- 0
sleCodes$SeriousInjury_W2[Waves$H2GH47 >= 3 & Waves$H2GH47 <= 5] <- 1
sleCodes$SeriousInjury_W2[Waves$H3ID28 == "." | Waves$H3ID31 == "."] <- "."
sleCodes$SeriousInjury_W3[Waves$H3ID28 != "Injury/accident" | 
                              Waves$H3ID31 != "Injury/accident"] <- 0
sleCodes$SeriousInjury_W3[Waves$H3ID28 == "Injury/accident" | 
                              Waves$H3ID31 == "Injury/accident"] <- 1
sleCodes$SeriousInjury_W3[Waves$H3ID28 == "." | Waves$H3ID31 == "."] <- "."
sleCodes$SeriousInjury_W4[Waves$H4ID7 == "No"] <- 0
sleCodes$SeriousInjury_W4[Waves$H4ID7 == "Yes"] <- 1


### Father recieved welfare

sleCodes$FatherWelfare_W1[Waves$H1RF9 == "No"] <- 0
sleCodes$FatherWelfare_W1[Waves$H1RF9 == "Yes"] <- 1
sleCodes$FatherWelfare_W2[Waves$H2RF9 == 0] <- 0
sleCodes$FatherWelfare_W2[Waves$H2RF9 == 1] <- 1


### Mother recieved welfare

sleCodes$MotherWelfare_W1[Waves$H1RM9 == "No"] <- 0
sleCodes$MotherWelfare_W1[Waves$H1RM9 == "Yes"] <- 1
sleCodes$MotherWelfare_W2[Waves$H2RM9 == 0] <- 0
sleCodes$MotherWelfare_W2[Waves$H2RM9 == 1] <- 1

### Was raped

## Was raped (rather than committed rape) only asked of females for Wave I & II
## Treat Waves I & II as missing for Men?
## Needs decision on whether to use Waves$H4RD21 for rape indicator for Wave IV
## whether partner partner insisted or made sexual relations again will

# Tansform WaveIII_adds$H3RD114 and Waves$H3MA4 to type int
WaveIIITrans$H3RD114 <- as.integer(WaveIII_adds$H3RD114)
WavesTrans$H3MA4 <- as.integer(Waves$H3MA4)

sleCodes$Raped_W1[Waves$H1CO10 == "."] <- "." 
sleCodes$Raped_W1[Waves$BIO_SEX == "Female" & Waves$H1CO10 == "No"] <- 0
sleCodes$Raped_W1[Waves$BIO_SEX == "Female" & Waves$H1CO10 == "Yes"] <- 1
sleCodes$Raped_W2[Waves$BIO_SEX2 == 2 & Waves$H2CO12 == 0] <- 0
sleCodes$Raped_W2[Waves$BIO_SEX2 == 2 & Waves$H2CO12 == 1] <- 1
sleCodes$Raped_W3[Waves$H3MA4 == "." | Waves$AID %in%  
                  WaveIII_adds$AID[WaveIII_adds$H3RD114 == "."]] <- "."
sleCodes$Raped_W3[WavesTrans$H3MA4 <= 6 |
                 (Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3RD114 == 1])] <- 0
sleCodes$Raped_W3[WavesTrans$H3MA4 <= 5 |
                 (Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3RD114 >= 2 & 
                                                  WaveIIITrans$H3RD114 <= 8])] <- 1
sleCodes$Raped_W4[Waves$H4SE32 == "No" | Waves$H4SE34 == "No"] <- 0
sleCodes$Raped_W4[Waves$H4SE32 == "Yes" | Waves$H4SE34 == "Yes"] <- 1


### Ran away from home
## Wave III is included, but indicator asks ever. Should probably discard.
## Less than 0.37% were potentially 18 or younger at Wave III

# Transform Waves$H1DS7 to type int
WavesTrans$H1DS7 <- as.integer(Waves$H1DS7)

sleCodes$RanAway_W1[WavesTrans$H1DS7 == 1] <- 0
sleCodes$RanAway_W1[WavesTrans$H1DS7 >= 2 & WavesTrans$H1DS7 <= 4] <- 1
sleCodes$RanAway_W2[Waves$H2DS5 == 0] <- 0
sleCodes$RanAway_W2[Waves$H2DS5 >= 1 & Waves$H2DS5 <= 3] <- 1
sleCodes$RanAway_W3[Waves$H3HR23 == "."] <- 1
sleCodes$RanAway_W3[Waves$H3HR23 == "No"] <- 0
sleCodes$RanAway_W3[Waves$H3HR23 == "Yes"] <- 1


#### Nonromantic sexual relationship ended
## No raltionship end variable for NR (nonrelationship) section. I sequued every
## ounce of information I could out of this data (and spent way too much time doing it).
## The RX data, because romantic partner section was filled out, has relationship end
## variable, but Nonromantic section does not


sleCodes$NonRomanticRelEnd_W1[Waves$H1RX10_1 == "." | Waves$H1RX10_2 == "."] <- "."
sleCodes$NonRomanticRelEnd_W1[(Waves$H1RX10_1 == "Yes" | 
                               Waves$H1RX24A1 == "No" | Waves$H1RX21O1 == "Card rejected") |
                              (Waves$H1RX10_2 == "Yes" | 
                               Waves$H1RX24A2 == "Nos" | Waves$H1RX21O2 == "Card rejected") |
                              (Waves$H1RX10_2 == "Yes" | 
                               Waves$H1RX24A3 == "No" | Waves$H1RX21O3 == "Card rejected") |
                              (Waves$H2RX19_1 == 1 | (Waves$H2RX33M1 == 0 |
                               Waves$H2RX35_1 == 0 | Waves$H2RX36_1 == 0) &
                               Waves$H2RX20Y1 <= 95) | 
                              (Waves$H2RX19_2 == 1 | (Waves$H2RX33M2 == 0 | 
                               Waves$H2RX35_2 == 0 | Waves$H2RX36_2 == 0) &
                               Waves$H2RX20Y2 <= 95) |
                              (Waves$H2RX19_3 == 1 & (Waves$H2RX33M3 == 0 | 
                               Waves$H2RX35_3 == 0 | Waves$H2RX36_3 == 1) &
                               Waves$H2RX20Y3 <= 95)] <- 0
sleCodes$NonRomanticRelEnd_W1[Waves$H1RX10_1 == "No" & 
                             (Waves$H1RX24A1 == "Yes" | Waves$H1RX21O1 == "Card kept") |
                              Waves$H1RX10_2 == "No" & 
                             (Waves$H1RX24A2 == "Yes" | Waves$H1RX21O2 == "Card kept") |
                              Waves$H1RX10_2 == "No" & 
                             (Waves$H1RX24A3 == "Yes" | Waves$H1RX21O3 == "Card kept") |
                             (Waves$H2RX19_1 == 0 & (Waves$H2RX33M1 == 1 | 
                              Waves$H2RX35_1 == 1 | Waves$H2RX36_1 == 1) &
                                                    Waves$H2RX20Y1 <= 95) | 
                             (Waves$H2RX19_2 == 0 | (Waves$H2RX33M2 == 1 | 
                              Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1) &
                                                    Waves$H2RX20Y2 <= 95) |
                             (Waves$H2RX19_3 == 0 & (Waves$H2RX33M3 == 1 | 
                              Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1) &
                                                    Waves$H2RX20Y3 <= 95)] <- 1
sleCodes$NonRomanticRelEnd_W2[(Waves$H2RX19_1 == 1 | (Waves$H2RX33M1 == 0 | 
                               Waves$H2RX35_1 == 0 | Waves$H2RX36_1 == 0) &
                                                     Waves$H2RX20Y1 == 96) | 
                              (Waves$H2RX19_2 == 0 | (Waves$H2RX33M2 == 1 | 
                               Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1) &
                                                     Waves$H2RX20Y2 == 96) |
                              (Waves$H2RX19_3 == 0 & (Waves$H2RX33M3 == 1 | 
                               Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1) &
                                                     Waves$H2RX20Y3 == 96)] <- 0
sleCodes$NonRomanticRelEnd_W2[(Waves$H2RX19_1 == 0 & (Waves$H2RX33M1 == 1 | 
                               Waves$H2RX35_1 == 1 | Waves$H2RX36_1 == 1) &
                                                     Waves$H2RX20Y1 == 96) | 
                              (Waves$H2RX19_2 == 0 | (Waves$H2RX33M2 == 1 | 
                               Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1) &
                                                     Waves$H2RX20Y2 == 96) |
                              (Waves$H2RX19_3 == 0 & (Waves$H2RX33M3 == 1 | 
                               Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1) &
                                                     Waves$H2RX20Y3 == 96)] <- 1


#### Suffered verbal abuse in romantic relationship
# Didn't use indicator for if partner swore at respondent. Can add.

sleCodes$NonRomanticVerbalAbuse_W1[Waves$H2RI9_1 == 0 & Waves$H2RI9_2 == 0 & 
                                       Waves$H2RI9_3 == 0 & Waves$H2RI13_1 == 0 & 
                                       Waves$H2RI13_2 == 0 & Waves$H2RI13_3 == 0] <- 0
sleCodes$NonRomanticVerbalAbuse_W1[(Waves$H2RI9_1 == 1 & Waves$H2RI10Y1 <= 95) |
                                       (Waves$H2RI9_2 == 1 & Waves$H2RI10Y2 <= 95) |
                                       (Waves$H2RI9_3 == 1 & Waves$H2RI10Y3 <= 95) |
                                       (Waves$H2RI13_1 == 1 & Waves$H2RI12Y1 <= 95) |
                                       (Waves$H2RI13_2 == 1 & Waves$H2RI12Y2 <= 95) |
                                       (Waves$H2RI13_3 == 1 & Waves$H2RI12Y3 <= 95)] <- 1
sleCodes$NonRomanticVerbalAbuse_W2[Waves$H2RI9_1 == 0 & Waves$H2RI9_2 == 0 & 
                                       Waves$H2RI9_3 == 0 & Waves$H2RI13_1 == 0 & 
                                       Waves$H2RI13_2 == 0 & Waves$H2RI13_3 == 0] <- 0
sleCodes$NonRomanticVerbalAbuse_W2[(Waves$H2RI9_1 == 1 & Waves$H2RI10Y1 == 96) |
                                       (Waves$H2RI9_2 == 1 & Waves$H2RI10Y2 == 96) |
                                       (Waves$H2RI9_3 == 1 & Waves$H2RI10Y3 == 96) |
                                       (Waves$H2RI13_1 == 1 & Waves$H2RI12Y1 == 96) |
                                       (Waves$H2RI13_2 == 1 & Waves$H2RI12Y2 == 96) |
                                       (Waves$H2RI13_3 == 1 & Waves$H2RI12Y3 == 96)] <- 1


#### Suffered physical abuse in a romantic relationship
## Requires additional input on whether to include additional varaiables in SLEs_Master
## Was able to construct indicators using narow (marked green in SLEs_Master) indicators
## of violence and time indicators. Time and event indicators do not sum perfectly (very close)

# Transforming WaveIII_adds$H3RD112, WaveIII_adds$H3RD115, Waves$H4RD19 and
# Waves$H4RD20 to type int
WaveIIITrans$H3RD112 <- as.integer(WaveIII_adds$H3RD112)
WaveIIITrans$H3RD115 <- as.integer(WaveIII_adds$H3RD115)
WavesTrans$H4RD19 <- as.integer(Waves$H4RD19)
WavesTrans$H4RD20 <- as.integer(Waves$H4RD20)

sleCodes$RomanticPhysicalAbuse_W1[Waves$H2RI15_1 == 1 | Waves$H2RI15_2 == 1 | 
                                                        Waves$H2RI15_3 == 1] <- 0
sleCodes$RomanticPhysicalAbuse_W1[(Waves$H2RI15_1 == 1 & Waves$H2RI16Y1 <= 95) |
                                  (Waves$H2RI15_2 == 1 & Waves$H2RI16Y2 <= 95) |
                                  (Waves$H2RI15_3 == 1 & Waves$H2RI16Y3 <= 95)] <- 1
sleCodes$RomanticPhysicalAbuse_W2[Waves$H2RI15_1 == 0 | Waves$H2RI15_2 == 0 | 
                                                        Waves$H2RI15_3 == 0] <- 0
sleCodes$RomanticPhysicalAbuse_W2[Waves$H2RI15_1 == 1 | Waves$H2RI15_2 == 1 |
                                                        Waves$H2RI15_3 == 1] <- 1
sleCodes$RomanticPhysicalAbuse_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3RD112 == "."]] <- "." 
sleCodes$RomanticPhysicalAbuse_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3RD115 == "."]] <- "."
sleCodes$RomanticPhysicalAbuse_W3[Waves$AID %in% WaveIIITrans$AID[WaveIIITrans$H3RD112 == 1 | 
                                                                  WaveIIITrans$H3RD115 == 1]] <- 0
sleCodes$RomanticPhysicalAbuse_W3[Waves$AID %in% WaveIIITrans$AID[(WaveIIITrans$H3RD112 >= 2 & 
                                                                   WaveIIITrans$H3RD112 <= 8) | 
                                      (WaveIIITrans$H3RD115 >= 2 & WaveIIITrans$H3RD115 <= 8)]] <-1
sleCodes$RomanticPhysicalAbuse_W4[Waves$H4RD19 == "." | Waves$H4RD19 == "."] <- "."
sleCodes$RomanticPhysicalAbuse_W4[WavesTrans$H4RD19 == 1 | WavesTrans$H4RD20 == 1] <- 0
sleCodes$RomanticPhysicalAbuse_W4[(WavesTrans$H4RD19 >= 2 & WavesTrans$H4RD19 <= 8) | 
                                  (WavesTrans$H4RD20 >= 2 & WavesTrans$H4RD20 <= 8)] <-1


### Suffered verbal abuse in a nonromantic relationship
# Didn't use indicator for if NR partner swore at respondent. Can add.

sleCodes$NonRomanticVerbalAbuse_W1[Waves$H2NR31_2 == 0 | Waves$H2NR31_3 == 0 |
                                   Waves$H2NR35_1 == 0 | Waves$H2NR35_2 == 0 |
                                   Waves$H2NR35_3 == 0 | Waves$H2RX9_1  == 0 |
                                   Waves$H2RX9_2  == 0 | Waves$H2RX9_3  == 0 |
                                   Waves$H2RX13_1 == 0 | Waves$H2RX13_2 == 0 |
                                   Waves$H2RX13_3 == 0] <- 0
sleCodes$NonRomanticVerbalAbuse_W1[(Waves$H2NR31_1 == 1 & Waves$H2NR32Y1 <= 95) |
                                   (Waves$H2NR31_2 == 1 & Waves$H2NR32Y2 <= 95) |
                                   (Waves$H2NR31_3 == 1 & Waves$H2NR32Y3 <= 95) |
                                   (Waves$H2NR35_1 == 1 & Waves$H2NR36Y1 <= 95) |
                                   (Waves$H2NR35_2 == 1 & Waves$H2NR36Y2 <= 95) |
                                   (Waves$H2NR35_3 == 1 & Waves$H2NR36Y3 <= 95) |
                                   (Waves$H2RX9_1  == 1 & Waves$H2RX10Y1 <= 95) |
                                   (Waves$H2RX9_2  == 1 & Waves$H2RX10Y2 <= 95) |
                                   (Waves$H2RX9_3  == 1 & Waves$H2RX10Y3 <= 95) |
                                   (Waves$H2RX13_1 == 1 & Waves$H2RX12Y1 <= 95) |
                                   (Waves$H2RX13_2 == 1 & Waves$H2RX12Y2 <= 95) |
                                   (Waves$H2RX13_3 == 1 & Waves$H2RX12Y3 <= 95)] <- 1


sleCodes$NonRomanticVerbalAbuse_W2[Waves$H2NR31_1 == 0 | Waves$H2NR31_2 == 0 | 
                                   Waves$H2NR31_3 == 0 | Waves$H2NR35_1 == 0 |
                                   Waves$H2NR35_2 == 0 | Waves$H2NR35_3 == 0 | 
                                   Waves$H2RX9_1 == 0 | Waves$H2RX9_2 == 0 | 
                                   Waves$H2RX9_3 == 0 | Waves$H2RX13_1 == 0 |
                                   Waves$H2RX13_2 == 0 | Waves$H2RX13_3 == 0] <- 0
sleCodes$NonRomanticVerbalAbuse_W2[(Waves$H2NR31_1 == 1 & Waves$H2NR32Y1 == 96) |
                                   (Waves$H2NR31_2 == 1 & Waves$H2NR32Y2 == 96) |
                                   (Waves$H2NR31_3 == 1 & Waves$H2NR32Y3 == 96) |
                                   (Waves$H2NR35_1 == 1 & Waves$H2NR36Y1 == 96) |
                                   (Waves$H2NR35_2 == 1 & Waves$H2NR36Y2 == 96) |
                                   (Waves$H2NR35_3 == 1 & Waves$H2NR36Y3 == 96) |
                                   (Waves$H2RX9_1  == 1 & Waves$H2RX10Y1 == 96) |
                                   (Waves$H2RX9_2  == 1 & Waves$H2RX10Y2 == 96) |
                                   (Waves$H2RX9_3  == 1 & Waves$H2RX10Y3 == 96) |
                                   (Waves$H2RX13_1 == 1 & Waves$H2RX12Y1 == 96) |
                                   (Waves$H2RX13_2 == 1 & Waves$H2RX12Y2 == 96) |
                                   (Waves$H2RX13_3 == 1 & Waves$H2RX12Y3 == 96)] <- 1


#### Suffered physical abuse in a nonromantic relationship
## Did not include "Did {INITIALS} throw something at you that could hurt you?"
## Was able to construct indicators for all Waves using narow (marked green in SLEs_Master) indicators
## of violence and time indicators. Time and event indicators do not sum perfectly (very close)

sleCodes$NonRomanticPhysicalAbuse_W1[Waves$H2NR37_1 == 0 | Waves$H2NR37_2 == 0 |
                                     Waves$H2NR37_3 == 0 | Waves$H2RX15_1 == 0 |
                                     Waves$H2RX15_2 == 0 | Waves$H2RX15_3 == 0] <- 0
sleCodes$NonRomanticPhysicalAbuse_W1[(Waves$H2NR37_1 == 1 & Waves$H2NR38Y1 <= 95) |
                                     (Waves$H2NR37_2 == 1 & Waves$H2NR38Y2 <= 95) |
                                     (Waves$H2NR37_3 == 1 & Waves$H2RI16Y3 <= 95) |
                                     (Waves$H2RX15_1 == 1 & Waves$H2RX16Y1 <= 95) |
                                     (Waves$H2RX15_2 == 1 & Waves$H2RX16Y2 <= 95) |
                                     (Waves$H2RX15_3 == 1 & Waves$H2RX16Y3 <= 95)] <- 1
sleCodes$NonRomanticPhysicalAbuse_W2[Waves$H2NR37_1 == 0 | Waves$H2NR37_2 == 0 | 
                                     Waves$H2NR37_3 == 0 | Waves$H2RX15_1 == 0 |
                                     Waves$H2RX15_2 == 0 | Waves$H2RX15_3 == 0] <- 0
sleCodes$NonRomanticPhysicalAbuse_W2[(Waves$H2NR37_1 == 1 & Waves$H2NR38Y1 == 96) |
                                     (Waves$H2NR37_2 == 1 & Waves$H2NR38Y2 == 96) |
                                     (Waves$H2NR37_3 == 1 & Waves$H2NR38Y3 == 96) |
                                     (Waves$H2RX15_1 == 1 & Waves$H2RX16Y1 == 96) |
                                     (Waves$H2RX15_2 == 1 & Waves$H2RX16Y2 == 96) |
                                     (Waves$H2RX15_3 == 1 & Waves$H2RX16Y3 == 96)] <- 1


### Evicted from residence
## Should inability to pay rent/mortgage be a seperate SLE? Indicators exsist. See SLEs_Master

sleCodes$Evicted_W3[Waves$H3EC20 == "."] <- "."
sleCodes$Evicted_W3[Waves$H3EC20 == "No"] <- 0
sleCodes$Evicted_W3[Waves$H3EC20 == "Yes"] <- 1
sleCodes$Evicted_W4[Waves$H4EC12 == "No"] <- 0
sleCodes$Evicted_W4[Waves$H4EC12 == "Yes"] <- 1


### Cutoff service


sleCodes$ServiceCut_W3[Waves$H3EC18 == "." | Waves$H3EC22 == "."] <- "."
sleCodes$ServiceCut_W3[Waves$H3EC18 == "No" | Waves$H3EC22 == "No"] <- 0
sleCodes$ServiceCut_W3[Waves$H3EC18 == "Yes" | Waves$H3EC22 == "Yes"] <- 1
sleCodes$ServiceCut_W4[Waves$H4EC10 == "No" | Waves$H4EC14 == "No"] <- 0
sleCodes$ServiceCut_W4[Waves$H4EC10 == "Yes" | Waves$H4EC14 == "Yes"] <- 1


### Entered full time active miitary duty
## Was able to construct indicators for all 4 Waves using time indicators

sleCodes$EnteredMilitary_W1[Waves$H3LM40Y == 9997 | Waves$H4MI8Y == 9997] <- 0
sleCodes$EnteredMilitary_W1[Waves$H3LM40Y <= 1995 | Waves$H4MI8Y <= 1995] <- 1
sleCodes$EnteredMilitary_W2[Waves$H3LM40Y == 9997 | Waves$H4MI8Y == 9997] <- 0
sleCodes$EnteredMilitary_W2[Waves$H3LM40Y == 1996 | Waves$H4MI8Y == 1996] <- 1
sleCodes$EnteredMilitary_W3[Waves$H3LM40Y == 9997 | Waves$H4MI8Y == 9997] <- 0
sleCodes$EnteredMilitary_W3[(Waves$H3LM40Y >= 1997 & Waves$H3LM40Y <= 2002) |
                            (Waves$H4MI8Y >= 1997 & Waves$H4MI8Y <= 2002)] <- 1
sleCodes$EnteredMilitary_W4[Waves$H4MI8Y == 9997] <- 0
sleCodes$EnteredMilitary_W4[Waves$H4MI8Y >= 2003 & Waves$H4MI8Y <= 2008] <- 1



### Discharged from the armed services
## Was able to construct indicators for all 4 Waves using time indicators

sleCodes$DischargedMilitary_W1[Waves$H3LM46Y == 9997 | Waves$H4MI9Y == 9997] <- 0
sleCodes$DischargedMilitary_W1[Waves$H3LM46Y <= 1995 | Waves$H4MI9Y <= 1995] <- 1
sleCodes$DischargedMilitary_W2[Waves$H3LM46Y == 9997 | Waves$H4MI9Y == 9997] <- 0
sleCodes$DischargedMilitary_W2[Waves$H3LM46Y == 1996 | Waves$H4MI9Y == 1996] <- 1
sleCodes$DischargedMilitary_W3[Waves$H3LM46Y == 9997 | Waves$H4MI9Y == 9997] <- 0
sleCodes$DischargedMilitary_W3[(Waves$H3LM46Y >= 1997 & Waves$H3LM46Y <= 2002) |
                               (Waves$H4MI9Y >= 1997 & Waves$H4MI9Y <= 2002)] <- 1
sleCodes$DischargedMilitary_W4[Waves$H4MI9Y == 9997 & Waves$H4MI9Y == 9997] <- 0
sleCodes$DischargedMilitary_W4[Waves$H4MI9Y >= 2003 & Waves$H4MI9Y <= 2008] <- 1

                              
### Cohabitation dissolution

# Below comment lead to nth (who even knows) dig on final verification pass
# for better indicator and for more than one wave leading to finding a series to 
# construct precise time variant indicators for all for waves AND for marriage dissolution

# Used H4RD6 marriage status: Living apart / seperated as indicator for Wave IV


sleCodes$CohabitationEnd_W1[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Yes"]] <- 0
sleCodes$CohabitationEnd_W1[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 1995])] <- 1
sleCodes$CohabitationEnd_W2[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Yes"]] <- 0
sleCodes$CohabitationEnd_W2[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y == 1996])] <- 1
# Probably lacks precision
#sleCodes$CohabitationEnd_W3 <- 0
#sleCodes$CohabitationEnd_W3[Waves$H3MR6_A == "No" | Waves$H3MR6_B == "No" |
#Waves$H3MR6_C == "No"] <- 1
sleCodes$CohabitationEnd_W3[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Yes"]] <- 0
sleCodes$CohabitationEnd_W3[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] & 
                           (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y >= 1997] & 
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 2002])] <- 1
sleCodes$CohabitationEnd_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Yes"]] <- 0
sleCodes$CohabitationEnd_W4[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Cohabitation"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                            (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y >= 2003] & 
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 2009])) |
                             Waves$H4RD6 == "Living apart because of legal separation"] <- 1


### Received welfare

# Able to construct indicator across all 4 Waves. Most respondetns would have been
# too young to revieve wlefare, but many were not. Wave I was conducted in 1995,
# and birth year must be set at <= 1982 to capture all 205 reporting recieving 
# welfare in 1995
table(Waves$H3EC51 == "Yes" & Waves$H3OD1Y <= 1982)
# Setting birth year to <= 1983 captures 272 of 273 reporting recieving welfare
# in 1996 (year of Wave II), <= 1983 captures all 273
table(Waves$H3EC49 == "Yes" & Waves$H3OD1Y <= 1983)

# H3EC26 asks if currecntly on welfare, but not does not sum perfectly with H3EC39
# & H3EC41 (2000-2001). But there are no cases of reporting yes for the former and no
# for either of the later, this a comprehensive indicator can be constructed for Wave III
table(Waves$H3EC26 == "Yes" & (Waves$H3EC39 == "No" | Waves$H3EC41 == "No"))

# Wave IV indicator H4EC18 is problematic. The wording is ambiguous ("Between {1995/2002} 
# and {2006/2007/2008}") and the numbers seem extreame, nearly 24% of responsents.

sleCodes$RecievedWelfare_W1[Waves$H3EC51 == "."] <- "."
sleCodes$RecievedWelfare_W1[Waves$H3EC51 == "No (skip to Q.53)"] <- 0
sleCodes$RecievedWelfare_W1[Waves$H3EC51 == "Yes"] <- 1
sleCodes$RecievedWelfare_W2[Waves$H3EC49 == "."] <- "."
sleCodes$RecievedWelfare_W2[Waves$H3EC49 == "No (skip to Q.51)"] <- 0
sleCodes$RecievedWelfare_W2[Waves$H3EC49 == "Yes"] <- 1
sleCodes$RecievedWelfare_W3[Waves$H3EC47 == "."] <- "."
sleCodes$RecievedWelfare_W3[Waves$H3EC26 == "No" | Waves$H3EC39 == "No" |
                            Waves$H3EC41 == "No" | Waves$H3EC43 == "No" |
                            Waves$H3EC45 == "NO" | Waves$H3EC47 == "No"] <- 0
sleCodes$RecievedWelfare_W3[Waves$H3EC26 == "Yes" | Waves$H3EC39 == "Yes" |
                            Waves$H3EC41 == "Yes" | Waves$H3EC43 == "Yes" |
                            Waves$H3EC45 == "Yes" | Waves$H3EC47 == "Yes"] <- 1
sleCodes$RecievedWelfare_W4[Waves$H4EC18 == "No"] <- 0
sleCodes$RecievedWelfare_W4[Waves$H4EC18 == "Yes"] <- 1


### Involuntarily dropped from welfare

# Numers compute, using more accurate / robust single indicator
# ie no repondent reported reported being cut off but reported "No" to 
# to recieving aid
table(Waves$H3EC27 == "No" & 
      Waves$H3EC28 == "Cut off by welfare office (skip to Q.30)")
table(Waves$H3EC27 == "Yes (skip to Q.31)" & 
      Waves$H3EC28 == "Cut off by welfare office (skip to Q.30)")
table(Waves$H3EC28 == "Cut off by welfare office (skip to Q.30)")

sleCodes$DroppedFromWelfare_W3[Waves$H3EC28 == "."] <- "."
sleCodes$DroppedFromWelfare_W3[Waves$H3EC27 == "No" | 
                               Waves$H3EC27 == "Legitimate skip"] <- 0
sleCodes$DroppedFromWelfare_W3[Waves$H3EC28 == 
                                   "Cut off by welfare office (skip to Q.30)"] <- 1


### Marriage dissolution

# Was able to construct an indicator for Wave II

# Found better variables to construct indicator across all waves
# Original codes commented out. Numbers exact same for Wave II & III between new and old
# (Original from main data, new from addendum files, asked in different waves. 
# kind of amazing validation).


sleCodes$CohabitationEnd_W1[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 != "Marriage"] |
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] |
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Legitimate skip"] |
                           (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y != 1995] &
                            Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 9996])] <- 0
sleCodes$CohabitationEnd_W1[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Marriage"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                             Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 1995])] <- 1
# Original Wave II
sleCodes$MarriageEnd_W2[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 != "Marriage"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Legitimate skip"] |
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y != 1996] &
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 9996])] <- 0
# sleCodes$MarriageEnd_W2[(Waves$H2GI3 == 1 & Waves$H2GI5 == 1) |
# Waves$H3MR5Y_A == 1996 | Waves$H3MR5Y_B == 1996] <- 1
# New Wave II
sleCodes$MarriageEnd_W2[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Marriage"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y == 1996])] <- 1
# Original Wave III
sleCodes$MarriageEnd_W3[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 != "Marriage"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Legitimate skip"] |
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 1997] &
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y > 2003] &
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 9996])] <- 0
# sleCodes$MarriageEnd_W3[(Waves$H3MR5Y_A >= 1997 & Waves$H3MR5Y_A <= 2002) |
# (Waves$H3MR5Y_B >= 1997 & Waves$H3MR5Y_B <= 2002)] <- 1
# New Wave III
sleCodes$MarriageEnd_W3[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Marriage"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y >= 1997] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 2003])] <- 1

sleCodes$MarriageEnd_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 != "Marriage"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] |
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "Legitimate skip"] |
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 2003] &
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 9996])] <- 0
sleCodes$MarriageEnd_W4[(Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR13 == "Marriage"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR14 == "No"] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y >= 2003] &
                         Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 2009])] <- 1


### Baby had major health problems at birth

# Still looking for a clever way to construct an indicator for WaveIV

sleCodes$BabyHealthProbs_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB10 == "."]] <- "."
sleCodes$BabyHealthProbs_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB10 == "No"] |
                            Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB10 == "Not applicable"]] <- 0
sleCodes$BabyHealthProbs_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB10 == "Yes"]] <- 1


### Death of Baby

# Transform WaveIII_adds$H3LB12Y to type int 
WaveIIITrans$H3LB12Y <- as.integer(WaveIII_adds$H3LB12Y)

sleCodes$BabyDeath_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "Yes (skip to Q.13)"] |
                      Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "Not applicable"] |
                      Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "Legitimate skip"] |
                     (Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB12Y < 1997] &
                      Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB12Y > 2001])] <- 0
sleCodes$BabyDeath_W3[Waves$AID %in% WaveIII_adds$AID[WaveIII_adds$H3LB11 == "No"] & 
                     (Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB12Y >= 1997] &
                      Waves$AID %in% WaveIII_adds$AID[WaveIIITrans$H3LB12Y <= 2002])] <- 1
sleCodes$BabyDeath_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "Yes"] |
                      Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "Legitimate skip"] |
                     (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y < 2002] &
                      Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y > 2008])] <- 0
sleCodes$BabyDeath_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB10 == "No"] & 
                     (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y >= 2002] &
                      Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4LB11Y <= 2008])] <- 1

### Death of a romantic partner


sleCodes$DeathPartner_W3[Waves$H3MR14_A != "Refused" | 
                         Waves$H3MR14_A != "Don't know" |
                         Waves$H3MR14_B != "Refused" |
                         Waves$H3MR14_B != "Don't know"] <- 0
sleCodes$DeathPartner_W3[Waves$H3MR14_A == "."] <- "."
sleCodes$DeathPartner_W3[Waves$H3MR14_B == "."] <- "."
sleCodes$DeathPartner_W3[Waves$H3MR14_A == "Your partner died" | 
                         Waves$H3MR14_B == "Your partner died"] <- 1


### Death of a spouse

sleCodes$DeathSpouse_W3[Waves$H3MR4_A == "."] <- "."
sleCodes$DeathSpouse_W3[Waves$H3MR4_A != "Don't know" | Waves$H3MR4_A != "." |
                       (Waves$H3MR5Y_A < 1997 & Waves$H3MR5Y_A > 2002)] <- 0
sleCodes$DeathSpouse_W3[Waves$H3MR4_A == "Spouse's death" & 
                       (Waves$H3MR5Y_A >= 1997 & Waves$H3MR5Y_A <= 2002)] <- 1
sleCodes$DeathSpouse_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR29 != "Refused"] |
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y < 2003] & 
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y > 2009])] <- 0
sleCodes$DeathSpouse_W4[Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR29 == "Spouse died"] &
                       (Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y >= 2003] &
                        Waves$AID %in% WaveIV_adds$AID[WaveIV_adds$H4TR28Y <= 2009])] <-1

########################
#### Additions


### Household recieved welfare
# All 4 Waves ask about recieveing welfare before 18. Overlaps with previous.
# Possible to construct indicator for Waves III & IV using Waves$H4EC18
# but will will be partial and incomplete because indicator will cover interval
# between Waves II & IV for some and III and & IV for others (at least as I understand)


### Threatened with gun or knife

#### **** H4DS15 is just wrong. Not same numbers as codebook
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/variable?VariableId=8005

# Transform Waves$H1FV2, Waves$H3DS18B & Waves$H3DS18C to type int
WavesTrans$H1FV2 <- as.integer(Waves$H1FV2)
WavesTrans$H3DS18B <- as.integer(Waves$H3DS18B)
WavesTrans$H3DS18C <- as.integer(Waves$H3DS18C)

sleCodes$ThreatenedGunKnife_W1[WavesTrans$H1FV2 == 1] <- 0
sleCodes$ThreatenedGunKnife_W1[WavesTrans$H1FV2 == 2 | WavesTrans$H1FV2 == 3] <- 1
sleCodes$ThreatenedGunKnife_W2[Waves$H2FV2 == 0] <- 0
sleCodes$ThreatenedGunKnife_W2[Waves$H2FV2 == 1 | Waves$H2FV2 == 2] <- 1
sleCodes$ThreatenedGunKnife_W3[Waves$H3DS18B == "." | Waves$H3DS18C == "."] <- "."
sleCodes$ThreatenedGunKnife_W3[WavesTrans$H3DS18B == 1 | WavesTrans$H3DS18C == 1] <- 0
sleCodes$ThreatenedGunKnife_W3[WavesTrans$H3DS18B == 2 | WavesTrans$H3DS18C == 2] <- 1
sleCodes$ThreatenedGunKnife_W4[Waves$H4DS15 == "No"] <- 0
sleCodes$ThreatenedGunKnife_W4[Waves$H4DS15 == "Yes"] <- 1


### Weapon use

WavesTrans$H3DS11 <- as.integer(Waves$H3DS11)

sleCodes$WeaponUse_W1[Waves$H1JO26 == "No"] <- 0
sleCodes$WeaponUse_W1[Waves$H1JO26 == "Yes"] <- 1
sleCodes$WeaponUse_W2[Waves$H2FV10 == 0] <- 0
sleCodes$WeaponUse_W2[Waves$H2FV10 == 1] <- 1
sleCodes$WeaponUse_W3[Waves$H3DS11 == "."] <- "."
sleCodes$WeaponUse_W3[WavesTrans$H3DS11 == 1] <- 0
sleCodes$WeaponUse_W3[WavesTrans$H3DS11 >= 2 & WavesTrans$H3DS11 <= 4] <- 1


### Sibling death

# Needs review. There are time variant indicators from Waves III & IV. Wave III
# indicators are in waves ranges and fine. Wave IV have responses for sibling deaths
# as much as 5 decades prior to Wave I interview.

WavesTrans$H3WS2YD <- as.integer(Waves$H3WS2YD)


sleCodes$SiblingDeath_W1[((Waves$H3WS2YA != 1995 & Waves$H3WS2YA != 9998) | 
                              (Waves$H3WS2YB != 1995 & Waves$H3WS2YB != 9998) |
                              (Waves$H3WS2YC != 1995 & Waves$H3WS2YC != 1995) | 
                              (WavesTrans$H3WS2YD != 1995 & WavesTrans$H3WS2YD != 9995)) |
                             ((Waves$H4WS3A != 1995 & Waves$H4WS3A != 9995) | 
                             (Waves$H4WS3B != 1995 & Waves$H4WS3B != 9995) |
                             (Waves$H4WS3C != 1995 & Waves$H4WS3C != 9995) |
                             (Waves$H4WS3D != 1995 & Waves$H4WS3D != 9995) |
                             (Waves$H4WS3E != 1995 & Waves$H4WS3E != 9995) |
                             (Waves$H4WS3F != 1995 & Waves$H4WS3F != 9995) |
                             (Waves$H4WS3G != 1995 & Waves$H4WS3G != 9995) |
                             (Waves$H4WS3H != 1995 & Waves$H4WS3H != 9995))] <- 0
sleCodes$SiblingDeath_W1[(Waves$H3WS2YA <= 1995 | Waves$H3WS2YB <= 1995 |
                              Waves$H3WS2YC <= 1995) |
                             (Waves$H4WS3A <= 1995 | Waves$H4WS3B <= 1995 |
                              Waves$H4WS3C <= 1995 | Waves$H4WS3D <= 1995 |
                              Waves$H4WS3E <= 1995 | Waves$H4WS3F <= 1995 |
                              Waves$H4WS3G <= 1995 | Waves$H4WS3H <= 1995)] <- 1
sleCodes$SiblingDeath_W2[((Waves$H3WS2YA != 1996 & Waves$H3WS2YA != 9998) | 
                              (Waves$H3WS2YB != 1996 & Waves$H3WS2YB != 9998) |
                              (Waves$H3WS2YC != 1996 & Waves$H3WS2YC != 1995) | 
                              (WavesTrans$H3WS2YD != 1995 & WavesTrans$H3WS2YD != 9995)) |
                             ((Waves$H4WS3A != 1996 & Waves$H4WS3A != 9995) | 
                              (Waves$H4WS3B != 1996 & Waves$H4WS3B != 9995) |
                              (Waves$H4WS3C != 1996 & Waves$H4WS3C != 9995) |
                              (Waves$H4WS3D != 1996 & Waves$H4WS3D != 9995) |
                              (Waves$H4WS3E != 1996 & Waves$H4WS3E != 9995) |
                              (Waves$H4WS3F != 1996 & Waves$H4WS3F != 9995) |
                              (Waves$H4WS3G != 1996 & Waves$H4WS3G != 9995) |
                             (Waves$H4WS3H != 1996 & Waves$H4WS3H != 9995))] <- 0
sleCodes$SiblingDeath_W2[(Waves$H3WS2YA == 1996 | Waves$H3WS2YB == 1996 |
                              Waves$H3WS2YC == 1996) |
                             (Waves$H4WS3A == 1996 | Waves$H4WS3B == 1996 |
                              Waves$H4WS3C == 1996 | Waves$H4WS3D == 1996 |
                              Waves$H4WS3E == 1996 | Waves$H4WS3F == 1996 |
                              Waves$H4WS3G == 1996 | Waves$H4WS3H == 1996)] <- 1
sleCodes$SiblingDeath_W3[(Waves$H3WS2YA < 1997 & Waves$H3WS2YA > 2002 & Waves$H3WS2YA != 9998) |
                             (Waves$H3WS2YB < 1997 & Waves$H3WS2YB > 2002 & Waves$H3WS2YB != 9998) |
                             (Waves$H3WS2YC < 1997 & Waves$H3WS2YC > 2002 & Waves$H3WS2YC != 9998) |
                             (WavesTrans$H3WS2YD != 9995) |
                             (Waves$H4WS3A < 1997 & Waves$H4WS3A > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3B < 1997 & Waves$H4WS3B > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3C < 1997 & Waves$H4WS3C > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3D < 1997 & Waves$H4WS3D > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3A < 1997 & Waves$H4WS3A > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3B < 1997 & Waves$H4WS3B > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3C < 1997 & Waves$H4WS3C > 2002 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3D < 1997 & Waves$H4WS3D > 2002 & Waves$H4WS3A != 9995)] <- 0
sleCodes$SiblingDeath_W3[(Waves$H3WS2YA >= 1997 & Waves$H3WS2YA <= 2002) |
                             (Waves$H3WS2YB >= 1997 & Waves$H3WS2YB <= 2002) |
                             (Waves$H3WS2YC >= 1997 & Waves$H3WS2YC <= 2002) |
                             (Waves$H4WS3A >= 1997 & Waves$H4WS3A <= 2002) |
                             (Waves$H4WS3B >= 1997 & Waves$H4WS3B <= 2002) |
                             (Waves$H4WS3C >= 1997 & Waves$H4WS3C <= 2002) |
                             (Waves$H4WS3D >= 1997 & Waves$H4WS3D <= 2002) |
                             (Waves$H4WS3A >= 1997 & Waves$H4WS3A <= 2002) |
                             (Waves$H4WS3B >= 1997 & Waves$H4WS3B <= 2002) |
                             (Waves$H4WS3C >= 1997 & Waves$H4WS3C <= 2002) |
                             (Waves$H4WS3D >= 1997 & Waves$H4WS3D <= 2002)] <- 1
sleCodes$SiblingDeath_W4[(Waves$H3WS2YA < 2003 & Waves$H3WS2YA > 2002 & Waves$H3WS2YA != 9998) |
                             (Waves$H3WS2YB < 2003 & Waves$H3WS2YB > 2002 & Waves$H3WS2YB != 9998) |
                             (Waves$H3WS2YC < 2003 & Waves$H3WS2YC > 2002 & Waves$H3WS2YC != 9998) |
                             (WavesTrans$H3WS2YD != 9995) |
                             (Waves$H4WS3A < 2003 & Waves$H4WS3A > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3B < 2003 & Waves$H4WS3B > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3C < 2003 & Waves$H4WS3C > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3D < 2003 & Waves$H4WS3D > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3A < 2003 & Waves$H4WS3A > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3B < 2003 & Waves$H4WS3B > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3C < 2003 & Waves$H4WS3C > 2008 & Waves$H4WS3A != 9995) |
                             (Waves$H4WS3D < 2003 & Waves$H4WS3D > 2008 & Waves$H4WS3A != 9995)] <- 0
sleCodes$SiblingDeath_W4[(Waves$H4WS3A >= 2003 & Waves$H4WS3A <= 2008) |
                             (Waves$H4WS3B >= 2003 & Waves$H4WS3B <= 2008) |
                             (Waves$H4WS3C >= 2003 & Waves$H4WS3C <= 2008) |
                             (Waves$H4WS3D >= 2003 & Waves$H4WS3D <= 2008) |
                             (Waves$H4WS3A >= 2003 & Waves$H4WS3A <= 2008) |
                             (Waves$H4WS3B >= 2003 & Waves$H4WS3B <= 2008) |
                             (Waves$H4WS3C >= 2003 & Waves$H4WS3C <= 2008) |
                             (Waves$H4WS3D >= 2003 & Waves$H4WS3D <= 2008)] <- 1


### Threatend someone with weapon to get something

# Transforming to type int
WavesTrans$H1DS11 <- as.integer(Waves$H1DS11)
WavesTrans$H3DS4 <- as.integer(Waves$H3DS4)
WavesTrans$H4DS4 <- as.integer(Waves$H4DS4)

sleCodes$ThreatWithWeapon_W1[WavesTrans$H1DS11 == 1 | WavesTrans$H1DS11 == 7] <- 0
sleCodes$ThreatWithWeapon_W1[WavesTrans$H1DS11 >= 2 & WavesTrans$H1DS11 <= 4] <- 1
sleCodes$ThreatWithWeapon_W2[Waves$H2DS9 == 0] <- 0
sleCodes$ThreatWithWeapon_W2[Waves$H2DS9 >= 1 & Waves$H2DS9 <= 3] <- 1
sleCodes$ThreatWithWeapon_W3[Waves$H3DS4 == "."] <- "."
sleCodes$ThreatWithWeapon_W3[WavesTrans$H3DS4 == 1 | WavesTrans$H3DS4 == 7] <- 0
sleCodes$ThreatWithWeapon_W3[WavesTrans$H3DS4 >= 2 & WavesTrans$H3DS4 <= 4] <- 1
sleCodes$ThreatWithWeapon_W4[WavesTrans$H4DS4 == 1] <- 0
sleCodes$ThreatWithWeapon_W4[WavesTrans$H4DS4 >= 2 & WavesTrans$H4DS4 <= 4] <- 1


# Got in serious physical fight

# Transforming to type int
WavesTrans$H1DS5 <- as.integer(Waves$H1DS5)
WavesTrans$H1FV5 <- as.integer(Waves$H1FV5)
WavesTrans$H4DS11 <- as.integer(Waves$H4DS11)

sleCodes$HurtOther_W1[WavesTrans$H1DS5 == 1 | WavesTrans$H1DS5 == 7] <- 0
sleCodes$HurtOther_W1[WavesTrans$H1DS5 >= 2 & WavesTrans$H1DS5 <= 4] <- 1
sleCodes$HurtOther_W2[WavesTrans$H1FV5 == 1 | WavesTrans$H1FV5 == 6] <- 0
sleCodes$HurtOther_W2[WavesTrans$H1FV5 >= 2 & WavesTrans$H1FV5 <= 2] <- 1
sleCodes$HurtOther_W3[Waves$H3DS16 == "."] <- "."
sleCodes$HurtOther_W3[Waves$H3DS16 == 0 | Waves$H3DS16 == 99] <- 0
sleCodes$HurtOther_W3[Waves$H3DS16 >= 1 & Waves$H3DS16 < 96] <- 1
sleCodes$HurtOther_W4[WavesTrans$H4DS11 == 1 | WavesTrans$H4DS11 == 6] <- 0
sleCodes$HurtOther_W4[WavesTrans$H4DS11 >= 2 & WavesTrans$H4DS11 <= 4] <- 1

### Hurt someone badly enough to need medical care

# Apparent miscoding for Wave IV

# Transforming to type int
WavesTrans$H1DS6 <- as.integer(Waves$H1DS6)
WavesTrans$H4DS12 <- as.integer(Waves$H4DS12)


sleCodes$HurtOther_W1[WavesTrans$H1DS6 == 1 | WavesTrans$H1DS6 == 7] <- 0
sleCodes$HurtOther_W1[WavesTrans$H1DS6 >= 2 & WavesTrans$H1DS6 <= 4] <- 1
sleCodes$HurtOther_W2[Waves$H2FV22 == 1 | Waves$H2FV22 == 7] <- 0
sleCodes$HurtOther_W2[Waves$H2FV22 >= 1 & Waves$H2FV22 <= 3] <- 1
sleCodes$HurtOther_W3[Waves$H3DS17 == "."] <- "."
sleCodes$HurtOther_W3[Waves$H3DS17 == 0 | Waves$H3DS17 == 99] <- 0
sleCodes$HurtOther_W3[Waves$H3DS17 >= 1 & Waves$H3DS17 <= 9] <- 1
# Coding has to be wrong. 15000 people didn’t refuse to answer this question.
sleCodes$HurtOther_W4[WavesTrans$H4DS12 == 1 & WavesTrans$H4DS12 <= 4] <- 0
sleCodes$HurtOther_W4[WavesTrans$H4DS12 >= 2 & WavesTrans$H4DS12 <= 4] <- 1


####### Additional requests / edits

### Caregiver sexual assualt 
# H4MA6 asks age of event, all < 18 years of age

WavesTrans$H4MA4 <- as.integer(Waves$H4MA4)


sleCodes$CaregiverSexualAssault[Waves$H3MA4 =="."] <- "."
sleCodes$CaregiverSexualAssault[WavesTrans$H3MA4 == 6 | WavesTrans$H4MA4 == 9] <- 0
sleCodes$CaregiverSexualAssault[WavesTrans$H3MA4 <= 5 | WavesTrans$H4MA4 <= 5] <- 1


### Caregive Neglect

WavesTrans$H3MA2 <- as.integer(Waves$H3MA2)

sleCodes$CaregiverNeglect[Waves$H3MA2 =="."] <- "."
sleCodes$CaregiverNeglect[WavesTrans$H3MA2 == 6 | Waves$H3MA2 == 9] <- 0 
sleCodes$CaregiverNeglect[WavesTrans$H3MA2 <= 5] <- 1

### Caregiver Abuse

WavesTrans$H4MA3 <- as.integer(Waves$H4MA3)

sleCodes$CaregiverAbuse[Waves$H3MA3 =="."] <- "."
sleCodes$CaregiverAbuse[WavesTrans$H4MA3 == 6 | WavesTrans$H4MA3 == 9] <- 0
sleCodes$CaregiverAbuse[WavesTrans$H4MA3 <= 5] <- 1


### Social Service intervention (investigation or attempt to remove)

WavesTrans$H3MA5 <- as.integer(Waves$H3MA5)

sleCodes$SocialServiceIntervention[Waves$H3MA5 =="."] <- "."
sleCodes$SocialServiceIntervention[WavesTrans$H3MA5 == 1 | WavesTrans$H3MA5 == 18] <- 0
sleCodes$SocialServiceIntervention[WavesTrans$H3MA5 >= 2 & WavesTrans$H3MA5 <= 60] <- 1

#### Social Service Removed from home

WavesTrans$H3MA6 <- as.integer(Waves$H3MA6)

sleCodes$RemovedFromHome[Waves$H3MA6 =="."] <- "."
sleCodes$RemovedFromHome[WavesTrans$H3MA6 == 1 | WavesTrans$H3MA6 == 18] <- 0
sleCodes$RemovedFromHome[WavesTrans$H3MA6 >= 2 & WavesTrans$H3MA6 <= 15] <- 1


#### Mother / Father or Mother / Father Figure ever incarcerated

sleCodes$ParentJail[Waves$H3CJ160 == "."] <- "."
sleCodes$ParentJail[Waves$H3CJ160 == "No" | Waves$H3CJ160 == "Not applicable" | 
                    Waves$H4WP9 == "No" | Waves$H4WP3 == "No" |
                    Waves$H4WP16 == "No" | Waves$H4WP16 == "Legitimate skip" |  
                    Waves$H4WP30 == "No" | Waves$H4WP30 == "Legitimate skip"] <- 0
sleCodes$ParentJail[Waves$H3CJ160 == "Yes" | Waves$H4WP9 == "Yes" | Waves$H4WP3 == "Yes" |
                    Waves$H4WP16 == "Yes" |  Waves$H4WP30 == "Yes"] <- 1

#### Indicator for if Mother / Father or Mother / Father Figure incarcerated 
### (first time) at 18 years of agre or before. 


sleCodes$ParentJail18[Waves$H4WP11 > 18 | Waves$H4WP11 == 97 | 
                      Waves$H4WP5 > 18 | Waves$H4WP5 == 97 |
                      Waves$H4WP32 > 18 | Waves$H4WP32 == 97 |
                      Waves$H4WP19 > 18 | Waves$H4WP19 == 97] <- 0
sleCodes$ParentJail18[Waves$H4WP11 <= 18 | Waves$H4WP5 <= 18 |
                      Waves$H4WP32 <= 18 | Waves$H4WP19 <= 18] <- 1


### Household Substance abuse
# May be possible to scrape it out of parent survey, but don't really see great
# indicators. Easy availablity of illegal drugs in the home seems most resaonable.

sleCodes$SubstanceAbuseHome[Waves$H1TO52 == "No" | Waves$H1TO52 == "Not applicable" | 
                            Waves$H2TO68 == "No"] <- 0
sleCodes$SubstanceAbuseHome[Waves$H1TO52 == "Yes" | Waves$H2TO68 == "Yes"] <- 1


### Parental mental or physically disabled (from Waves I $ II)
# Could additionally use indicator for if child had mental disability from III & IV
# http://www.cpc.unc.edu/projects/addhealth/documentation/ace/tool/variablecollection?VariableCollectionId=1438

sleCodes$ParentalDisability[Waves$H1RF10 == "No" | Waves$H1RF10 == "Legitimate skip (no DAD)" |
                            Waves$H2RF10 == 0 | Waves$H2RF10 == 7 |
                            Waves$H1RM10 == "Yes" | Waves$H1RM10 == "Legitimate skip (no MOM)" |
                            Waves$H2RM10 == 0 | Waves$H2RM10 == 7] <- 0
sleCodes$ParentalDisability[Waves$H1RF10 == "Yes" | Waves$H2RF10 == "Yes" |
                            Waves$H1RM10 == "Yes" | Waves$H1RM10 == "Yes"] <- 1




write.csv(sleCodes, "sleCodes.csv")