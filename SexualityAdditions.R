# Wave I Nonromantic partner

# Wave I In-Home Interview	
# What is {INITIALS}'s sex?
# Male Female Refused
table(Waves$H1NR15_1) 
table(Waves$H1NR15_2) 
table(Waves$H1NR15_3)

# Wave I In-Home Interview
# Have you had sexual intercourse with {INITIALS}?
# No Yes Refused *has missing values
table(Waves$H1NR22_1) 
table(Waves$H1NR22_2) 
table(Waves$H1NR22_3)

# Tables for sex with NR partner

table(sexuality$sexuality_change == 0 & 
    ((Waves$H1NR15_1 == "Male" & Waves$H1NR22_1 == "Yes") |
     (Waves$H1NR15_2 == "Male" & Waves$H1NR22_2 == "Yes") |
     (Waves$H1NR15_3 == "Male" & Waves$H1NR22_3 == "Yes")))


table(sexuality$sexuality_change == 2 & 
    ((Waves$H1NR15_1 == "Female" & Waves$H1NR22_1 == "Yes") 
     (Waves$H1NR15_2 == "Female" & Waves$H1NR22_2 == "Yes") |
     (Waves$H1NR15_3 == "Female" & Waves$H1NR22_3 == "Yes")))


# Wave I incorrect filled section (RX) nonromantic partner data
# (Filled out section 25 instead of 26 for NR partner. Sepearate data created (RX))

# Wave I In-Home Interview	
# What is {INITIALS}'s sex?
# Male Female Refused 
table(Waves$H1RX20_1) 
table(Waves$H1RX20_2) 
table(Waves$H1RX20_3)

# Wave I In-Home Interview	
# Go through the deck of cards and reject any cards that describe things that have 
# NOT happened in your romantic relationship with {INITIALS}: You had sexual intercourse.
# 0 Rejected (no sex)  1 Card kept (had sex)
table(Waves$H1RX21O1) 
table(Waves$H1RX21O2) 
table(Waves$H1RX21O3)

# Wave I In-Home Interview	
# [If R is male:] When you had sexual intercourse with {INITIALS} did you insert 
# your penis into her vagina? [If R is female:] When you had sexual intercourse 
# with {INITIALS} did he insert his penis into your vagina?
# No Yes Refused  
table(Waves$H1RX24A1) 
table(Waves$H1RX24A2) 
table(Waves$H1RX24A3)

# Using both indicators for intercourse captures most information

table(Waves$H1RX24A1 == "Yes" | Waves$H1RX21O1 == "Card kept") 
table(Waves$H1RX24A2 == "Yes" | Waves$H1RX21O2 == "Card kept") 
table(Waves$H1RX24A3 == "Yes" | Waves$H1RX21O3 == "Card kept")

# Tables for sex with NR partner (RX data)

table(sexuality$sexuality_change == 0 & 
    ((Waves$H1RX20_1 == "Male" & 
     (Waves$H1RX21O1 == "Card kept" | Waves$H1RX24A1 == "Yes")) | 
     (Waves$H1RX20_2 == "Male" & 
     (Waves$H1RX21O2 == "Card kept" | Waves$H1RX24A2 == "Yes")) |
     (Waves$H1RX20_1 == "Male" & 
     (Waves$H1RX21O3 == "Card kept" | Waves$H1RX24A3 == "Yes"))))


table(sexuality$sexuality_change == 2 & 
   ((Waves$H1RX20_1 == "Female" & 
    (Waves$H1RX21O1 == "Card kept" | Waves$H1RX24A1 == "Yes")) | 
    (Waves$H1RX20_2 == "Female" & 
    (Waves$H1RX21O2 == "Card kept" | Waves$H1RX24A2 == "Yes")) |
    (Waves$H1RX20_1 == "Female" & 
    (Waves$H1RX21O3 == "Card kept" | Waves$H1RX24A3 == "Yes"))))


# Wave I sex of additional non-romantic SEXUAL partners

# Wave I In-Home Interview
# What is that person's sex?

table(Waves$H1NR47 == "Male")
table(Waves$H1NR47 == "Female")


# Wave II Nonromantic partner

# Wave II In-Home Interview	
# What is {INITIALS}'s sex?
# 1 Male  2 Female  6 Refused
table(Waves$H2NR24_1) 
table(Waves$H2NR24_2) 
table(Waves$H2NR24_3)

# Wave II In-Home Interview	
# Have you had sexual intercourse with {INITIALS}?
# 0 No  1 Yes  6 Refused
table(Waves$H2NR41_1) 
table(Waves$H2NR41_2) 
table(Waves$H2NR41_3)

# Tables for sex with NR partner

table(sexuality$sexuality_change == 0 & 
    ((Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1) |
     (Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1) |
     (Waves$H2NR24_1 == 1 & Waves$H2NR41_1 == 1)))


table(sexuality$sexuality_change == 0 & 
    ((Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1) |
     (Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1) |
     (Waves$H2NR24_1 == 2 & Waves$H2NR41_1 == 1)))


# Wave II incorrect filled section (RX) nonromantic partner data
# (Filled out section 25 instead of 26 for NR partner. Sepearate data created (RX))
# Because worong section was filled out, there are 3 indicators for sex with
# NR partner asked differently and they do not sum. Needs deeper dive.

# Wave II In-Home Interview	
# What is {INITIALS}'s sex?
# 1 Male  2 Female  6 Refused
table(Waves$H2RX32_1) 
table(Waves$H2RX32_2) 
table(Waves$H2RX32_3)

## Below are 3 ways of asking if they had sex, and they don't sum
## Further, the question with most responces makes it hard to catch inconsistency

# Wave II In-Home Interview	
# Go through the deck of cards and reject any cards that describe things that have 
# NOT happened in your romantic relationship with {INITIALS}: You had sexual intercourse.
# 0 Rejected (no sex)  1 Card kept (had sex)
table(Waves$H2RX33M1) 
table(Waves$H2RX33M2) 
table(Waves$H2RX33M3)

# Wave II In-Home Interview	
# Have you had sexual intercourse with {INITIALS}?
# 0 No  1 Yes  6 Refused 
table(Waves$H2RX35_1) 
table(Waves$H2RX35_2) 
table(Waves$H2RX35_3)

# Wave II In-Home Interview	
# [If R is male:] When you had sexual intercourse with {INITIALS} did you insert 
# your penis into her vagina? [If R is female:] When you had sexual intercourse 
# with {INITIALS} did he insert his penis into your vagina?
table(Waves$H2RX36_1) 
table(Waves$H2RX36_2) 
table(Waves$H2RX36_3)

# Using an OR bool produces sums in close range of highest freq of 3 indicators
# Using all 3 caputres the most information with highest accuracy
# Actually, 2nd add no information, 1st and 3rd yield same sum as full withoth 2nd.
# meaning the most direct question is least informative (though using 1st and 2nd
# sums to only 1 less in all three cases). Just using all 3 for completeness.

table(Waves$H2RX33M1 == 1 | Waves$H2RX35_1 == 1 | Waves$H2RX36_1 == 1)
table(Waves$H2RX33M2 == 1 | Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1)
table(Waves$H2RX33M3 == 1 | Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1)

# Tables for sex with NR partner (RX data)

table(sexuality$sexuality_change == 0 & 
     ((Waves$H2RX32_1 == 1 & (Waves$H2RX33M1 == 1 |
      Waves$H2RX35_1 == 1 | Waves$H2RX36_1 == 1)) |
     (Waves$H2RX32_2 == 1 & (Waves$H2RX33M2 == 1 |
      Waves$H2RX35_2 == 1 | Waves$H2RX36_2 == 1)) |
     (Waves$H2RX32_3 == 1 & (Waves$H2RX33M3 == 1 |
      Waves$H2RX35_3 == 1 | Waves$H2RX36_3 == 1))))

table(sexuality$sexuality_change == 2 & 
     ((Waves$H2RX32_1 == 2 & (Waves$H2RX33M1 == 1 |
      Waves$H2RX35_1 == 2 | Waves$H2RX36_1 == 1)) |
     (Waves$H2RX32_2 == 2 & (Waves$H2RX33M2 == 1 |
      Waves$H2RX35_2 == 2 | Waves$H2RX36_2 == 1)) |
     (Waves$H2RX32_3 == 2 & (Waves$H2RX33M3 == 1 |
      Waves$H2RX35_3 == 2 | Waves$H2RX36_3 == 1))))


# Wave II sex of additional non-romantic SEXUAL partners

# Wave II In-Home Interview
# What is that person's sex?

table(Waves$H2NR82 == 1)
table(Waves$H2NR82 == 2)


# Wave III indicator asking if ever had sex with partner
# Currently not used will require reworking data and sexuality script 

# Wave III In-Home Interview
# Have you had sexual relations with {INITIALS}?
H3TR8

# One simple possible data mangement solution is to add in without changing methos
# Just add in H3TR8 and still cast DF based on H3TR3 and RRELNO leaving duplicated 
# rows based on H3TR8 and then delete rows != Yes, had sex with partner. 
# Eliminates need to include H3TR8 in boolean tests to generate codes

WaveIII_rels <- read.dta(files[4])

WaveIII_rels <- WaveIII_rels[c("AID", "RRELNO", "H3TR3", "H3TR8")]
WaveIII_rels <- spread(WaveIII_rels, RRELNO, H3TR3, sep = "_")
WaveIII_rels <- WaveIII_rels[WaveIII_rels$H3TR8 == "Yes, we have had sexual relations", ]
WaveIII_rels <- WaveIII_rels[-2]



# Wave IV - No equivilent asking about sex with partners


  

### Need looking into ###

# Wave III In-Home Interview	
# Please choose the description that best fits how you think about yourself
table(Waves$H3SE13)

# Wave IV In-Home Interview	
# Please choose the description that best fits how you think about yourself
table(Waves$H4SE31)



# Reported change BIO_SEX 

# 20 male to female changes. Wave II code book sates there were 7 BIO_SEX
# miscodes in Wave I and that BIO_SEX2 in Wave II reflects correct sex.
# All 20 cases switched to Female in Wave II. Only 3 case reported male again
# after switching. All 3 cases only reported female once. All 3 possible miscodes
# Will probably need to swtich base bio sex case for codes to BIO_SEX2

switch <- which(sexuality$BIO_SEX == "Male" & (sexuality$BIO_SEX2 == 2 | 
                sexuality$BIO_SEX3 == "Female" | sexuality$BIO_SEX4 == "Female"))

maleSwitch <- sexuality[switch, 1:5]

# Female switch appears fine. Wave II code book sates there were 7 BIO_SEX
# miscodes in Wave I and that BIO_SEX2 in Wave II reflects correct sex.
# All 7 cases continued to report as Male for durration of Waves
# Will definitely swtich base bio sex case for codes to BIO_SEX2

switch <- which(sexuality$BIO_SEX == "Female" & (sexuality$BIO_SEX2 == 1 |
                sexuality$BIO_SEX3 == "Male" | sexuality$BIO_SEX4 == "Male"))

femaleSwitch <- sexuality[switch, 1:5]


# SEXFLG2 (incorrect biological sex for skips)
#Seems to indicated that preloaded sex code (from BIO_SEX) was wrong and 
# was corrected in BIO_SEX2 (7 respondents

        
# SAME SEX and OTHER SEX control codes listed in Wave I&II code books.
# Unclear what they are getting at  

