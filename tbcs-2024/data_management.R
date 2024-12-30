library(tidyverse)


######### PUBLIC (WEB ONLY) ######### 

# NOTE: Data management tasks performed prior to running this script:
#   - Validation and mapping of street intersection data
#   - "Other" ethnicity response categorization


# import data (with amended column names done in Excel)
public_web <- read.csv("tbcs-2024/survey_data/raw/public_web.csv")
public_web_coords <- read.csv("tbcs-2024/survey_data/raw/public_web_coords.csv")


######### * DATA CORRECTIONS ######### 

### Convert blank cells to NAs ###

public_web$OWNERSHIP_OTHER_TEXT <- ifelse(public_web$OWNERSHIP_OTHER_TEXT == "" | public_web$OWNERSHIP_OTHER_TEXT == " ", NA, public_web$OWNERSHIP_OTHER_TEXT)
public_web$YEARS_RESIDENT <- ifelse(public_web$YEARS_RESIDENT == "" | public_web$YEARS_RESIDENT == " ", NA, public_web$YEARS_RESIDENT)
public_web$YEARS_UNSURE <- ifelse(public_web$YEARS_UNSURE == "" | public_web$YEARS_UNSURE == " ", NA, public_web$YEARS_UNSURE)
public_web$BEHAVIOR_FERTLZ <- ifelse(public_web$BEHAVIOR_FERTLZ == "" | public_web$BEHAVIOR_FERTLZ == " ", NA, public_web$BEHAVIOR_FERTLZ)
public_web$BEHAVIOR_WATER <- ifelse(public_web$BEHAVIOR_WATER == "" | public_web$BEHAVIOR_WATER == " ", NA, public_web$BEHAVIOR_WATER)
public_web$BEHAVIOR_CLPPNG <- ifelse(public_web$BEHAVIOR_CLPPNG == "" | public_web$BEHAVIOR_CLPPNG == " ", NA, public_web$BEHAVIOR_CLPPNG)
public_web$BEHAVIOR_GRNINF <- ifelse(public_web$BEHAVIOR_GRNINF == "" | public_web$BEHAVIOR_GRNINF == " ", NA, public_web$BEHAVIOR_GRNINF)
public_web$NORM_FERTLZ <- ifelse(public_web$NORM_FERTLZ == "" | public_web$NORM_FERTLZ == " ", NA, public_web$NORM_FERTLZ)
public_web$NORM_WATER <- ifelse(public_web$NORM_WATER == "" | public_web$NORM_WATER == " ", NA, public_web$NORM_WATER)
public_web$NORM_CLPPNG <- ifelse(public_web$NORM_CLPPNG == "" | public_web$NORM_CLPPNG == " ", NA, public_web$NORM_CLPPNG)
public_web$NORM_GRNINF <- ifelse(public_web$NORM_GRNINF == "" | public_web$NORM_GRNINF == " ", NA, public_web$NORM_GRNINF)
public_web$FERTLZ_SEASON <- ifelse(public_web$FERTLZ_SEASON == "" | public_web$FERTLZ_SEASON == " ", NA, public_web$FERTLZ_SEASON)
public_web$FISH_LOCATN <- ifelse(public_web$FISH_LOCATN == "" | public_web$FISH_LOCATN == " ", NA, public_web$FISH_LOCATN)
public_web$FISH_REASON <- ifelse(public_web$FISH_REASON == "" | public_web$FISH_REASON == " ", NA, public_web$FISH_REASON)
public_web$ECOANXIETY_1 <- ifelse(public_web$ECOANXIETY_1 == "" | public_web$ECOANXIETY_1 == " ", NA, public_web$ECOANXIETY_1)
public_web$ECOANXIETY_2 <- ifelse(public_web$ECOANXIETY_2 == "" | public_web$ECOANXIETY_2 == " ", NA, public_web$ECOANXIETY_2)
public_web$ECOANXIETY_3 <- ifelse(public_web$ECOANXIETY_3 == "" | public_web$ECOANXIETY_3 == " ", NA, public_web$ECOANXIETY_3)
public_web$ECOANXIETY_4 <- ifelse(public_web$ECOANXIETY_4 == "" | public_web$ECOANXIETY_4 == " ", NA, public_web$ECOANXIETY_4)
public_web$ECOANXIETY_5 <- ifelse(public_web$ECOANXIETY_5 == "" | public_web$ECOANXIETY_5 == " ", NA, public_web$ECOANXIETY_5)
public_web$ECOANXIETY_6 <- ifelse(public_web$ECOANXIETY_6 == "" | public_web$ECOANXIETY_6 == " ", NA, public_web$ECOANXIETY_6)
public_web$ECOANXIETY_7 <- ifelse(public_web$ECOANXIETY_7 == "" | public_web$ECOANXIETY_7 == " ", NA, public_web$ECOANXIETY_7)
public_web$GENDER_OTHER_TEXT <- ifelse(public_web$GENDER_OTHER_TEXT == "" | public_web$GENDER_OTHER_TEXT == " ", NA, public_web$GENDER_OTHER_TEXT)
public_web$ETHNICITY_OTHER_TEXT <- ifelse(public_web$ETHNICITY_OTHER_TEXT == "" | public_web$ETHNICITY_OTHER_TEXT == " ", NA, public_web$ETHNICITY_OTHER_TEXT)

### Combine ZIPCODE and YEARS fields ###

public_web$ZIPCODE <- ifelse(is.na(public_web$ZIPCODE_RESIDENT), public_web$ZIPCODE_UNSURE, public_web$ZIPCODE_RESIDENT)
public_web$YEARS <- ifelse(is.na(public_web$YEARS_RESIDENT), public_web$YEARS_UNSURE, public_web$YEARS_RESIDENT)


### Set Other classifications ###
# Review and reclassification of all "Other" text entries for the following variables:
#     - OWNERSHIP
#     - GENDER

# GENDER
unique(public_web$GENDER_OTHER_TEXT)
public_web$GENDER <- ifelse(public_web$GENDER == "Other description:", "Non-binary", public_web$GENDER)

# OWNERSHIP
unique(public_web$OWNERSHIP_OTHER_TEXT)

familyowned <- c("Live with family","Live with relatives","Live with parents ","live with family",
                 "Parents","Stay with family ","Live with owner","Parents own","Live with parents",
                 "Parents signed financing for my RV","Mother owns it","It's my grandmother's house ",
                 "Family owned I camp here ")
lotrental <- c("rent lot","pay lot rent","Family member rents","own home pay lot rent")
public_web$OWNERSHIP <- ifelse(public_web$OWNERSHIP_OTHER_TEXT %in% familyowned, "Own",
                               ifelse(public_web$OWNERSHIP_OTHER_TEXT %in% lotrental, "Rent",
                                      ifelse(public_web$OWNERSHIP == "Other arrangement:", "Other", public_web$OWNERSHIP)))


### Reverse coding ###

public_web$JUSTICE_DIST <- ifelse(public_web$JUSTICE_DIST.R == "Not true at all", "Completely true",
                                  ifelse(public_web$JUSTICE_DIST.R == "A little true", "Very true",
                                         ifelse(public_web$JUSTICE_DIST.R == "Moderately true", "Moderately true",
                                                ifelse(public_web$JUSTICE_DIST.R == "Very true", "A little true",
                                                       ifelse(public_web$JUSTICE_DIST.R == "Completely true", "Not true at all", NA)))))
public_web$JUSTICE_PRO <- ifelse(public_web$JUSTICE_PRO.R == "Not true at all", "Completely true",
                                 ifelse(public_web$JUSTICE_PRO.R == "A little true", "Very true",
                                        ifelse(public_web$JUSTICE_PRO.R == "Moderately true", "Moderately true",
                                               ifelse(public_web$JUSTICE_PRO.R == "Very true", "A little true",
                                                      ifelse(public_web$JUSTICE_PRO.R == "Completely true", "Not true at all", NA)))))
public_web$JUSTICE_REC <- ifelse(public_web$JUSTICE_REC.R == "Not true at all", "Completely true",
                                 ifelse(public_web$JUSTICE_REC.R == "A little true", "Very true",
                                        ifelse(public_web$JUSTICE_REC.R == "Moderately true", "Moderately true",
                                               ifelse(public_web$JUSTICE_REC.R == "Very true", "A little true",
                                                      ifelse(public_web$JUSTICE_REC.R == "Completely true", "Not true at all", NA)))))
public_web$JUSTICE_AGN <- ifelse(public_web$JUSTICE_AGN.R == "Not true at all", "Completely true",
                                 ifelse(public_web$JUSTICE_AGN.R == "A little true", "Very true",
                                        ifelse(public_web$JUSTICE_AGN.R == "Moderately true", "Moderately true",
                                               ifelse(public_web$JUSTICE_AGN.R == "Very true", "A little true",
                                                      ifelse(public_web$JUSTICE_AGN.R == "Completely true", "Not true at all", NA)))))
public_web$ATTITUDE_BAYLNK <- ifelse(public_web$ATTITUDE_BAYLNK.R == "Not true at all", "Completely true",
                                     ifelse(public_web$ATTITUDE_BAYLNK.R == "A little true", "Very true",
                                            ifelse(public_web$ATTITUDE_BAYLNK.R == "Moderately true", "Moderately true",
                                                   ifelse(public_web$ATTITUDE_BAYLNK.R == "Very true", "A little true",
                                                          ifelse(public_web$ATTITUDE_BAYLNK.R == "Completely true", "Not true at all", NA)))))
public_web$HOPEPATH_CANFIX <- ifelse(public_web$HOPEPATH_CANFIX.R == "Not true at all", "Completely true",
                                     ifelse(public_web$HOPEPATH_CANFIX.R == "A little true", "Very true",
                                            ifelse(public_web$HOPEPATH_CANFIX.R == "Moderately true", "Moderately true",
                                                   ifelse(public_web$HOPEPATH_CANFIX.R == "Very true", "A little true",
                                                          ifelse(public_web$HOPEPATH_CANFIX.R == "Completely true", "Not true at all", NA)))))
public_web$HOPEPATH_ENOUGH <- ifelse(public_web$HOPEPATH_ENOUGH.R == "Not true at all", "Completely true",
                                     ifelse(public_web$HOPEPATH_ENOUGH.R == "A little true", "Very true",
                                            ifelse(public_web$HOPEPATH_ENOUGH.R == "Moderately true", "Moderately true",
                                                   ifelse(public_web$HOPEPATH_ENOUGH.R == "Very true", "A little true",
                                                          ifelse(public_web$HOPEPATH_ENOUGH.R == "Completely true", "Not true at all", NA)))))
public_web$ATTITUDE_OUTWEL <- ifelse(public_web$ATTITUDE_OUTWEL.R == "Not true at all", "Completely true",
                                     ifelse(public_web$ATTITUDE_OUTWEL.R == "A little true", "Very true",
                                            ifelse(public_web$ATTITUDE_OUTWEL.R == "Moderately true", "Moderately true",
                                                   ifelse(public_web$ATTITUDE_OUTWEL.R == "Very true", "A little true",
                                                          ifelse(public_web$ATTITUDE_OUTWEL.R == "Completely true", "Not true at all", NA)))))
public_web$ATTITUDE_SWMSAF <- ifelse(public_web$ATTITUDE_SWMSAF.R == "Not true at all", "Completely true",
                                     ifelse(public_web$ATTITUDE_SWMSAF.R == "A little true", "Very true",
                                            ifelse(public_web$ATTITUDE_SWMSAF.R == "Moderately true", "Moderately true",
                                                   ifelse(public_web$ATTITUDE_SWMSAF.R == "Very true", "A little true",
                                                          ifelse(public_web$ATTITUDE_SWMSAF.R == "Completely true", "Not true at all", NA)))))
public_web$MHI_A1 <- ifelse(public_web$MHI_A1.R == "None of the time", "All of the time",
                            ifelse(public_web$MHI_A1.R == "A little of the time", "Most of the time",
                                   ifelse(public_web$MHI_A1.R == "Some of the time", "A good bit of the time",
                                          ifelse(public_web$MHI_A1.R == "A good bit of the time", "Some of the time",
                                                 ifelse(public_web$MHI_A1.R == "Most of the time", "A little of the time",
                                                        ifelse(public_web$MHI_A1.R == "All of the time", "None of the time", NA))))))
public_web$MHI_D2 <- ifelse(public_web$MHI_D2.R == "None of the time", "All of the time",
                            ifelse(public_web$MHI_D2.R == "A little of the time", "Most of the time",
                                   ifelse(public_web$MHI_D2.R == "Some of the time", "A good bit of the time",
                                          ifelse(public_web$MHI_D2.R == "A good bit of the time", "Some of the time",
                                                 ifelse(public_web$MHI_D2.R == "Most of the time", "A little of the time",
                                                        ifelse(public_web$MHI_D2.R == "All of the time", "None of the time", NA))))))
public_web$MHI_D3 <- ifelse(public_web$MHI_D3.R == "None of the time", "All of the time",
                            ifelse(public_web$MHI_D3.R == "A little of the time", "Most of the time",
                                   ifelse(public_web$MHI_D3.R == "Some of the time", "A good bit of the time",
                                          ifelse(public_web$MHI_D3.R == "A good bit of the time", "Some of the time",
                                                 ifelse(public_web$MHI_D3.R == "Most of the time", "A little of the time",
                                                        ifelse(public_web$MHI_D3.R == "All of the time", "None of the time", NA))))))


### Knowledge scoring ###

public_web$KNOWLEDGE_WQTRND_Points <- ifelse(public_web$KNOWLEDGE_WQTRND == "Improved", 1, 0)
public_web$KNOWLEDGE_NUTRNT_Points <- ifelse(public_web$KNOWLEDGE_NUTRNT == "Nitrogen", 1, 0)
public_web$KNOWLEDGE_SOURCE_Points <- ifelse(public_web$KNOWLEDGE_SOURCE == "Stormwater runoff", 1, 0)
public_web$KNOWLEDGE_FRTSSN_Points <- ifelse(public_web$KNOWLEDGE_FRTSSN == "Summer (June - September)", 1, 0)
public_web$KNOWLEDGE_SGLOSS_Points <- ifelse(public_web$KNOWLEDGE_SGLOSS == "Boat propeller blades,Nutrient pollution", 1, 
                                             ifelse(grepl("propeller", public_web$KNOWLEDGE_SGLOSS) | grepl("Nutrient", public_web$KNOWLEDGE_SGLOSS), 0.5, 0))
public_web$KNOWLEDGE_WQTRND_Score <- public_web$KNOWLEDGE_WQTRND_Points*(public_web$KNOWLEDGE_WQTRND_C/10)
public_web$KNOWLEDGE_NUTRNT_Score <- public_web$KNOWLEDGE_NUTRNT_Points*(public_web$KNOWLEDGE_NUTRNT_C/10)
public_web$KNOWLEDGE_SOURCE_Score <- public_web$KNOWLEDGE_SOURCE_Points*(public_web$KNOWLEDGE_SOURCE_C/10)
public_web$KNOWLEDGE_FRTSSN_Score <- public_web$KNOWLEDGE_FRTSSN_Points*(public_web$KNOWLEDGE_FRTSSN_C/10)
public_web$KNOWLEDGE_SGLOSS_Score <- public_web$KNOWLEDGE_SGLOSS_Points*(public_web$KNOWLEDGE_SGLOSS_C/10)
public_web$KNOWLEDGE_POINTS <- public_web$KNOWLEDGE_WQTRND_Points + public_web$KNOWLEDGE_NUTRNT_Points + public_web$KNOWLEDGE_SOURCE_Points + public_web$KNOWLEDGE_FRTSSN_Points + public_web$KNOWLEDGE_SGLOSS_Points
public_web$KNOWLEDGE_SCORE <- public_web$KNOWLEDGE_WQTRND_Score + public_web$KNOWLEDGE_NUTRNT_Score + public_web$KNOWLEDGE_SOURCE_Score + public_web$KNOWLEDGE_FRTSSN_Score + public_web$KNOWLEDGE_SGLOSS_Score
public_web$KNOWLEDGE_SCORE_Pct <- (public_web$KNOWLEDGE_SCORE/5)*100


### Break up multiple selections into discrete answers ###

public_web$FERTLZ_SPRING <- ifelse(is.na(public_web$FERTLZ_SEASON), NA, ifelse(grepl("Spring", public_web$FERTLZ_SEASON), 1, 0))
public_web$FERTLZ_SUMMER <- ifelse(is.na(public_web$FERTLZ_SEASON), NA, ifelse(grepl("Summer", public_web$FERTLZ_SEASON), 1, 0))
public_web$FERTLZ_FALL <- ifelse(is.na(public_web$FERTLZ_SEASON), NA, ifelse(grepl("Fall", public_web$FERTLZ_SEASON), 1, 0))
public_web$FERTLZ_WINTER <- ifelse(is.na(public_web$FERTLZ_SEASON), NA, ifelse(grepl("Winter", public_web$FERTLZ_SEASON), 1, 0))
public_web$FERTLZ_SEASON_N <- public_web$FERTLZ_SPRING + public_web$FERTLZ_SUMMER + public_web$FERTLZ_FALL + public_web$FERTLZ_WINTER

public_web$FISH_LOC_SHORE <- ifelse(is.na(public_web$FISH_LOCATN), NA, ifelse(grepl("shoreline", public_web$FISH_LOCATN), 1, 0))
public_web$FISH_LOC_PIER <- ifelse(is.na(public_web$FISH_LOCATN), NA, ifelse(grepl("pier", public_web$FISH_LOCATN), 1, 0))
public_web$FISH_LOC_WATER <- ifelse(is.na(public_web$FISH_LOCATN), NA, ifelse(grepl("boat", public_web$FISH_LOCATN), 1, 0))
public_web$FISH_RSN_SPORT <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("sport", public_web$FISH_REASON), 1, 0))
public_web$FISH_RSN_RELAX <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("relaxation", public_web$FISH_REASON), 1, 0))
public_web$FISH_RSN_FOOD <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("own", public_web$FISH_REASON), 1, 0))
public_web$FISH_RSN_SELL <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("sell", public_web$FISH_REASON), 1, 0))
public_web$FISH_RSN_WATCH <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("friends", public_web$FISH_REASON), 1, 0))
public_web$FISH_RSN_SOCIAL <- ifelse(is.na(public_web$FISH_REASON), NA, ifelse(grepl("anglers", public_web$FISH_REASON), 1, 0))
public_web$FISH_LOCATN_N <- public_web$FISH_LOC_SHORE + public_web$FISH_LOC_PIER + public_web$FISH_LOC_WATER 
public_web$FISH_REASON_N <- public_web$FISH_RSN_SPORT + public_web$FISH_RSN_RELAX + public_web$FISH_RSN_FOOD + public_web$FISH_RSN_SELL + public_web$FISH_RSN_WATCH + public_web$FISH_RSN_SOCIAL

public_web$INFO_TV <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Television", public_web$INFOSOURCES), 1, 0))
public_web$INFO_RADIO <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Radio", public_web$INFOSOURCES), 1, 0))
public_web$INFO_SOCIAL <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Social", public_web$INFOSOURCES), 1, 0))
public_web$INFO_PERIOD <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Newspapers", public_web$INFOSOURCES), 1, 0))
public_web$INFO_MAILE <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("email", public_web$INFOSOURCES), 1, 0))
public_web$INFO_PUBLIC <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Public", public_web$INFOSOURCES), 1, 0))
public_web$INFO_GOV <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("Government websites", public_web$INFOSOURCES), 1, 0))
public_web$INFO_NGO <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("NGO", public_web$INFOSOURCES), 1, 0))
public_web$INFO_WORD <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("mouth", public_web$INFOSOURCES), 1, 0))
public_web$INFO_OWN <- ifelse(is.na(public_web$INFOSOURCES), NA, ifelse(grepl("observations", public_web$INFOSOURCES), 1, 0))
public_web$INFOSOURCES_N <- public_web$INFO_TV + public_web$INFO_RADIO + public_web$INFO_SOCIAL + public_web$INFO_PERIOD + public_web$INFO_MAILE + public_web$INFO_PUBLIC + public_web$INFO_GOV + public_web$INFO_NGO + public_web$INFO_PERIOD + public_web$INFO_WORD + public_web$INFO_OWN

public_web$RACE_NATIVE <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Alaska", public_web$ETHNICITY), 1, 0))
public_web$RACE_ASIAN <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Asian", public_web$ETHNICITY), 1, 0))
public_web$RACE_BLACK <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Black", public_web$ETHNICITY), 1, 0))
public_web$RACE_MIDEAST <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Middle", public_web$ETHNICITY), 1, 0))
public_web$RACE_PACIFIC <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Pacific", public_web$ETHNICITY), 1, 0))
public_web$RACE_WHITE <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("White", public_web$ETHNICITY), 1, 0))
public_web$RACE_OTHER <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Other", public_web$ETHNICITY), 1, 0))
public_web$RACE_HISPANIC <- ifelse(is.na(public_web$ETHNICITY), NA, ifelse(grepl("Hispanic", public_web$ETHNICITY), 1, 0))
public_web$RACE_N <- ifelse(is.na(public_web$ETHNICITY), NA, 
                            public_web$RACE_NATIVE + public_web$RACE_ASIAN + public_web$RACE_BLACK + public_web$RACE_MIDEAST + public_web$RACE_PACIFIC + public_web$RACE_WHITE + public_web$RACE_OTHER + public_web$RACE_HISPANIC)
public_web$RACE <- ifelse(is.na(public_web$ETHNICITY), NA,
                          ifelse(public_web$RACE_N == 0, "Other",
                                 ifelse(public_web$RACE_N > 1, "Multiple",
                                        ifelse(public_web$RACE_NATIVE == 1 & public_web$RACE_N == 1, "Other",
                                               ifelse(public_web$RACE_ASIAN == 1 & public_web$RACE_N == 1, "Asian",
                                                      ifelse(public_web$RACE_BLACK == 1 & public_web$RACE_N == 1, "Black",
                                                             ifelse(public_web$RACE_MIDEAST == 1 & public_web$RACE_N == 1, "Other",
                                                                    ifelse(public_web$RACE_PACIFIC == 1 & public_web$RACE_N == 1, "Other",
                                                                           ifelse(public_web$RACE_WHITE == 1 & public_web$RACE_N == 1, "White",
                                                                                  ifelse(public_web$RACE_OTHER == 1 & public_web$RACE_N == 1, "Other",
                                                                                         ifelse(public_web$RACE_HISPANIC == 1 & public_web$RACE_N == 1, "Hispanic",NA)))))))))))
public_web$EMPLOY_FULL <- ifelse(is.na(public_web$EMPLOYMENT), NA, ifelse(grepl("Full", public_web$EMPLOYMENT), 1, 0))
public_web$EMPLOY_PART <- ifelse(is.na(public_web$EMPLOYMENT), NA, ifelse(grepl("Part", public_web$EMPLOYMENT), 1, 0))
public_web$EMPLOY_NONE <- ifelse(is.na(public_web$EMPLOYMENT), NA, ifelse(grepl("Unemployed", public_web$EMPLOYMENT), 1, 0))
public_web$EMPLOYED <- ifelse(public_web$EMPLOY_FULL == 1, "Full-time", 
                              ifelse(public_web$EMPLOY_PART == 1, "Part-time", "Unemployed")) 
public_web$EMPLOYED_STUDENT <- ifelse(is.na(public_web$EMPLOYMENT), NA, ifelse(grepl("Student", public_web$EMPLOYMENT), 1, 0))
public_web$EMPLOYED_RETIRED <- ifelse(is.na(public_web$EMPLOYMENT), NA, ifelse(grepl("Retired", public_web$EMPLOYMENT), 1, 0))


######### * EXCLUSIONS ######### 

# flag responses during the preview/testing phase
public_web$QC_PREVIEW <- ifelse(public_web$Status == "Survey Preview", "Remove", "OK")

# flag responses that spent less than 5 minutes (300 seconds) on the survey
public_web$QC_DURATION <- ifelse(public_web$Duration..in.seconds. < 300, "Remove", "OK")

# flag responses that did not reach the end of the survey
public_web$QC_FINISHED <- ifelse(public_web$Finished == "False", "Remove", "OK")

# flag responses that did not pass attention checks
public_web$QC_ATTENTION <- ifelse(public_web$ATTNCHECK1 != "A little true" | public_web$ATTNCHECK2 != "Very true", "Remove", "OK")

# flag fraudulent responses
public_web$QC_FRAUDULENT <- ifelse(public_web$Q_RecaptchaScore < 0.5, "Caution", "OK")

# flag potential duplicate responses
public_web$QC_DUPLICATE <- ifelse(public_web$Q_RelevantIDDuplicateScore >= 75, "Remove", "OK")

# flag responses exhibiting response biases
## check if respondents are (too) frequently selecting the same answer (12 question blocks to check)
#### priorities = 3 sections (nataccess/costliv/rodbrg) 
#### attitudes = 2 sections (justdist/bayimp) 
#### behaviors = 3 sections (cmphik/exrcso/histsit) 
#### norms = 1 sections
#### distress = 3 sections (pident/hopeknow/degraded) 
#### safety = 1 sections (safout)


public_web$QC_BIASCHECK_P1 <- ifelse(public_web$PRIORITY_ACSNAT == public_web$PRIORITY_ACSEDU & 
                                       public_web$PRIORITY_ACSNAT == public_web$PRIORITY_ACSHLT & 
                                       public_web$PRIORITY_ACSNAT == public_web$PRIORITY_JOBINC & 
                                       public_web$PRIORITY_ACSNAT == public_web$PRIORITY_WAGINC & 
                                       public_web$PRIORITY_ACSNAT == public_web$PRIORITY_TRASH, "Caution", "OK")
public_web$QC_BIASCHECK_P2 <- ifelse(public_web$PRIORITY_CSTLIV == public_web$PRIORITY_PEDCYC & 
                                       public_web$PRIORITY_CSTLIV == public_web$PRIORITY_HOUSNG & 
                                       public_web$PRIORITY_CSTLIV == public_web$PRIORITY_JOBDIV & 
                                       public_web$PRIORITY_CSTLIV == public_web$PRIORITY_AIRQTY & 
                                       public_web$PRIORITY_CSTLIV == public_web$PRIORITY_WTRQTY, "Caution", "OK")
public_web$QC_BIASCHECK_P3 <- ifelse(public_web$PRIORITY_RODBRG == public_web$PRIORITY_UTILTY & 
                                       public_web$PRIORITY_RODBRG == public_web$PRIORITY_FLDPRT & 
                                       public_web$PRIORITY_RODBRG == public_web$PRIORITY_PUBTRN, "Caution", "OK")
public_web$QC_BIASCHECK_A1 <- ifelse(public_web$JUSTICE_DIST == public_web$JUSTICE_PRO & 
                                       public_web$JUSTICE_DIST == public_web$JUSTICE_REC & 
                                       public_web$JUSTICE_DIST == public_web$JUSTICE_AGN & 
                                       public_web$JUSTICE_DIST == public_web$ATTITUDE_BAYLNK.R, "Caution", "OK")
public_web$QC_BIASCHECK_A2 <- ifelse(public_web$ATTITUDE_BAYIMP == public_web$ATTITUDE_BIODIV & 
                                       public_web$ATTITUDE_BAYIMP == public_web$ATTITUDE_OPPSAT & 
                                       public_web$ATTITUDE_BAYIMP == public_web$ATTNCHECK1 & 
                                       public_web$ATTITUDE_BAYIMP == public_web$NR_IMPPRT, "Caution", "OK")
public_web$QC_BIASCHECK_B1 <- ifelse(public_web$ACTIVITY_CMPHIK == public_web$ACTIVITY_PADBOT & 
                                       public_web$ACTIVITY_CMPHIK == public_web$ACTIVITY_SWIMDV & 
                                       public_web$ACTIVITY_CMPHIK == public_web$ACTIVITY_BWATCH & 
                                       public_web$ACTIVITY_CMPHIK == public_web$ACTIVITY_GARDEN & 
                                       public_web$ACTIVITY_CMPHIK == public_web$ACTIVITY_FISHNG, "Caution", "OK")
public_web$QC_BIASCHECK_B2 <- ifelse(public_web$ACTIVITY_EXRCSO == public_web$ACTIVITY_RELAXO & 
                                       public_web$ACTIVITY_EXRCSO == public_web$ACTIVITY_PICNIC & 
                                       public_web$ACTIVITY_EXRCSO == public_web$ACTIVITY_PLYGND & 
                                       public_web$ACTIVITY_EXRCSO == public_web$ACTIVITY_ZOOAQU & 
                                       public_web$ACTIVITY_EXRCSO == public_web$ACTIVITY_LSNPRK, "Caution", "OK")
public_web$QC_BIASCHECK_B3 <- ifelse(public_web$ACTIVITY_HSTSIT == public_web$ACTIVITY_BEACH & 
                                       public_web$ACTIVITY_HSTSIT == public_web$ACTIVITY_PIER & 
                                       public_web$ACTIVITY_HSTSIT == public_web$BEHAVIOR_NODRIV & 
                                       public_web$ACTIVITY_HSTSIT == public_web$BEHAVIOR_RECYCL & 
                                       public_web$ACTIVITY_HSTSIT == public_web$BEHAVIOR_PLNTFF & 
                                       public_web$ACTIVITY_HSTSIT == public_web$BEHAVIOR_RESTOR, "Caution", "OK")
public_web$QC_BIASCHECK_N1 <- ifelse(public_web$NORM_NODRIV == public_web$NORM_RECYCL & 
                                       public_web$NORM_NODRIV == public_web$NORM_PLNTFF & 
                                       public_web$NORM_NODRIV == public_web$NORM_RESTOR, "Caution", "OK")
public_web$QC_BIASCHECK_D1 <- ifelse(public_web$PIDENTITY_IDENTY == public_web$SOLASTALGIA_BELONG & 
                                       public_web$PIDENTITY_IDENTY == public_web$SOLASTALGIA_LOSSES & 
                                       public_web$PIDENTITY_IDENTY == public_web$HOPEPATH_WAYFIX & 
                                       public_web$PIDENTITY_IDENTY == public_web$SOLASTALGIA_ASHAMD & 
                                       public_web$PIDENTITY_IDENTY == public_web$SOLASTALGIA_DISAPR, "Caution", "OK")
public_web$QC_BIASCHECK_D2 <- ifelse(public_web$HOPEPATH_KNOWLG == public_web$PIDENTITY_SPCIAL & 
                                       public_web$HOPEPATH_KNOWLG == public_web$SOLASTALGIA_UQLOSS & 
                                       public_web$HOPEPATH_KNOWLG == public_web$SOLASTALGIA_PEACEQ & 
                                       public_web$HOPEPATH_KNOWLG == public_web$ATTNCHECK2 & 
                                       public_web$HOPEPATH_KNOWLG == public_web$HOPEPATH_CANFIX.R, "Caution", "OK")
public_web$QC_BIASCHECK_D3 <- ifelse(public_web$SOLASTALGIA_DEGRAD == public_web$ACTIVITY_BEACH & 
                                       public_web$SOLASTALGIA_DEGRAD == public_web$HOPEPATH_RESOLV & 
                                       public_web$SOLASTALGIA_DEGRAD == public_web$SOLASTALGIA_LFSTYL & 
                                       public_web$SOLASTALGIA_DEGRAD == public_web$SOLASTALGIA_FLEAVE & 
                                       public_web$SOLASTALGIA_DEGRAD == public_web$HOPEPATH_ENOUGH.R, "Caution", "OK")
public_web$QC_BIASCHECK_S1 <- ifelse(public_web$ATTITUDE_SAFOUT == public_web$ATTITUDE_OUTWEL.R & 
                                       public_web$ATTITUDE_SAFOUT == public_web$ATTITUDE_SWMSAF.R & 
                                       public_web$ATTITUDE_SAFOUT == public_web$ATTITUDE_CHNGFW, "Caution", "OK")

public_web <- public_web %>%
  mutate(QC_BIAS_CAUTIONS = str_count(QC_BIASCHECK_P1, "Caution") + str_count(QC_BIASCHECK_P2, "Caution") + 
           str_count(QC_BIASCHECK_P3, "Caution") + str_count(QC_BIASCHECK_A1, "Caution") + str_count(QC_BIASCHECK_A2, "Caution") + 
           str_count(QC_BIASCHECK_B1, "Caution") + str_count(QC_BIASCHECK_B2, "Caution") + str_count(QC_BIASCHECK_B3, "Caution") + 
           str_count(QC_BIASCHECK_N1, "Caution") + str_count(QC_BIASCHECK_D1, "Caution") + str_count(QC_BIASCHECK_D2, "Caution") + 
           str_count(QC_BIASCHECK_D3, "Caution") + str_count(QC_BIASCHECK_S1, "Caution"))

# INCLUSION DETERMINATION
public_web$INCLUDE <- ifelse(public_web$QC_PREVIEW == "Remove" | public_web$QC_DURATION == "Remove" | 
                               public_web$QC_FINISHED == "Remove" | public_web$QC_ATTENTION == "Remove" | 
                               public_web$QC_DUPLICATE == "Remove", "No", "Yes")
public_web$INCLUDE <- ifelse(public_web$INCLUDE == "Yes" & public_web$QC_BIAS_CAUTIONS > 4, "No", 
                             ifelse(public_web$INCLUDE == "Yes" & public_web$QC_FRAUDULENT == "Caution", "Confirm Fraudulent", public_web$INCLUDE))


public_web %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# Confirm Fraudulent  57
# No                  47
# Yes                 961
# <NA>                7



######### * QUANTITATIVE SCALING ######### 

# Separate dataset with quantitative values instead of text values
public_web_quant <- public_web

### PRIORITY ###

public_web_quant$PRIORITY_ACSNAT <- ifelse(public_web_quant$PRIORITY_ACSNAT == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_ACSNAT == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_ACSNAT == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_ACSNAT == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_ACSNAT == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_ACSEDU <- ifelse(public_web_quant$PRIORITY_ACSEDU == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_ACSEDU == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_ACSEDU == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_ACSEDU == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_ACSEDU == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_ACSHLT <- ifelse(public_web_quant$PRIORITY_ACSHLT == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_ACSHLT == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_ACSHLT == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_ACSHLT == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_ACSHLT == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_JOBINC <- ifelse(public_web_quant$PRIORITY_JOBINC == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_JOBINC == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_JOBINC == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_JOBINC == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_JOBINC == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_WAGINC <- ifelse(public_web_quant$PRIORITY_WAGINC == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_WAGINC == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_WAGINC == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_WAGINC == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_WAGINC == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_TRASH <- ifelse(public_web_quant$PRIORITY_TRASH == "Not a priority", 0,
                                          ifelse(public_web_quant$PRIORITY_TRASH == "Low priority", 1,
                                                 ifelse(public_web_quant$PRIORITY_TRASH == "Moderate priority", 2,
                                                        ifelse(public_web_quant$PRIORITY_TRASH == "High priority", 3,
                                                               ifelse(public_web_quant$PRIORITY_TRASH == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_CSTLIV <- ifelse(public_web_quant$PRIORITY_CSTLIV == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_CSTLIV == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_CSTLIV == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_CSTLIV == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_CSTLIV == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_PEDCYC <- ifelse(public_web_quant$PRIORITY_PEDCYC == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_PEDCYC == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_PEDCYC == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_PEDCYC == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_PEDCYC == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_HOUSNG <- ifelse(public_web_quant$PRIORITY_HOUSNG == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_HOUSNG == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_HOUSNG == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_HOUSNG == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_HOUSNG == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_JOBDIV <- ifelse(public_web_quant$PRIORITY_JOBDIV == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_JOBDIV == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_JOBDIV == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_JOBDIV == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_JOBDIV == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_AIRQTY <- ifelse(public_web_quant$PRIORITY_AIRQTY == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_AIRQTY == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_AIRQTY == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_AIRQTY == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_AIRQTY == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_WTRQTY <- ifelse(public_web_quant$PRIORITY_WTRQTY == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_WTRQTY == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_WTRQTY == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_WTRQTY == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_WTRQTY == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_RODBRG <- ifelse(public_web_quant$PRIORITY_RODBRG == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_RODBRG == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_RODBRG == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_RODBRG == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_RODBRG == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_UTILTY <- ifelse(public_web_quant$PRIORITY_UTILTY == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_UTILTY == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_UTILTY == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_UTILTY == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_UTILTY == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_FLDPRT <- ifelse(public_web_quant$PRIORITY_FLDPRT == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_FLDPRT == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_FLDPRT == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_FLDPRT == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_FLDPRT == "Top priority", 4, NA)))))
public_web_quant$PRIORITY_PUBTRN <- ifelse(public_web_quant$PRIORITY_PUBTRN == "Not a priority", 0,
                                           ifelse(public_web_quant$PRIORITY_PUBTRN == "Low priority", 1,
                                                  ifelse(public_web_quant$PRIORITY_PUBTRN == "Moderate priority", 2,
                                                         ifelse(public_web_quant$PRIORITY_PUBTRN == "High priority", 3,
                                                                ifelse(public_web_quant$PRIORITY_PUBTRN == "Top priority", 4, NA)))))
# Create average by sector
public_web_quant$PRIORITY_ENVIRON <- (public_web_quant$PRIORITY_ACSNAT + public_web_quant$PRIORITY_TRASH + public_web_quant$PRIORITY_AIRQTY + public_web_quant$PRIORITY_WTRQTY)/4
public_web_quant$PRIORITY_ECONOMY <- (public_web_quant$PRIORITY_JOBINC + public_web_quant$PRIORITY_WAGINC + public_web_quant$PRIORITY_CSTLIV + public_web_quant$PRIORITY_JOBDIV)/4
public_web_quant$PRIORITY_INFRAST <- (public_web_quant$PRIORITY_PEDCYC + public_web_quant$PRIORITY_RODBRG + public_web_quant$PRIORITY_UTILTY + public_web_quant$PRIORITY_FLDPRT)/4
public_web_quant$PRIORITY_SOCSERV <- (public_web_quant$PRIORITY_ACSEDU + public_web_quant$PRIORITY_ACSHLT + public_web_quant$PRIORITY_HOUSNG + public_web_quant$PRIORITY_PUBTRN)/4

# Determine rank for environmental priorities
public_web_quant <- public_web_quant %>%
  mutate(max_priority = apply(public_web_quant[,249:252], MARGIN = 1, FUN = max, na.rm = TRUE),
         max_priority = na_if(max_priority, -Inf),
         min_priority = apply(public_web_quant[,249:252], MARGIN = 1, FUN = min, na.rm = TRUE),
         min_priority = na_if(min_priority, -Inf))

public_web_quant$PRIORITIZE_ENVIRON <- ifelse(public_web_quant$PRIORITY_ENVIRON == public_web_quant$max_priority, 1, 0)
public_web_quant$RANK_ENVIRON <- ifelse(public_web_quant$PRIORITY_ENVIRON == public_web_quant$max_priority, "Top",
                                        ifelse(public_web_quant$PRIORITY_ENVIRON == public_web_quant$min_priority, "Bottom", "Middle"))



### ATTITUDES, NATURE RELATEDNESS, ETC ###

public_web_quant$JUSTICE_DIST <- ifelse(public_web_quant$JUSTICE_DIST == "Not true at all", 0,
                                        ifelse(public_web_quant$JUSTICE_DIST == "A little true", 1,
                                               ifelse(public_web_quant$JUSTICE_DIST == "Moderately true", 2,
                                                      ifelse(public_web_quant$JUSTICE_DIST == "Very true", 3,
                                                             ifelse(public_web_quant$JUSTICE_DIST == "Completely true", 4, NA)))))
public_web_quant$JUSTICE_PRO <- ifelse(public_web_quant$JUSTICE_PRO == "Not true at all", 0,
                                       ifelse(public_web_quant$JUSTICE_PRO == "A little true", 1,
                                              ifelse(public_web_quant$JUSTICE_PRO == "Moderately true", 2,
                                                     ifelse(public_web_quant$JUSTICE_PRO == "Very true", 3,
                                                            ifelse(public_web_quant$JUSTICE_PRO == "Completely true", 4, NA)))))
public_web_quant$JUSTICE_REC <- ifelse(public_web_quant$JUSTICE_REC == "Not true at all", 0,
                                       ifelse(public_web_quant$JUSTICE_REC == "A little true", 1,
                                              ifelse(public_web_quant$JUSTICE_REC == "Moderately true", 2,
                                                     ifelse(public_web_quant$JUSTICE_REC == "Very true", 3,
                                                            ifelse(public_web_quant$JUSTICE_REC == "Completely true", 4, NA)))))
public_web_quant$JUSTICE_AGN <- ifelse(public_web_quant$JUSTICE_AGN == "Not true at all", 0,
                                       ifelse(public_web_quant$JUSTICE_AGN == "A little true", 1,
                                              ifelse(public_web_quant$JUSTICE_AGN == "Moderately true", 2,
                                                     ifelse(public_web_quant$JUSTICE_AGN == "Very true", 3,
                                                            ifelse(public_web_quant$JUSTICE_AGN == "Completely true", 4, NA)))))
public_web_quant$JUSTICE <- (public_web_quant$JUSTICE_DIST + public_web_quant$JUSTICE_PRO + 
                               public_web_quant$JUSTICE_REC)/3
public_web_quant$ATTITUDE_BAYLNK <- ifelse(public_web_quant$ATTITUDE_BAYLNK == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_BAYLNK == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_BAYLNK == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_BAYLNK == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_BAYLNK == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_BAYIMP <- ifelse(public_web_quant$ATTITUDE_BAYIMP == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_BAYIMP == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_BAYIMP == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_BAYIMP == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_BAYIMP == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_BIODIV <- ifelse(public_web_quant$ATTITUDE_BIODIV == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_BIODIV == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_BIODIV == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_BIODIV == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_BIODIV == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_OPPSAT <- ifelse(public_web_quant$ATTITUDE_OPPSAT == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_OPPSAT == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_OPPSAT == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_OPPSAT == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_OPPSAT == "Completely true", 4, NA)))))
public_web_quant$NR_IMPPRT <- ifelse(public_web_quant$NR_IMPPRT == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_IMPPRT == "A little true", 1,
                                            ifelse(public_web_quant$NR_IMPPRT == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_IMPPRT == "Very true", 3,
                                                          ifelse(public_web_quant$NR_IMPPRT == "Completely true", 4, NA)))))
public_web_quant$NR_CONNCT <- ifelse(public_web_quant$NR_CONNCT == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_CONNCT == "A little true", 1,
                                            ifelse(public_web_quant$NR_CONNCT == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_CONNCT == "Very true", 3,
                                                          ifelse(public_web_quant$NR_CONNCT == "Completely true", 4, NA)))))
public_web_quant$NR_SPIRIT <- ifelse(public_web_quant$NR_SPIRIT == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_SPIRIT == "A little true", 1,
                                            ifelse(public_web_quant$NR_SPIRIT == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_SPIRIT == "Very true", 3,
                                                          ifelse(public_web_quant$NR_SPIRIT == "Completely true", 4, NA)))))
public_web_quant$NR_NOTICE <- ifelse(public_web_quant$NR_NOTICE == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_NOTICE == "A little true", 1,
                                            ifelse(public_web_quant$NR_NOTICE == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_NOTICE == "Very true", 3,
                                                          ifelse(public_web_quant$NR_NOTICE == "Completely true", 4, NA)))))
public_web_quant$NR_VACSPT <- ifelse(public_web_quant$NR_VACSPT == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_VACSPT == "A little true", 1,
                                            ifelse(public_web_quant$NR_VACSPT == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_VACSPT == "Very true", 3,
                                                          ifelse(public_web_quant$NR_VACSPT == "Completely true", 4, NA)))))
public_web_quant$NR_AFFECT <- ifelse(public_web_quant$NR_AFFECT == "Not true at all", 0,
                                     ifelse(public_web_quant$NR_AFFECT == "A little true", 1,
                                            ifelse(public_web_quant$NR_AFFECT == "Moderately true", 2,
                                                   ifelse(public_web_quant$NR_AFFECT == "Very true", 3,
                                                          ifelse(public_web_quant$NR_AFFECT == "Completely true", 4, NA)))))
public_web_quant$NRELATEDNESS <- (public_web_quant$NR_IMPPRT + public_web_quant$NR_CONNCT + public_web_quant$NR_SPIRIT + 
                                    public_web_quant$NR_NOTICE + public_web_quant$NR_VACSPT + public_web_quant$NR_AFFECT)/6
public_web_quant$PIDENTITY_IDENTY <- ifelse(public_web_quant$PIDENTITY_IDENTY == "Not true at all", 0,
                                            ifelse(public_web_quant$PIDENTITY_IDENTY == "A little true", 1,
                                                   ifelse(public_web_quant$PIDENTITY_IDENTY == "Moderately true", 2,
                                                          ifelse(public_web_quant$PIDENTITY_IDENTY == "Very true", 3,
                                                                 ifelse(public_web_quant$PIDENTITY_IDENTY == "Completely true", 4, NA)))))
public_web_quant$PIDENTITY_SPCIAL <- ifelse(public_web_quant$PIDENTITY_SPCIAL == "Not true at all", 0,
                                            ifelse(public_web_quant$PIDENTITY_SPCIAL == "A little true", 1,
                                                   ifelse(public_web_quant$PIDENTITY_SPCIAL == "Moderately true", 2,
                                                          ifelse(public_web_quant$PIDENTITY_SPCIAL == "Very true", 3,
                                                                 ifelse(public_web_quant$PIDENTITY_SPCIAL == "Completely true", 4, NA)))))
public_web_quant$PIDENTITY_ATTCHD <- ifelse(public_web_quant$PIDENTITY_ATTCHD == "Not true at all", 0,
                                            ifelse(public_web_quant$PIDENTITY_ATTCHD == "A little true", 1,
                                                   ifelse(public_web_quant$PIDENTITY_ATTCHD == "Moderately true", 2,
                                                          ifelse(public_web_quant$PIDENTITY_ATTCHD == "Very true", 3,
                                                                 ifelse(public_web_quant$PIDENTITY_ATTCHD == "Completely true", 4, NA)))))
public_web_quant$PIDENTITY <- (public_web_quant$PIDENTITY_IDENTY + public_web_quant$PIDENTITY_SPCIAL + public_web_quant$PIDENTITY_ATTCHD)/3
public_web_quant$SOLASTALGIA_BELONG <- ifelse(public_web_quant$SOLASTALGIA_BELONG == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_BELONG == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_BELONG == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_BELONG == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_BELONG == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_LOSSES <- ifelse(public_web_quant$SOLASTALGIA_LOSSES == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_LOSSES == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_LOSSES == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_LOSSES == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_LOSSES == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_ASHAMD <- ifelse(public_web_quant$SOLASTALGIA_ASHAMD == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_ASHAMD == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_ASHAMD == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_ASHAMD == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_ASHAMD == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_DISAPR <- ifelse(public_web_quant$SOLASTALGIA_DISAPR == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_DISAPR == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_DISAPR == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_DISAPR == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_DISAPR == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_UQLOSS <- ifelse(public_web_quant$SOLASTALGIA_UQLOSS == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_UQLOSS == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_UQLOSS == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_UQLOSS == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_UQLOSS == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_PEACEQ <- ifelse(public_web_quant$SOLASTALGIA_PEACEQ == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_PEACEQ == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_PEACEQ == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_PEACEQ == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_PEACEQ == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_DEGRAD <- ifelse(public_web_quant$SOLASTALGIA_DEGRAD == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_DEGRAD == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_DEGRAD == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_DEGRAD == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_DEGRAD == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_LFSTYL <- ifelse(public_web_quant$SOLASTALGIA_LFSTYL == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_LFSTYL == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_LFSTYL == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_LFSTYL == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_LFSTYL == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA_FLEAVE <- ifelse(public_web_quant$SOLASTALGIA_FLEAVE == "Not true at all", 0,
                                              ifelse(public_web_quant$SOLASTALGIA_FLEAVE == "A little true", 1,
                                                     ifelse(public_web_quant$SOLASTALGIA_FLEAVE == "Moderately true", 2,
                                                            ifelse(public_web_quant$SOLASTALGIA_FLEAVE == "Very true", 3,
                                                                   ifelse(public_web_quant$SOLASTALGIA_FLEAVE == "Completely true", 4, NA)))))
public_web_quant$SOLASTALGIA <- (public_web_quant$SOLASTALGIA_LOSSES + public_web_quant$SOLASTALGIA_ASHAMD + 
                                   public_web_quant$SOLASTALGIA_DISAPR + public_web_quant$SOLASTALGIA_UQLOSS + public_web_quant$SOLASTALGIA_PEACEQ + 
                                   public_web_quant$SOLASTALGIA_DEGRAD + public_web_quant$SOLASTALGIA_LFSTYL)/7
public_web_quant$HOPEPATH_WAYFIX <- ifelse(public_web_quant$HOPEPATH_WAYFIX == "Not true at all", 0,
                                           ifelse(public_web_quant$HOPEPATH_WAYFIX == "A little true", 1,
                                                  ifelse(public_web_quant$HOPEPATH_WAYFIX == "Moderately true", 2,
                                                         ifelse(public_web_quant$HOPEPATH_WAYFIX == "Very true", 3,
                                                                ifelse(public_web_quant$HOPEPATH_WAYFIX == "Completely true", 4, NA)))))
public_web_quant$HOPEPATH_KNOWLG <- ifelse(public_web_quant$HOPEPATH_KNOWLG == "Not true at all", 0,
                                           ifelse(public_web_quant$HOPEPATH_KNOWLG == "A little true", 1,
                                                  ifelse(public_web_quant$HOPEPATH_KNOWLG == "Moderately true", 2,
                                                         ifelse(public_web_quant$HOPEPATH_KNOWLG == "Very true", 3,
                                                                ifelse(public_web_quant$HOPEPATH_KNOWLG == "Completely true", 4, NA)))))
public_web_quant$HOPEPATH_CANFIX <- ifelse(public_web_quant$HOPEPATH_CANFIX == "Not true at all", 0,
                                           ifelse(public_web_quant$HOPEPATH_CANFIX == "A little true", 1,
                                                  ifelse(public_web_quant$HOPEPATH_CANFIX == "Moderately true", 2,
                                                         ifelse(public_web_quant$HOPEPATH_CANFIX == "Very true", 3,
                                                                ifelse(public_web_quant$HOPEPATH_CANFIX == "Completely true", 4, NA)))))
public_web_quant$HOPEPATH_RESOLV <- ifelse(public_web_quant$HOPEPATH_RESOLV == "Not true at all", 0,
                                           ifelse(public_web_quant$HOPEPATH_RESOLV == "A little true", 1,
                                                  ifelse(public_web_quant$HOPEPATH_RESOLV == "Moderately true", 2,
                                                         ifelse(public_web_quant$HOPEPATH_RESOLV == "Very true", 3,
                                                                ifelse(public_web_quant$HOPEPATH_RESOLV == "Completely true", 4, NA)))))
public_web_quant$HOPEPATH_ENOUGH <- ifelse(public_web_quant$HOPEPATH_ENOUGH == "Not true at all", 0,
                                           ifelse(public_web_quant$HOPEPATH_ENOUGH == "A little true", 1,
                                                  ifelse(public_web_quant$HOPEPATH_ENOUGH == "Moderately true", 2,
                                                         ifelse(public_web_quant$HOPEPATH_ENOUGH == "Very true", 3,
                                                                ifelse(public_web_quant$HOPEPATH_ENOUGH == "Completely true", 4, NA)))))
public_web_quant$HOPEPATH_SELFEFF <- (public_web_quant$HOPEPATH_WAYFIX + public_web_quant$HOPEPATH_KNOWLG)/2
public_web_quant$HOPEPATH_RESPEFF <- (public_web_quant$HOPEPATH_CANFIX + public_web_quant$HOPEPATH_ENOUGH)/2
public_web_quant$ATTITUDE_SAFOUT <- ifelse(public_web_quant$ATTITUDE_SAFOUT == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_SAFOUT == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_SAFOUT == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_SAFOUT == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_SAFOUT == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_OUTWEL <- ifelse(public_web_quant$ATTITUDE_OUTWEL == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_OUTWEL == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_OUTWEL == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_OUTWEL == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_OUTWEL == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_SWMSAF <- ifelse(public_web_quant$ATTITUDE_SWMSAF == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_SWMSAF == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_SWMSAF == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_SWMSAF == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_SWMSAF == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_CHNGFW <- ifelse(public_web_quant$ATTITUDE_CHNGFW == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_CHNGFW == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_CHNGFW == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_CHNGFW == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_CHNGFW == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_CHNGRN <- ifelse(public_web_quant$ATTITUDE_CHNGRN == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_CHNGRN == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_CHNGRN == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_CHNGRN == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_CHNGRN == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_CHNGTP <- ifelse(public_web_quant$ATTITUDE_CHNGTP == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_CHNGTP == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_CHNGTP == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_CHNGTP == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_CHNGTP == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_CHNGHS <- ifelse(public_web_quant$ATTITUDE_CHNGHS == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_CHNGHS == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_CHNGHS == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_CHNGHS == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_CHNGHS == "Completely true", 4, NA)))))
public_web_quant$ATTITUDE_CCTHRT <- ifelse(public_web_quant$ATTITUDE_CCTHRT == "Not true at all", 0,
                                           ifelse(public_web_quant$ATTITUDE_CCTHRT == "A little true", 1,
                                                  ifelse(public_web_quant$ATTITUDE_CCTHRT == "Moderately true", 2,
                                                         ifelse(public_web_quant$ATTITUDE_CCTHRT == "Very true", 3,
                                                                ifelse(public_web_quant$ATTITUDE_CCTHRT == "Completely true", 4, NA)))))


### ACTIVITIES/BEHAVIORS ###

public_web_quant$ACTIVITY_CMPHIK <- ifelse(public_web_quant$ACTIVITY_CMPHIK == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_CMPHIK == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_CMPHIK == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_CMPHIK == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_CMPHIK == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_PADBOT <- ifelse(public_web_quant$ACTIVITY_PADBOT == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_PADBOT == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_PADBOT == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_PADBOT == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_PADBOT == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_SWIMDV <- ifelse(public_web_quant$ACTIVITY_SWIMDV == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_SWIMDV == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_SWIMDV == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_SWIMDV == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_SWIMDV == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_BWATCH <- ifelse(public_web_quant$ACTIVITY_BWATCH == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_BWATCH == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_BWATCH == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_BWATCH == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_BWATCH == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_GARDEN <- ifelse(public_web_quant$ACTIVITY_GARDEN == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_GARDEN == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_GARDEN == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_GARDEN == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_GARDEN == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_FISHNG <- ifelse(public_web_quant$ACTIVITY_FISHNG == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_FISHNG == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_FISHNG == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_FISHNG == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_FISHNG == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_EXRCSO <- ifelse(public_web_quant$ACTIVITY_EXRCSO == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_EXRCSO == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_EXRCSO == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_EXRCSO == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_EXRCSO == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_RELAXO <- ifelse(public_web_quant$ACTIVITY_RELAXO == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_RELAXO == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_RELAXO == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_RELAXO == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_RELAXO == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_PICNIC <- ifelse(public_web_quant$ACTIVITY_PICNIC == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_PICNIC == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_PICNIC == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_PICNIC == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_PICNIC == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_PLYGND <- ifelse(public_web_quant$ACTIVITY_PLYGND == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_PLYGND == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_PLYGND == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_PLYGND == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_PLYGND == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_ZOOAQU <- ifelse(public_web_quant$ACTIVITY_ZOOAQU == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_ZOOAQU == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_ZOOAQU == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_ZOOAQU == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_ZOOAQU == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_LSNPRK <- ifelse(public_web_quant$ACTIVITY_LSNPRK == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_LSNPRK == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_LSNPRK == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_LSNPRK == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_LSNPRK == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_HSTSIT <- ifelse(public_web_quant$ACTIVITY_HSTSIT == "Never", 0,
                                           ifelse(public_web_quant$ACTIVITY_HSTSIT == "A few times", 1,
                                                  ifelse(public_web_quant$ACTIVITY_HSTSIT == "About once a month", 2,
                                                         ifelse(public_web_quant$ACTIVITY_HSTSIT == "About once a week", 3,
                                                                ifelse(public_web_quant$ACTIVITY_HSTSIT == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_BEACH <- ifelse(public_web_quant$ACTIVITY_BEACH == "Never", 0,
                                          ifelse(public_web_quant$ACTIVITY_BEACH == "A few times", 1,
                                                 ifelse(public_web_quant$ACTIVITY_BEACH == "About once a month", 2,
                                                        ifelse(public_web_quant$ACTIVITY_BEACH == "About once a week", 3,
                                                               ifelse(public_web_quant$ACTIVITY_BEACH == "Multiple times per week", 4, NA)))))
public_web_quant$ACTIVITY_PIER <- ifelse(public_web_quant$ACTIVITY_PIER == "Never", 0,
                                         ifelse(public_web_quant$ACTIVITY_PIER == "A few times", 1,
                                                ifelse(public_web_quant$ACTIVITY_PIER == "About once a month", 2,
                                                       ifelse(public_web_quant$ACTIVITY_PIER == "About once a week", 3,
                                                              ifelse(public_web_quant$ACTIVITY_PIER == "Multiple times per week", 4, NA)))))

# Measure active vs passive nature engagement
public_web_quant$ACTIVE_COUNT <- rowSums(public_web_quant[56:61] > 0)
public_web_quant$PASSIVE_COUNT <- rowSums(public_web_quant[62:64] > 0)
public_web_quant$ATTRACT_COUNT <- rowSums(public_web_quant[65:70] > 0)
public_web_quant$ACTIVE_AVG <- rowMeans(public_web_quant[56:61])
public_web_quant$PASSIVE_AVG <- rowMeans(public_web_quant[62:64])
public_web_quant$ATTRACT_AVG <- rowMeans(public_web_quant[65:70])


public_web_quant$BEHAVIOR_FERTLZ <- ifelse(public_web_quant$BEHAVIOR_FERTLZ == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_FERTLZ == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_FERTLZ == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_FERTLZ == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_FERTLZ == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_FERTLZweight <- ifelse(is.na(public_web$BEHAVIOR_FERTLZ), NA, 
                                                 ifelse(public_web_quant$FERTLZ_SUMMER == 1, public_web_quant$BEHAVIOR_FERTLZ * 2, public_web$BEHAVIOR_FERTLZ))
public_web_quant$BEHAVIOR_WATER <- ifelse(public_web_quant$BEHAVIOR_WATER == "Never", 0,
                                          ifelse(public_web_quant$BEHAVIOR_WATER == "A few times", 1,
                                                 ifelse(public_web_quant$BEHAVIOR_WATER == "About once a month", 2,
                                                        ifelse(public_web_quant$BEHAVIOR_WATER == "About once a week", 3,
                                                               ifelse(public_web_quant$BEHAVIOR_WATER == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_CLPPNG <- ifelse(public_web_quant$BEHAVIOR_CLPPNG == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_CLPPNG == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_CLPPNG == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_CLPPNG == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_CLPPNG == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_GRNINF <- ifelse(public_web_quant$BEHAVIOR_GRNINF == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_GRNINF == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_GRNINF == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_GRNINF == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_GRNINF == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_NODRIV <- ifelse(public_web_quant$BEHAVIOR_NODRIV == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_NODRIV == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_NODRIV == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_NODRIV == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_NODRIV == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_RECYCL <- ifelse(public_web_quant$BEHAVIOR_RECYCL == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_RECYCL == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_RECYCL == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_RECYCL == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_RECYCL == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_PLNTFF <- ifelse(public_web_quant$BEHAVIOR_PLNTFF == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_PLNTFF == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_PLNTFF == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_PLNTFF == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_PLNTFF == "Multiple times per week", 4, NA)))))
public_web_quant$BEHAVIOR_RESTOR <- ifelse(public_web_quant$BEHAVIOR_RESTOR == "Never", 0,
                                           ifelse(public_web_quant$BEHAVIOR_RESTOR == "A few times", 1,
                                                  ifelse(public_web_quant$BEHAVIOR_RESTOR == "About once a month", 2,
                                                         ifelse(public_web_quant$BEHAVIOR_RESTOR == "About once a week", 3,
                                                                ifelse(public_web_quant$BEHAVIOR_RESTOR == "Multiple times per week", 4, NA)))))

### NORMS ###

public_web_quant$NORM_FERTLZ <- ifelse(public_web_quant$NORM_FERTLZ == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_FERTLZ == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_FERTLZ == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_FERTLZ == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_FERTLZ == "Much more than I do", 2, NA)))))
public_web_quant$NORM_WATER <- ifelse(public_web_quant$NORM_WATER == "Much less than I do", -2,
                                      ifelse(public_web_quant$NORM_WATER == "A little less than I do", -1,
                                             ifelse(public_web_quant$NORM_WATER == "About the same as I do", 0,
                                                    ifelse(public_web_quant$NORM_WATER == "A little more than I do", 1,
                                                           ifelse(public_web_quant$NORM_WATER == "Much more than I do", 2, NA)))))
public_web_quant$NORM_CLPPNG <- ifelse(public_web_quant$NORM_CLPPNG == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_CLPPNG == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_CLPPNG == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_CLPPNG == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_CLPPNG == "Much more than I do", 2, NA)))))
public_web_quant$NORM_GRNINF <- ifelse(public_web_quant$NORM_GRNINF == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_GRNINF == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_GRNINF == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_GRNINF == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_GRNINF == "Much more than I do", 2, NA)))))
public_web_quant$NORM_NODRIV <- ifelse(public_web_quant$NORM_NODRIV == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_NODRIV == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_NODRIV == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_NODRIV == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_NODRIV == "Much more than I do", 2, NA)))))
public_web_quant$NORM_RECYCL <- ifelse(public_web_quant$NORM_RECYCL == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_RECYCL == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_RECYCL == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_RECYCL == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_RECYCL == "Much more than I do", 2, NA)))))
public_web_quant$NORM_PLNTFF <- ifelse(public_web_quant$NORM_PLNTFF == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_PLNTFF == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_PLNTFF == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_PLNTFF == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_PLNTFF == "Much more than I do", 2, NA)))))
public_web_quant$NORM_RESTOR <- ifelse(public_web_quant$NORM_RESTOR == "Much less than I do", -2,
                                       ifelse(public_web_quant$NORM_RESTOR == "A little less than I do", -1,
                                              ifelse(public_web_quant$NORM_RESTOR == "About the same as I do", 0,
                                                     ifelse(public_web_quant$NORM_RESTOR == "A little more than I do", 1,
                                                            ifelse(public_web_quant$NORM_RESTOR == "Much more than I do", 2, NA)))))


### GROUP INVOLVEMENT ###

public_web_quant$GROUP_ARTCLT <- ifelse(public_web_quant$GROUP_ARTCLT == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_ARTCLT == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_ARTCLT == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_ENVPRT <- ifelse(public_web_quant$GROUP_ENVPRT == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_ENVPRT == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_ENVPRT == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_HOANBH <- ifelse(public_web_quant$GROUP_HOANBH == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_HOANBH == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_HOANBH == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_POLTCL <- ifelse(public_web_quant$GROUP_POLTCL == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_POLTCL == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_POLTCL == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_SPIRIT <- ifelse(public_web_quant$GROUP_SPIRIT == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_SPIRIT == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_SPIRIT == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_CHLPAR <- ifelse(public_web_quant$GROUP_CHLPAR == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_CHLPAR == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_CHLPAR == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_SPORTS <- ifelse(public_web_quant$GROUP_SPORTS == "No, not involved", 0,
                                        ifelse(public_web_quant$GROUP_SPORTS == "Yes, a little involved", 1,
                                               ifelse(public_web_quant$GROUP_SPORTS == "Yes, very involved", 2, NA)))
public_web_quant$GROUP_INVOLVEMENT <- (public_web_quant$GROUP_ARTCLT + public_web_quant$GROUP_CHLPAR + public_web_quant$GROUP_ENVPRT +
                                         public_web_quant$GROUP_HOANBH + public_web_quant$GROUP_POLTCL + public_web_quant$GROUP_SPIRIT + 
                                         public_web_quant$GROUP_SPORTS)/7


### MENTAL HEALTH ###

public_web_quant$MHI_A1 <- ifelse(public_web_quant$MHI_A1 == "None of the time", 0,
                                  ifelse(public_web_quant$MHI_A1 == "A little of the time", 1,
                                         ifelse(public_web_quant$MHI_A1 == "Some of the time", 2,
                                                ifelse(public_web_quant$MHI_A1 == "A good bit of the time", 3,
                                                       ifelse(public_web_quant$MHI_A1 == "Most of the time", 4,
                                                              ifelse(public_web_quant$MHI_A1 == "All of the time", 5, NA))))))
public_web_quant$MHI_A2 <- ifelse(public_web_quant$MHI_A2 == "None of the time", 0,
                                  ifelse(public_web_quant$MHI_A2 == "A little of the time", 1,
                                         ifelse(public_web_quant$MHI_A2 == "Some of the time", 2,
                                                ifelse(public_web_quant$MHI_A2 == "A good bit of the time", 3,
                                                       ifelse(public_web_quant$MHI_A2 == "Most of the time", 4,
                                                              ifelse(public_web_quant$MHI_A2 == "All of the time", 5, NA))))))
public_web_quant$MHI_D1 <- ifelse(public_web_quant$MHI_D1 == "None of the time", 0,
                                  ifelse(public_web_quant$MHI_D1 == "A little of the time", 1,
                                         ifelse(public_web_quant$MHI_D1 == "Some of the time", 2,
                                                ifelse(public_web_quant$MHI_D1 == "A good bit of the time", 3,
                                                       ifelse(public_web_quant$MHI_D1 == "Most of the time", 4,
                                                              ifelse(public_web_quant$MHI_D1 == "All of the time", 5, NA))))))
public_web_quant$MHI_D2 <- ifelse(public_web_quant$MHI_D2 == "None of the time", 0,
                                  ifelse(public_web_quant$MHI_D2 == "A little of the time", 1,
                                         ifelse(public_web_quant$MHI_D2 == "Some of the time", 2,
                                                ifelse(public_web_quant$MHI_D2 == "A good bit of the time", 3,
                                                       ifelse(public_web_quant$MHI_D2 == "Most of the time", 4,
                                                              ifelse(public_web_quant$MHI_D2 == "All of the time", 5, NA))))))
public_web_quant$MHI_D3 <- ifelse(public_web_quant$MHI_D3 == "None of the time", 0,
                                  ifelse(public_web_quant$MHI_D3 == "A little of the time", 1,
                                         ifelse(public_web_quant$MHI_D3 == "Some of the time", 2,
                                                ifelse(public_web_quant$MHI_D3 == "A good bit of the time", 3,
                                                       ifelse(public_web_quant$MHI_D3 == "Most of the time", 4,
                                                              ifelse(public_web_quant$MHI_D3 == "All of the time", 5, NA))))))
public_web_quant$MHI_ANXIETY <- public_web_quant$MHI_A1 + public_web_quant$MHI_A2
public_web_quant$MHI_DEPRESN <- public_web_quant$MHI_D1 + public_web_quant$MHI_D2 + public_web_quant$MHI_D3
public_web_quant$MHI_SCORE <- public_web_quant$MHI_A1 + public_web_quant$MHI_A2 + public_web_quant$MHI_D1 + public_web_quant$MHI_D2 + public_web_quant$MHI_D3
public_web_quant$LIFESATISFACTION <- ifelse(public_web_quant$LIFESATISFACTION == "None of the time", 0,
                                            ifelse(public_web_quant$LIFESATISFACTION == "A little of the time", 1,
                                                   ifelse(public_web_quant$LIFESATISFACTION == "Some of the time", 2,
                                                          ifelse(public_web_quant$LIFESATISFACTION == "A good bit of the time", 3,
                                                                 ifelse(public_web_quant$LIFESATISFACTION == "Most of the time", 4,
                                                                        ifelse(public_web_quant$LIFESATISFACTION == "All of the time", 5, NA))))))
public_web_quant$MHI <- (public_web_quant$MHI_A1 + public_web_quant$MHI_A2 + public_web_quant$MHI_D1 + public_web_quant$MHI_D2 + public_web_quant$MHI_D3)/5
public_web_quant$SWELLBEING <- (public_web_quant$MHI_A1 + public_web_quant$MHI_A2 + public_web_quant$MHI_D1 + public_web_quant$MHI_D2 + public_web_quant$MHI_D3 + public_web_quant$LIFESATISFACTION)/6
public_web_quant$ECOANXIETY_1 <- ifelse(public_web_quant$ECOANXIETY_1 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_1 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_1 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_1 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_2 <- ifelse(public_web_quant$ECOANXIETY_2 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_2 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_2 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_2 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_3 <- ifelse(public_web_quant$ECOANXIETY_3 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_3 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_3 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_3 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_4 <- ifelse(public_web_quant$ECOANXIETY_4 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_4 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_4 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_4 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_5 <- ifelse(public_web_quant$ECOANXIETY_5 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_5 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_5 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_5 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_6 <- ifelse(public_web_quant$ECOANXIETY_6 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_6 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_6 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_6 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_7 <- ifelse(public_web_quant$ECOANXIETY_7 == "Not at all", 0,
                                        ifelse(public_web_quant$ECOANXIETY_7 == "Several days", 1,
                                               ifelse(public_web_quant$ECOANXIETY_7 == "More than half the days", 2,
                                                      ifelse(public_web_quant$ECOANXIETY_7 == "Nearly every day", 3, NA))))
public_web_quant$ECOANXIETY_SCORE <- public_web_quant$ECOANXIETY_1 + public_web_quant$ECOANXIETY_2 + public_web_quant$ECOANXIETY_3 + public_web_quant$ECOANXIETY_4 + public_web_quant$ECOANXIETY_5 + public_web_quant$ECOANXIETY_6 + public_web_quant$ECOANXIETY_7
public_web_quant$ECOANXIETY_CAT <- ifelse(public_web_quant$ECOANXIETY_SCORE < 5, "Low",
                                          ifelse(public_web_quant$ECOANXIETY_SCORE >= 5 & public_web_quant$ECOANXIETY_SCORE < 10, "Mild",
                                                 ifelse(public_web_quant$ECOANXIETY_SCORE >= 10 & public_web_quant$ECOANXIETY_SCORE < 15, "Moderate",
                                                        ifelse(public_web_quant$ECOANXIETY_SCORE >= 15, "Severe", NA))))

### DEMOGRAPHICS ###

public_web_quant$CONSERVATIVE <- ifelse(public_web_quant$POLITICS == "Strongly liberal", -3,
                                        ifelse(public_web_quant$POLITICS == "Moderately liberal", -2,
                                               ifelse(public_web_quant$POLITICS == "Slightly liberal", -1,
                                                      ifelse(public_web_quant$POLITICS == "Neutral", 0,
                                                             ifelse(public_web_quant$POLITICS == "Slightly conservative", 1,
                                                                    ifelse(public_web_quant$POLITICS == "Moderately conservative", 2, 
                                                                           ifelse(public_web_quant$POLITICS == "Strongly conservative", 3, NA)))))))


######### * DATA CLEANING ######### 

# Match Street Intersection coordinates to each respondent by Response ID
public_web <- merge(public_web, public_web_coords, by = "ResponseId", all.x = TRUE)
public_web_quant <- merge(public_web_quant, public_web_coords, by = "ResponseId", all.x = TRUE)

# remove responses flagged for exclusion
public_web$INCLUDE <- ifelse(public_web$INCLUDE == "No", "No", 
                             ifelse(is.na(public_web$POINT_X) & public_web$QC_FRAUDULENT == "Caution", "No", 
                                    ifelse(is.na(public_web$POINT_X) & is.na(public_web$QC_FRAUDULENT), "No", "Yes")))
public_web %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# No                  70
# Yes                 995
# <NA>                7
public_web_quant$INCLUDE <- ifelse(public_web_quant$INCLUDE == "No", "No", 
                                   ifelse(is.na(public_web_quant$POINT_X) & public_web_quant$QC_FRAUDULENT == "Caution", "No", 
                                          ifelse(is.na(public_web_quant$POINT_X) & is.na(public_web_quant$QC_FRAUDULENT), "No", "Yes")))
public_web_quant %>%
  count(INCLUDE)
public_web_clean <- subset(public_web, INCLUDE != "No")
public_web_quant_clean <- subset(public_web_quant, INCLUDE != "No")

# keep/reorder the variables we need
public_web_final <- public_web_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                         "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                         "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                         "POLITICS","YEARS","PROPERTY","OWNERSHIP",
                                         "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                         "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                         "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                         "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                         "NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                         "PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                         "SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                         "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                         "HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                         "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                         "ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                         "JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                         "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                         "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                         "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                         "GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                         "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                         "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                         "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                         "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                         "FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                         "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                         "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                         "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                         "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                         "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                         "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                         "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                         "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN")]

public_web_quant_final <- public_web_quant_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                                     "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                                     "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                                     "POLITICS","CONSERVATIVE","YEARS","PROPERTY","OWNERSHIP",
                                                     "PRIORITIZE_ENVIRON","RANK_ENVIRON","PRIORITY_ENVIRON","PRIORITY_ECONOMY","PRIORITY_INFRAST","PRIORITY_SOCSERV",
                                                     "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                                     "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                                     "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                                     "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                                     "NRELATEDNESS","NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                                     "PIDENTITY","PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                                     "SOLASTALGIA","SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                                     "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                                     "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                                     "SWELLBEING","MHI","MHI_SCORE","MHI_ANXIETY","MHI_DEPRESN",
                                                     "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                                     "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                                     "JUSTICE","JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                                     "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                                     "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                                     "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                                     "GROUP_INVOLVEMENT","GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                                     "ACTIVE_COUNT","PASSIVE_COUNT","ATTRACT_COUNT","ACTIVE_AVG","PASSIVE_AVG","ATTRACT_AVG",
                                                     "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                                     "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                                     "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                                     "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                                     "BEHAVIOR_FERTLZweight","FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                                     "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                                     "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                                     "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                                     "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                                     "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                                     "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                                     "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                                     "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN")]

public_web_final$REFERRAL <- "Panel"
public_web_final$AUDIENCE <- "Public"
public_web_quant_final$REFERRAL <- "Panel"
public_web_quant_final$AUDIENCE <- "Public"

# This creates the final quantitative and qualitative datasets for the public (web) respondents.
# This data has already been merged with other audiences in the final datasets.





######### IN-NETWORK ######### 

# NOTE: Data management tasks performed prior to running this script:
#   - Validation and mapping of street intersection data
#   - "Other" ethnicity response categorization
#   - Categorization of Referral responses


# import data (with amended column names done in Excel)
innetwork <- read.csv("tbcs-2024/survey_data/raw/innetwork.csv")
innetwork_coords <- read.csv("tbcs-2024/survey_data/raw/innetwork_coords.csv")


######### * DATA CORRECTIONS ######### 

### Convert blank cells to NAs ###

innetwork$OWNERSHIP_OTHER_TEXT <- ifelse(innetwork$OWNERSHIP_OTHER_TEXT == "" | innetwork$OWNERSHIP_OTHER_TEXT == " ", NA, innetwork$OWNERSHIP_OTHER_TEXT)
innetwork$YEARS_RESIDENT <- ifelse(innetwork$YEARS_RESIDENT == "" | innetwork$YEARS_RESIDENT == " ", NA, innetwork$YEARS_RESIDENT)
innetwork$YEARS_UNSURE <- ifelse(innetwork$YEARS_UNSURE == "" | innetwork$YEARS_UNSURE == " ", NA, innetwork$YEARS_UNSURE)
innetwork$BEHAVIOR_FERTLZ <- ifelse(innetwork$BEHAVIOR_FERTLZ == "" | innetwork$BEHAVIOR_FERTLZ == " ", NA, innetwork$BEHAVIOR_FERTLZ)
innetwork$BEHAVIOR_WATER <- ifelse(innetwork$BEHAVIOR_WATER == "" | innetwork$BEHAVIOR_WATER == " ", NA, innetwork$BEHAVIOR_WATER)
innetwork$BEHAVIOR_CLPPNG <- ifelse(innetwork$BEHAVIOR_CLPPNG == "" | innetwork$BEHAVIOR_CLPPNG == " ", NA, innetwork$BEHAVIOR_CLPPNG)
innetwork$BEHAVIOR_GRNINF <- ifelse(innetwork$BEHAVIOR_GRNINF == "" | innetwork$BEHAVIOR_GRNINF == " ", NA, innetwork$BEHAVIOR_GRNINF)
innetwork$NORM_FERTLZ <- ifelse(innetwork$NORM_FERTLZ == "" | innetwork$NORM_FERTLZ == " ", NA, innetwork$NORM_FERTLZ)
innetwork$NORM_WATER <- ifelse(innetwork$NORM_WATER == "" | innetwork$NORM_WATER == " ", NA, innetwork$NORM_WATER)
innetwork$NORM_CLPPNG <- ifelse(innetwork$NORM_CLPPNG == "" | innetwork$NORM_CLPPNG == " ", NA, innetwork$NORM_CLPPNG)
innetwork$NORM_GRNINF <- ifelse(innetwork$NORM_GRNINF == "" | innetwork$NORM_GRNINF == " ", NA, innetwork$NORM_GRNINF)
innetwork$FERTLZ_SEASON <- ifelse(innetwork$FERTLZ_SEASON == "" | innetwork$FERTLZ_SEASON == " ", NA, innetwork$FERTLZ_SEASON)
innetwork$FISH_LOCATN <- ifelse(innetwork$FISH_LOCATN == "" | innetwork$FISH_LOCATN == " ", NA, innetwork$FISH_LOCATN)
innetwork$FISH_REASON <- ifelse(innetwork$FISH_REASON == "" | innetwork$FISH_REASON == " ", NA, innetwork$FISH_REASON)
innetwork$ECOANXIETY_1 <- ifelse(innetwork$ECOANXIETY_1 == "" | innetwork$ECOANXIETY_1 == " ", NA, innetwork$ECOANXIETY_1)
innetwork$ECOANXIETY_2 <- ifelse(innetwork$ECOANXIETY_2 == "" | innetwork$ECOANXIETY_2 == " ", NA, innetwork$ECOANXIETY_2)
innetwork$ECOANXIETY_3 <- ifelse(innetwork$ECOANXIETY_3 == "" | innetwork$ECOANXIETY_3 == " ", NA, innetwork$ECOANXIETY_3)
innetwork$ECOANXIETY_4 <- ifelse(innetwork$ECOANXIETY_4 == "" | innetwork$ECOANXIETY_4 == " ", NA, innetwork$ECOANXIETY_4)
innetwork$ECOANXIETY_5 <- ifelse(innetwork$ECOANXIETY_5 == "" | innetwork$ECOANXIETY_5 == " ", NA, innetwork$ECOANXIETY_5)
innetwork$ECOANXIETY_6 <- ifelse(innetwork$ECOANXIETY_6 == "" | innetwork$ECOANXIETY_6 == " ", NA, innetwork$ECOANXIETY_6)
innetwork$ECOANXIETY_7 <- ifelse(innetwork$ECOANXIETY_7 == "" | innetwork$ECOANXIETY_7 == " ", NA, innetwork$ECOANXIETY_7)
innetwork$GENDER_OTHER_TEXT <- ifelse(innetwork$GENDER_OTHER_TEXT == "" | innetwork$GENDER_OTHER_TEXT == " ", NA, innetwork$GENDER_OTHER_TEXT)
innetwork$ETHNICITY_OTHER_TEXT <- ifelse(innetwork$ETHNICITY_OTHER_TEXT == "" | innetwork$ETHNICITY_OTHER_TEXT == " ", NA, innetwork$ETHNICITY_OTHER_TEXT)

### Combine ZIPCODE and YEARS fields ###

innetwork$ZIPCODE <- ifelse(is.na(innetwork$ZIPCODE_RESIDENT), innetwork$ZIPCODE_UNSURE, innetwork$ZIPCODE_RESIDENT)
innetwork$YEARS <- ifelse(is.na(innetwork$YEARS_RESIDENT), innetwork$YEARS_UNSURE, innetwork$YEARS_RESIDENT)


### Set Other classifications ###
# Review and reclassification of all "Other" text entries for the following variables:
#     - OWNERSHIP
#     - GENDER

# GENDER
unique(innetwork$GENDER_OTHER_TEXT)
innetwork$GENDER <- ifelse(innetwork$GENDER == "Other description:", "Non-binary", innetwork$GENDER)

# OWNERSHIP
unique(innetwork$OWNERSHIP_OTHER_TEXT)

familyowned <- c("Live with family","Live with relatives","Live with parents ","live with family",
                 "Parents","Stay with family ","Live with owner","Parents own","Live with parents",
                 "Parents signed financing for my RV","Mother owns it","It's my grandmother's house ",
                 "Family owned I camp here ")
lotrental <- c("rent lot","pay lot rent","Family member rents","own home pay lot rent")
innetwork$OWNERSHIP <- ifelse(innetwork$OWNERSHIP_OTHER_TEXT %in% familyowned, "Own",
                              ifelse(innetwork$OWNERSHIP_OTHER_TEXT %in% lotrental, "Rent",
                                     ifelse(innetwork$OWNERSHIP == "Other arrangement:", "Other", innetwork$OWNERSHIP)))


### Reverse coding ###

innetwork$JUSTICE_DIST <- ifelse(innetwork$JUSTICE_DIST.R == "Not true at all", "Completely true",
                                 ifelse(innetwork$JUSTICE_DIST.R == "A little true", "Very true",
                                        ifelse(innetwork$JUSTICE_DIST.R == "Moderately true", "Moderately true",
                                               ifelse(innetwork$JUSTICE_DIST.R == "Very true", "A little true",
                                                      ifelse(innetwork$JUSTICE_DIST.R == "Completely true", "Not true at all", NA)))))
innetwork$JUSTICE_PRO <- ifelse(innetwork$JUSTICE_PRO.R == "Not true at all", "Completely true",
                                ifelse(innetwork$JUSTICE_PRO.R == "A little true", "Very true",
                                       ifelse(innetwork$JUSTICE_PRO.R == "Moderately true", "Moderately true",
                                              ifelse(innetwork$JUSTICE_PRO.R == "Very true", "A little true",
                                                     ifelse(innetwork$JUSTICE_PRO.R == "Completely true", "Not true at all", NA)))))
innetwork$JUSTICE_REC <- ifelse(innetwork$JUSTICE_REC.R == "Not true at all", "Completely true",
                                ifelse(innetwork$JUSTICE_REC.R == "A little true", "Very true",
                                       ifelse(innetwork$JUSTICE_REC.R == "Moderately true", "Moderately true",
                                              ifelse(innetwork$JUSTICE_REC.R == "Very true", "A little true",
                                                     ifelse(innetwork$JUSTICE_REC.R == "Completely true", "Not true at all", NA)))))
innetwork$JUSTICE_AGN <- ifelse(innetwork$JUSTICE_AGN.R == "Not true at all", "Completely true",
                                ifelse(innetwork$JUSTICE_AGN.R == "A little true", "Very true",
                                       ifelse(innetwork$JUSTICE_AGN.R == "Moderately true", "Moderately true",
                                              ifelse(innetwork$JUSTICE_AGN.R == "Very true", "A little true",
                                                     ifelse(innetwork$JUSTICE_AGN.R == "Completely true", "Not true at all", NA)))))
innetwork$ATTITUDE_BAYLNK <- ifelse(innetwork$ATTITUDE_BAYLNK.R == "Not true at all", "Completely true",
                                    ifelse(innetwork$ATTITUDE_BAYLNK.R == "A little true", "Very true",
                                           ifelse(innetwork$ATTITUDE_BAYLNK.R == "Moderately true", "Moderately true",
                                                  ifelse(innetwork$ATTITUDE_BAYLNK.R == "Very true", "A little true",
                                                         ifelse(innetwork$ATTITUDE_BAYLNK.R == "Completely true", "Not true at all", NA)))))
innetwork$HOPEPATH_CANFIX <- ifelse(innetwork$HOPEPATH_CANFIX.R == "Not true at all", "Completely true",
                                    ifelse(innetwork$HOPEPATH_CANFIX.R == "A little true", "Very true",
                                           ifelse(innetwork$HOPEPATH_CANFIX.R == "Moderately true", "Moderately true",
                                                  ifelse(innetwork$HOPEPATH_CANFIX.R == "Very true", "A little true",
                                                         ifelse(innetwork$HOPEPATH_CANFIX.R == "Completely true", "Not true at all", NA)))))
innetwork$HOPEPATH_ENOUGH <- ifelse(innetwork$HOPEPATH_ENOUGH.R == "Not true at all", "Completely true",
                                    ifelse(innetwork$HOPEPATH_ENOUGH.R == "A little true", "Very true",
                                           ifelse(innetwork$HOPEPATH_ENOUGH.R == "Moderately true", "Moderately true",
                                                  ifelse(innetwork$HOPEPATH_ENOUGH.R == "Very true", "A little true",
                                                         ifelse(innetwork$HOPEPATH_ENOUGH.R == "Completely true", "Not true at all", NA)))))
innetwork$ATTITUDE_OUTWEL <- ifelse(innetwork$ATTITUDE_OUTWEL.R == "Not true at all", "Completely true",
                                    ifelse(innetwork$ATTITUDE_OUTWEL.R == "A little true", "Very true",
                                           ifelse(innetwork$ATTITUDE_OUTWEL.R == "Moderately true", "Moderately true",
                                                  ifelse(innetwork$ATTITUDE_OUTWEL.R == "Very true", "A little true",
                                                         ifelse(innetwork$ATTITUDE_OUTWEL.R == "Completely true", "Not true at all", NA)))))
innetwork$ATTITUDE_SWMSAF <- ifelse(innetwork$ATTITUDE_SWMSAF.R == "Not true at all", "Completely true",
                                    ifelse(innetwork$ATTITUDE_SWMSAF.R == "A little true", "Very true",
                                           ifelse(innetwork$ATTITUDE_SWMSAF.R == "Moderately true", "Moderately true",
                                                  ifelse(innetwork$ATTITUDE_SWMSAF.R == "Very true", "A little true",
                                                         ifelse(innetwork$ATTITUDE_SWMSAF.R == "Completely true", "Not true at all", NA)))))
innetwork$MHI_A1 <- ifelse(innetwork$MHI_A1.R == "None of the time", "All of the time",
                           ifelse(innetwork$MHI_A1.R == "A little of the time", "Most of the time",
                                  ifelse(innetwork$MHI_A1.R == "Some of the time", "A good bit of the time",
                                         ifelse(innetwork$MHI_A1.R == "A good bit of the time", "Some of the time",
                                                ifelse(innetwork$MHI_A1.R == "Most of the time", "A little of the time",
                                                       ifelse(innetwork$MHI_A1.R == "All of the time", "None of the time", NA))))))
innetwork$MHI_D2 <- ifelse(innetwork$MHI_D2.R == "None of the time", "All of the time",
                           ifelse(innetwork$MHI_D2.R == "A little of the time", "Most of the time",
                                  ifelse(innetwork$MHI_D2.R == "Some of the time", "A good bit of the time",
                                         ifelse(innetwork$MHI_D2.R == "A good bit of the time", "Some of the time",
                                                ifelse(innetwork$MHI_D2.R == "Most of the time", "A little of the time",
                                                       ifelse(innetwork$MHI_D2.R == "All of the time", "None of the time", NA))))))
innetwork$MHI_D3 <- ifelse(innetwork$MHI_D3.R == "None of the time", "All of the time",
                           ifelse(innetwork$MHI_D3.R == "A little of the time", "Most of the time",
                                  ifelse(innetwork$MHI_D3.R == "Some of the time", "A good bit of the time",
                                         ifelse(innetwork$MHI_D3.R == "A good bit of the time", "Some of the time",
                                                ifelse(innetwork$MHI_D3.R == "Most of the time", "A little of the time",
                                                       ifelse(innetwork$MHI_D3.R == "All of the time", "None of the time", NA))))))


### Knowledge scoring ###

innetwork$KNOWLEDGE_WQTRND_Points <- ifelse(innetwork$KNOWLEDGE_WQTRND == "Improved", 1, 0)
innetwork$KNOWLEDGE_NUTRNT_Points <- ifelse(innetwork$KNOWLEDGE_NUTRNT == "Nitrogen", 1, 0)
innetwork$KNOWLEDGE_SOURCE_Points <- ifelse(innetwork$KNOWLEDGE_SOURCE == "Stormwater runoff", 1, 0)
innetwork$KNOWLEDGE_FRTSSN_Points <- ifelse(innetwork$KNOWLEDGE_FRTSSN == "Summer (June - September)", 1, 0)
innetwork$KNOWLEDGE_SGLOSS_Points <- ifelse(innetwork$KNOWLEDGE_SGLOSS == "Boat propeller blades,Nutrient pollution", 1, 
                                            ifelse(grepl("propeller", innetwork$KNOWLEDGE_SGLOSS) | grepl("Nutrient", innetwork$KNOWLEDGE_SGLOSS), 0.5, 0))
innetwork$KNOWLEDGE_WQTRND_Score <- innetwork$KNOWLEDGE_WQTRND_Points*(innetwork$KNOWLEDGE_WQTRND_C/10)
innetwork$KNOWLEDGE_NUTRNT_Score <- innetwork$KNOWLEDGE_NUTRNT_Points*(innetwork$KNOWLEDGE_NUTRNT_C/10)
innetwork$KNOWLEDGE_SOURCE_Score <- innetwork$KNOWLEDGE_SOURCE_Points*(innetwork$KNOWLEDGE_SOURCE_C/10)
innetwork$KNOWLEDGE_FRTSSN_Score <- innetwork$KNOWLEDGE_FRTSSN_Points*(innetwork$KNOWLEDGE_FRTSSN_C/10)
innetwork$KNOWLEDGE_SGLOSS_Score <- innetwork$KNOWLEDGE_SGLOSS_Points*(innetwork$KNOWLEDGE_SGLOSS_C/10)
innetwork$KNOWLEDGE_POINTS <- innetwork$KNOWLEDGE_WQTRND_Points + innetwork$KNOWLEDGE_NUTRNT_Points + innetwork$KNOWLEDGE_SOURCE_Points + innetwork$KNOWLEDGE_FRTSSN_Points + innetwork$KNOWLEDGE_SGLOSS_Points
innetwork$KNOWLEDGE_SCORE <- innetwork$KNOWLEDGE_WQTRND_Score + innetwork$KNOWLEDGE_NUTRNT_Score + innetwork$KNOWLEDGE_SOURCE_Score + innetwork$KNOWLEDGE_FRTSSN_Score + innetwork$KNOWLEDGE_SGLOSS_Score
innetwork$KNOWLEDGE_SCORE_Pct <- (innetwork$KNOWLEDGE_SCORE/5)*100


### Break up multiple selections into discrete answers ###

innetwork$FERTLZ_SPRING <- ifelse(is.na(innetwork$FERTLZ_SEASON), NA, ifelse(grepl("Spring", innetwork$FERTLZ_SEASON), 1, 0))
innetwork$FERTLZ_SUMMER <- ifelse(is.na(innetwork$FERTLZ_SEASON), NA, ifelse(grepl("Summer", innetwork$FERTLZ_SEASON), 1, 0))
innetwork$FERTLZ_FALL <- ifelse(is.na(innetwork$FERTLZ_SEASON), NA, ifelse(grepl("Fall", innetwork$FERTLZ_SEASON), 1, 0))
innetwork$FERTLZ_WINTER <- ifelse(is.na(innetwork$FERTLZ_SEASON), NA, ifelse(grepl("Winter", innetwork$FERTLZ_SEASON), 1, 0))
innetwork$FERTLZ_SEASON_N <- innetwork$FERTLZ_SPRING + innetwork$FERTLZ_SUMMER + innetwork$FERTLZ_FALL + innetwork$FERTLZ_WINTER

innetwork$FISH_LOC_SHORE <- ifelse(is.na(innetwork$FISH_LOCATN), NA, ifelse(grepl("shoreline", innetwork$FISH_LOCATN), 1, 0))
innetwork$FISH_LOC_PIER <- ifelse(is.na(innetwork$FISH_LOCATN), NA, ifelse(grepl("pier", innetwork$FISH_LOCATN), 1, 0))
innetwork$FISH_LOC_WATER <- ifelse(is.na(innetwork$FISH_LOCATN), NA, ifelse(grepl("boat", innetwork$FISH_LOCATN), 1, 0))
innetwork$FISH_RSN_SPORT <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("sport", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_RSN_RELAX <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("relaxation", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_RSN_FOOD <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("own", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_RSN_SELL <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("sell", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_RSN_WATCH <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("friends", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_RSN_SOCIAL <- ifelse(is.na(innetwork$FISH_REASON), NA, ifelse(grepl("anglers", innetwork$FISH_REASON), 1, 0))
innetwork$FISH_LOCATN_N <- innetwork$FISH_LOC_SHORE + innetwork$FISH_LOC_PIER + innetwork$FISH_LOC_WATER 
innetwork$FISH_REASON_N <- innetwork$FISH_RSN_SPORT + innetwork$FISH_RSN_RELAX + innetwork$FISH_RSN_FOOD + innetwork$FISH_RSN_SELL + innetwork$FISH_RSN_WATCH + innetwork$FISH_RSN_SOCIAL

innetwork$INFO_TV <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Television", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_RADIO <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Radio", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_SOCIAL <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Social", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_PERIOD <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Newspapers", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_MAILE <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("email", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_PUBLIC <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Public", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_GOV <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("Government websites", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_NGO <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("NGO", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_WORD <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("mouth", innetwork$INFOSOURCES), 1, 0))
innetwork$INFO_OWN <- ifelse(is.na(innetwork$INFOSOURCES), NA, ifelse(grepl("observations", innetwork$INFOSOURCES), 1, 0))
innetwork$INFOSOURCES_N <- innetwork$INFO_TV + innetwork$INFO_RADIO + innetwork$INFO_SOCIAL + innetwork$INFO_PERIOD + innetwork$INFO_MAILE + innetwork$INFO_PUBLIC + innetwork$INFO_GOV + innetwork$INFO_NGO + innetwork$INFO_PERIOD + innetwork$INFO_WORD + innetwork$INFO_OWN

innetwork$RACE_NATIVE <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Alaska", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_ASIAN <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Asian", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_BLACK <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Black", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_MIDEAST <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Middle", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_PACIFIC <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Pacific", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_WHITE <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("White", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_OTHER <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Other", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_HISPANIC <- ifelse(is.na(innetwork$ETHNICITY), NA, ifelse(grepl("Hispanic", innetwork$ETHNICITY), 1, 0))
innetwork$RACE_N <- ifelse(is.na(innetwork$ETHNICITY), NA, 
                           innetwork$RACE_NATIVE + innetwork$RACE_ASIAN + innetwork$RACE_BLACK + innetwork$RACE_MIDEAST + innetwork$RACE_PACIFIC + innetwork$RACE_WHITE + innetwork$RACE_OTHER + innetwork$RACE_HISPANIC)
innetwork$RACE <- ifelse(is.na(innetwork$ETHNICITY), NA,
                         ifelse(innetwork$RACE_N == 0, "Other",
                                ifelse(innetwork$RACE_N > 1, "Multiple",
                                       ifelse(innetwork$RACE_NATIVE == 1 & innetwork$RACE_N == 1, "Other",
                                              ifelse(innetwork$RACE_ASIAN == 1 & innetwork$RACE_N == 1, "Asian",
                                                     ifelse(innetwork$RACE_BLACK == 1 & innetwork$RACE_N == 1, "Black",
                                                            ifelse(innetwork$RACE_MIDEAST == 1 & innetwork$RACE_N == 1, "Other",
                                                                   ifelse(innetwork$RACE_PACIFIC == 1 & innetwork$RACE_N == 1, "Other",
                                                                          ifelse(innetwork$RACE_WHITE == 1 & innetwork$RACE_N == 1, "White",
                                                                                 ifelse(innetwork$RACE_OTHER == 1 & innetwork$RACE_N == 1, "Other",
                                                                                        ifelse(innetwork$RACE_HISPANIC == 1 & innetwork$RACE_N == 1, "Hispanic",NA)))))))))))
innetwork$EMPLOY_FULL <- ifelse(is.na(innetwork$EMPLOYMENT), NA, ifelse(grepl("Full", innetwork$EMPLOYMENT), 1, 0))
innetwork$EMPLOY_PART <- ifelse(is.na(innetwork$EMPLOYMENT), NA, ifelse(grepl("Part", innetwork$EMPLOYMENT), 1, 0))
innetwork$EMPLOY_NONE <- ifelse(is.na(innetwork$EMPLOYMENT), NA, ifelse(grepl("Unemployed", innetwork$EMPLOYMENT), 1, 0))
innetwork$EMPLOYED <- ifelse(innetwork$EMPLOY_FULL == 1, "Full-time", 
                             ifelse(innetwork$EMPLOY_PART == 1, "Part-time", "Unemployed")) 
innetwork$EMPLOYED_STUDENT <- ifelse(is.na(innetwork$EMPLOYMENT), NA, ifelse(grepl("Student", innetwork$EMPLOYMENT), 1, 0))
innetwork$EMPLOYED_RETIRED <- ifelse(is.na(innetwork$EMPLOYMENT), NA, ifelse(grepl("Retired", innetwork$EMPLOYMENT), 1, 0))


######### * EXCLUSIONS ######### 

# flag responses during the preview/testing phase
innetwork$QC_PREVIEW <- ifelse(innetwork$Status == "Survey Preview", "Remove", "OK")

# flag responses that spent less than 5 minutes (300 seconds) on the survey
innetwork$QC_DURATION <- ifelse(innetwork$Duration..in.seconds. < 300, "Remove", "OK")

# flag responses that did not reach the end of the survey
innetwork$QC_FINISHED <- ifelse(innetwork$Finished == "False", "Remove", "OK")

# flag responses that did not pass attention checks
innetwork$QC_ATTENTION <- ifelse(innetwork$ATTNCHECK1 != "A little true" | innetwork$ATTNCHECK2 != "Very true", "Remove", "OK")

# flag fraudulent responses
innetwork$QC_FRAUDULENT <- ifelse(innetwork$Q_RecaptchaScore < 0.5, "Caution", "OK")

# flag potential duplicate responses
innetwork$QC_DUPLICATE <- ifelse(innetwork$Q_RelevantIDDuplicateScore >= 75, "Remove", "OK")

# flag responses that did not provide sufficient answer referral question
innetwork$QC_REFERRAL <- ifelse(is.na(innetwork$REFERRAL), "Remove", "OK")

# flag responses exhibiting response biases
## check if respondents are (too) frequently selecting the same answer (12 question blocks to check)
#### priorities = 3 sections (nataccess/costliv/rodbrg) 
#### attitudes = 2 sections (justdist/bayimp) 
#### behaviors = 3 sections (cmphik/exrcso/histsit) 
#### norms = 1 sections
#### distress = 3 sections (pident/hopeknow/degraded) 
#### safety = 2 sections (safout/chngrn)


innetwork$QC_BIASCHECK_P1 <- ifelse(innetwork$PRIORITY_ACSNAT == innetwork$PRIORITY_ACSEDU & 
                                      innetwork$PRIORITY_ACSNAT == innetwork$PRIORITY_ACSHLT & 
                                      innetwork$PRIORITY_ACSNAT == innetwork$PRIORITY_JOBINC & 
                                      innetwork$PRIORITY_ACSNAT == innetwork$PRIORITY_WAGINC & 
                                      innetwork$PRIORITY_ACSNAT == innetwork$PRIORITY_TRASH, "Caution", "OK")
innetwork$QC_BIASCHECK_P2 <- ifelse(innetwork$PRIORITY_CSTLIV == innetwork$PRIORITY_PEDCYC & 
                                      innetwork$PRIORITY_CSTLIV == innetwork$PRIORITY_HOUSNG & 
                                      innetwork$PRIORITY_CSTLIV == innetwork$PRIORITY_JOBDIV & 
                                      innetwork$PRIORITY_CSTLIV == innetwork$PRIORITY_AIRQTY & 
                                      innetwork$PRIORITY_CSTLIV == innetwork$PRIORITY_WTRQTY, "Caution", "OK")
innetwork$QC_BIASCHECK_P3 <- ifelse(innetwork$PRIORITY_RODBRG == innetwork$PRIORITY_UTILTY & 
                                      innetwork$PRIORITY_RODBRG == innetwork$PRIORITY_FLDPRT & 
                                      innetwork$PRIORITY_RODBRG == innetwork$PRIORITY_PUBTRN, "Caution", "OK")
innetwork$QC_BIASCHECK_A1 <- ifelse(innetwork$JUSTICE_DIST == innetwork$JUSTICE_PRO & 
                                      innetwork$JUSTICE_DIST == innetwork$JUSTICE_REC & 
                                      innetwork$JUSTICE_DIST == innetwork$JUSTICE_AGN & 
                                      innetwork$JUSTICE_DIST == innetwork$ATTITUDE_BAYLNK.R, "Caution", "OK")
innetwork$QC_BIASCHECK_A2 <- ifelse(innetwork$ATTITUDE_BAYIMP == innetwork$ATTITUDE_BIODIV & 
                                      innetwork$ATTITUDE_BAYIMP == innetwork$ATTITUDE_OPPSAT & 
                                      innetwork$ATTITUDE_BAYIMP == innetwork$ATTNCHECK1 & 
                                      innetwork$ATTITUDE_BAYIMP == innetwork$NR_IMPPRT, "Caution", "OK")
innetwork$QC_BIASCHECK_B1 <- ifelse(innetwork$ACTIVITY_CMPHIK == innetwork$ACTIVITY_PADBOT & 
                                      innetwork$ACTIVITY_CMPHIK == innetwork$ACTIVITY_SWIMDV & 
                                      innetwork$ACTIVITY_CMPHIK == innetwork$ACTIVITY_BWATCH & 
                                      innetwork$ACTIVITY_CMPHIK == innetwork$ACTIVITY_GARDEN & 
                                      innetwork$ACTIVITY_CMPHIK == innetwork$ACTIVITY_FISHNG, "Caution", "OK")
innetwork$QC_BIASCHECK_B2 <- ifelse(innetwork$ACTIVITY_EXRCSO == innetwork$ACTIVITY_RELAXO & 
                                      innetwork$ACTIVITY_EXRCSO == innetwork$ACTIVITY_PICNIC & 
                                      innetwork$ACTIVITY_EXRCSO == innetwork$ACTIVITY_PLYGND & 
                                      innetwork$ACTIVITY_EXRCSO == innetwork$ACTIVITY_ZOOAQU & 
                                      innetwork$ACTIVITY_EXRCSO == innetwork$ACTIVITY_LSNPRK, "Caution", "OK")
innetwork$QC_BIASCHECK_B3 <- ifelse(innetwork$ACTIVITY_HSTSIT == innetwork$ACTIVITY_BEACH & 
                                      innetwork$ACTIVITY_HSTSIT == innetwork$ACTIVITY_PIER & 
                                      innetwork$ACTIVITY_HSTSIT == innetwork$BEHAVIOR_NODRIV & 
                                      innetwork$ACTIVITY_HSTSIT == innetwork$BEHAVIOR_RECYCL & 
                                      innetwork$ACTIVITY_HSTSIT == innetwork$BEHAVIOR_PLNTFF & 
                                      innetwork$ACTIVITY_HSTSIT == innetwork$BEHAVIOR_RESTOR, "Caution", "OK")
innetwork$QC_BIASCHECK_N1 <- ifelse(innetwork$NORM_NODRIV == innetwork$NORM_RECYCL & 
                                      innetwork$NORM_NODRIV == innetwork$NORM_PLNTFF & 
                                      innetwork$NORM_NODRIV == innetwork$NORM_RESTOR, "Caution", "OK")
innetwork$QC_BIASCHECK_D1 <- ifelse(innetwork$PIDENTITY_IDENTY == innetwork$SOLASTALGIA_BELONG & 
                                      innetwork$PIDENTITY_IDENTY == innetwork$SOLASTALGIA_LOSSES & 
                                      innetwork$PIDENTITY_IDENTY == innetwork$HOPEPATH_WAYFIX & 
                                      innetwork$PIDENTITY_IDENTY == innetwork$SOLASTALGIA_ASHAMD & 
                                      innetwork$PIDENTITY_IDENTY == innetwork$SOLASTALGIA_DISAPR, "Caution", "OK")
innetwork$QC_BIASCHECK_D2 <- ifelse(innetwork$HOPEPATH_KNOWLG == innetwork$PIDENTITY_SPCIAL & 
                                      innetwork$HOPEPATH_KNOWLG == innetwork$SOLASTALGIA_UQLOSS & 
                                      innetwork$HOPEPATH_KNOWLG == innetwork$SOLASTALGIA_PEACEQ & 
                                      innetwork$HOPEPATH_KNOWLG == innetwork$ATTNCHECK2 & 
                                      innetwork$HOPEPATH_KNOWLG == innetwork$HOPEPATH_CANFIX.R, "Caution", "OK")
innetwork$QC_BIASCHECK_D3 <- ifelse(innetwork$SOLASTALGIA_DEGRAD == innetwork$ACTIVITY_BEACH & 
                                      innetwork$SOLASTALGIA_DEGRAD == innetwork$HOPEPATH_RESOLV & 
                                      innetwork$SOLASTALGIA_DEGRAD == innetwork$SOLASTALGIA_LFSTYL & 
                                      innetwork$SOLASTALGIA_DEGRAD == innetwork$SOLASTALGIA_FLEAVE & 
                                      innetwork$SOLASTALGIA_DEGRAD == innetwork$HOPEPATH_ENOUGH.R, "Caution", "OK")
innetwork$QC_BIASCHECK_S1 <- ifelse(innetwork$ATTITUDE_SAFOUT == innetwork$ATTITUDE_OUTWEL.R & 
                                      innetwork$ATTITUDE_SAFOUT == innetwork$ATTITUDE_SWMSAF.R & 
                                      innetwork$ATTITUDE_SAFOUT == innetwork$ATTITUDE_CHNGFW, "Caution", "OK")

innetwork <- innetwork %>%
  mutate(QC_BIAS_CAUTIONS = str_count(QC_BIASCHECK_P1, "Caution") + str_count(QC_BIASCHECK_P2, "Caution") + 
           str_count(QC_BIASCHECK_P3, "Caution") + str_count(QC_BIASCHECK_A1, "Caution") + str_count(QC_BIASCHECK_A2, "Caution") + 
           str_count(QC_BIASCHECK_B1, "Caution") + str_count(QC_BIASCHECK_B2, "Caution") + str_count(QC_BIASCHECK_B3, "Caution") + 
           str_count(QC_BIASCHECK_N1, "Caution") + str_count(QC_BIASCHECK_D1, "Caution") + str_count(QC_BIASCHECK_D2, "Caution") + 
           str_count(QC_BIASCHECK_D3, "Caution") + str_count(QC_BIASCHECK_S1, "Caution"))

# INCLUSION DETERMINATION
innetwork$INCLUDE <- ifelse(innetwork$QC_PREVIEW == "Remove" | innetwork$QC_DURATION == "Remove" | 
                              innetwork$QC_FINISHED == "Remove" | innetwork$QC_ATTENTION == "Remove" | 
                              innetwork$QC_DUPLICATE == "Remove" | innetwork$QC_REFERRAL == "Remove", "No", "Yes")
innetwork$INCLUDE <- ifelse(innetwork$INCLUDE == "Yes" & innetwork$QC_BIAS_CAUTIONS > 4, "No", 
                            ifelse(innetwork$INCLUDE == "Yes" & innetwork$QC_FRAUDULENT == "Caution", "Confirm Fraudulent", innetwork$INCLUDE))


innetwork %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# No                  19
# Yes                 178
# <NA>                2



######### * QUANTITATIVE SCALING ######### 

# Separate dataset with quantitative values instead of text values
innetwork_quant <- innetwork

### PRIORITY ###

innetwork_quant$PRIORITY_ACSNAT <- ifelse(innetwork_quant$PRIORITY_ACSNAT == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_ACSNAT == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_ACSNAT == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_ACSNAT == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_ACSNAT == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_ACSEDU <- ifelse(innetwork_quant$PRIORITY_ACSEDU == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_ACSEDU == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_ACSEDU == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_ACSEDU == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_ACSEDU == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_ACSHLT <- ifelse(innetwork_quant$PRIORITY_ACSHLT == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_ACSHLT == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_ACSHLT == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_ACSHLT == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_ACSHLT == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_JOBINC <- ifelse(innetwork_quant$PRIORITY_JOBINC == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_JOBINC == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_JOBINC == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_JOBINC == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_JOBINC == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_WAGINC <- ifelse(innetwork_quant$PRIORITY_WAGINC == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_WAGINC == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_WAGINC == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_WAGINC == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_WAGINC == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_TRASH <- ifelse(innetwork_quant$PRIORITY_TRASH == "Not a priority", 0,
                                         ifelse(innetwork_quant$PRIORITY_TRASH == "Low priority", 1,
                                                ifelse(innetwork_quant$PRIORITY_TRASH == "Moderate priority", 2,
                                                       ifelse(innetwork_quant$PRIORITY_TRASH == "High priority", 3,
                                                              ifelse(innetwork_quant$PRIORITY_TRASH == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_CSTLIV <- ifelse(innetwork_quant$PRIORITY_CSTLIV == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_CSTLIV == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_CSTLIV == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_CSTLIV == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_CSTLIV == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_PEDCYC <- ifelse(innetwork_quant$PRIORITY_PEDCYC == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_PEDCYC == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_PEDCYC == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_PEDCYC == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_PEDCYC == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_HOUSNG <- ifelse(innetwork_quant$PRIORITY_HOUSNG == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_HOUSNG == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_HOUSNG == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_HOUSNG == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_HOUSNG == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_JOBDIV <- ifelse(innetwork_quant$PRIORITY_JOBDIV == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_JOBDIV == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_JOBDIV == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_JOBDIV == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_JOBDIV == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_AIRQTY <- ifelse(innetwork_quant$PRIORITY_AIRQTY == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_AIRQTY == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_AIRQTY == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_AIRQTY == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_AIRQTY == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_WTRQTY <- ifelse(innetwork_quant$PRIORITY_WTRQTY == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_WTRQTY == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_WTRQTY == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_WTRQTY == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_WTRQTY == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_RODBRG <- ifelse(innetwork_quant$PRIORITY_RODBRG == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_RODBRG == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_RODBRG == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_RODBRG == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_RODBRG == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_UTILTY <- ifelse(innetwork_quant$PRIORITY_UTILTY == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_UTILTY == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_UTILTY == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_UTILTY == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_UTILTY == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_FLDPRT <- ifelse(innetwork_quant$PRIORITY_FLDPRT == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_FLDPRT == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_FLDPRT == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_FLDPRT == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_FLDPRT == "Top priority", 4, NA)))))
innetwork_quant$PRIORITY_PUBTRN <- ifelse(innetwork_quant$PRIORITY_PUBTRN == "Not a priority", 0,
                                          ifelse(innetwork_quant$PRIORITY_PUBTRN == "Low priority", 1,
                                                 ifelse(innetwork_quant$PRIORITY_PUBTRN == "Moderate priority", 2,
                                                        ifelse(innetwork_quant$PRIORITY_PUBTRN == "High priority", 3,
                                                               ifelse(innetwork_quant$PRIORITY_PUBTRN == "Top priority", 4, NA)))))
# Create average by sector
innetwork_quant$PRIORITY_ENVIRON <- (innetwork_quant$PRIORITY_ACSNAT + innetwork_quant$PRIORITY_TRASH + innetwork_quant$PRIORITY_AIRQTY + innetwork_quant$PRIORITY_WTRQTY)/4
innetwork_quant$PRIORITY_ECONOMY <- (innetwork_quant$PRIORITY_JOBINC + innetwork_quant$PRIORITY_WAGINC + innetwork_quant$PRIORITY_CSTLIV + innetwork_quant$PRIORITY_JOBDIV)/4
innetwork_quant$PRIORITY_INFRAST <- (innetwork_quant$PRIORITY_PEDCYC + innetwork_quant$PRIORITY_RODBRG + innetwork_quant$PRIORITY_UTILTY + innetwork_quant$PRIORITY_FLDPRT)/4
innetwork_quant$PRIORITY_SOCSERV <- (innetwork_quant$PRIORITY_ACSEDU + innetwork_quant$PRIORITY_ACSHLT + innetwork_quant$PRIORITY_HOUSNG + innetwork_quant$PRIORITY_PUBTRN)/4

# Determine rank for environmental priorities
innetwork_quant <- innetwork_quant %>%
  mutate(max_priority = apply(innetwork_quant[,250:253], MARGIN = 1, FUN = max, na.rm = TRUE),
         max_priority = na_if(max_priority, -Inf),
         min_priority = apply(innetwork_quant[,250:253], MARGIN = 1, FUN = min, na.rm = TRUE),
         min_priority = na_if(min_priority, -Inf))

innetwork_quant$PRIORITIZE_ENVIRON <- ifelse(innetwork_quant$PRIORITY_ENVIRON == innetwork_quant$max_priority, 1, 0)
innetwork_quant$RANK_ENVIRON <- ifelse(innetwork_quant$PRIORITY_ENVIRON == innetwork_quant$max_priority, "Top",
                                       ifelse(innetwork_quant$PRIORITY_ENVIRON == innetwork_quant$min_priority, "Bottom", "Middle"))



### ATTITUDES, NATURE RELATEDNESS, ETC ###

innetwork_quant$JUSTICE_DIST <- ifelse(innetwork_quant$JUSTICE_DIST == "Not true at all", 0,
                                       ifelse(innetwork_quant$JUSTICE_DIST == "A little true", 1,
                                              ifelse(innetwork_quant$JUSTICE_DIST == "Moderately true", 2,
                                                     ifelse(innetwork_quant$JUSTICE_DIST == "Very true", 3,
                                                            ifelse(innetwork_quant$JUSTICE_DIST == "Completely true", 4, NA)))))
innetwork_quant$JUSTICE_PRO <- ifelse(innetwork_quant$JUSTICE_PRO == "Not true at all", 0,
                                      ifelse(innetwork_quant$JUSTICE_PRO == "A little true", 1,
                                             ifelse(innetwork_quant$JUSTICE_PRO == "Moderately true", 2,
                                                    ifelse(innetwork_quant$JUSTICE_PRO == "Very true", 3,
                                                           ifelse(innetwork_quant$JUSTICE_PRO == "Completely true", 4, NA)))))
innetwork_quant$JUSTICE_REC <- ifelse(innetwork_quant$JUSTICE_REC == "Not true at all", 0,
                                      ifelse(innetwork_quant$JUSTICE_REC == "A little true", 1,
                                             ifelse(innetwork_quant$JUSTICE_REC == "Moderately true", 2,
                                                    ifelse(innetwork_quant$JUSTICE_REC == "Very true", 3,
                                                           ifelse(innetwork_quant$JUSTICE_REC == "Completely true", 4, NA)))))
innetwork_quant$JUSTICE_AGN <- ifelse(innetwork_quant$JUSTICE_AGN == "Not true at all", 0,
                                      ifelse(innetwork_quant$JUSTICE_AGN == "A little true", 1,
                                             ifelse(innetwork_quant$JUSTICE_AGN == "Moderately true", 2,
                                                    ifelse(innetwork_quant$JUSTICE_AGN == "Very true", 3,
                                                           ifelse(innetwork_quant$JUSTICE_AGN == "Completely true", 4, NA)))))
innetwork_quant$JUSTICE <- (innetwork_quant$JUSTICE_DIST + innetwork_quant$JUSTICE_PRO + 
                              innetwork_quant$JUSTICE_REC)/3
innetwork_quant$ATTITUDE_BAYLNK <- ifelse(innetwork_quant$ATTITUDE_BAYLNK == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_BAYLNK == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_BAYLNK == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_BAYLNK == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_BAYLNK == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_BAYIMP <- ifelse(innetwork_quant$ATTITUDE_BAYIMP == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_BAYIMP == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_BAYIMP == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_BAYIMP == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_BAYIMP == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_BIODIV <- ifelse(innetwork_quant$ATTITUDE_BIODIV == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_BIODIV == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_BIODIV == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_BIODIV == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_BIODIV == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_OPPSAT <- ifelse(innetwork_quant$ATTITUDE_OPPSAT == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_OPPSAT == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_OPPSAT == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_OPPSAT == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_OPPSAT == "Completely true", 4, NA)))))
innetwork_quant$NR_IMPPRT <- ifelse(innetwork_quant$NR_IMPPRT == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_IMPPRT == "A little true", 1,
                                           ifelse(innetwork_quant$NR_IMPPRT == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_IMPPRT == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_IMPPRT == "Completely true", 4, NA)))))
innetwork_quant$NR_CONNCT <- ifelse(innetwork_quant$NR_CONNCT == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_CONNCT == "A little true", 1,
                                           ifelse(innetwork_quant$NR_CONNCT == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_CONNCT == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_CONNCT == "Completely true", 4, NA)))))
innetwork_quant$NR_SPIRIT <- ifelse(innetwork_quant$NR_SPIRIT == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_SPIRIT == "A little true", 1,
                                           ifelse(innetwork_quant$NR_SPIRIT == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_SPIRIT == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_SPIRIT == "Completely true", 4, NA)))))
innetwork_quant$NR_NOTICE <- ifelse(innetwork_quant$NR_NOTICE == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_NOTICE == "A little true", 1,
                                           ifelse(innetwork_quant$NR_NOTICE == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_NOTICE == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_NOTICE == "Completely true", 4, NA)))))
innetwork_quant$NR_VACSPT <- ifelse(innetwork_quant$NR_VACSPT == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_VACSPT == "A little true", 1,
                                           ifelse(innetwork_quant$NR_VACSPT == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_VACSPT == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_VACSPT == "Completely true", 4, NA)))))
innetwork_quant$NR_AFFECT <- ifelse(innetwork_quant$NR_AFFECT == "Not true at all", 0,
                                    ifelse(innetwork_quant$NR_AFFECT == "A little true", 1,
                                           ifelse(innetwork_quant$NR_AFFECT == "Moderately true", 2,
                                                  ifelse(innetwork_quant$NR_AFFECT == "Very true", 3,
                                                         ifelse(innetwork_quant$NR_AFFECT == "Completely true", 4, NA)))))
innetwork_quant$NRELATEDNESS <- (innetwork_quant$NR_IMPPRT + innetwork_quant$NR_CONNCT + innetwork_quant$NR_SPIRIT + 
                                   innetwork_quant$NR_NOTICE + innetwork_quant$NR_VACSPT + innetwork_quant$NR_AFFECT)/6
innetwork_quant$PIDENTITY_IDENTY <- ifelse(innetwork_quant$PIDENTITY_IDENTY == "Not true at all", 0,
                                           ifelse(innetwork_quant$PIDENTITY_IDENTY == "A little true", 1,
                                                  ifelse(innetwork_quant$PIDENTITY_IDENTY == "Moderately true", 2,
                                                         ifelse(innetwork_quant$PIDENTITY_IDENTY == "Very true", 3,
                                                                ifelse(innetwork_quant$PIDENTITY_IDENTY == "Completely true", 4, NA)))))
innetwork_quant$PIDENTITY_SPCIAL <- ifelse(innetwork_quant$PIDENTITY_SPCIAL == "Not true at all", 0,
                                           ifelse(innetwork_quant$PIDENTITY_SPCIAL == "A little true", 1,
                                                  ifelse(innetwork_quant$PIDENTITY_SPCIAL == "Moderately true", 2,
                                                         ifelse(innetwork_quant$PIDENTITY_SPCIAL == "Very true", 3,
                                                                ifelse(innetwork_quant$PIDENTITY_SPCIAL == "Completely true", 4, NA)))))
innetwork_quant$PIDENTITY_ATTCHD <- ifelse(innetwork_quant$PIDENTITY_ATTCHD == "Not true at all", 0,
                                           ifelse(innetwork_quant$PIDENTITY_ATTCHD == "A little true", 1,
                                                  ifelse(innetwork_quant$PIDENTITY_ATTCHD == "Moderately true", 2,
                                                         ifelse(innetwork_quant$PIDENTITY_ATTCHD == "Very true", 3,
                                                                ifelse(innetwork_quant$PIDENTITY_ATTCHD == "Completely true", 4, NA)))))
innetwork_quant$PIDENTITY <- (innetwork_quant$PIDENTITY_IDENTY + innetwork_quant$PIDENTITY_SPCIAL + innetwork_quant$PIDENTITY_ATTCHD)/3
innetwork_quant$SOLASTALGIA_BELONG <- ifelse(innetwork_quant$SOLASTALGIA_BELONG == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_BELONG == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_BELONG == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_BELONG == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_BELONG == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_LOSSES <- ifelse(innetwork_quant$SOLASTALGIA_LOSSES == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_LOSSES == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_LOSSES == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_LOSSES == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_LOSSES == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_ASHAMD <- ifelse(innetwork_quant$SOLASTALGIA_ASHAMD == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_ASHAMD == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_ASHAMD == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_ASHAMD == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_ASHAMD == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_DISAPR <- ifelse(innetwork_quant$SOLASTALGIA_DISAPR == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_DISAPR == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_DISAPR == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_DISAPR == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_DISAPR == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_UQLOSS <- ifelse(innetwork_quant$SOLASTALGIA_UQLOSS == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_UQLOSS == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_UQLOSS == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_UQLOSS == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_UQLOSS == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_PEACEQ <- ifelse(innetwork_quant$SOLASTALGIA_PEACEQ == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_PEACEQ == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_PEACEQ == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_PEACEQ == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_PEACEQ == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_DEGRAD <- ifelse(innetwork_quant$SOLASTALGIA_DEGRAD == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_DEGRAD == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_DEGRAD == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_DEGRAD == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_DEGRAD == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_LFSTYL <- ifelse(innetwork_quant$SOLASTALGIA_LFSTYL == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_LFSTYL == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_LFSTYL == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_LFSTYL == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_LFSTYL == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA_FLEAVE <- ifelse(innetwork_quant$SOLASTALGIA_FLEAVE == "Not true at all", 0,
                                             ifelse(innetwork_quant$SOLASTALGIA_FLEAVE == "A little true", 1,
                                                    ifelse(innetwork_quant$SOLASTALGIA_FLEAVE == "Moderately true", 2,
                                                           ifelse(innetwork_quant$SOLASTALGIA_FLEAVE == "Very true", 3,
                                                                  ifelse(innetwork_quant$SOLASTALGIA_FLEAVE == "Completely true", 4, NA)))))
innetwork_quant$SOLASTALGIA <- (innetwork_quant$SOLASTALGIA_LOSSES + innetwork_quant$SOLASTALGIA_ASHAMD + 
                                  innetwork_quant$SOLASTALGIA_DISAPR + innetwork_quant$SOLASTALGIA_UQLOSS + innetwork_quant$SOLASTALGIA_PEACEQ + 
                                  innetwork_quant$SOLASTALGIA_DEGRAD + innetwork_quant$SOLASTALGIA_LFSTYL)/7
innetwork_quant$HOPEPATH_WAYFIX <- ifelse(innetwork_quant$HOPEPATH_WAYFIX == "Not true at all", 0,
                                          ifelse(innetwork_quant$HOPEPATH_WAYFIX == "A little true", 1,
                                                 ifelse(innetwork_quant$HOPEPATH_WAYFIX == "Moderately true", 2,
                                                        ifelse(innetwork_quant$HOPEPATH_WAYFIX == "Very true", 3,
                                                               ifelse(innetwork_quant$HOPEPATH_WAYFIX == "Completely true", 4, NA)))))
innetwork_quant$HOPEPATH_KNOWLG <- ifelse(innetwork_quant$HOPEPATH_KNOWLG == "Not true at all", 0,
                                          ifelse(innetwork_quant$HOPEPATH_KNOWLG == "A little true", 1,
                                                 ifelse(innetwork_quant$HOPEPATH_KNOWLG == "Moderately true", 2,
                                                        ifelse(innetwork_quant$HOPEPATH_KNOWLG == "Very true", 3,
                                                               ifelse(innetwork_quant$HOPEPATH_KNOWLG == "Completely true", 4, NA)))))
innetwork_quant$HOPEPATH_CANFIX <- ifelse(innetwork_quant$HOPEPATH_CANFIX == "Not true at all", 0,
                                          ifelse(innetwork_quant$HOPEPATH_CANFIX == "A little true", 1,
                                                 ifelse(innetwork_quant$HOPEPATH_CANFIX == "Moderately true", 2,
                                                        ifelse(innetwork_quant$HOPEPATH_CANFIX == "Very true", 3,
                                                               ifelse(innetwork_quant$HOPEPATH_CANFIX == "Completely true", 4, NA)))))
innetwork_quant$HOPEPATH_RESOLV <- ifelse(innetwork_quant$HOPEPATH_RESOLV == "Not true at all", 0,
                                          ifelse(innetwork_quant$HOPEPATH_RESOLV == "A little true", 1,
                                                 ifelse(innetwork_quant$HOPEPATH_RESOLV == "Moderately true", 2,
                                                        ifelse(innetwork_quant$HOPEPATH_RESOLV == "Very true", 3,
                                                               ifelse(innetwork_quant$HOPEPATH_RESOLV == "Completely true", 4, NA)))))
innetwork_quant$HOPEPATH_ENOUGH <- ifelse(innetwork_quant$HOPEPATH_ENOUGH == "Not true at all", 0,
                                          ifelse(innetwork_quant$HOPEPATH_ENOUGH == "A little true", 1,
                                                 ifelse(innetwork_quant$HOPEPATH_ENOUGH == "Moderately true", 2,
                                                        ifelse(innetwork_quant$HOPEPATH_ENOUGH == "Very true", 3,
                                                               ifelse(innetwork_quant$HOPEPATH_ENOUGH == "Completely true", 4, NA)))))
innetwork_quant$HOPEPATH_SELFEFF <- (innetwork_quant$HOPEPATH_WAYFIX + innetwork_quant$HOPEPATH_KNOWLG)/2
innetwork_quant$HOPEPATH_RESPEFF <- (innetwork_quant$HOPEPATH_CANFIX + innetwork_quant$HOPEPATH_ENOUGH)/2
innetwork_quant$ATTITUDE_SAFOUT <- ifelse(innetwork_quant$ATTITUDE_SAFOUT == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_SAFOUT == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_SAFOUT == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_SAFOUT == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_SAFOUT == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_OUTWEL <- ifelse(innetwork_quant$ATTITUDE_OUTWEL == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_OUTWEL == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_OUTWEL == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_OUTWEL == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_OUTWEL == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_SWMSAF <- ifelse(innetwork_quant$ATTITUDE_SWMSAF == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_SWMSAF == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_SWMSAF == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_SWMSAF == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_SWMSAF == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_CHNGFW <- ifelse(innetwork_quant$ATTITUDE_CHNGFW == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_CHNGFW == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_CHNGFW == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_CHNGFW == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_CHNGFW == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_CHNGRN <- ifelse(innetwork_quant$ATTITUDE_CHNGRN == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_CHNGRN == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_CHNGRN == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_CHNGRN == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_CHNGRN == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_CHNGTP <- ifelse(innetwork_quant$ATTITUDE_CHNGTP == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_CHNGTP == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_CHNGTP == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_CHNGTP == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_CHNGTP == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_CHNGHS <- ifelse(innetwork_quant$ATTITUDE_CHNGHS == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_CHNGHS == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_CHNGHS == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_CHNGHS == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_CHNGHS == "Completely true", 4, NA)))))
innetwork_quant$ATTITUDE_CCTHRT <- ifelse(innetwork_quant$ATTITUDE_CCTHRT == "Not true at all", 0,
                                          ifelse(innetwork_quant$ATTITUDE_CCTHRT == "A little true", 1,
                                                 ifelse(innetwork_quant$ATTITUDE_CCTHRT == "Moderately true", 2,
                                                        ifelse(innetwork_quant$ATTITUDE_CCTHRT == "Very true", 3,
                                                               ifelse(innetwork_quant$ATTITUDE_CCTHRT == "Completely true", 4, NA)))))


### ACTIVITIES/BEHAVIORS ###

innetwork_quant$ACTIVITY_CMPHIK <- ifelse(innetwork_quant$ACTIVITY_CMPHIK == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_CMPHIK == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_CMPHIK == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_CMPHIK == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_CMPHIK == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_PADBOT <- ifelse(innetwork_quant$ACTIVITY_PADBOT == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_PADBOT == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_PADBOT == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_PADBOT == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_PADBOT == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_SWIMDV <- ifelse(innetwork_quant$ACTIVITY_SWIMDV == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_SWIMDV == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_SWIMDV == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_SWIMDV == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_SWIMDV == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_BWATCH <- ifelse(innetwork_quant$ACTIVITY_BWATCH == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_BWATCH == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_BWATCH == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_BWATCH == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_BWATCH == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_GARDEN <- ifelse(innetwork_quant$ACTIVITY_GARDEN == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_GARDEN == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_GARDEN == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_GARDEN == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_GARDEN == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_FISHNG <- ifelse(innetwork_quant$ACTIVITY_FISHNG == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_FISHNG == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_FISHNG == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_FISHNG == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_FISHNG == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_EXRCSO <- ifelse(innetwork_quant$ACTIVITY_EXRCSO == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_EXRCSO == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_EXRCSO == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_EXRCSO == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_EXRCSO == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_RELAXO <- ifelse(innetwork_quant$ACTIVITY_RELAXO == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_RELAXO == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_RELAXO == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_RELAXO == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_RELAXO == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_PICNIC <- ifelse(innetwork_quant$ACTIVITY_PICNIC == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_PICNIC == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_PICNIC == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_PICNIC == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_PICNIC == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_PLYGND <- ifelse(innetwork_quant$ACTIVITY_PLYGND == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_PLYGND == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_PLYGND == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_PLYGND == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_PLYGND == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_ZOOAQU <- ifelse(innetwork_quant$ACTIVITY_ZOOAQU == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_ZOOAQU == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_ZOOAQU == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_ZOOAQU == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_ZOOAQU == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_LSNPRK <- ifelse(innetwork_quant$ACTIVITY_LSNPRK == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_LSNPRK == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_LSNPRK == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_LSNPRK == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_LSNPRK == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_HSTSIT <- ifelse(innetwork_quant$ACTIVITY_HSTSIT == "Never", 0,
                                          ifelse(innetwork_quant$ACTIVITY_HSTSIT == "A few times", 1,
                                                 ifelse(innetwork_quant$ACTIVITY_HSTSIT == "About once a month", 2,
                                                        ifelse(innetwork_quant$ACTIVITY_HSTSIT == "About once a week", 3,
                                                               ifelse(innetwork_quant$ACTIVITY_HSTSIT == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_BEACH <- ifelse(innetwork_quant$ACTIVITY_BEACH == "Never", 0,
                                         ifelse(innetwork_quant$ACTIVITY_BEACH == "A few times", 1,
                                                ifelse(innetwork_quant$ACTIVITY_BEACH == "About once a month", 2,
                                                       ifelse(innetwork_quant$ACTIVITY_BEACH == "About once a week", 3,
                                                              ifelse(innetwork_quant$ACTIVITY_BEACH == "Multiple times per week", 4, NA)))))
innetwork_quant$ACTIVITY_PIER <- ifelse(innetwork_quant$ACTIVITY_PIER == "Never", 0,
                                        ifelse(innetwork_quant$ACTIVITY_PIER == "A few times", 1,
                                               ifelse(innetwork_quant$ACTIVITY_PIER == "About once a month", 2,
                                                      ifelse(innetwork_quant$ACTIVITY_PIER == "About once a week", 3,
                                                             ifelse(innetwork_quant$ACTIVITY_PIER == "Multiple times per week", 4, NA)))))

# Measure active vs passive nature engagement
innetwork_quant$ACTIVE_COUNT <- rowSums(innetwork_quant[56:61] > 0)
innetwork_quant$PASSIVE_COUNT <- rowSums(innetwork_quant[62:64] > 0)
innetwork_quant$ATTRACT_COUNT <- rowSums(innetwork_quant[65:70] > 0)
innetwork_quant$ACTIVE_AVG <- rowMeans(innetwork_quant[56:61])
innetwork_quant$PASSIVE_AVG <- rowMeans(innetwork_quant[62:64])
innetwork_quant$ATTRACT_AVG <- rowMeans(innetwork_quant[65:70])


innetwork_quant$BEHAVIOR_FERTLZ <- ifelse(innetwork_quant$BEHAVIOR_FERTLZ == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_FERTLZ == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_FERTLZ == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_FERTLZ == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_FERTLZ == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_FERTLZweight <- ifelse(is.na(innetwork$BEHAVIOR_FERTLZ), NA, 
                                                ifelse(innetwork_quant$FERTLZ_SUMMER == 1, innetwork_quant$BEHAVIOR_FERTLZ * 2, innetwork$BEHAVIOR_FERTLZ))
innetwork_quant$BEHAVIOR_WATER <- ifelse(innetwork_quant$BEHAVIOR_WATER == "Never", 0,
                                         ifelse(innetwork_quant$BEHAVIOR_WATER == "A few times", 1,
                                                ifelse(innetwork_quant$BEHAVIOR_WATER == "About once a month", 2,
                                                       ifelse(innetwork_quant$BEHAVIOR_WATER == "About once a week", 3,
                                                              ifelse(innetwork_quant$BEHAVIOR_WATER == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_CLPPNG <- ifelse(innetwork_quant$BEHAVIOR_CLPPNG == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_CLPPNG == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_CLPPNG == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_CLPPNG == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_CLPPNG == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_GRNINF <- ifelse(innetwork_quant$BEHAVIOR_GRNINF == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_GRNINF == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_GRNINF == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_GRNINF == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_GRNINF == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_NODRIV <- ifelse(innetwork_quant$BEHAVIOR_NODRIV == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_NODRIV == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_NODRIV == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_NODRIV == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_NODRIV == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_RECYCL <- ifelse(innetwork_quant$BEHAVIOR_RECYCL == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_RECYCL == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_RECYCL == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_RECYCL == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_RECYCL == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_PLNTFF <- ifelse(innetwork_quant$BEHAVIOR_PLNTFF == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_PLNTFF == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_PLNTFF == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_PLNTFF == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_PLNTFF == "Multiple times per week", 4, NA)))))
innetwork_quant$BEHAVIOR_RESTOR <- ifelse(innetwork_quant$BEHAVIOR_RESTOR == "Never", 0,
                                          ifelse(innetwork_quant$BEHAVIOR_RESTOR == "A few times", 1,
                                                 ifelse(innetwork_quant$BEHAVIOR_RESTOR == "About once a month", 2,
                                                        ifelse(innetwork_quant$BEHAVIOR_RESTOR == "About once a week", 3,
                                                               ifelse(innetwork_quant$BEHAVIOR_RESTOR == "Multiple times per week", 4, NA)))))

### NORMS ###

innetwork_quant$NORM_FERTLZ <- ifelse(innetwork_quant$NORM_FERTLZ == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_FERTLZ == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_FERTLZ == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_FERTLZ == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_FERTLZ == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_WATER <- ifelse(innetwork_quant$NORM_WATER == "Much less than I do", -2,
                                     ifelse(innetwork_quant$NORM_WATER == "A little less than I do", -1,
                                            ifelse(innetwork_quant$NORM_WATER == "About the same as I do", 0,
                                                   ifelse(innetwork_quant$NORM_WATER == "A little more than I do", 1,
                                                          ifelse(innetwork_quant$NORM_WATER == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_CLPPNG <- ifelse(innetwork_quant$NORM_CLPPNG == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_CLPPNG == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_CLPPNG == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_CLPPNG == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_CLPPNG == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_GRNINF <- ifelse(innetwork_quant$NORM_GRNINF == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_GRNINF == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_GRNINF == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_GRNINF == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_GRNINF == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_NODRIV <- ifelse(innetwork_quant$NORM_NODRIV == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_NODRIV == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_NODRIV == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_NODRIV == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_NODRIV == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_RECYCL <- ifelse(innetwork_quant$NORM_RECYCL == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_RECYCL == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_RECYCL == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_RECYCL == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_RECYCL == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_PLNTFF <- ifelse(innetwork_quant$NORM_PLNTFF == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_PLNTFF == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_PLNTFF == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_PLNTFF == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_PLNTFF == "Much more than I do", 2, NA)))))
innetwork_quant$NORM_RESTOR <- ifelse(innetwork_quant$NORM_RESTOR == "Much less than I do", -2,
                                      ifelse(innetwork_quant$NORM_RESTOR == "A little less than I do", -1,
                                             ifelse(innetwork_quant$NORM_RESTOR == "About the same as I do", 0,
                                                    ifelse(innetwork_quant$NORM_RESTOR == "A little more than I do", 1,
                                                           ifelse(innetwork_quant$NORM_RESTOR == "Much more than I do", 2, NA)))))


### GROUP INVOLVEMENT ###

innetwork_quant$GROUP_ARTCLT <- ifelse(innetwork_quant$GROUP_ARTCLT == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_ARTCLT == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_ARTCLT == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_ENVPRT <- ifelse(innetwork_quant$GROUP_ENVPRT == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_ENVPRT == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_ENVPRT == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_HOANBH <- ifelse(innetwork_quant$GROUP_HOANBH == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_HOANBH == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_HOANBH == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_POLTCL <- ifelse(innetwork_quant$GROUP_POLTCL == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_POLTCL == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_POLTCL == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_SPIRIT <- ifelse(innetwork_quant$GROUP_SPIRIT == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_SPIRIT == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_SPIRIT == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_CHLPAR <- ifelse(innetwork_quant$GROUP_CHLPAR == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_CHLPAR == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_CHLPAR == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_SPORTS <- ifelse(innetwork_quant$GROUP_SPORTS == "No, not involved", 0,
                                       ifelse(innetwork_quant$GROUP_SPORTS == "Yes, a little involved", 1,
                                              ifelse(innetwork_quant$GROUP_SPORTS == "Yes, very involved", 2, NA)))
innetwork_quant$GROUP_INVOLVEMENT <- (innetwork_quant$GROUP_ARTCLT + innetwork_quant$GROUP_CHLPAR + innetwork_quant$GROUP_ENVPRT +
                                        innetwork_quant$GROUP_HOANBH + innetwork_quant$GROUP_POLTCL + innetwork_quant$GROUP_SPIRIT + 
                                        innetwork_quant$GROUP_SPORTS)/7


### MENTAL HEALTH ###

innetwork_quant$MHI_A1 <- ifelse(innetwork_quant$MHI_A1 == "None of the time", 0,
                                 ifelse(innetwork_quant$MHI_A1 == "A little of the time", 1,
                                        ifelse(innetwork_quant$MHI_A1 == "Some of the time", 2,
                                               ifelse(innetwork_quant$MHI_A1 == "A good bit of the time", 3,
                                                      ifelse(innetwork_quant$MHI_A1 == "Most of the time", 4,
                                                             ifelse(innetwork_quant$MHI_A1 == "All of the time", 5, NA))))))
innetwork_quant$MHI_A2 <- ifelse(innetwork_quant$MHI_A2 == "None of the time", 0,
                                 ifelse(innetwork_quant$MHI_A2 == "A little of the time", 1,
                                        ifelse(innetwork_quant$MHI_A2 == "Some of the time", 2,
                                               ifelse(innetwork_quant$MHI_A2 == "A good bit of the time", 3,
                                                      ifelse(innetwork_quant$MHI_A2 == "Most of the time", 4,
                                                             ifelse(innetwork_quant$MHI_A2 == "All of the time", 5, NA))))))
innetwork_quant$MHI_D1 <- ifelse(innetwork_quant$MHI_D1 == "None of the time", 0,
                                 ifelse(innetwork_quant$MHI_D1 == "A little of the time", 1,
                                        ifelse(innetwork_quant$MHI_D1 == "Some of the time", 2,
                                               ifelse(innetwork_quant$MHI_D1 == "A good bit of the time", 3,
                                                      ifelse(innetwork_quant$MHI_D1 == "Most of the time", 4,
                                                             ifelse(innetwork_quant$MHI_D1 == "All of the time", 5, NA))))))
innetwork_quant$MHI_D2 <- ifelse(innetwork_quant$MHI_D2 == "None of the time", 0,
                                 ifelse(innetwork_quant$MHI_D2 == "A little of the time", 1,
                                        ifelse(innetwork_quant$MHI_D2 == "Some of the time", 2,
                                               ifelse(innetwork_quant$MHI_D2 == "A good bit of the time", 3,
                                                      ifelse(innetwork_quant$MHI_D2 == "Most of the time", 4,
                                                             ifelse(innetwork_quant$MHI_D2 == "All of the time", 5, NA))))))
innetwork_quant$MHI_D3 <- ifelse(innetwork_quant$MHI_D3 == "None of the time", 0,
                                 ifelse(innetwork_quant$MHI_D3 == "A little of the time", 1,
                                        ifelse(innetwork_quant$MHI_D3 == "Some of the time", 2,
                                               ifelse(innetwork_quant$MHI_D3 == "A good bit of the time", 3,
                                                      ifelse(innetwork_quant$MHI_D3 == "Most of the time", 4,
                                                             ifelse(innetwork_quant$MHI_D3 == "All of the time", 5, NA))))))
innetwork_quant$MHI_ANXIETY <- innetwork_quant$MHI_A1 + innetwork_quant$MHI_A2
innetwork_quant$MHI_DEPRESN <- innetwork_quant$MHI_D1 + innetwork_quant$MHI_D2 + innetwork_quant$MHI_D3
innetwork_quant$MHI_SCORE <- innetwork_quant$MHI_A1 + innetwork_quant$MHI_A2 + innetwork_quant$MHI_D1 + innetwork_quant$MHI_D2 + innetwork_quant$MHI_D3
innetwork_quant$LIFESATISFACTION <- ifelse(innetwork_quant$LIFESATISFACTION == "None of the time", 0,
                                           ifelse(innetwork_quant$LIFESATISFACTION == "A little of the time", 1,
                                                  ifelse(innetwork_quant$LIFESATISFACTION == "Some of the time", 2,
                                                         ifelse(innetwork_quant$LIFESATISFACTION == "A good bit of the time", 3,
                                                                ifelse(innetwork_quant$LIFESATISFACTION == "Most of the time", 4,
                                                                       ifelse(innetwork_quant$LIFESATISFACTION == "All of the time", 5, NA))))))
innetwork_quant$MHI <- (innetwork_quant$MHI_A1 + innetwork_quant$MHI_A2 + innetwork_quant$MHI_D1 + innetwork_quant$MHI_D2 + innetwork_quant$MHI_D3)/5
innetwork_quant$SWELLBEING <- (innetwork_quant$MHI_A1 + innetwork_quant$MHI_A2 + innetwork_quant$MHI_D1 + innetwork_quant$MHI_D2 + innetwork_quant$MHI_D3 + innetwork_quant$LIFESATISFACTION)/6
innetwork_quant$ECOANXIETY_1 <- ifelse(innetwork_quant$ECOANXIETY_1 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_1 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_1 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_1 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_2 <- ifelse(innetwork_quant$ECOANXIETY_2 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_2 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_2 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_2 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_3 <- ifelse(innetwork_quant$ECOANXIETY_3 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_3 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_3 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_3 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_4 <- ifelse(innetwork_quant$ECOANXIETY_4 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_4 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_4 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_4 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_5 <- ifelse(innetwork_quant$ECOANXIETY_5 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_5 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_5 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_5 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_6 <- ifelse(innetwork_quant$ECOANXIETY_6 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_6 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_6 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_6 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_7 <- ifelse(innetwork_quant$ECOANXIETY_7 == "Not at all", 0,
                                       ifelse(innetwork_quant$ECOANXIETY_7 == "Several days", 1,
                                              ifelse(innetwork_quant$ECOANXIETY_7 == "More than half the days", 2,
                                                     ifelse(innetwork_quant$ECOANXIETY_7 == "Nearly every day", 3, NA))))
innetwork_quant$ECOANXIETY_SCORE <- innetwork_quant$ECOANXIETY_1 + innetwork_quant$ECOANXIETY_2 + innetwork_quant$ECOANXIETY_3 + innetwork_quant$ECOANXIETY_4 + innetwork_quant$ECOANXIETY_5 + innetwork_quant$ECOANXIETY_6 + innetwork_quant$ECOANXIETY_7
innetwork_quant$ECOANXIETY_CAT <- ifelse(innetwork_quant$ECOANXIETY_SCORE < 5, "Low",
                                         ifelse(innetwork_quant$ECOANXIETY_SCORE >= 5 & innetwork_quant$ECOANXIETY_SCORE < 10, "Mild",
                                                ifelse(innetwork_quant$ECOANXIETY_SCORE >= 10 & innetwork_quant$ECOANXIETY_SCORE < 15, "Moderate",
                                                       ifelse(innetwork_quant$ECOANXIETY_SCORE >= 15, "Severe", NA))))

### DEMOGRAPHICS ###

innetwork_quant$CONSERVATIVE <- ifelse(innetwork_quant$POLITICS == "Strongly liberal", -3,
                                       ifelse(innetwork_quant$POLITICS == "Moderately liberal", -2,
                                              ifelse(innetwork_quant$POLITICS == "Slightly liberal", -1,
                                                     ifelse(innetwork_quant$POLITICS == "Neutral", 0,
                                                            ifelse(innetwork_quant$POLITICS == "Slightly conservative", 1,
                                                                   ifelse(innetwork_quant$POLITICS == "Moderately conservative", 2, 
                                                                          ifelse(innetwork_quant$POLITICS == "Strongly conservative", 3, NA)))))))


######### * DATA CLEANING ######### 

# Match Street Intersection coordinates to each respondent by Response ID
innetwork <- merge(innetwork, innetwork_coords, by = "ResponseId", all.x = TRUE)
innetwork_quant <- merge(innetwork_quant, innetwork_coords, by = "ResponseId", all.x = TRUE)

innetwork$INCLUDE <- ifelse(innetwork$INCLUDE == "No", "No", 
                            ifelse(is.na(innetwork$POINT_X) & innetwork$QC_FRAUDULENT == "Caution", "No", 
                                   ifelse(is.na(innetwork$POINT_X) & is.na(innetwork$QC_FRAUDULENT), "No", "Yes")))

innetwork %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# No                  19
# Yes                 178
# <NA>                2

innetwork_quant$INCLUDE <- ifelse(innetwork_quant$INCLUDE == "No", "No", 
                                  ifelse(is.na(innetwork_quant$POINT_X) & innetwork_quant$QC_FRAUDULENT == "Caution", "No", 
                                         ifelse(is.na(innetwork_quant$POINT_X) & is.na(innetwork_quant$QC_FRAUDULENT), "No", "Yes")))
innetwork_quant %>%
  count(INCLUDE)

innetwork_clean <- subset(innetwork, INCLUDE != "No")
innetwork_quant_clean <- subset(innetwork_quant, INCLUDE != "No")

# keep/reorder the variables we need
innetwork_final <- innetwork_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                       "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                       "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                       "POLITICS","YEARS","PROPERTY","OWNERSHIP",
                                       "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                       "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                       "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                       "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                       "NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                       "PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                       "SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                       "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                       "HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                       "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                       "ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                       "JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                       "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                       "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                       "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                       "GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                       "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                       "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                       "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                       "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                       "FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                       "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                       "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                       "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                       "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                       "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                       "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                       "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                       "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN","REFERRAL")]

innetwork_quant_final <- innetwork_quant_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                                   "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                                   "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                                   "POLITICS","CONSERVATIVE","YEARS","PROPERTY","OWNERSHIP",
                                                   "PRIORITIZE_ENVIRON","RANK_ENVIRON","PRIORITY_ENVIRON","PRIORITY_ECONOMY","PRIORITY_INFRAST","PRIORITY_SOCSERV",
                                                   "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                                   "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                                   "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                                   "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                                   "NRELATEDNESS","NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                                   "PIDENTITY","PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                                   "SOLASTALGIA","SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                                   "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                                   "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                                   "SWELLBEING","MHI","MHI_SCORE","MHI_ANXIETY","MHI_DEPRESN",
                                                   "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                                   "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                                   "JUSTICE","JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                                   "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                                   "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                                   "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                                   "GROUP_INVOLVEMENT","GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                                   "ACTIVE_COUNT","PASSIVE_COUNT","ATTRACT_COUNT","ACTIVE_AVG","PASSIVE_AVG","ATTRACT_AVG",
                                                   "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                                   "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                                   "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                                   "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                                   "BEHAVIOR_FERTLZweight","FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                                   "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                                   "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                                   "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                                   "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                                   "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                                   "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                                   "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                                   "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN","REFERRAL")]

innetwork_final$AUDIENCE <- "In-network"
innetwork_quant_final$AUDIENCE <- "In-network"

# This creates the final quantitative and qualitative datasets for the in-network respondents.
# This data has already been merged with other audiences in the final datasets.









######### PUBLIC (MAIL) ######### 

# NOTE: Data management tasks performed prior to running this script:
#   - Validation and mapping of street intersection data
#   - "Other" ethnicity response categorization


# import data (with amended column names done in Excel)
public_mail <- read.csv("tbcs-2024/survey_data/raw/public_mail.csv")
public_mail_coords <- read.csv("tbcs-2024/survey_data/raw/public_mail_coords.csv")


######### * DATA CORRECTIONS ######### 

### Convert blank cells to NAs ###

public_mail$OWNERSHIP_OTHER_TEXT <- ifelse(public_mail$OWNERSHIP_OTHER_TEXT == "" | public_mail$OWNERSHIP_OTHER_TEXT == " ", NA, public_mail$OWNERSHIP_OTHER_TEXT)
public_mail$YEARS_RESIDENT <- ifelse(public_mail$YEARS_RESIDENT == "" | public_mail$YEARS_RESIDENT == " ", NA, public_mail$YEARS_RESIDENT)
public_mail$YEARS_UNSURE <- ifelse(public_mail$YEARS_UNSURE == "" | public_mail$YEARS_UNSURE == " ", NA, public_mail$YEARS_UNSURE)
public_mail$BEHAVIOR_FERTLZ <- ifelse(public_mail$BEHAVIOR_FERTLZ == "" | public_mail$BEHAVIOR_FERTLZ == " ", NA, public_mail$BEHAVIOR_FERTLZ)
public_mail$BEHAVIOR_WATER <- ifelse(public_mail$BEHAVIOR_WATER == "" | public_mail$BEHAVIOR_WATER == " ", NA, public_mail$BEHAVIOR_WATER)
public_mail$BEHAVIOR_CLPPNG <- ifelse(public_mail$BEHAVIOR_CLPPNG == "" | public_mail$BEHAVIOR_CLPPNG == " ", NA, public_mail$BEHAVIOR_CLPPNG)
public_mail$BEHAVIOR_GRNINF <- ifelse(public_mail$BEHAVIOR_GRNINF == "" | public_mail$BEHAVIOR_GRNINF == " ", NA, public_mail$BEHAVIOR_GRNINF)
public_mail$NORM_FERTLZ <- ifelse(public_mail$NORM_FERTLZ == "" | public_mail$NORM_FERTLZ == " ", NA, public_mail$NORM_FERTLZ)
public_mail$NORM_WATER <- ifelse(public_mail$NORM_WATER == "" | public_mail$NORM_WATER == " ", NA, public_mail$NORM_WATER)
public_mail$NORM_CLPPNG <- ifelse(public_mail$NORM_CLPPNG == "" | public_mail$NORM_CLPPNG == " ", NA, public_mail$NORM_CLPPNG)
public_mail$NORM_GRNINF <- ifelse(public_mail$NORM_GRNINF == "" | public_mail$NORM_GRNINF == " ", NA, public_mail$NORM_GRNINF)
public_mail$FERTLZ_SEASON <- ifelse(public_mail$FERTLZ_SEASON == "" | public_mail$FERTLZ_SEASON == " ", NA, public_mail$FERTLZ_SEASON)
public_mail$FISH_LOCATN <- ifelse(public_mail$FISH_LOCATN == "" | public_mail$FISH_LOCATN == " ", NA, public_mail$FISH_LOCATN)
public_mail$FISH_REASON <- ifelse(public_mail$FISH_REASON == "" | public_mail$FISH_REASON == " ", NA, public_mail$FISH_REASON)
public_mail$ECOANXIETY_1 <- ifelse(public_mail$ECOANXIETY_1 == "" | public_mail$ECOANXIETY_1 == " ", NA, public_mail$ECOANXIETY_1)
public_mail$ECOANXIETY_2 <- ifelse(public_mail$ECOANXIETY_2 == "" | public_mail$ECOANXIETY_2 == " ", NA, public_mail$ECOANXIETY_2)
public_mail$ECOANXIETY_3 <- ifelse(public_mail$ECOANXIETY_3 == "" | public_mail$ECOANXIETY_3 == " ", NA, public_mail$ECOANXIETY_3)
public_mail$ECOANXIETY_4 <- ifelse(public_mail$ECOANXIETY_4 == "" | public_mail$ECOANXIETY_4 == " ", NA, public_mail$ECOANXIETY_4)
public_mail$ECOANXIETY_5 <- ifelse(public_mail$ECOANXIETY_5 == "" | public_mail$ECOANXIETY_5 == " ", NA, public_mail$ECOANXIETY_5)
public_mail$ECOANXIETY_6 <- ifelse(public_mail$ECOANXIETY_6 == "" | public_mail$ECOANXIETY_6 == " ", NA, public_mail$ECOANXIETY_6)
public_mail$ECOANXIETY_7 <- ifelse(public_mail$ECOANXIETY_7 == "" | public_mail$ECOANXIETY_7 == " ", NA, public_mail$ECOANXIETY_7)
public_mail$GENDER_OTHER_TEXT <- ifelse(public_mail$GENDER_OTHER_TEXT == "" | public_mail$GENDER_OTHER_TEXT == " ", NA, public_mail$GENDER_OTHER_TEXT)
public_mail$ETHNICITY_OTHER_TEXT <- ifelse(public_mail$ETHNICITY_OTHER_TEXT == "" | public_mail$ETHNICITY_OTHER_TEXT == " ", NA, public_mail$ETHNICITY_OTHER_TEXT)

### Combine ZIPCODE and YEARS fields ###

public_mail$ZIPCODE <- ifelse(is.na(public_mail$ZIPCODE_RESIDENT), public_mail$ZIPCODE_UNSURE, public_mail$ZIPCODE_RESIDENT)
public_mail$YEARS <- ifelse(is.na(public_mail$YEARS_RESIDENT), public_mail$YEARS_UNSURE, public_mail$YEARS_RESIDENT)


### Set Other classifications ###
# Review and reclassification of all "Other" text entries for the following variables:
#     - OWNERSHIP
#     - GENDER

# GENDER
unique(public_mail$GENDER_OTHER_TEXT)
public_mail$GENDER <- ifelse(public_mail$GENDER == "Other description:", "Non-binary", public_mail$GENDER)

# OWNERSHIP
unique(public_mail$OWNERSHIP_OTHER_TEXT)

familyowned <- c("Live with family","Live with relatives","Live with parents ","live with family",
                 "Parents","Stay with family ","Live with owner","Parents own","Live with parents",
                 "Parents signed financing for my RV","Mother owns it","It's my grandmother's house ",
                 "Family owned I camp here ")
lotrental <- c("rent lot","pay lot rent","Family member rents","own home pay lot rent")
public_mail$OWNERSHIP <- ifelse(public_mail$OWNERSHIP_OTHER_TEXT %in% familyowned, "Own",
                                ifelse(public_mail$OWNERSHIP_OTHER_TEXT %in% lotrental, "Rent",
                                       ifelse(public_mail$OWNERSHIP == "Other arrangement:", "Other", public_mail$OWNERSHIP)))


### Reverse coding ###

public_mail$JUSTICE_DIST <- ifelse(public_mail$JUSTICE_DIST.R == "Not true at all", "Completely true",
                                   ifelse(public_mail$JUSTICE_DIST.R == "A little true", "Very true",
                                          ifelse(public_mail$JUSTICE_DIST.R == "Moderately true", "Moderately true",
                                                 ifelse(public_mail$JUSTICE_DIST.R == "Very true", "A little true",
                                                        ifelse(public_mail$JUSTICE_DIST.R == "Completely true", "Not true at all", NA)))))
public_mail$JUSTICE_PRO <- ifelse(public_mail$JUSTICE_PRO.R == "Not true at all", "Completely true",
                                  ifelse(public_mail$JUSTICE_PRO.R == "A little true", "Very true",
                                         ifelse(public_mail$JUSTICE_PRO.R == "Moderately true", "Moderately true",
                                                ifelse(public_mail$JUSTICE_PRO.R == "Very true", "A little true",
                                                       ifelse(public_mail$JUSTICE_PRO.R == "Completely true", "Not true at all", NA)))))
public_mail$JUSTICE_REC <- ifelse(public_mail$JUSTICE_REC.R == "Not true at all", "Completely true",
                                  ifelse(public_mail$JUSTICE_REC.R == "A little true", "Very true",
                                         ifelse(public_mail$JUSTICE_REC.R == "Moderately true", "Moderately true",
                                                ifelse(public_mail$JUSTICE_REC.R == "Very true", "A little true",
                                                       ifelse(public_mail$JUSTICE_REC.R == "Completely true", "Not true at all", NA)))))
public_mail$JUSTICE_AGN <- ifelse(public_mail$JUSTICE_AGN.R == "Not true at all", "Completely true",
                                  ifelse(public_mail$JUSTICE_AGN.R == "A little true", "Very true",
                                         ifelse(public_mail$JUSTICE_AGN.R == "Moderately true", "Moderately true",
                                                ifelse(public_mail$JUSTICE_AGN.R == "Very true", "A little true",
                                                       ifelse(public_mail$JUSTICE_AGN.R == "Completely true", "Not true at all", NA)))))
public_mail$ATTITUDE_BAYLNK <- ifelse(public_mail$ATTITUDE_BAYLNK.R == "Not true at all", "Completely true",
                                      ifelse(public_mail$ATTITUDE_BAYLNK.R == "A little true", "Very true",
                                             ifelse(public_mail$ATTITUDE_BAYLNK.R == "Moderately true", "Moderately true",
                                                    ifelse(public_mail$ATTITUDE_BAYLNK.R == "Very true", "A little true",
                                                           ifelse(public_mail$ATTITUDE_BAYLNK.R == "Completely true", "Not true at all", NA)))))
public_mail$HOPEPATH_CANFIX <- ifelse(public_mail$HOPEPATH_CANFIX.R == "Not true at all", "Completely true",
                                      ifelse(public_mail$HOPEPATH_CANFIX.R == "A little true", "Very true",
                                             ifelse(public_mail$HOPEPATH_CANFIX.R == "Moderately true", "Moderately true",
                                                    ifelse(public_mail$HOPEPATH_CANFIX.R == "Very true", "A little true",
                                                           ifelse(public_mail$HOPEPATH_CANFIX.R == "Completely true", "Not true at all", NA)))))
public_mail$HOPEPATH_ENOUGH <- ifelse(public_mail$HOPEPATH_ENOUGH.R == "Not true at all", "Completely true",
                                      ifelse(public_mail$HOPEPATH_ENOUGH.R == "A little true", "Very true",
                                             ifelse(public_mail$HOPEPATH_ENOUGH.R == "Moderately true", "Moderately true",
                                                    ifelse(public_mail$HOPEPATH_ENOUGH.R == "Very true", "A little true",
                                                           ifelse(public_mail$HOPEPATH_ENOUGH.R == "Completely true", "Not true at all", NA)))))
public_mail$ATTITUDE_OUTWEL <- ifelse(public_mail$ATTITUDE_OUTWEL.R == "Not true at all", "Completely true",
                                      ifelse(public_mail$ATTITUDE_OUTWEL.R == "A little true", "Very true",
                                             ifelse(public_mail$ATTITUDE_OUTWEL.R == "Moderately true", "Moderately true",
                                                    ifelse(public_mail$ATTITUDE_OUTWEL.R == "Very true", "A little true",
                                                           ifelse(public_mail$ATTITUDE_OUTWEL.R == "Completely true", "Not true at all", NA)))))
public_mail$ATTITUDE_SWMSAF <- ifelse(public_mail$ATTITUDE_SWMSAF.R == "Not true at all", "Completely true",
                                      ifelse(public_mail$ATTITUDE_SWMSAF.R == "A little true", "Very true",
                                             ifelse(public_mail$ATTITUDE_SWMSAF.R == "Moderately true", "Moderately true",
                                                    ifelse(public_mail$ATTITUDE_SWMSAF.R == "Very true", "A little true",
                                                           ifelse(public_mail$ATTITUDE_SWMSAF.R == "Completely true", "Not true at all", NA)))))
public_mail$MHI_A1 <- ifelse(public_mail$MHI_A1.R == "None of the time", "All of the time",
                             ifelse(public_mail$MHI_A1.R == "A little of the time", "Most of the time",
                                    ifelse(public_mail$MHI_A1.R == "Some of the time", "A good bit of the time",
                                           ifelse(public_mail$MHI_A1.R == "A good bit of the time", "Some of the time",
                                                  ifelse(public_mail$MHI_A1.R == "Most of the time", "A little of the time",
                                                         ifelse(public_mail$MHI_A1.R == "All of the time", "None of the time", NA))))))
public_mail$MHI_D2 <- ifelse(public_mail$MHI_D2.R == "None of the time", "All of the time",
                             ifelse(public_mail$MHI_D2.R == "A little of the time", "Most of the time",
                                    ifelse(public_mail$MHI_D2.R == "Some of the time", "A good bit of the time",
                                           ifelse(public_mail$MHI_D2.R == "A good bit of the time", "Some of the time",
                                                  ifelse(public_mail$MHI_D2.R == "Most of the time", "A little of the time",
                                                         ifelse(public_mail$MHI_D2.R == "All of the time", "None of the time", NA))))))
public_mail$MHI_D3 <- ifelse(public_mail$MHI_D3.R == "None of the time", "All of the time",
                             ifelse(public_mail$MHI_D3.R == "A little of the time", "Most of the time",
                                    ifelse(public_mail$MHI_D3.R == "Some of the time", "A good bit of the time",
                                           ifelse(public_mail$MHI_D3.R == "A good bit of the time", "Some of the time",
                                                  ifelse(public_mail$MHI_D3.R == "Most of the time", "A little of the time",
                                                         ifelse(public_mail$MHI_D3.R == "All of the time", "None of the time", NA))))))


### Knowledge scoring ###

public_mail$KNOWLEDGE_WQTRND_Points <- ifelse(public_mail$KNOWLEDGE_WQTRND == "Improved", 1, 0)
public_mail$KNOWLEDGE_NUTRNT_Points <- ifelse(public_mail$KNOWLEDGE_NUTRNT == "Nitrogen", 1, 0)
public_mail$KNOWLEDGE_SOURCE_Points <- ifelse(public_mail$KNOWLEDGE_SOURCE == "Stormwater runoff", 1, 0)
public_mail$KNOWLEDGE_FRTSSN_Points <- ifelse(public_mail$KNOWLEDGE_FRTSSN == "Summer (June - September)", 1, 0)
public_mail$KNOWLEDGE_SGLOSS_Points <- ifelse(public_mail$KNOWLEDGE_SGLOSS == "Boat propeller blades,Nutrient pollution", 1, 
                                              ifelse(grepl("propeller", public_mail$KNOWLEDGE_SGLOSS) | grepl("Nutrient", public_mail$KNOWLEDGE_SGLOSS), 0.5, 0))
public_mail$KNOWLEDGE_WQTRND_Score <- public_mail$KNOWLEDGE_WQTRND_Points*(public_mail$KNOWLEDGE_WQTRND_C/10)
public_mail$KNOWLEDGE_NUTRNT_Score <- public_mail$KNOWLEDGE_NUTRNT_Points*(public_mail$KNOWLEDGE_NUTRNT_C/10)
public_mail$KNOWLEDGE_SOURCE_Score <- public_mail$KNOWLEDGE_SOURCE_Points*(public_mail$KNOWLEDGE_SOURCE_C/10)
public_mail$KNOWLEDGE_FRTSSN_Score <- public_mail$KNOWLEDGE_FRTSSN_Points*(public_mail$KNOWLEDGE_FRTSSN_C/10)
public_mail$KNOWLEDGE_SGLOSS_Score <- public_mail$KNOWLEDGE_SGLOSS_Points*(public_mail$KNOWLEDGE_SGLOSS_C/10)
public_mail$KNOWLEDGE_POINTS <- public_mail$KNOWLEDGE_WQTRND_Points + public_mail$KNOWLEDGE_NUTRNT_Points + public_mail$KNOWLEDGE_SOURCE_Points + public_mail$KNOWLEDGE_FRTSSN_Points + public_mail$KNOWLEDGE_SGLOSS_Points
public_mail$KNOWLEDGE_SCORE <- public_mail$KNOWLEDGE_WQTRND_Score + public_mail$KNOWLEDGE_NUTRNT_Score + public_mail$KNOWLEDGE_SOURCE_Score + public_mail$KNOWLEDGE_FRTSSN_Score + public_mail$KNOWLEDGE_SGLOSS_Score
public_mail$KNOWLEDGE_SCORE_Pct <- (public_mail$KNOWLEDGE_SCORE/5)*100


### Break up multiple selections into discrete answers ###

public_mail$FERTLZ_SPRING <- ifelse(is.na(public_mail$FERTLZ_SEASON), NA, ifelse(grepl("Spring", public_mail$FERTLZ_SEASON), 1, 0))
public_mail$FERTLZ_SUMMER <- ifelse(is.na(public_mail$FERTLZ_SEASON), NA, ifelse(grepl("Summer", public_mail$FERTLZ_SEASON), 1, 0))
public_mail$FERTLZ_FALL <- ifelse(is.na(public_mail$FERTLZ_SEASON), NA, ifelse(grepl("Fall", public_mail$FERTLZ_SEASON), 1, 0))
public_mail$FERTLZ_WINTER <- ifelse(is.na(public_mail$FERTLZ_SEASON), NA, ifelse(grepl("Winter", public_mail$FERTLZ_SEASON), 1, 0))
public_mail$FERTLZ_SEASON_N <- public_mail$FERTLZ_SPRING + public_mail$FERTLZ_SUMMER + public_mail$FERTLZ_FALL + public_mail$FERTLZ_WINTER

public_mail$FISH_LOC_SHORE <- ifelse(is.na(public_mail$FISH_LOCATN), NA, ifelse(grepl("shoreline", public_mail$FISH_LOCATN), 1, 0))
public_mail$FISH_LOC_PIER <- ifelse(is.na(public_mail$FISH_LOCATN), NA, ifelse(grepl("pier", public_mail$FISH_LOCATN), 1, 0))
public_mail$FISH_LOC_WATER <- ifelse(is.na(public_mail$FISH_LOCATN), NA, ifelse(grepl("boat", public_mail$FISH_LOCATN), 1, 0))
public_mail$FISH_RSN_SPORT <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("sport", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_RSN_RELAX <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("relaxation", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_RSN_FOOD <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("own", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_RSN_SELL <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("sell", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_RSN_WATCH <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("friends", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_RSN_SOCIAL <- ifelse(is.na(public_mail$FISH_REASON), NA, ifelse(grepl("anglers", public_mail$FISH_REASON), 1, 0))
public_mail$FISH_LOCATN_N <- public_mail$FISH_LOC_SHORE + public_mail$FISH_LOC_PIER + public_mail$FISH_LOC_WATER 
public_mail$FISH_REASON_N <- public_mail$FISH_RSN_SPORT + public_mail$FISH_RSN_RELAX + public_mail$FISH_RSN_FOOD + public_mail$FISH_RSN_SELL + public_mail$FISH_RSN_WATCH + public_mail$FISH_RSN_SOCIAL

public_mail$INFO_TV <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Television", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_RADIO <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Radio", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_SOCIAL <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Social", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_PERIOD <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Newspapers", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_MAILE <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("email", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_PUBLIC <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Public", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_GOV <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("Government websites", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_NGO <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("NGO", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_WORD <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("mouth", public_mail$INFOSOURCES), 1, 0))
public_mail$INFO_OWN <- ifelse(is.na(public_mail$INFOSOURCES), NA, ifelse(grepl("observations", public_mail$INFOSOURCES), 1, 0))
public_mail$INFOSOURCES_N <- public_mail$INFO_TV + public_mail$INFO_RADIO + public_mail$INFO_SOCIAL + public_mail$INFO_PERIOD + public_mail$INFO_MAILE + public_mail$INFO_PUBLIC + public_mail$INFO_GOV + public_mail$INFO_NGO + public_mail$INFO_PERIOD + public_mail$INFO_WORD + public_mail$INFO_OWN

public_mail$RACE_NATIVE <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Alaska", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_ASIAN <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Asian", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_BLACK <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Black", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_MIDEAST <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Middle", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_PACIFIC <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Pacific", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_WHITE <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("White", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_OTHER <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Other", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_HISPANIC <- ifelse(is.na(public_mail$ETHNICITY), NA, ifelse(grepl("Hispanic", public_mail$ETHNICITY), 1, 0))
public_mail$RACE_N <- ifelse(is.na(public_mail$ETHNICITY), NA, 
                             public_mail$RACE_NATIVE + public_mail$RACE_ASIAN + public_mail$RACE_BLACK + public_mail$RACE_MIDEAST + public_mail$RACE_PACIFIC + public_mail$RACE_WHITE + public_mail$RACE_OTHER + public_mail$RACE_HISPANIC)
public_mail$RACE <- ifelse(is.na(public_mail$ETHNICITY), NA,
                           ifelse(public_mail$RACE_N == 0, "Other",
                                  ifelse(public_mail$RACE_N > 1, "Multiple",
                                         ifelse(public_mail$RACE_NATIVE == 1 & public_mail$RACE_N == 1, "Other",
                                                ifelse(public_mail$RACE_ASIAN == 1 & public_mail$RACE_N == 1, "Asian",
                                                       ifelse(public_mail$RACE_BLACK == 1 & public_mail$RACE_N == 1, "Black",
                                                              ifelse(public_mail$RACE_MIDEAST == 1 & public_mail$RACE_N == 1, "Other",
                                                                     ifelse(public_mail$RACE_PACIFIC == 1 & public_mail$RACE_N == 1, "Other",
                                                                            ifelse(public_mail$RACE_WHITE == 1 & public_mail$RACE_N == 1, "White",
                                                                                   ifelse(public_mail$RACE_OTHER == 1 & public_mail$RACE_N == 1, "Other",
                                                                                          ifelse(public_mail$RACE_HISPANIC == 1 & public_mail$RACE_N == 1, "Hispanic",NA)))))))))))
public_mail$EMPLOY_FULL <- ifelse(is.na(public_mail$EMPLOYMENT), NA, ifelse(grepl("Full", public_mail$EMPLOYMENT), 1, 0))
public_mail$EMPLOY_PART <- ifelse(is.na(public_mail$EMPLOYMENT), NA, ifelse(grepl("Part", public_mail$EMPLOYMENT), 1, 0))
public_mail$EMPLOY_NONE <- ifelse(is.na(public_mail$EMPLOYMENT), NA, ifelse(grepl("Unemployed", public_mail$EMPLOYMENT), 1, 0))
public_mail$EMPLOYED <- ifelse(public_mail$EMPLOY_FULL == 1, "Full-time", 
                               ifelse(public_mail$EMPLOY_PART == 1, "Part-time", "Unemployed")) 
public_mail$EMPLOYED_STUDENT <- ifelse(is.na(public_mail$EMPLOYMENT), NA, ifelse(grepl("Student", public_mail$EMPLOYMENT), 1, 0))
public_mail$EMPLOYED_RETIRED <- ifelse(is.na(public_mail$EMPLOYMENT), NA, ifelse(grepl("Retired", public_mail$EMPLOYMENT), 1, 0))


######### * EXCLUSIONS ######### 

# flag responses that spent less than 5 minutes (300 seconds) on the survey
public_mail$QC_DURATION <- ifelse(public_mail$Status == "Survey Preview", "OK",
                                  ifelse(public_mail$Duration..in.seconds. < 300, "Remove", "OK"))

# flag responses that did not reach the end of the survey
public_mail$QC_FINISHED <- ifelse(public_mail$Finished == "False", "Remove", "OK")

# flag responses that did not pass attention checks
public_mail$QC_ATTENTION <- ifelse(public_mail$ATTNCHECK1 != "A little true" | public_mail$ATTNCHECK2 != "Very true", "Remove", "OK")

# flag fraudulent responses
public_mail$QC_FRAUDULENT <- ifelse(public_mail$Status == "Survey Preview", "OK",
                                    ifelse(public_mail$Q_RecaptchaScore < 0.5, "Caution", "OK"))

# flag potential duplicate responses
public_mail$QC_DUPLICATE <- ifelse(public_mail$Status == "Survey Preview", "OK",
                                   ifelse(public_mail$Q_RelevantIDDuplicateScore >= 75, "Remove", "OK"))


# flag responses exhibiting response biases
## check if respondents are (too) frequently selecting the same answer (12 question blocks to check)
#### priorities = 3 sections (nataccess/costliv/rodbrg) 
#### attitudes = 2 sections (justdist/bayimp) 
#### behaviors = 3 sections (cmphik/exrcso/histsit) 
#### norms = 1 sections
#### distress = 3 sections (pident/hopeknow/degraded) 
#### safety = 1 sections (safout)


public_mail$QC_BIASCHECK_P1 <- ifelse(public_mail$PRIORITY_ACSNAT == public_mail$PRIORITY_ACSEDU & 
                                        public_mail$PRIORITY_ACSNAT == public_mail$PRIORITY_ACSHLT & 
                                        public_mail$PRIORITY_ACSNAT == public_mail$PRIORITY_JOBINC & 
                                        public_mail$PRIORITY_ACSNAT == public_mail$PRIORITY_WAGINC & 
                                        public_mail$PRIORITY_ACSNAT == public_mail$PRIORITY_TRASH, "Caution", "OK")
public_mail$QC_BIASCHECK_P2 <- ifelse(public_mail$PRIORITY_CSTLIV == public_mail$PRIORITY_PEDCYC & 
                                        public_mail$PRIORITY_CSTLIV == public_mail$PRIORITY_HOUSNG & 
                                        public_mail$PRIORITY_CSTLIV == public_mail$PRIORITY_JOBDIV & 
                                        public_mail$PRIORITY_CSTLIV == public_mail$PRIORITY_AIRQTY & 
                                        public_mail$PRIORITY_CSTLIV == public_mail$PRIORITY_WTRQTY, "Caution", "OK")
public_mail$QC_BIASCHECK_P3 <- ifelse(public_mail$PRIORITY_RODBRG == public_mail$PRIORITY_UTILTY & 
                                        public_mail$PRIORITY_RODBRG == public_mail$PRIORITY_FLDPRT & 
                                        public_mail$PRIORITY_RODBRG == public_mail$PRIORITY_PUBTRN, "Caution", "OK")
public_mail$QC_BIASCHECK_A1 <- ifelse(public_mail$JUSTICE_DIST == public_mail$JUSTICE_PRO & 
                                        public_mail$JUSTICE_DIST == public_mail$JUSTICE_REC & 
                                        public_mail$JUSTICE_DIST == public_mail$JUSTICE_AGN & 
                                        public_mail$JUSTICE_DIST == public_mail$ATTITUDE_BAYLNK.R, "Caution", "OK")
public_mail$QC_BIASCHECK_A2 <- ifelse(public_mail$ATTITUDE_BAYIMP == public_mail$ATTITUDE_BIODIV & 
                                        public_mail$ATTITUDE_BAYIMP == public_mail$ATTITUDE_OPPSAT & 
                                        public_mail$ATTITUDE_BAYIMP == public_mail$ATTNCHECK1 & 
                                        public_mail$ATTITUDE_BAYIMP == public_mail$NR_IMPPRT, "Caution", "OK")
public_mail$QC_BIASCHECK_B1 <- ifelse(public_mail$ACTIVITY_CMPHIK == public_mail$ACTIVITY_PADBOT & 
                                        public_mail$ACTIVITY_CMPHIK == public_mail$ACTIVITY_SWIMDV & 
                                        public_mail$ACTIVITY_CMPHIK == public_mail$ACTIVITY_BWATCH & 
                                        public_mail$ACTIVITY_CMPHIK == public_mail$ACTIVITY_GARDEN & 
                                        public_mail$ACTIVITY_CMPHIK == public_mail$ACTIVITY_FISHNG, "Caution", "OK")
public_mail$QC_BIASCHECK_B2 <- ifelse(public_mail$ACTIVITY_EXRCSO == public_mail$ACTIVITY_RELAXO & 
                                        public_mail$ACTIVITY_EXRCSO == public_mail$ACTIVITY_PICNIC & 
                                        public_mail$ACTIVITY_EXRCSO == public_mail$ACTIVITY_PLYGND & 
                                        public_mail$ACTIVITY_EXRCSO == public_mail$ACTIVITY_ZOOAQU & 
                                        public_mail$ACTIVITY_EXRCSO == public_mail$ACTIVITY_LSNPRK, "Caution", "OK")
public_mail$QC_BIASCHECK_B3 <- ifelse(public_mail$ACTIVITY_HSTSIT == public_mail$ACTIVITY_BEACH & 
                                        public_mail$ACTIVITY_HSTSIT == public_mail$ACTIVITY_PIER & 
                                        public_mail$ACTIVITY_HSTSIT == public_mail$BEHAVIOR_NODRIV & 
                                        public_mail$ACTIVITY_HSTSIT == public_mail$BEHAVIOR_RECYCL & 
                                        public_mail$ACTIVITY_HSTSIT == public_mail$BEHAVIOR_PLNTFF & 
                                        public_mail$ACTIVITY_HSTSIT == public_mail$BEHAVIOR_RESTOR, "Caution", "OK")
public_mail$QC_BIASCHECK_N1 <- ifelse(public_mail$NORM_NODRIV == public_mail$NORM_RECYCL & 
                                        public_mail$NORM_NODRIV == public_mail$NORM_PLNTFF & 
                                        public_mail$NORM_NODRIV == public_mail$NORM_RESTOR, "Caution", "OK")
public_mail$QC_BIASCHECK_D1 <- ifelse(public_mail$PIDENTITY_IDENTY == public_mail$SOLASTALGIA_BELONG & 
                                        public_mail$PIDENTITY_IDENTY == public_mail$SOLASTALGIA_LOSSES & 
                                        public_mail$PIDENTITY_IDENTY == public_mail$HOPEPATH_WAYFIX & 
                                        public_mail$PIDENTITY_IDENTY == public_mail$SOLASTALGIA_ASHAMD & 
                                        public_mail$PIDENTITY_IDENTY == public_mail$SOLASTALGIA_DISAPR, "Caution", "OK")
public_mail$QC_BIASCHECK_D2 <- ifelse(public_mail$HOPEPATH_KNOWLG == public_mail$PIDENTITY_SPCIAL & 
                                        public_mail$HOPEPATH_KNOWLG == public_mail$SOLASTALGIA_UQLOSS & 
                                        public_mail$HOPEPATH_KNOWLG == public_mail$SOLASTALGIA_PEACEQ & 
                                        public_mail$HOPEPATH_KNOWLG == public_mail$ATTNCHECK2 & 
                                        public_mail$HOPEPATH_KNOWLG == public_mail$HOPEPATH_CANFIX.R, "Caution", "OK")
public_mail$QC_BIASCHECK_D3 <- ifelse(public_mail$SOLASTALGIA_DEGRAD == public_mail$ACTIVITY_BEACH & 
                                        public_mail$SOLASTALGIA_DEGRAD == public_mail$HOPEPATH_RESOLV & 
                                        public_mail$SOLASTALGIA_DEGRAD == public_mail$SOLASTALGIA_LFSTYL & 
                                        public_mail$SOLASTALGIA_DEGRAD == public_mail$SOLASTALGIA_FLEAVE & 
                                        public_mail$SOLASTALGIA_DEGRAD == public_mail$HOPEPATH_ENOUGH.R, "Caution", "OK")
public_mail$QC_BIASCHECK_S1 <- ifelse(public_mail$ATTITUDE_SAFOUT == public_mail$ATTITUDE_OUTWEL.R & 
                                        public_mail$ATTITUDE_SAFOUT == public_mail$ATTITUDE_SWMSAF.R & 
                                        public_mail$ATTITUDE_SAFOUT == public_mail$ATTITUDE_CHNGFW, "Caution", "OK")

public_mail <- public_mail %>%
  mutate(QC_BIAS_CAUTIONS = str_count(QC_BIASCHECK_P1, "Caution") + str_count(QC_BIASCHECK_P2, "Caution") + 
           str_count(QC_BIASCHECK_P3, "Caution") + str_count(QC_BIASCHECK_A1, "Caution") + str_count(QC_BIASCHECK_A2, "Caution") + 
           str_count(QC_BIASCHECK_B1, "Caution") + str_count(QC_BIASCHECK_B2, "Caution") + str_count(QC_BIASCHECK_B3, "Caution") + 
           str_count(QC_BIASCHECK_N1, "Caution") + str_count(QC_BIASCHECK_D1, "Caution") + str_count(QC_BIASCHECK_D2, "Caution") + 
           str_count(QC_BIASCHECK_D3, "Caution") + str_count(QC_BIASCHECK_S1, "Caution"))

# INCLUSION DETERMINATION
public_mail$INCLUDE <- ifelse(public_mail$QC_DURATION == "Remove" | 
                                public_mail$QC_FINISHED == "Remove" | public_mail$QC_ATTENTION == "Remove" | 
                                public_mail$QC_DUPLICATE == "Remove", "No", "Yes")
public_mail$INCLUDE <- ifelse(public_mail$INCLUDE == "Yes" & public_mail$QC_BIAS_CAUTIONS > 4, "No", 
                              ifelse(public_mail$INCLUDE == "Yes" & public_mail$QC_FRAUDULENT == "Caution", "Confirm Fraudulent", public_mail$INCLUDE))


public_mail %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# Yes                 65
# <NA>                1



######### * QUANTITATIVE SCALING ######### 

# Separate dataset with quantitative values instead of text values
public_mail_quant <- public_mail

### PRIORITY ###

public_mail_quant$PRIORITY_ACSNAT <- ifelse(public_mail_quant$PRIORITY_ACSNAT == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_ACSNAT == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_ACSNAT == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_ACSNAT == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_ACSNAT == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_ACSEDU <- ifelse(public_mail_quant$PRIORITY_ACSEDU == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_ACSEDU == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_ACSEDU == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_ACSEDU == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_ACSEDU == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_ACSHLT <- ifelse(public_mail_quant$PRIORITY_ACSHLT == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_ACSHLT == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_ACSHLT == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_ACSHLT == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_ACSHLT == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_JOBINC <- ifelse(public_mail_quant$PRIORITY_JOBINC == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_JOBINC == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_JOBINC == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_JOBINC == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_JOBINC == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_WAGINC <- ifelse(public_mail_quant$PRIORITY_WAGINC == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_WAGINC == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_WAGINC == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_WAGINC == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_WAGINC == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_TRASH <- ifelse(public_mail_quant$PRIORITY_TRASH == "Not a priority", 0,
                                           ifelse(public_mail_quant$PRIORITY_TRASH == "Low priority", 1,
                                                  ifelse(public_mail_quant$PRIORITY_TRASH == "Moderate priority", 2,
                                                         ifelse(public_mail_quant$PRIORITY_TRASH == "High priority", 3,
                                                                ifelse(public_mail_quant$PRIORITY_TRASH == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_CSTLIV <- ifelse(public_mail_quant$PRIORITY_CSTLIV == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_CSTLIV == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_CSTLIV == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_CSTLIV == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_CSTLIV == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_PEDCYC <- ifelse(public_mail_quant$PRIORITY_PEDCYC == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_PEDCYC == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_PEDCYC == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_PEDCYC == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_PEDCYC == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_HOUSNG <- ifelse(public_mail_quant$PRIORITY_HOUSNG == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_HOUSNG == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_HOUSNG == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_HOUSNG == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_HOUSNG == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_JOBDIV <- ifelse(public_mail_quant$PRIORITY_JOBDIV == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_JOBDIV == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_JOBDIV == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_JOBDIV == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_JOBDIV == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_AIRQTY <- ifelse(public_mail_quant$PRIORITY_AIRQTY == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_AIRQTY == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_AIRQTY == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_AIRQTY == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_AIRQTY == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_WTRQTY <- ifelse(public_mail_quant$PRIORITY_WTRQTY == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_WTRQTY == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_WTRQTY == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_WTRQTY == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_WTRQTY == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_RODBRG <- ifelse(public_mail_quant$PRIORITY_RODBRG == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_RODBRG == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_RODBRG == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_RODBRG == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_RODBRG == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_UTILTY <- ifelse(public_mail_quant$PRIORITY_UTILTY == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_UTILTY == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_UTILTY == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_UTILTY == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_UTILTY == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_FLDPRT <- ifelse(public_mail_quant$PRIORITY_FLDPRT == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_FLDPRT == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_FLDPRT == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_FLDPRT == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_FLDPRT == "Top priority", 4, NA)))))
public_mail_quant$PRIORITY_PUBTRN <- ifelse(public_mail_quant$PRIORITY_PUBTRN == "Not a priority", 0,
                                            ifelse(public_mail_quant$PRIORITY_PUBTRN == "Low priority", 1,
                                                   ifelse(public_mail_quant$PRIORITY_PUBTRN == "Moderate priority", 2,
                                                          ifelse(public_mail_quant$PRIORITY_PUBTRN == "High priority", 3,
                                                                 ifelse(public_mail_quant$PRIORITY_PUBTRN == "Top priority", 4, NA)))))
# Create average by sector
public_mail_quant$PRIORITY_ENVIRON <- (public_mail_quant$PRIORITY_ACSNAT + public_mail_quant$PRIORITY_TRASH + public_mail_quant$PRIORITY_AIRQTY + public_mail_quant$PRIORITY_WTRQTY)/4
public_mail_quant$PRIORITY_ECONOMY <- (public_mail_quant$PRIORITY_JOBINC + public_mail_quant$PRIORITY_WAGINC + public_mail_quant$PRIORITY_CSTLIV + public_mail_quant$PRIORITY_JOBDIV)/4
public_mail_quant$PRIORITY_INFRAST <- (public_mail_quant$PRIORITY_PEDCYC + public_mail_quant$PRIORITY_RODBRG + public_mail_quant$PRIORITY_UTILTY + public_mail_quant$PRIORITY_FLDPRT)/4
public_mail_quant$PRIORITY_SOCSERV <- (public_mail_quant$PRIORITY_ACSEDU + public_mail_quant$PRIORITY_ACSHLT + public_mail_quant$PRIORITY_HOUSNG + public_mail_quant$PRIORITY_PUBTRN)/4

# Determine rank for environmental priorities
public_mail_quant <- public_mail_quant %>%
  mutate(max_priority = apply(public_mail_quant[,247:250], MARGIN = 1, FUN = max, na.rm = TRUE),
         max_priority = na_if(max_priority, -Inf),
         min_priority = apply(public_mail_quant[,247:250], MARGIN = 1, FUN = min, na.rm = TRUE),
         min_priority = na_if(min_priority, -Inf))

public_mail_quant$PRIORITIZE_ENVIRON <- ifelse(public_mail_quant$PRIORITY_ENVIRON == public_mail_quant$max_priority, 1, 0)
public_mail_quant$RANK_ENVIRON <- ifelse(public_mail_quant$PRIORITY_ENVIRON == public_mail_quant$max_priority, "Top",
                                         ifelse(public_mail_quant$PRIORITY_ENVIRON == public_mail_quant$min_priority, "Bottom", "Middle"))



### ATTITUDES, NATURE RELATEDNESS, ETC ###

public_mail_quant$JUSTICE_DIST <- ifelse(public_mail_quant$JUSTICE_DIST == "Not true at all", 0,
                                         ifelse(public_mail_quant$JUSTICE_DIST == "A little true", 1,
                                                ifelse(public_mail_quant$JUSTICE_DIST == "Moderately true", 2,
                                                       ifelse(public_mail_quant$JUSTICE_DIST == "Very true", 3,
                                                              ifelse(public_mail_quant$JUSTICE_DIST == "Completely true", 4, NA)))))
public_mail_quant$JUSTICE_PRO <- ifelse(public_mail_quant$JUSTICE_PRO == "Not true at all", 0,
                                        ifelse(public_mail_quant$JUSTICE_PRO == "A little true", 1,
                                               ifelse(public_mail_quant$JUSTICE_PRO == "Moderately true", 2,
                                                      ifelse(public_mail_quant$JUSTICE_PRO == "Very true", 3,
                                                             ifelse(public_mail_quant$JUSTICE_PRO == "Completely true", 4, NA)))))
public_mail_quant$JUSTICE_REC <- ifelse(public_mail_quant$JUSTICE_REC == "Not true at all", 0,
                                        ifelse(public_mail_quant$JUSTICE_REC == "A little true", 1,
                                               ifelse(public_mail_quant$JUSTICE_REC == "Moderately true", 2,
                                                      ifelse(public_mail_quant$JUSTICE_REC == "Very true", 3,
                                                             ifelse(public_mail_quant$JUSTICE_REC == "Completely true", 4, NA)))))
public_mail_quant$JUSTICE_AGN <- ifelse(public_mail_quant$JUSTICE_AGN == "Not true at all", 0,
                                        ifelse(public_mail_quant$JUSTICE_AGN == "A little true", 1,
                                               ifelse(public_mail_quant$JUSTICE_AGN == "Moderately true", 2,
                                                      ifelse(public_mail_quant$JUSTICE_AGN == "Very true", 3,
                                                             ifelse(public_mail_quant$JUSTICE_AGN == "Completely true", 4, NA)))))
public_mail_quant$JUSTICE <- (public_mail_quant$JUSTICE_DIST + public_mail_quant$JUSTICE_PRO + 
                                public_mail_quant$JUSTICE_REC)/3
public_mail_quant$ATTITUDE_BAYLNK <- ifelse(public_mail_quant$ATTITUDE_BAYLNK == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_BAYLNK == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_BAYLNK == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_BAYLNK == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_BAYLNK == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_BAYIMP <- ifelse(public_mail_quant$ATTITUDE_BAYIMP == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_BAYIMP == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_BAYIMP == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_BAYIMP == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_BAYIMP == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_BIODIV <- ifelse(public_mail_quant$ATTITUDE_BIODIV == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_BIODIV == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_BIODIV == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_BIODIV == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_BIODIV == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_OPPSAT <- ifelse(public_mail_quant$ATTITUDE_OPPSAT == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_OPPSAT == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_OPPSAT == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_OPPSAT == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_OPPSAT == "Completely true", 4, NA)))))
public_mail_quant$NR_IMPPRT <- ifelse(public_mail_quant$NR_IMPPRT == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_IMPPRT == "A little true", 1,
                                             ifelse(public_mail_quant$NR_IMPPRT == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_IMPPRT == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_IMPPRT == "Completely true", 4, NA)))))
public_mail_quant$NR_CONNCT <- ifelse(public_mail_quant$NR_CONNCT == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_CONNCT == "A little true", 1,
                                             ifelse(public_mail_quant$NR_CONNCT == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_CONNCT == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_CONNCT == "Completely true", 4, NA)))))
public_mail_quant$NR_SPIRIT <- ifelse(public_mail_quant$NR_SPIRIT == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_SPIRIT == "A little true", 1,
                                             ifelse(public_mail_quant$NR_SPIRIT == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_SPIRIT == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_SPIRIT == "Completely true", 4, NA)))))
public_mail_quant$NR_NOTICE <- ifelse(public_mail_quant$NR_NOTICE == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_NOTICE == "A little true", 1,
                                             ifelse(public_mail_quant$NR_NOTICE == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_NOTICE == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_NOTICE == "Completely true", 4, NA)))))
public_mail_quant$NR_VACSPT <- ifelse(public_mail_quant$NR_VACSPT == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_VACSPT == "A little true", 1,
                                             ifelse(public_mail_quant$NR_VACSPT == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_VACSPT == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_VACSPT == "Completely true", 4, NA)))))
public_mail_quant$NR_AFFECT <- ifelse(public_mail_quant$NR_AFFECT == "Not true at all", 0,
                                      ifelse(public_mail_quant$NR_AFFECT == "A little true", 1,
                                             ifelse(public_mail_quant$NR_AFFECT == "Moderately true", 2,
                                                    ifelse(public_mail_quant$NR_AFFECT == "Very true", 3,
                                                           ifelse(public_mail_quant$NR_AFFECT == "Completely true", 4, NA)))))
public_mail_quant$NRELATEDNESS <- (public_mail_quant$NR_IMPPRT + public_mail_quant$NR_CONNCT + public_mail_quant$NR_SPIRIT + 
                                     public_mail_quant$NR_NOTICE + public_mail_quant$NR_VACSPT + public_mail_quant$NR_AFFECT)/6
public_mail_quant$PIDENTITY_IDENTY <- ifelse(public_mail_quant$PIDENTITY_IDENTY == "Not true at all", 0,
                                             ifelse(public_mail_quant$PIDENTITY_IDENTY == "A little true", 1,
                                                    ifelse(public_mail_quant$PIDENTITY_IDENTY == "Moderately true", 2,
                                                           ifelse(public_mail_quant$PIDENTITY_IDENTY == "Very true", 3,
                                                                  ifelse(public_mail_quant$PIDENTITY_IDENTY == "Completely true", 4, NA)))))
public_mail_quant$PIDENTITY_SPCIAL <- ifelse(public_mail_quant$PIDENTITY_SPCIAL == "Not true at all", 0,
                                             ifelse(public_mail_quant$PIDENTITY_SPCIAL == "A little true", 1,
                                                    ifelse(public_mail_quant$PIDENTITY_SPCIAL == "Moderately true", 2,
                                                           ifelse(public_mail_quant$PIDENTITY_SPCIAL == "Very true", 3,
                                                                  ifelse(public_mail_quant$PIDENTITY_SPCIAL == "Completely true", 4, NA)))))
public_mail_quant$PIDENTITY_ATTCHD <- ifelse(public_mail_quant$PIDENTITY_ATTCHD == "Not true at all", 0,
                                             ifelse(public_mail_quant$PIDENTITY_ATTCHD == "A little true", 1,
                                                    ifelse(public_mail_quant$PIDENTITY_ATTCHD == "Moderately true", 2,
                                                           ifelse(public_mail_quant$PIDENTITY_ATTCHD == "Very true", 3,
                                                                  ifelse(public_mail_quant$PIDENTITY_ATTCHD == "Completely true", 4, NA)))))
public_mail_quant$PIDENTITY <- (public_mail_quant$PIDENTITY_IDENTY + public_mail_quant$PIDENTITY_SPCIAL + public_mail_quant$PIDENTITY_ATTCHD)/3
public_mail_quant$SOLASTALGIA_BELONG <- ifelse(public_mail_quant$SOLASTALGIA_BELONG == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_BELONG == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_BELONG == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_BELONG == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_BELONG == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_LOSSES <- ifelse(public_mail_quant$SOLASTALGIA_LOSSES == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_LOSSES == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_LOSSES == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_LOSSES == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_LOSSES == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_ASHAMD <- ifelse(public_mail_quant$SOLASTALGIA_ASHAMD == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_ASHAMD == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_ASHAMD == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_ASHAMD == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_ASHAMD == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_DISAPR <- ifelse(public_mail_quant$SOLASTALGIA_DISAPR == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_DISAPR == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_DISAPR == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_DISAPR == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_DISAPR == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_UQLOSS <- ifelse(public_mail_quant$SOLASTALGIA_UQLOSS == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_UQLOSS == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_UQLOSS == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_UQLOSS == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_UQLOSS == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_PEACEQ <- ifelse(public_mail_quant$SOLASTALGIA_PEACEQ == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_PEACEQ == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_PEACEQ == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_PEACEQ == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_PEACEQ == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_DEGRAD <- ifelse(public_mail_quant$SOLASTALGIA_DEGRAD == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_DEGRAD == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_DEGRAD == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_DEGRAD == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_DEGRAD == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_LFSTYL <- ifelse(public_mail_quant$SOLASTALGIA_LFSTYL == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_LFSTYL == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_LFSTYL == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_LFSTYL == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_LFSTYL == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA_FLEAVE <- ifelse(public_mail_quant$SOLASTALGIA_FLEAVE == "Not true at all", 0,
                                               ifelse(public_mail_quant$SOLASTALGIA_FLEAVE == "A little true", 1,
                                                      ifelse(public_mail_quant$SOLASTALGIA_FLEAVE == "Moderately true", 2,
                                                             ifelse(public_mail_quant$SOLASTALGIA_FLEAVE == "Very true", 3,
                                                                    ifelse(public_mail_quant$SOLASTALGIA_FLEAVE == "Completely true", 4, NA)))))
public_mail_quant$SOLASTALGIA <- (public_mail_quant$SOLASTALGIA_LOSSES + public_mail_quant$SOLASTALGIA_ASHAMD + 
                                    public_mail_quant$SOLASTALGIA_DISAPR + public_mail_quant$SOLASTALGIA_UQLOSS + public_mail_quant$SOLASTALGIA_PEACEQ + 
                                    public_mail_quant$SOLASTALGIA_DEGRAD + public_mail_quant$SOLASTALGIA_LFSTYL)/7
public_mail_quant$HOPEPATH_WAYFIX <- ifelse(public_mail_quant$HOPEPATH_WAYFIX == "Not true at all", 0,
                                            ifelse(public_mail_quant$HOPEPATH_WAYFIX == "A little true", 1,
                                                   ifelse(public_mail_quant$HOPEPATH_WAYFIX == "Moderately true", 2,
                                                          ifelse(public_mail_quant$HOPEPATH_WAYFIX == "Very true", 3,
                                                                 ifelse(public_mail_quant$HOPEPATH_WAYFIX == "Completely true", 4, NA)))))
public_mail_quant$HOPEPATH_KNOWLG <- ifelse(public_mail_quant$HOPEPATH_KNOWLG == "Not true at all", 0,
                                            ifelse(public_mail_quant$HOPEPATH_KNOWLG == "A little true", 1,
                                                   ifelse(public_mail_quant$HOPEPATH_KNOWLG == "Moderately true", 2,
                                                          ifelse(public_mail_quant$HOPEPATH_KNOWLG == "Very true", 3,
                                                                 ifelse(public_mail_quant$HOPEPATH_KNOWLG == "Completely true", 4, NA)))))
public_mail_quant$HOPEPATH_CANFIX <- ifelse(public_mail_quant$HOPEPATH_CANFIX == "Not true at all", 0,
                                            ifelse(public_mail_quant$HOPEPATH_CANFIX == "A little true", 1,
                                                   ifelse(public_mail_quant$HOPEPATH_CANFIX == "Moderately true", 2,
                                                          ifelse(public_mail_quant$HOPEPATH_CANFIX == "Very true", 3,
                                                                 ifelse(public_mail_quant$HOPEPATH_CANFIX == "Completely true", 4, NA)))))
public_mail_quant$HOPEPATH_RESOLV <- ifelse(public_mail_quant$HOPEPATH_RESOLV == "Not true at all", 0,
                                            ifelse(public_mail_quant$HOPEPATH_RESOLV == "A little true", 1,
                                                   ifelse(public_mail_quant$HOPEPATH_RESOLV == "Moderately true", 2,
                                                          ifelse(public_mail_quant$HOPEPATH_RESOLV == "Very true", 3,
                                                                 ifelse(public_mail_quant$HOPEPATH_RESOLV == "Completely true", 4, NA)))))
public_mail_quant$HOPEPATH_ENOUGH <- ifelse(public_mail_quant$HOPEPATH_ENOUGH == "Not true at all", 0,
                                            ifelse(public_mail_quant$HOPEPATH_ENOUGH == "A little true", 1,
                                                   ifelse(public_mail_quant$HOPEPATH_ENOUGH == "Moderately true", 2,
                                                          ifelse(public_mail_quant$HOPEPATH_ENOUGH == "Very true", 3,
                                                                 ifelse(public_mail_quant$HOPEPATH_ENOUGH == "Completely true", 4, NA)))))
public_mail_quant$HOPEPATH_SELFEFF <- (public_mail_quant$HOPEPATH_WAYFIX + public_mail_quant$HOPEPATH_KNOWLG)/2
public_mail_quant$HOPEPATH_RESPEFF <- (public_mail_quant$HOPEPATH_CANFIX + public_mail_quant$HOPEPATH_ENOUGH)/2
public_mail_quant$ATTITUDE_SAFOUT <- ifelse(public_mail_quant$ATTITUDE_SAFOUT == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_SAFOUT == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_SAFOUT == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_SAFOUT == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_SAFOUT == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_OUTWEL <- ifelse(public_mail_quant$ATTITUDE_OUTWEL == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_OUTWEL == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_OUTWEL == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_OUTWEL == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_OUTWEL == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_SWMSAF <- ifelse(public_mail_quant$ATTITUDE_SWMSAF == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_SWMSAF == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_SWMSAF == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_SWMSAF == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_SWMSAF == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_CHNGFW <- ifelse(public_mail_quant$ATTITUDE_CHNGFW == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_CHNGFW == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_CHNGFW == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_CHNGFW == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_CHNGFW == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_CHNGRN <- ifelse(public_mail_quant$ATTITUDE_CHNGRN == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_CHNGRN == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_CHNGRN == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_CHNGRN == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_CHNGRN == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_CHNGTP <- ifelse(public_mail_quant$ATTITUDE_CHNGTP == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_CHNGTP == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_CHNGTP == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_CHNGTP == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_CHNGTP == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_CHNGHS <- ifelse(public_mail_quant$ATTITUDE_CHNGHS == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_CHNGHS == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_CHNGHS == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_CHNGHS == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_CHNGHS == "Completely true", 4, NA)))))
public_mail_quant$ATTITUDE_CCTHRT <- ifelse(public_mail_quant$ATTITUDE_CCTHRT == "Not true at all", 0,
                                            ifelse(public_mail_quant$ATTITUDE_CCTHRT == "A little true", 1,
                                                   ifelse(public_mail_quant$ATTITUDE_CCTHRT == "Moderately true", 2,
                                                          ifelse(public_mail_quant$ATTITUDE_CCTHRT == "Very true", 3,
                                                                 ifelse(public_mail_quant$ATTITUDE_CCTHRT == "Completely true", 4, NA)))))


### ACTIVITIES/BEHAVIORS ###

public_mail_quant$ACTIVITY_CMPHIK <- ifelse(public_mail_quant$ACTIVITY_CMPHIK == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_CMPHIK == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_CMPHIK == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_CMPHIK == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_CMPHIK == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_PADBOT <- ifelse(public_mail_quant$ACTIVITY_PADBOT == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_PADBOT == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_PADBOT == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_PADBOT == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_PADBOT == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_SWIMDV <- ifelse(public_mail_quant$ACTIVITY_SWIMDV == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_SWIMDV == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_SWIMDV == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_SWIMDV == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_SWIMDV == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_BWATCH <- ifelse(public_mail_quant$ACTIVITY_BWATCH == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_BWATCH == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_BWATCH == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_BWATCH == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_BWATCH == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_GARDEN <- ifelse(public_mail_quant$ACTIVITY_GARDEN == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_GARDEN == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_GARDEN == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_GARDEN == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_GARDEN == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_FISHNG <- ifelse(public_mail_quant$ACTIVITY_FISHNG == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_FISHNG == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_FISHNG == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_FISHNG == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_FISHNG == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_EXRCSO <- ifelse(public_mail_quant$ACTIVITY_EXRCSO == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_EXRCSO == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_EXRCSO == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_EXRCSO == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_EXRCSO == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_RELAXO <- ifelse(public_mail_quant$ACTIVITY_RELAXO == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_RELAXO == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_RELAXO == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_RELAXO == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_RELAXO == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_PICNIC <- ifelse(public_mail_quant$ACTIVITY_PICNIC == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_PICNIC == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_PICNIC == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_PICNIC == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_PICNIC == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_PLYGND <- ifelse(public_mail_quant$ACTIVITY_PLYGND == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_PLYGND == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_PLYGND == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_PLYGND == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_PLYGND == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_ZOOAQU <- ifelse(public_mail_quant$ACTIVITY_ZOOAQU == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_ZOOAQU == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_ZOOAQU == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_ZOOAQU == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_ZOOAQU == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_LSNPRK <- ifelse(public_mail_quant$ACTIVITY_LSNPRK == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_LSNPRK == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_LSNPRK == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_LSNPRK == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_LSNPRK == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_HSTSIT <- ifelse(public_mail_quant$ACTIVITY_HSTSIT == "Never", 0,
                                            ifelse(public_mail_quant$ACTIVITY_HSTSIT == "A few times", 1,
                                                   ifelse(public_mail_quant$ACTIVITY_HSTSIT == "About once a month", 2,
                                                          ifelse(public_mail_quant$ACTIVITY_HSTSIT == "About once a week", 3,
                                                                 ifelse(public_mail_quant$ACTIVITY_HSTSIT == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_BEACH <- ifelse(public_mail_quant$ACTIVITY_BEACH == "Never", 0,
                                           ifelse(public_mail_quant$ACTIVITY_BEACH == "A few times", 1,
                                                  ifelse(public_mail_quant$ACTIVITY_BEACH == "About once a month", 2,
                                                         ifelse(public_mail_quant$ACTIVITY_BEACH == "About once a week", 3,
                                                                ifelse(public_mail_quant$ACTIVITY_BEACH == "Multiple times per week", 4, NA)))))
public_mail_quant$ACTIVITY_PIER <- ifelse(public_mail_quant$ACTIVITY_PIER == "Never", 0,
                                          ifelse(public_mail_quant$ACTIVITY_PIER == "A few times", 1,
                                                 ifelse(public_mail_quant$ACTIVITY_PIER == "About once a month", 2,
                                                        ifelse(public_mail_quant$ACTIVITY_PIER == "About once a week", 3,
                                                               ifelse(public_mail_quant$ACTIVITY_PIER == "Multiple times per week", 4, NA)))))

# Measure active vs passive nature engagement
public_mail_quant$ACTIVE_COUNT <- rowSums(public_mail_quant[56:61] > 0)
public_mail_quant$PASSIVE_COUNT <- rowSums(public_mail_quant[62:64] > 0)
public_mail_quant$ATTRACT_COUNT <- rowSums(public_mail_quant[65:70] > 0)
public_mail_quant$ACTIVE_AVG <- rowMeans(public_mail_quant[56:61])
public_mail_quant$PASSIVE_AVG <- rowMeans(public_mail_quant[62:64])
public_mail_quant$ATTRACT_AVG <- rowMeans(public_mail_quant[65:70])


public_mail_quant$BEHAVIOR_FERTLZ <- ifelse(public_mail_quant$BEHAVIOR_FERTLZ == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_FERTLZ == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_FERTLZ == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_FERTLZ == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_FERTLZ == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_FERTLZweight <- ifelse(is.na(public_mail$BEHAVIOR_FERTLZ), NA, 
                                                  ifelse(public_mail_quant$FERTLZ_SUMMER == 1, public_mail_quant$BEHAVIOR_FERTLZ * 2, public_mail$BEHAVIOR_FERTLZ))
public_mail_quant$BEHAVIOR_WATER <- ifelse(public_mail_quant$BEHAVIOR_WATER == "Never", 0,
                                           ifelse(public_mail_quant$BEHAVIOR_WATER == "A few times", 1,
                                                  ifelse(public_mail_quant$BEHAVIOR_WATER == "About once a month", 2,
                                                         ifelse(public_mail_quant$BEHAVIOR_WATER == "About once a week", 3,
                                                                ifelse(public_mail_quant$BEHAVIOR_WATER == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_CLPPNG <- ifelse(public_mail_quant$BEHAVIOR_CLPPNG == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_CLPPNG == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_CLPPNG == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_CLPPNG == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_CLPPNG == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_GRNINF <- ifelse(public_mail_quant$BEHAVIOR_GRNINF == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_GRNINF == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_GRNINF == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_GRNINF == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_GRNINF == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_NODRIV <- ifelse(public_mail_quant$BEHAVIOR_NODRIV == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_NODRIV == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_NODRIV == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_NODRIV == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_NODRIV == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_RECYCL <- ifelse(public_mail_quant$BEHAVIOR_RECYCL == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_RECYCL == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_RECYCL == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_RECYCL == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_RECYCL == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_PLNTFF <- ifelse(public_mail_quant$BEHAVIOR_PLNTFF == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_PLNTFF == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_PLNTFF == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_PLNTFF == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_PLNTFF == "Multiple times per week", 4, NA)))))
public_mail_quant$BEHAVIOR_RESTOR <- ifelse(public_mail_quant$BEHAVIOR_RESTOR == "Never", 0,
                                            ifelse(public_mail_quant$BEHAVIOR_RESTOR == "A few times", 1,
                                                   ifelse(public_mail_quant$BEHAVIOR_RESTOR == "About once a month", 2,
                                                          ifelse(public_mail_quant$BEHAVIOR_RESTOR == "About once a week", 3,
                                                                 ifelse(public_mail_quant$BEHAVIOR_RESTOR == "Multiple times per week", 4, NA)))))

### NORMS ###

public_mail_quant$NORM_FERTLZ <- ifelse(public_mail_quant$NORM_FERTLZ == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_FERTLZ == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_FERTLZ == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_FERTLZ == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_FERTLZ == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_WATER <- ifelse(public_mail_quant$NORM_WATER == "Much less than I do", -2,
                                       ifelse(public_mail_quant$NORM_WATER == "A little less than I do", -1,
                                              ifelse(public_mail_quant$NORM_WATER == "About the same as I do", 0,
                                                     ifelse(public_mail_quant$NORM_WATER == "A little more than I do", 1,
                                                            ifelse(public_mail_quant$NORM_WATER == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_CLPPNG <- ifelse(public_mail_quant$NORM_CLPPNG == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_CLPPNG == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_CLPPNG == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_CLPPNG == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_CLPPNG == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_GRNINF <- ifelse(public_mail_quant$NORM_GRNINF == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_GRNINF == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_GRNINF == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_GRNINF == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_GRNINF == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_NODRIV <- ifelse(public_mail_quant$NORM_NODRIV == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_NODRIV == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_NODRIV == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_NODRIV == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_NODRIV == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_RECYCL <- ifelse(public_mail_quant$NORM_RECYCL == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_RECYCL == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_RECYCL == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_RECYCL == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_RECYCL == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_PLNTFF <- ifelse(public_mail_quant$NORM_PLNTFF == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_PLNTFF == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_PLNTFF == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_PLNTFF == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_PLNTFF == "Much more than I do", 2, NA)))))
public_mail_quant$NORM_RESTOR <- ifelse(public_mail_quant$NORM_RESTOR == "Much less than I do", -2,
                                        ifelse(public_mail_quant$NORM_RESTOR == "A little less than I do", -1,
                                               ifelse(public_mail_quant$NORM_RESTOR == "About the same as I do", 0,
                                                      ifelse(public_mail_quant$NORM_RESTOR == "A little more than I do", 1,
                                                             ifelse(public_mail_quant$NORM_RESTOR == "Much more than I do", 2, NA)))))


### GROUP INVOLVEMENT ###

public_mail_quant$GROUP_ARTCLT <- ifelse(public_mail_quant$GROUP_ARTCLT == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_ARTCLT == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_ARTCLT == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_ENVPRT <- ifelse(public_mail_quant$GROUP_ENVPRT == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_ENVPRT == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_ENVPRT == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_HOANBH <- ifelse(public_mail_quant$GROUP_HOANBH == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_HOANBH == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_HOANBH == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_POLTCL <- ifelse(public_mail_quant$GROUP_POLTCL == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_POLTCL == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_POLTCL == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_SPIRIT <- ifelse(public_mail_quant$GROUP_SPIRIT == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_SPIRIT == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_SPIRIT == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_CHLPAR <- ifelse(public_mail_quant$GROUP_CHLPAR == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_CHLPAR == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_CHLPAR == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_SPORTS <- ifelse(public_mail_quant$GROUP_SPORTS == "No, not involved", 0,
                                         ifelse(public_mail_quant$GROUP_SPORTS == "Yes, a little involved", 1,
                                                ifelse(public_mail_quant$GROUP_SPORTS == "Yes, very involved", 2, NA)))
public_mail_quant$GROUP_INVOLVEMENT <- (public_mail_quant$GROUP_ARTCLT + public_mail_quant$GROUP_CHLPAR + public_mail_quant$GROUP_ENVPRT +
                                          public_mail_quant$GROUP_HOANBH + public_mail_quant$GROUP_POLTCL + public_mail_quant$GROUP_SPIRIT + 
                                          public_mail_quant$GROUP_SPORTS)/7


### MENTAL HEALTH ###

public_mail_quant$MHI_A1 <- ifelse(public_mail_quant$MHI_A1 == "None of the time", 0,
                                   ifelse(public_mail_quant$MHI_A1 == "A little of the time", 1,
                                          ifelse(public_mail_quant$MHI_A1 == "Some of the time", 2,
                                                 ifelse(public_mail_quant$MHI_A1 == "A good bit of the time", 3,
                                                        ifelse(public_mail_quant$MHI_A1 == "Most of the time", 4,
                                                               ifelse(public_mail_quant$MHI_A1 == "All of the time", 5, NA))))))
public_mail_quant$MHI_A2 <- ifelse(public_mail_quant$MHI_A2 == "None of the time", 0,
                                   ifelse(public_mail_quant$MHI_A2 == "A little of the time", 1,
                                          ifelse(public_mail_quant$MHI_A2 == "Some of the time", 2,
                                                 ifelse(public_mail_quant$MHI_A2 == "A good bit of the time", 3,
                                                        ifelse(public_mail_quant$MHI_A2 == "Most of the time", 4,
                                                               ifelse(public_mail_quant$MHI_A2 == "All of the time", 5, NA))))))
public_mail_quant$MHI_D1 <- ifelse(public_mail_quant$MHI_D1 == "None of the time", 0,
                                   ifelse(public_mail_quant$MHI_D1 == "A little of the time", 1,
                                          ifelse(public_mail_quant$MHI_D1 == "Some of the time", 2,
                                                 ifelse(public_mail_quant$MHI_D1 == "A good bit of the time", 3,
                                                        ifelse(public_mail_quant$MHI_D1 == "Most of the time", 4,
                                                               ifelse(public_mail_quant$MHI_D1 == "All of the time", 5, NA))))))
public_mail_quant$MHI_D2 <- ifelse(public_mail_quant$MHI_D2 == "None of the time", 0,
                                   ifelse(public_mail_quant$MHI_D2 == "A little of the time", 1,
                                          ifelse(public_mail_quant$MHI_D2 == "Some of the time", 2,
                                                 ifelse(public_mail_quant$MHI_D2 == "A good bit of the time", 3,
                                                        ifelse(public_mail_quant$MHI_D2 == "Most of the time", 4,
                                                               ifelse(public_mail_quant$MHI_D2 == "All of the time", 5, NA))))))
public_mail_quant$MHI_D3 <- ifelse(public_mail_quant$MHI_D3 == "None of the time", 0,
                                   ifelse(public_mail_quant$MHI_D3 == "A little of the time", 1,
                                          ifelse(public_mail_quant$MHI_D3 == "Some of the time", 2,
                                                 ifelse(public_mail_quant$MHI_D3 == "A good bit of the time", 3,
                                                        ifelse(public_mail_quant$MHI_D3 == "Most of the time", 4,
                                                               ifelse(public_mail_quant$MHI_D3 == "All of the time", 5, NA))))))
public_mail_quant$MHI_ANXIETY <- public_mail_quant$MHI_A1 + public_mail_quant$MHI_A2
public_mail_quant$MHI_DEPRESN <- public_mail_quant$MHI_D1 + public_mail_quant$MHI_D2 + public_mail_quant$MHI_D3
public_mail_quant$MHI_SCORE <- public_mail_quant$MHI_A1 + public_mail_quant$MHI_A2 + public_mail_quant$MHI_D1 + public_mail_quant$MHI_D2 + public_mail_quant$MHI_D3
public_mail_quant$LIFESATISFACTION <- ifelse(public_mail_quant$LIFESATISFACTION == "None of the time", 0,
                                             ifelse(public_mail_quant$LIFESATISFACTION == "A little of the time", 1,
                                                    ifelse(public_mail_quant$LIFESATISFACTION == "Some of the time", 2,
                                                           ifelse(public_mail_quant$LIFESATISFACTION == "A good bit of the time", 3,
                                                                  ifelse(public_mail_quant$LIFESATISFACTION == "Most of the time", 4,
                                                                         ifelse(public_mail_quant$LIFESATISFACTION == "All of the time", 5, NA))))))
public_mail_quant$MHI <- (public_mail_quant$MHI_A1 + public_mail_quant$MHI_A2 + public_mail_quant$MHI_D1 + public_mail_quant$MHI_D2 + public_mail_quant$MHI_D3)/5
public_mail_quant$SWELLBEING <- (public_mail_quant$MHI_A1 + public_mail_quant$MHI_A2 + public_mail_quant$MHI_D1 + public_mail_quant$MHI_D2 + public_mail_quant$MHI_D3 + public_mail_quant$LIFESATISFACTION)/6
public_mail_quant$ECOANXIETY_1 <- ifelse(public_mail_quant$ECOANXIETY_1 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_1 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_1 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_1 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_2 <- ifelse(public_mail_quant$ECOANXIETY_2 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_2 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_2 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_2 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_3 <- ifelse(public_mail_quant$ECOANXIETY_3 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_3 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_3 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_3 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_4 <- ifelse(public_mail_quant$ECOANXIETY_4 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_4 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_4 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_4 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_5 <- ifelse(public_mail_quant$ECOANXIETY_5 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_5 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_5 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_5 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_6 <- ifelse(public_mail_quant$ECOANXIETY_6 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_6 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_6 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_6 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_7 <- ifelse(public_mail_quant$ECOANXIETY_7 == "Not at all", 0,
                                         ifelse(public_mail_quant$ECOANXIETY_7 == "Several days", 1,
                                                ifelse(public_mail_quant$ECOANXIETY_7 == "More than half the days", 2,
                                                       ifelse(public_mail_quant$ECOANXIETY_7 == "Nearly every day", 3, NA))))
public_mail_quant$ECOANXIETY_SCORE <- public_mail_quant$ECOANXIETY_1 + public_mail_quant$ECOANXIETY_2 + public_mail_quant$ECOANXIETY_3 + public_mail_quant$ECOANXIETY_4 + public_mail_quant$ECOANXIETY_5 + public_mail_quant$ECOANXIETY_6 + public_mail_quant$ECOANXIETY_7
public_mail_quant$ECOANXIETY_CAT <- ifelse(public_mail_quant$ECOANXIETY_SCORE < 5, "Low",
                                           ifelse(public_mail_quant$ECOANXIETY_SCORE >= 5 & public_mail_quant$ECOANXIETY_SCORE < 10, "Mild",
                                                  ifelse(public_mail_quant$ECOANXIETY_SCORE >= 10 & public_mail_quant$ECOANXIETY_SCORE < 15, "Moderate",
                                                         ifelse(public_mail_quant$ECOANXIETY_SCORE >= 15, "Severe", NA))))

### DEMOGRAPHICS ###

public_mail_quant$CONSERVATIVE <- ifelse(public_mail_quant$POLITICS == "Strongly liberal", -3,
                                         ifelse(public_mail_quant$POLITICS == "Moderately liberal", -2,
                                                ifelse(public_mail_quant$POLITICS == "Slightly liberal", -1,
                                                       ifelse(public_mail_quant$POLITICS == "Neutral", 0,
                                                              ifelse(public_mail_quant$POLITICS == "Slightly conservative", 1,
                                                                     ifelse(public_mail_quant$POLITICS == "Moderately conservative", 2, 
                                                                            ifelse(public_mail_quant$POLITICS == "Strongly conservative", 3, NA)))))))


######### * DATA CLEANING ######### 

# Match Street Intersection coordinates to each respondent by Response ID
public_mail <- merge(public_mail, public_mail_coords, by = "ResponseId", all.x = TRUE)
public_mail_quant <- merge(public_mail_quant, public_mail_coords, by = "ResponseId", all.x = TRUE)

# remove responses flagged for exclusion
public_mail$INCLUDE <- ifelse(public_mail$INCLUDE == "No", "No", 
                              ifelse(is.na(public_mail$POINT_X) & public_mail$QC_FRAUDULENT == "Caution", "No", 
                                     ifelse(is.na(public_mail$POINT_X) & is.na(public_mail$QC_FRAUDULENT), "No", "Yes")))
public_mail %>%
  count(INCLUDE)
# should look like:
# INCLUDE             n
# Yes                 65
# <NA>                1
public_mail_quant$INCLUDE <- ifelse(public_mail_quant$INCLUDE == "No", "No", 
                                    ifelse(is.na(public_mail_quant$POINT_X) & public_mail_quant$QC_FRAUDULENT == "Caution", "No", 
                                           ifelse(is.na(public_mail_quant$POINT_X) & is.na(public_mail_quant$QC_FRAUDULENT), "No", "Yes")))
public_mail_quant %>%
  count(INCLUDE)
public_mail_clean <- subset(public_mail, INCLUDE != "No")
public_mail_quant_clean <- subset(public_mail_quant, INCLUDE != "No")

# keep/reorder the variables we need
public_mail_final <- public_mail_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                           "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                           "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                           "POLITICS","YEARS","PROPERTY","OWNERSHIP",
                                           "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                           "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                           "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                           "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                           "NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                           "PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                           "SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                           "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                           "HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                           "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                           "ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                           "JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                           "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                           "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                           "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                           "GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                           "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                           "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                           "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                           "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                           "FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                           "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                           "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                           "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                           "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                           "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                           "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                           "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                           "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN")]

public_mail_quant_final <- public_mail_quant_clean[, c("ResponseId","POINT_X","POINT_Y","ZIPCODE","AGE","GENDER","EDUCATION",
                                                       "HHINCOME","EMPLOYED","EMPLOYED_STUDENT","EMPLOYED_RETIRED","RACE",
                                                       "RACE_NATIVE","RACE_ASIAN","RACE_BLACK","RACE_MIDEAST","RACE_PACIFIC","RACE_WHITE","RACE_OTHER","RACE_HISPANIC",
                                                       "POLITICS","CONSERVATIVE","YEARS","PROPERTY","OWNERSHIP",
                                                       "PRIORITIZE_ENVIRON","RANK_ENVIRON","PRIORITY_ENVIRON","PRIORITY_ECONOMY","PRIORITY_INFRAST","PRIORITY_SOCSERV",
                                                       "PRIORITY_ACSNAT","PRIORITY_TRASH","PRIORITY_AIRQTY","PRIORITY_WTRQTY",
                                                       "PRIORITY_JOBINC","PRIORITY_WAGINC","PRIORITY_CSTLIV","PRIORITY_JOBDIV",
                                                       "PRIORITY_PEDCYC","PRIORITY_RODBRG","PRIORITY_UTILTY","PRIORITY_FLDPRT",
                                                       "PRIORITY_ACSEDU","PRIORITY_ACSHLT","PRIORITY_HOUSNG","PRIORITY_PUBTRN",
                                                       "NRELATEDNESS","NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT",
                                                       "PIDENTITY","PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD",
                                                       "SOLASTALGIA","SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR",
                                                       "SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE",
                                                       "HOPEPATH_SELFEFF","HOPEPATH_RESPEFF","HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH",
                                                       "SWELLBEING","MHI","MHI_SCORE","MHI_ANXIETY","MHI_DEPRESN",
                                                       "MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION",
                                                       "ECOANXIETY_SCORE","ECOANXIETY_CAT","ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7",
                                                       "JUSTICE","JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN",
                                                       "ATTITUDE_BAYLNK","ATTITUDE_BAYIMP","ATTITUDE_BIODIV","ATTITUDE_OPPSAT",
                                                       "ATTITUDE_SAFOUT","ATTITUDE_OUTWEL","ATTITUDE_SWMSAF",
                                                       "ATTITUDE_CHNGFW","ATTITUDE_CHNGRN","ATTITUDE_CHNGTP","ATTITUDE_CHNGHS","ATTITUDE_CCTHRT",
                                                       "GROUP_INVOLVEMENT","GROUP_ARTCLT","GROUP_ENVPRT","GROUP_HOANBH","GROUP_POLTCL","GROUP_SPIRIT","GROUP_CHLPAR","GROUP_SPORTS",
                                                       "ACTIVE_COUNT","PASSIVE_COUNT","ATTRACT_COUNT","ACTIVE_AVG","PASSIVE_AVG","ATTRACT_AVG",
                                                       "ACTIVITY_CMPHIK","ACTIVITY_PADBOT","ACTIVITY_SWIMDV","ACTIVITY_BWATCH","ACTIVITY_GARDEN","ACTIVITY_FISHNG",
                                                       "ACTIVITY_EXRCSO","ACTIVITY_RELAXO","ACTIVITY_PICNIC",
                                                       "ACTIVITY_PLYGND","ACTIVITY_ZOOAQU","ACTIVITY_LSNPRK","ACTIVITY_HSTSIT","ACTIVITY_BEACH","ACTIVITY_PIER",
                                                       "BEHAVIOR_FERTLZ","BEHAVIOR_WATER","BEHAVIOR_CLPPNG","BEHAVIOR_GRNINF","BEHAVIOR_NODRIV","BEHAVIOR_RECYCL","BEHAVIOR_PLNTFF","BEHAVIOR_RESTOR",
                                                       "BEHAVIOR_FERTLZweight","FERTLZ_SEASON_N","FERTLZ_SPRING","FERTLZ_SUMMER","FERTLZ_FALL","FERTLZ_WINTER",
                                                       "NORM_FERTLZ","NORM_WATER","NORM_CLPPNG","NORM_GRNINF","NORM_NODRIV","NORM_RECYCL","NORM_PLNTFF","NORM_RESTOR",
                                                       "FISH_LOCATN_N","FISH_LOC_SHORE","FISH_LOC_PIER","FISH_LOC_WATER",
                                                       "FISH_REASON_N","FISH_RSN_SPORT","FISH_RSN_RELAX","FISH_RSN_FOOD","FISH_RSN_SELL","FISH_RSN_WATCH","FISH_RSN_SOCIAL",
                                                       "KNOWLEDGE_WQTRND","KNOWLEDGE_NUTRNT","KNOWLEDGE_SOURCE","KNOWLEDGE_FRTSSN","KNOWLEDGE_SGLOSS",
                                                       "KNOWLEDGE_WQTRND_C","KNOWLEDGE_NUTRNT_C","KNOWLEDGE_SOURCE_C","KNOWLEDGE_FRTSSN_C","KNOWLEDGE_SGLOSS_C",
                                                       "KNOWLEDGE_POINTS","KNOWLEDGE_SCORE","KNOWLEDGE_SCORE_Pct",
                                                       "INFOSOURCES_N","INFO_TV","INFO_RADIO","INFO_PERIOD","INFO_SOCIAL","INFO_MAILE",
                                                       "INFO_GOV","INFO_NGO","INFO_PUBLIC","INFO_WORD","INFO_OWN")]

public_mail_final$REFERRAL <- "Mail"
public_mail_final$AUDIENCE <- "Public"
public_mail_quant_final$REFERRAL <- "Mail"
public_mail_quant_final$AUDIENCE <- "Public"

# This creates the final quantitative and qualitative datasets for the public (mail) respondents.
# This data has already been merged with other audiences in the final datasets.

