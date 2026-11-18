library(tidyverse)


################## TARGET POPULATION ##################

# 2020 zip code tabulation areas within watershed
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=ZIP+Code+Tabulation+Areas
zcta <- read.csv("tbcs-trends/population_data/TBEP_Census_ZCTA_2020.csv")


######### * GENDER + AGE ######### 

# zip code 5-year population data from 2024 American Community Survey (2020-2024)
# https://data.census.gov/table/ACSST5Y2024.S0101?q=All+5-digit+ZIP+Code+Tabulation+Areas+within+United+States+Populations+and+People
popdata <- read.csv("tbcs-trends/population_data/ACSST5Y2024S0101.csv")


# desired age/sex combinations and coded variables
agesexfields <- read.csv("tbcs-trends/population_data/fields_agesex.csv")


### Extract the population data for the ZIP codes in our watershed
# get last 5 characters of ZCTA Name
popdata <- popdata %>%
  mutate(ZCTA5CE20 = str_sub(NAME, start = -5))

# merge population data and list of TBEP zip codes
zcta$ZCTA5CE20 <- as.character(zcta$ZCTA5CE20)
tbpopdata <- merge(popdata, zcta, by = "ZCTA5CE20", all.x = FALSE, all.y = TRUE)

# keep only desired variables
tbpopdata <- tbpopdata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
    S0101_C03_007E,  # Estimate!!Male!!Total population!!AGE!!25 to 29 years
    S0101_C03_008E,  # Estimate!!Male!!Total population!!AGE!!30 to 34 years
    S0101_C03_009E,  # Estimate!!Male!!Total population!!AGE!!35 to 39 years
    S0101_C03_010E,  # Estimate!!Male!!Total population!!AGE!!40 to 44 years
    S0101_C03_011E,  # Estimate!!Male!!Total population!!AGE!!45 to 49 years
    S0101_C03_012E,  # Estimate!!Male!!Total population!!AGE!!50 to 54 years
    S0101_C03_013E,  # Estimate!!Male!!Total population!!AGE!!55 to 59 years
    S0101_C03_014E,  # Estimate!!Male!!Total population!!AGE!!60 to 64 years
    S0101_C03_015E,  # Estimate!!Male!!Total population!!AGE!!65 to 69 years
    S0101_C03_016E,  # Estimate!!Male!!Total population!!AGE!!70 to 74 years
    S0101_C03_017E,  # Estimate!!Male!!Total population!!AGE!!75 to 79 years
    S0101_C03_018E,  # Estimate!!Male!!Total population!!AGE!!80 to 84 years
    S0101_C03_019E,  # Estimate!!Male!!Total population!!AGE!!85 years and over
    S0101_C03_023E,  # Estimate!!Male!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years
    S0101_C05_007E,  # Estimate!!Female!!Total population!!AGE!!25 to 29 years
    S0101_C05_008E,  # Estimate!!Female!!Total population!!AGE!!30 to 34 years
    S0101_C05_009E,  # Estimate!!Female!!Total population!!AGE!!35 to 39 years
    S0101_C05_010E,  # Estimate!!Female!!Total population!!AGE!!40 to 44 years
    S0101_C05_011E,  # Estimate!!Female!!Total population!!AGE!!45 to 49 years
    S0101_C05_012E,  # Estimate!!Female!!Total population!!AGE!!50 to 54 years
    S0101_C05_013E,  # Estimate!!Female!!Total population!!AGE!!55 to 59 years
    S0101_C05_014E,  # Estimate!!Female!!Total population!!AGE!!60 to 64 years
    S0101_C05_015E,  # Estimate!!Female!!Total population!!AGE!!65 to 69 years
    S0101_C05_016E,  # Estimate!!Female!!Total population!!AGE!!70 to 74 years
    S0101_C05_017E,  # Estimate!!Female!!Total population!!AGE!!75 to 79 years
    S0101_C05_018E,  # Estimate!!Female!!Total population!!AGE!!80 to 84 years
    S0101_C05_019E,  # Estimate!!Female!!Total population!!AGE!!85 years and over
    S0101_C05_023E)  # Estimate!!Female!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years


# transform data to match coded variables with simple field names
tbpopdata_long <- tbpopdata %>%
  pivot_longer(!ZCTA5CE20, names_to = "Column.Name", values_to = "Population.Estimate")
tbpopdata_long <- merge(tbpopdata_long, agesexfields, by = "Column.Name", all.x = TRUE)

# group age classes by decade
tbpopdata_long <- tbpopdata_long %>%
  mutate(Age = ifelse(Age.Group == "25 to 29" | Age.Group == "30 to 34", "25 - 34",
                      ifelse(Age.Group == "35 to 39" | Age.Group == "40 to 44", "35 - 44",
                             ifelse(Age.Group == "45 to 49" | Age.Group == "50 to 54", "45 - 54",
                                    ifelse(Age.Group == "55 to 59" | Age.Group == "60 to 64", "55 - 64",
                                           ifelse(Age.Group == "18 to 24", "18 - 24", "65 or older"))))))
targetpop_agesex <- tbpopdata_long[-c(1,4,5)] %>%
  group_by(ZCTA5CE20, Sex, Age) %>%
  summarise(Population = sum(Population.Estimate)) %>%
  ungroup() %>%
  # align with survey fields
  rename(ZIPCODE = ZCTA5CE20,
         GENDER = Sex,
         AGE = Age) %>%
  mutate(ZIPCODE = as.integer(ZIPCODE))

# aggregate zip codes to the first 3 digits to ensure everything is represented
targetpop_agesex <- targetpop_agesex %>%
  mutate(ZIP_AGG = as.character(substr(ZIPCODE, 1, 3)))

# calculate zip code area margin (adults per 3-digit aggregation)
margin_area <- targetpop_agesex %>%
  group_by(ZIP_AGG) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# calculate gender x age margin (adults per gender-age group for entire watershed)
margin_genderage <- targetpop_agesex %>%
  mutate(GENDERAGE = paste(GENDER, AGE, sep = " | ")) %>%
  group_by(GENDERAGE) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")



######### * RACE/ETHNICITY ######### 

# Four categories:
#  - Hispanic (any race)
#  - Non-Hispanic White (alone)
#  - Non-Hispanic Black (alone)
#  - Everyone else

### zip code 5-year population data from 2024 American Community Survey (2020-2024)
# B01001: all adults
# B01001I: Hispanic adults
# B01001H: Non-Hispanic White-alone adults
# B01001B: Black-alone adults (includes Hispanic)
# B03002: Hispanic and non-Hispanic Black (everyone)


### ADULT POPULATION

adultdata <- read.csv("tbcs-trends/population_data/ACSDT5Y2024B01001.csv")

# calculate adult only population
adultdata <- adultdata %>%
  mutate(Adults = 
           B01001_001E - # Total population
           B01001_003E - # Male < 5 years
           B01001_004E - # Male 5-9 years
           B01001_005E - # Male 10-14 years
           B01001_006E - # Male 15-17 years
           B01001_027E - # Female < 5 years
           B01001_028E - # Female 5-9 years
           B01001_029E - # Female 10-14 years
           B01001_030E) # Female 15-17 years
           
# get last 5 characters of ZCTA Name
adultdata <- adultdata %>%
  mutate(ZCTA5CE20 = str_sub(NAME, start = -5))

# keep only desired variables
adultdata <- adultdata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
         Adults)
         

### HISPANIC ADULTS

hispanicdata <- read.csv("tbcs-trends/population_data/ACSDT5Y2024B01001I.csv")

# calculate adult Hispanic population
hispanicdata <- hispanicdata %>%
  mutate(Hispanic = 
           B01001I_001E - # Total population
           B01001I_003E - # Male < 5 years
           B01001I_004E - # Male 5-9 years
           B01001I_005E - # Male 10-14 years
           B01001I_006E - # Male 15-17 years
           B01001I_018E - # Female < 5 years
           B01001I_019E - # Female 5-9 years
           B01001I_020E - # Female 10-14 years
           B01001I_021E) # Female 15-17 years

# get last 5 characters of ZCTA Name
hispanicdata <- hispanicdata %>%
  mutate(ZCTA5CE20 = str_sub(NAME, start = -5))

# keep only desired variables
hispanicdata <- hispanicdata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
         Hispanic)


### NON-HISPANIC WHITE ONLY ADULTS

whitedata <- read.csv("tbcs-trends/population_data/ACSDT5Y2024B01001H.csv")

# calculate adult Hispanic population
whitedata <- whitedata %>%
  mutate(White = 
           B01001H_001E - # Total population
           B01001H_003E - # Male < 5 years
           B01001H_004E - # Male 5-9 years
           B01001H_005E - # Male 10-14 years
           B01001H_006E - # Male 15-17 years
           B01001H_018E - # Female < 5 years
           B01001H_019E - # Female 5-9 years
           B01001H_020E - # Female 10-14 years
           B01001H_021E) # Female 15-17 years

# get last 5 characters of ZCTA Name
whitedata <- whitedata %>%
  mutate(ZCTA5CE20 = str_sub(NAME, start = -5))

# keep only desired variables
whitedata <- whitedata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
         White)


### NON-HISPANIC BLACK ONLY ADULTS

# Black population (all)
black1data <- read.csv("tbcs-trends/population_data/ACSDT5Y2024B01001B.csv")

# Hispanic and non-Hispanic population by race
black2data <- read.csv("tbcs-trends/population_data/ACSDT5Y2024B03002.csv")

# calculate adult Black (any ethnic composition) population
black1data <- black1data %>%
  mutate(BlackAny = 
           B01001B_001E - # Total population
           B01001B_003E - # Male < 5 years
           B01001B_004E - # Male 5-9 years
           B01001B_005E - # Male 10-14 years
           B01001B_006E - # Male 15-17 years
           B01001B_018E - # Female < 5 years
           B01001B_019E - # Female 5-9 years
           B01001B_020E - # Female 10-14 years
           B01001B_021E) # Female 15-17 years

# calculate the proportion of non-Hispanic Black versus Hispanic Black in zip code
black2data <- black2data %>%
  mutate(BlackNHrate = ifelse(B03002_004E == 0 & B03002_014E == 0, 0,
           B03002_004E / # Non-Hispanic Black
           (B03002_004E + B03002_014E))) # Non-Hispanic Black + Hispanic Black

# merge the data
blackdata <- merge(black1data, black2data, by = "NAME", all.x = TRUE, all.y = TRUE)

# estimate the expected number of non-Hispanic Black adults in zip code
blackdata <- blackdata %>%
  mutate(Black = as.integer(round(BlackAny * BlackNHrate, digits = 0)))
  
# get last 5 characters of ZCTA Name
blackdata <- blackdata %>%
  mutate(ZCTA5CE20 = str_sub(NAME, start = -5))

# keep only desired variables
blackdata <- blackdata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
         Black)


### OTHER ADULTS

# combine all race datasets
df_list <- list(adultdata, hispanicdata, whitedata, blackdata)
racedata <- df_list %>% 
  reduce(full_join, by = "ZCTA5CE20")

# calculate remaining population that is non-Hispanic other (other individual races, mixed races)
racedata <- racedata %>%
  mutate(Other = Adults-Hispanic-White-Black) %>% # may result in negatives due to manual estimate of non-Hispanic Black adults
  # no longer need Adults
  select(-Adults)


### Extract the population data for the ZIP codes in our watershed

# merge population data and list of TBEP zip codes
zcta$ZCTA5CE20 <- as.character(zcta$ZCTA5CE20)
tbpopdata <- merge(racedata, zcta, by = "ZCTA5CE20", all.x = FALSE, all.y = TRUE)

# keep only desired variables
tbpopdata <- tbpopdata %>%
  select(ZCTA5CE20, # Zip codes in Tampa Bay watershed boundary
         Hispanic,  # All Hispanic adults
         White,  # non-Hispanic White (only) adults
         Black,  # non-Hispanic Black (only) adults
         Other)  # non-Hispanic other adults


# transform data to match coded variables with simple field names
tbpopdata_long <- tbpopdata %>%
  pivot_longer(!ZCTA5CE20, names_to = "Race", values_to = "Population.Estimate")

# group age classes by decade
targetpop_race <- tbpopdata_long %>%
  # align with survey fields
  rename(ZIPCODE = ZCTA5CE20,
         Population = Population.Estimate,
         RACE_CAT = Race) %>%
  mutate(ZIPCODE = as.integer(ZIPCODE))


# calculate race/ethnicity margin (adults per race/ethnic group for entire watershed)
margin_race <- targetpop_race %>%
  group_by(RACE_CAT) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")



################### PREPARE SURVEY DATA ###########################

# Load individual survey data
quant2024 <- read.csv("tbcs-2024/survey_data/final/tbcs_quant_2024.csv")
qual2024 <- read.csv("tbcs-2024/survey_data/final/tbcs_qual_2024.csv")
quant2026 <- read.csv("tbcs-2026/survey_data/final/tbcs_quant_2026.csv")
qual2026 <- read.csv("tbcs-2026/survey_data/final/tbcs_qual_2026.csv")

# Add wave identifier
quant2024$WAVE <- "2024"
qual2024$WAVE <- "2024"

quant2026$WAVE <- "2026"
qual2026$WAVE <- "2026"

# Drop in-network respondents (they are not representative of population at large)
quant2024 <- quant2024 %>%
  filter(AUDIENCE != "In-network")
qual2024 <- qual2024 %>%
  filter(AUDIENCE != "In-network")
quant2026 <- quant2026 %>%
  filter(AUDIENCE != "In-network")
qual2026 <- qual2026 %>%
  filter(AUDIENCE != "In-network")

# Drop any respondents without information about zip code, gender, or age
quant2024 <- quant2024 %>%
  filter(if_all(c(ZIPCODE, AGE, GENDER), ~ !is.na(.) & . != ""))
qual2024 <- qual2024 %>%
  filter(if_all(c(ZIPCODE, AGE, GENDER), ~ !is.na(.) & . != ""))
quant2026 <- quant2026 %>%
  filter(if_all(c(ZIPCODE, AGE, GENDER), ~ !is.na(.) & . != ""))
qual2026 <- qual2026 %>%
  filter(if_all(c(ZIPCODE, AGE, GENDER), ~ !is.na(.) & . != ""))

# Drop any non-binary respondents (they are not captured in the census data)
quant2024 <- quant2024 %>%
  filter(GENDER != "Non-binary")
qual2024 <- qual2024 %>%
  filter(GENDER != "Non-binary")
quant2026 <- quant2026 %>%
  filter(GENDER != "Non-binary")
qual2026 <- qual2026 %>%
  filter(GENDER != "Non-binary")

# Drop any respondents outside of TBEP's zip codes
quant2024 <- quant2024 %>%
  filter(ZIPCODE %in% targetpop_agesex$ZIPCODE) # Lose 78 respondents (976)
qual2024 <- qual2024 %>%
  filter(ZIPCODE %in% targetpop_agesex$ZIPCODE) # Lose 78 respondents (976)
quant2026 <- quant2026 %>%
  filter(ZIPCODE %in% targetpop_agesex$ZIPCODE) # Lose 74 respondents (960)
qual2026 <- qual2026 %>%
  filter(ZIPCODE %in% targetpop_agesex$ZIPCODE) # Lose 74 respondents (960)

# Aggregate zip codes to the first 3 digits to ensure everything is represented
quant2024 <- quant2024 %>%
  mutate(ZIP_AGG = as.character(substr(ZIPCODE, 1, 3)))
qual2024 <- qual2024 %>%
  mutate(ZIP_AGG = as.character(substr(ZIPCODE, 1, 3)))
quant2026 <- quant2026 %>%
  mutate(ZIP_AGG = as.character(substr(ZIPCODE, 1, 3)))
qual2026 <- qual2026 %>%
  mutate(ZIP_AGG = as.character(substr(ZIPCODE, 1, 3)))

# Create gender x age category
quant2024 <- quant2024 %>%
  mutate(GENDERAGE = paste(GENDER, AGE, sep = " | "))
qual2024 <- qual2024 %>%
  mutate(GENDERAGE = paste(GENDER, AGE, sep = " | "))
quant2026 <- quant2026 %>%
  mutate(GENDERAGE = paste(GENDER, AGE, sep = " | "))
qual2026 <- qual2026 %>%
  mutate(GENDERAGE = paste(GENDER, AGE, sep = " | "))

# Create new race category to align with margins
quant2024 <- quant2024 %>%
  mutate(RACE_CAT = ifelse(RACE_HISPANIC == 1, "Hispanic",
                           ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                  ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "White", 
                                         ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                                ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 1 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "Black", "Other"))))))
qual2024 <- qual2024 %>%
  mutate(RACE_CAT = ifelse(RACE_HISPANIC == 1, "Hispanic",
                           ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                  ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "White", 
                                         ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                                ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 1 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "Black", "Other"))))))
quant2026 <- quant2026 %>%
  mutate(RACE_CAT = ifelse(RACE_HISPANIC == 1, "Hispanic",
                           ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                  ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "White", 
                                         ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                                ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 1 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "Black", "Other"))))))
qual2026 <- qual2026 %>%
  mutate(RACE_CAT = ifelse(RACE_HISPANIC == 1, "Hispanic",
                           ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                  ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "White", 
                                         ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 0 & RACE_MIDEAST == 1 & RACE_PACIFIC == 0 & RACE_WHITE == 1 & RACE_OTHER == 0, "White", 
                                                ifelse(RACE_NATIVE == 0 & RACE_ASIAN == 0 & RACE_BLACK == 1 & RACE_MIDEAST == 0 & RACE_PACIFIC == 0 & RACE_WHITE == 0 & RACE_OTHER == 0, "Black", "Other"))))))


################### RAKING ###########################

library(survey)

### Confirm no categories are missing any respondents

check_categories <- function(df) {
  list(
    area_missing_in_sample   = setdiff(margin_area$ZIP_AGG,        unique(df$ZIP_AGG)),
    area_missing_in_targets  = setdiff(unique(df$ZIP_AGG),         margin_area$ZIP_AGG),
    ga_missing_in_sample     = setdiff(margin_genderage$GENDERAGE, unique(df$GENDERAGE)),
    ga_missing_in_targets    = setdiff(unique(df$GENDERAGE),       margin_genderage$GENDERAGE),
    race_missing_in_sample   = setdiff(margin_race$RACE_CAT,       unique(df$RACE_CAT)),
    race_missing_in_targets  = setdiff(unique(df$RACE_CAT),        margin_race$RACE_CAT)
  )
}

check_categories(qual2024)
check_categories(qual2026)

# Respondents per area and race group
count(qual2024, ZIP_AGG); count(qual2026, ZIP_AGG)
count(qual2024, RACE_CAT); count(qual2026, RACE_CAT)


### Rake each wave

# Rescale each margin to sum to the sample size of a given wave, so the margins agree with each other and weights average 1

to_freq <- function(margin, n) {
  margin %>%
    mutate(Freq = Population / sum(Population) * n) %>%
    select(-Population)
}

rake_wave <- function(df) {
  n   <- nrow(df)
  des <- svydesign(ids = ~1, data = df, weights = ~1)
  rake(des,
       sample.margins     = list(~ZIP_AGG, ~GENDERAGE, ~RACE_CAT),
       population.margins = list(to_freq(margin_area, n),
                                 to_freq(margin_genderage, n),
                                 to_freq(margin_race, n)),
       control = list(maxit = 100, epsilon = 1e-6))
}

raked2024 <- rake_wave(qual2024)
raked2026 <- rake_wave(qual2026)

# Check the weights

weight_check <- function(raked) {
  w <- weights(raked)
  tibble(n     = length(w),
         min   = min(w),
         max   = max(w),
         mean  = mean(w),
         n_eff = sum(w)^2 / sum(w^2))
}

weight_check(raked2024)
weight_check(raked2026)

# Weighted sample should now match the targets
svytable(~RACE_CAT,  raked2026, round = TRUE)
svytable(~GENDERAGE, raked2026, round = TRUE)
svytable(~ZIP_AGG,   raked2026, round = TRUE)


### Trim (if minimum weight is < 0.20 OR maximum weight is > 5, then rerun the weight checks)

#raked2024 <- trimWeights(raked2024, lower = 0.2, upper = 5, strict = TRUE)
#raked2026 <- trimWeights(raked2026, lower = 0.2, upper = 5, strict = TRUE)


################### CREATED WEIGHTED TEMPORAL DATASET ###########################

qual2024$WEIGHT <- weights(raked2024)
qual2026$WEIGHT <- weights(raked2026)

# add to quantitative datasets
quant2024 <- quant2024 %>%
  left_join(select(qual2024, ResponseId, WEIGHT), by = "ResponseId")
quant2026 <- quant2026 %>%
  left_join(select(qual2026, ResponseId, WEIGHT), by = "ResponseId")

# Should be 0 NA weights for both
sum(is.na(quant2024$WEIGHT)); sum(is.na(quant2026$WEIGHT))


# Create final temporal datasets
qual <- bind_rows(qual2024, qual2026) %>%
  # reorder
  relocate(WEIGHT, .after = ResponseId) %>%
  relocate(WAVE, .after = ResponseId)
quant <- bind_rows(quant2024, quant2026) %>%
  # reorder
  relocate(WEIGHT, .after = ResponseId) %>%
  relocate(WAVE, .after = ResponseId)


write.csv(quant, file = "tbcs-trends/temporal_data/tbcs_quant_2024-2026.csv", row.names = FALSE)
write.csv(qual, file = "tbcs-trends/temporal_data/tbcs_qual_2024-2026.csv", row.names = FALSE)


