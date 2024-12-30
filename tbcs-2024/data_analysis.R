library(tidyverse)


# Load data

mydata <- read.csv("tbcs-2024/survey_data/final/tbcs_quant_2024.csv")
quantdata <- mydata[,-1]
rownames(quantdata) <- mydata[,1]

mydata <- read.csv("tbcs-2024/survey_data/final/tbcs_qual_2024.csv")
qualdata <- mydata[,-1]
rownames(qualdata) <- mydata[,1]



########## WHAT THEY KNOW ##################

KNOWLEDGE_WQTRND <- quantdata %>%
  mutate(KNOWLEDGE_WQTRND = ifelse(KNOWLEDGE_WQTRND == "", NA, KNOWLEDGE_WQTRND),
         KNOWLEDGE_WQTRND_C = ifelse(KNOWLEDGE_WQTRND_C == "", NA, KNOWLEDGE_WQTRND_C)) %>%
  drop_na(KNOWLEDGE_WQTRND, KNOWLEDGE_WQTRND_C) %>% 
  group_by(AUDIENCE, KNOWLEDGE_WQTRND) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_WQTRND_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Question = "In the last five years, water quality in Tampa Bay overall has...") %>%
  rename(Answer = KNOWLEDGE_WQTRND)

KNOWLEDGE_NUTRNT <- quantdata %>%
  mutate(KNOWLEDGE_NUTRNT = ifelse(KNOWLEDGE_NUTRNT == "", NA, KNOWLEDGE_NUTRNT),
         KNOWLEDGE_NUTRNT_C = ifelse(KNOWLEDGE_NUTRNT_C == "", NA, KNOWLEDGE_NUTRNT_C)) %>%
  drop_na(KNOWLEDGE_NUTRNT, KNOWLEDGE_NUTRNT_C) %>% 
  group_by(AUDIENCE, KNOWLEDGE_NUTRNT) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_NUTRNT_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Question = "What nutrient is the biggest contributor to poor water quality in Tampa Bay?") %>%
  rename(Answer = KNOWLEDGE_NUTRNT)

KNOWLEDGE_SOURCE <- quantdata %>%
  mutate(KNOWLEDGE_SOURCE = ifelse(KNOWLEDGE_SOURCE == "", NA, KNOWLEDGE_SOURCE),
         KNOWLEDGE_SOURCE_C = ifelse(KNOWLEDGE_SOURCE_C == "", NA, KNOWLEDGE_SOURCE_C)) %>%
  drop_na(KNOWLEDGE_SOURCE, KNOWLEDGE_SOURCE_C) %>% 
  group_by(AUDIENCE, KNOWLEDGE_SOURCE) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SOURCE_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Question = "What is the main source of nutrient pollution in Tampa Bay??") %>%
  rename(Answer = KNOWLEDGE_SOURCE)

KNOWLEDGE_FRTSSN <- quantdata %>%
  mutate(KNOWLEDGE_FRTSSN = ifelse(KNOWLEDGE_FRTSSN == "", NA, KNOWLEDGE_FRTSSN),
         KNOWLEDGE_FRTSSN_C = ifelse(KNOWLEDGE_FRTSSN_C == "", NA, KNOWLEDGE_FRTSSN_C)) %>%
  drop_na(KNOWLEDGE_FRTSSN, KNOWLEDGE_FRTSSN_C) %>% 
  group_by(AUDIENCE, KNOWLEDGE_FRTSSN) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_FRTSSN_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Question = "To reduce the risk of nutrient pollution in the bay, what time of year should someone avoid applying fertilizer to their lawn or garden?") %>%
  rename(Answer = KNOWLEDGE_FRTSSN)


quantdata$SGLOSS_BOAT <- ifelse(is.na(quantdata$KNOWLEDGE_SGLOSS), NA, ifelse(grepl("propeller", quantdata$KNOWLEDGE_SGLOSS), "Yes", "No"))
quantdata$SGLOSS_TEMP <- ifelse(is.na(quantdata$KNOWLEDGE_SGLOSS), NA, ifelse(grepl("ocean", quantdata$KNOWLEDGE_SGLOSS), "Yes", "No"))
quantdata$SGLOSS_MICR <- ifelse(is.na(quantdata$KNOWLEDGE_SGLOSS), NA, ifelse(grepl("Microplastics", quantdata$KNOWLEDGE_SGLOSS), "Yes", "No"))
quantdata$SGLOSS_NUTR <- ifelse(is.na(quantdata$KNOWLEDGE_SGLOSS), NA, ifelse(grepl("Nutrient", quantdata$KNOWLEDGE_SGLOSS), "Yes", "No"))
SGLOSS_BOAT <- quantdata %>%
  drop_na(SGLOSS_BOAT, KNOWLEDGE_SGLOSS_C) %>% 
  group_by(AUDIENCE, SGLOSS_BOAT) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SGLOSS_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Boat propeller blades",
         Question = "Which of the following are major causes of seagrass loss in Tampa Bay?") %>%
  rename(Answer = SGLOSS_BOAT)
SGLOSS_TEMP <- quantdata %>%
  drop_na(SGLOSS_TEMP, KNOWLEDGE_SGLOSS_C) %>% 
  group_by(AUDIENCE, SGLOSS_TEMP) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SGLOSS_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Increased ocean temperature",
         Question = "Which of the following are major causes of seagrass loss in Tampa Bay?") %>%
  rename(Answer = SGLOSS_TEMP)
SGLOSS_MICR <- quantdata %>%
  drop_na(SGLOSS_MICR, KNOWLEDGE_SGLOSS_C) %>% 
  group_by(AUDIENCE, SGLOSS_MICR) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SGLOSS_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Microplastics",
         Question = "Which of the following are major causes of seagrass loss in Tampa Bay?") %>%
  rename(Answer = SGLOSS_MICR)
SGLOSS_NUTR <- quantdata %>%
  drop_na(SGLOSS_NUTR, KNOWLEDGE_SGLOSS_C) %>% 
  group_by(AUDIENCE, SGLOSS_NUTR) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SGLOSS_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Nutrient pollution",
         Question = "Which of the following are major causes of seagrass loss in Tampa Bay?") %>%
  rename(Answer = SGLOSS_NUTR)
KNOWLEDGE_SGLOSS <- rbind(SGLOSS_BOAT, SGLOSS_TEMP, SGLOSS_MICR, SGLOSS_NUTR)
KNOWLEDGE <- rbind(KNOWLEDGE_WQTRND, KNOWLEDGE_NUTRNT, KNOWLEDGE_SOURCE, KNOWLEDGE_FRTSSN, KNOWLEDGE_SGLOSS)



quantdata$KNOWLEDGE_C_MEAN <- ((quantdata$KNOWLEDGE_WQTRND_C+quantdata$KNOWLEDGE_NUTRNT_C+quantdata$KNOWLEDGE_SOURCE_C+quantdata$KNOWLEDGE_FRTSSN_C+quantdata$KNOWLEDGE_SGLOSS_C)/5)*10
KNOWLEDGE_SCORE <- quantdata %>%
  drop_na(KNOWLEDGE_SCORE) %>% 
  group_by(AUDIENCE) %>%
  summarise(Score = mean(KNOWLEDGE_SCORE),
            Confidence = mean(KNOWLEDGE_C_MEAN))



plot_knowledge <- ggplot(quantdata, aes(KNOWLEDGE_SCORE, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/2, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.33567415730337) +
  geom_vline(xintercept = 1.06445497630332) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_confidence <- ggplot(quantdata, aes(KNOWLEDGE_C_MEAN, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/2, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 72.8539325842697) +
  geom_vline(xintercept = 59.5905213270142) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())



plot_KNOWLEDGE_WQTRND <- ggplot(KNOWLEDGE_WQTRND, aes(x = Answer, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_WQTRND_C <- ggplot(KNOWLEDGE_WQTRND, aes(x=Answer, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

plot_KNOWLEDGE_FRTSSN <- ggplot(KNOWLEDGE_FRTSSN, aes(x = Answer, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_FRTSSN_C <- ggplot(KNOWLEDGE_FRTSSN, aes(x=Answer, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

plot_KNOWLEDGE_NUTRNT <- ggplot(KNOWLEDGE_NUTRNT, aes(x = Answer, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_NUTRNT_C <- ggplot(KNOWLEDGE_NUTRNT, aes(x=Answer, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


plot_KNOWLEDGE_SOURCE <- ggplot(KNOWLEDGE_SOURCE, aes(x = Answer, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_SOURCE_C <- ggplot(KNOWLEDGE_SOURCE, aes(x=Answer, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


plot_KNOWLEDGE_SGLOSS <- ggplot(subset(KNOWLEDGE_SGLOSS, Answer == "Yes"), aes(x = Option, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_SGLOSS_C <- ggplot(subset(KNOWLEDGE_SGLOSS, Answer == "Yes"), aes(x=Option, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


quantdata$SGLOSS_CORRECT <- ifelse(is.na(quantdata$KNOWLEDGE_SGLOSS), NA, 
                                   ifelse(quantdata$SGLOSS_BOAT == "Yes" & quantdata$SGLOSS_NUTR == "Yes" & quantdata$SGLOSS_TEMP == "No" & quantdata$SGLOSS_MICR == "No", "Correct", "Incorrect"))
SGLOSS_CORRECT <- quantdata %>%
  drop_na(SGLOSS_CORRECT, KNOWLEDGE_SGLOSS_C) %>% 
  group_by(AUDIENCE, SGLOSS_CORRECT) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_SGLOSS_C)*10) %>%
  mutate(pct = n/sum(n)*100,
         Question = "Which of the following are major causes of seagrass loss in Tampa Bay?")


plot_KNOWLEDGE_SGLOSS_CORRECT <- ggplot(SGLOSS_CORRECT, aes(x = SGLOSS_CORRECT, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_KNOWLEDGE_SGLOSS_CORRECT_C <- ggplot(SGLOSS_CORRECT, aes(x=SGLOSS_CORRECT, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


INFO_TV <- quantdata %>%
  drop_na(INFO_TV) %>% 
  group_by(AUDIENCE, INFO_TV) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_TV") %>%
  rename(Answer = INFO_TV)
INFO_RADIO <- quantdata %>%
  drop_na(INFO_RADIO) %>% 
  group_by(AUDIENCE, INFO_RADIO) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_RADIO") %>%
  rename(Answer = INFO_RADIO)
INFO_PERIOD <- quantdata %>%
  drop_na(INFO_PERIOD) %>% 
  group_by(AUDIENCE, INFO_PERIOD) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_PERIOD") %>%
  rename(Answer = INFO_PERIOD)
INFO_SOCIAL <- quantdata %>%
  drop_na(INFO_SOCIAL) %>% 
  group_by(AUDIENCE, INFO_SOCIAL) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_SOCIAL") %>%
  rename(Answer = INFO_SOCIAL)
INFO_MAILE <- quantdata %>%
  drop_na(INFO_MAILE) %>% 
  group_by(AUDIENCE, INFO_MAILE) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_MAILE") %>%
  rename(Answer = INFO_MAILE)
INFO_GOV <- quantdata %>%
  drop_na(INFO_GOV) %>% 
  group_by(AUDIENCE, INFO_GOV) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_GOV") %>%
  rename(Answer = INFO_GOV)
INFO_NGO <- quantdata %>%
  drop_na(INFO_NGO) %>% 
  group_by(AUDIENCE, INFO_NGO) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_NGO") %>%
  rename(Answer = INFO_NGO)
INFO_PUBLIC <- quantdata %>%
  drop_na(INFO_PUBLIC) %>% 
  group_by(AUDIENCE, INFO_PUBLIC) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_PUBLIC") %>%
  rename(Answer = INFO_PUBLIC)
INFO_WORD <- quantdata %>%
  drop_na(INFO_WORD) %>% 
  group_by(AUDIENCE, INFO_WORD) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_WORD") %>%
  rename(Answer = INFO_WORD)
INFO_OWN <- quantdata %>%
  drop_na(INFO_OWN) %>% 
  group_by(AUDIENCE, INFO_OWN) %>%
  summarise(n = n(),
            Confidence = mean(KNOWLEDGE_C_MEAN, na.rm = TRUE)) %>%
  mutate(pct = n/sum(n)*100,
         Source = "INFO_OWN") %>%
  rename(Answer = INFO_OWN)

INFO_SOURCES <- rbind(INFO_TV,INFO_RADIO,INFO_PERIOD,INFO_SOCIAL,INFO_MAILE,INFO_GOV,INFO_NGO,INFO_PUBLIC,INFO_WORD,INFO_OWN)

plot_INFO_SOURCES <- ggplot(subset(INFO_SOURCES, Answer == 1), aes(x = Source, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

plot_INFO_SOURCES_C <- ggplot(subset(INFO_SOURCES, Answer == 1), aes(x=Source, y=Confidence, colour = AUDIENCE)) +
  geom_point() +
  ylab("Confidence") +
  ylim(0,100) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Answer) str_wrap(Answer, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



########## WHAT THEY DO ##################

# Household conservation behaviors are only relevant for homeowners
qualdata$BEHAVIOR_FERTLZ <- ifelse(is.na(qualdata$BEHAVIOR_FERTLZ), NA, 
                                   ifelse(qualdata$OWNERSHIP=="Own" & (qualdata$PROPERTY=="Single-family home" | qualdata$PROPERTY=="Townhome or duplex" | qualdata$PROPERTY=="Condominium"), qualdata$BEHAVIOR_FERTLZ, NA))
quantdata$BEHAVIOR_FERTLZ <- ifelse(is.na(quantdata$BEHAVIOR_FERTLZ), NA, 
                                    ifelse(quantdata$OWNERSHIP=="Own" & (quantdata$PROPERTY=="Single-family home" | quantdata$PROPERTY=="Townhome or duplex" | quantdata$PROPERTY=="Condominium"), quantdata$BEHAVIOR_FERTLZ, NA))
qualdata$BEHAVIOR_WATER <- ifelse(is.na(qualdata$BEHAVIOR_WATER), NA, 
                                  ifelse(qualdata$OWNERSHIP=="Own" & (qualdata$PROPERTY=="Single-family home" | qualdata$PROPERTY=="Townhome or duplex" | qualdata$PROPERTY=="Condominium"), qualdata$BEHAVIOR_WATER, NA))
quantdata$BEHAVIOR_WATER <- ifelse(is.na(quantdata$BEHAVIOR_WATER), NA, 
                                   ifelse(quantdata$OWNERSHIP=="Own" & (quantdata$PROPERTY=="Single-family home" | quantdata$PROPERTY=="Townhome or duplex" | quantdata$PROPERTY=="Condominium"), quantdata$BEHAVIOR_WATER, NA))
qualdata$BEHAVIOR_CLPPNG <- ifelse(is.na(qualdata$BEHAVIOR_CLPPNG), NA, 
                                   ifelse(qualdata$OWNERSHIP=="Own" & (qualdata$PROPERTY=="Single-family home" | qualdata$PROPERTY=="Townhome or duplex" | qualdata$PROPERTY=="Condominium"), qualdata$BEHAVIOR_CLPPNG, NA))
quantdata$BEHAVIOR_CLPPNG <- ifelse(is.na(quantdata$BEHAVIOR_CLPPNG), NA, 
                                    ifelse(quantdata$OWNERSHIP=="Own" & (quantdata$PROPERTY=="Single-family home" | quantdata$PROPERTY=="Townhome or duplex" | quantdata$PROPERTY=="Condominium"), quantdata$BEHAVIOR_CLPPNG, NA))
qualdata$BEHAVIOR_GRNINF <- ifelse(is.na(qualdata$BEHAVIOR_GRNINF), NA, 
                                   ifelse(qualdata$OWNERSHIP=="Own" & (qualdata$PROPERTY=="Single-family home" | qualdata$PROPERTY=="Townhome or duplex" | qualdata$PROPERTY=="Condominium"), qualdata$BEHAVIOR_GRNINF, NA))
quantdata$BEHAVIOR_GRNINF <- ifelse(is.na(quantdata$BEHAVIOR_GRNINF), NA, 
                                    ifelse(quantdata$OWNERSHIP=="Own" & (quantdata$PROPERTY=="Single-family home" | quantdata$PROPERTY=="Townhome or duplex" | quantdata$PROPERTY=="Condominium"), quantdata$BEHAVIOR_GRNINF, NA))

# Set factor levels for qualitative answers
qualdata <- qualdata %>%
  mutate(ACTIVITY_CMPHIK = factor(ACTIVITY_CMPHIK, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_PADBOT = factor(ACTIVITY_PADBOT, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_SWIMDV = factor(ACTIVITY_SWIMDV, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_BWATCH = factor(ACTIVITY_BWATCH, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_GARDEN = factor(ACTIVITY_GARDEN, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_FISHNG = factor(ACTIVITY_FISHNG, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_EXRCSO = factor(ACTIVITY_EXRCSO, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_RELAXO = factor(ACTIVITY_RELAXO, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_PICNIC = factor(ACTIVITY_PICNIC, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_PLYGND = factor(ACTIVITY_PLYGND, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_ZOOAQU = factor(ACTIVITY_ZOOAQU, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_LSNPRK = factor(ACTIVITY_LSNPRK, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_HSTSIT = factor(ACTIVITY_HSTSIT, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_BEACH = factor(ACTIVITY_BEACH, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         ACTIVITY_PIER = factor(ACTIVITY_PIER, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_FERTLZ = factor(BEHAVIOR_FERTLZ, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_WATER = factor(BEHAVIOR_WATER, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_CLPPNG = factor(BEHAVIOR_CLPPNG, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_GRNINF = factor(BEHAVIOR_GRNINF, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_NODRIV = factor(BEHAVIOR_NODRIV, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_RECYCL = factor(BEHAVIOR_RECYCL, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_PLNTFF = factor(BEHAVIOR_PLNTFF, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         BEHAVIOR_RESTOR = factor(BEHAVIOR_RESTOR, levels = c("Never","A few times","About once a month","About once a week","Multiple times per week")),
         AUDIENCE = factor(AUDIENCE, levels = c("Public","In-network")),
         ATTITUDE_OPPSAT = factor(ATTITUDE_OPPSAT, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")))

ENGAGE_AVG <- quantdata %>%
  drop_na(ACTIVE_AVG, PASSIVE_AVG, ATTRACT_AVG) %>% 
  group_by(AUDIENCE) %>%
  summarise(Active = mean(ACTIVE_AVG),
            Passive = mean(PASSIVE_AVG),
            Attraction = mean(ATTRACT_AVG))


####  *- Active Engagement ####

ACTIVITY_CMPHIK <- qualdata %>%
  drop_na(ACTIVITY_CMPHIK) %>% 
  group_by(AUDIENCE, ACTIVITY_CMPHIK) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Camping, hiking") %>%
  rename(Frequency = ACTIVITY_CMPHIK)
ACTIVITY_PADBOT <- qualdata %>%
  drop_na(ACTIVITY_PADBOT) %>% 
  group_by(AUDIENCE, ACTIVITY_PADBOT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Paddling, boating, sailing") %>%
  rename(Frequency = ACTIVITY_PADBOT)
ACTIVITY_SWIMDV <- qualdata %>%
  drop_na(ACTIVITY_SWIMDV) %>% 
  group_by(AUDIENCE, ACTIVITY_SWIMDV) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Swimming, snorkeling, diving in the bay") %>%
  rename(Frequency = ACTIVITY_SWIMDV)
ACTIVITY_BWATCH <- qualdata %>%
  drop_na(ACTIVITY_BWATCH) %>% 
  group_by(AUDIENCE, ACTIVITY_BWATCH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Birdwatching, wildlife viewing") %>%
  rename(Frequency = ACTIVITY_BWATCH)
ACTIVITY_GARDEN <- qualdata %>%
  drop_na(ACTIVITY_GARDEN) %>% 
  group_by(AUDIENCE, ACTIVITY_GARDEN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Home or community gardening") %>%
  rename(Frequency = ACTIVITY_GARDEN)
ACTIVITY_FISHNG <- qualdata %>%
  drop_na(ACTIVITY_FISHNG) %>% 
  group_by(AUDIENCE, ACTIVITY_FISHNG) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Fishing") %>%
  rename(Frequency = ACTIVITY_FISHNG)

active_engagement <- rbind(ACTIVITY_CMPHIK, ACTIVITY_PADBOT, ACTIVITY_SWIMDV, ACTIVITY_BWATCH, ACTIVITY_GARDEN, ACTIVITY_FISHNG)

plot_active_engagement <- ggplot(active_engagement, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Never" = "#8EC5E7",
                               "A few times" = "#3E9CD5",
                               "About once a month" = "#006FB2",
                               "About once a week" = "#004F7E",
                               "Multiple times per week" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

plot_active_avg <- ggplot(quantdata, aes(ACTIVE_AVG, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/2, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 1.699438) +
  geom_vline(xintercept = 1.060405) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())



ACTIVITY_CMPHIK <- quantdata %>%
  drop_na(ACTIVITY_CMPHIK) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_CMPHIK)) %>%
  mutate(Activity = "Camping, hiking")
ACTIVITY_PADBOT <- quantdata %>%
  drop_na(ACTIVITY_PADBOT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_PADBOT)) %>%
  mutate(Activity = "Paddling, boating, sailing")
ACTIVITY_SWIMDV <- quantdata %>%
  drop_na(ACTIVITY_SWIMDV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_SWIMDV)) %>%
  mutate(Activity = "Swimming, snorkeling, diving in the bay")
ACTIVITY_BWATCH <- quantdata %>%
  drop_na(ACTIVITY_BWATCH) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_BWATCH)) %>%
  mutate(Activity = "Birdwatching, wildlife viewing")
ACTIVITY_GARDEN <- quantdata %>%
  drop_na(ACTIVITY_GARDEN) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_GARDEN)) %>%
  mutate(Activity = "Home or community gardening")
ACTIVITY_FISHNG <- quantdata %>%
  drop_na(ACTIVITY_FISHNG) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_FISHNG)) %>%
  mutate(Activity = "Fishing")

active_engagement <- rbind(ACTIVITY_CMPHIK, ACTIVITY_PADBOT, ACTIVITY_SWIMDV, ACTIVITY_BWATCH, ACTIVITY_GARDEN, ACTIVITY_FISHNG)

plot_active_engagement <- ggplot(active_engagement, aes(x=Activity, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


#### *-- Fishing ####

FISH_LOC_SHORE <- quantdata %>%
  drop_na(FISH_LOC_SHORE) %>% 
  group_by(AUDIENCE, FISH_LOC_SHORE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Shoreline") %>%
  rename(Answer = FISH_LOC_SHORE)
FISH_LOC_PIER <- quantdata %>%
  drop_na(FISH_LOC_PIER) %>% 
  group_by(AUDIENCE, FISH_LOC_PIER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Pier") %>%
  rename(Answer = FISH_LOC_PIER)
FISH_LOC_WATER <- quantdata %>%
  drop_na(FISH_LOC_WATER) %>% 
  group_by(AUDIENCE, FISH_LOC_WATER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Water") %>%
  rename(Answer = FISH_LOC_WATER)

FISH_LOC <- rbind(FISH_LOC_SHORE, FISH_LOC_PIER, FISH_LOC_WATER)

plot_FISH_LOC <- ggplot(subset(FISH_LOC, Answer == 1), aes(x = Option, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


FISH_RSN_SPORT <- quantdata %>%
  drop_na(FISH_RSN_SPORT) %>% 
  group_by(AUDIENCE, FISH_RSN_SPORT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Sport") %>%
  rename(Answer = FISH_RSN_SPORT)
FISH_RSN_RELAX <- quantdata %>%
  drop_na(FISH_RSN_RELAX) %>% 
  group_by(AUDIENCE, FISH_RSN_RELAX) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Relax") %>%
  rename(Answer = FISH_RSN_RELAX)
FISH_RSN_FOOD <- quantdata %>%
  drop_na(FISH_RSN_FOOD) %>% 
  group_by(AUDIENCE, FISH_RSN_FOOD) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Food") %>%
  rename(Answer = FISH_RSN_FOOD)
FISH_RSN_SELL <- quantdata %>%
  drop_na(FISH_RSN_SELL) %>% 
  group_by(AUDIENCE, FISH_RSN_SELL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Sell") %>%
  rename(Answer = FISH_RSN_SELL)
FISH_RSN_WATCH <- quantdata %>%
  drop_na(FISH_RSN_WATCH) %>% 
  group_by(AUDIENCE, FISH_RSN_WATCH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Watch") %>%
  rename(Answer = FISH_RSN_WATCH)
FISH_RSN_SOCIAL <- quantdata %>%
  drop_na(FISH_RSN_SOCIAL) %>% 
  group_by(AUDIENCE, FISH_RSN_SOCIAL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Option = "Socialize") %>%
  rename(Answer = FISH_RSN_SOCIAL)

FISH_RSN <- rbind(FISH_RSN_SPORT, FISH_RSN_RELAX, FISH_RSN_FOOD, FISH_RSN_SELL, FISH_RSN_WATCH, FISH_RSN_SOCIAL)

plot_FISH_RSN <- ggplot(subset(FISH_RSN, Answer == 1), aes(x = Option, y = pct, fill = AUDIENCE)) +
  geom_bar(position=position_dodge2(reverse=TRUE), stat="identity") +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  ylim(0,100) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())




####  *- Passive Engagement ####

ACTIVITY_EXRCSO <- qualdata %>%
  drop_na(ACTIVITY_EXRCSO) %>% 
  group_by(AUDIENCE, ACTIVITY_EXRCSO) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Exercising outdoors") %>%
  rename(Frequency = ACTIVITY_EXRCSO)
ACTIVITY_RELAXO <- qualdata %>%
  drop_na(ACTIVITY_RELAXO) %>% 
  group_by(AUDIENCE, ACTIVITY_RELAXO) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Reading, sleeping, relaxing outdoors") %>%
  rename(Frequency = ACTIVITY_RELAXO)
ACTIVITY_PICNIC <- qualdata %>%
  drop_na(ACTIVITY_PICNIC) %>% 
  group_by(AUDIENCE, ACTIVITY_PICNIC) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Having a picnic or cookout") %>%
  rename(Frequency = ACTIVITY_PICNIC)

passive_engagement <- rbind(ACTIVITY_EXRCSO, ACTIVITY_RELAXO, ACTIVITY_PICNIC)

plot_passive_engagement <- ggplot(passive_engagement, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Never" = "#8EC5E7",
                               "A few times" = "#3E9CD5",
                               "About once a month" = "#006FB2",
                               "About once a week" = "#004F7E",
                               "Multiple times per week" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

plot_passive_avg <- ggplot(quantdata, aes(PASSIVE_AVG, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.447566) +
  geom_vline(xintercept = 1.933903) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())




ACTIVITY_EXRCSO <- quantdata %>%
  drop_na(ACTIVITY_EXRCSO) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_EXRCSO)) %>%
  mutate(Activity = "Exercising outdoors")
ACTIVITY_RELAXO <- quantdata %>%
  drop_na(ACTIVITY_RELAXO) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_RELAXO)) %>%
  mutate(Activity = "Reading, sleeping, relaxing outdoors")
ACTIVITY_PICNIC <- quantdata %>%
  drop_na(ACTIVITY_PICNIC) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_PICNIC)) %>%
  mutate(Activity = "Having a picnic or cookout")

passive_engagement <- rbind(ACTIVITY_EXRCSO, ACTIVITY_RELAXO, ACTIVITY_PICNIC)

plot_passive_engagement <- ggplot(passive_engagement, aes(x=Activity, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


####  *- Attraction Engagement ####

ACTIVITY_PLYGND <- qualdata %>%
  drop_na(ACTIVITY_PLYGND) %>% 
  group_by(AUDIENCE, ACTIVITY_PLYGND) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting an outdoor playground") %>%
  rename(Frequency = ACTIVITY_PLYGND)
ACTIVITY_ZOOAQU <- qualdata %>%
  drop_na(ACTIVITY_ZOOAQU) %>% 
  group_by(AUDIENCE, ACTIVITY_ZOOAQU) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting a zoo, aquarium") %>%
  rename(Frequency = ACTIVITY_ZOOAQU)
ACTIVITY_LSNPRK <- qualdata %>%
  drop_na(ACTIVITY_LSNPRK) %>% 
  group_by(AUDIENCE, ACTIVITY_LSNPRK) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting a local, state, national park") %>%
  rename(Frequency = ACTIVITY_LSNPRK)
ACTIVITY_HSTSIT <- qualdata %>%
  drop_na(ACTIVITY_HSTSIT) %>% 
  group_by(AUDIENCE, ACTIVITY_HSTSIT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting a museum, historical place, archaeological site") %>%
  rename(Frequency = ACTIVITY_HSTSIT)
ACTIVITY_BEACH <- qualdata %>%
  drop_na(ACTIVITY_BEACH) %>% 
  group_by(AUDIENCE, ACTIVITY_BEACH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting a beach") %>%
  rename(Frequency = ACTIVITY_BEACH)
ACTIVITY_PIER <- qualdata %>%
  drop_na(ACTIVITY_PIER) %>% 
  group_by(AUDIENCE, ACTIVITY_PIER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "Visiting a pier, marina") %>%
  rename(Frequency = ACTIVITY_PIER)

attract_engagement <- rbind(ACTIVITY_PLYGND, ACTIVITY_ZOOAQU, ACTIVITY_LSNPRK, ACTIVITY_HSTSIT, ACTIVITY_BEACH, ACTIVITY_PIER)

plot_attract_engagement <- ggplot(attract_engagement, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Never" = "#8EC5E7",
                               "A few times" = "#3E9CD5",
                               "About once a month" = "#006FB2",
                               "About once a week" = "#004F7E",
                               "Multiple times per week" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


plot_attract_avg <- ggplot(quantdata, aes(ATTRACT_AVG, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 1.332397) +
  geom_vline(xintercept = 1.143106) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())





ACTIVITY_PLYGND <- quantdata %>%
  drop_na(ACTIVITY_PLYGND) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_PLYGND)) %>%
  mutate(Activity = "Visiting an outdoor playground")
ACTIVITY_ZOOAQU <- quantdata %>%
  drop_na(ACTIVITY_ZOOAQU) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_ZOOAQU)) %>%
  mutate(Activity = "Visiting a zoo, aquarium")
ACTIVITY_LSNPRK <- quantdata %>%
  drop_na(ACTIVITY_LSNPRK) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_LSNPRK)) %>%
  mutate(Activity = "Visiting a local, state, national park")
ACTIVITY_HSTSIT <- quantdata %>%
  drop_na(ACTIVITY_HSTSIT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_HSTSIT)) %>%
  mutate(Activity = "Visiting a museum, historical place, archaeological site")
ACTIVITY_BEACH <- quantdata %>%
  drop_na(ACTIVITY_BEACH) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_BEACH)) %>%
  mutate(Activity = "Visiting a beach")
ACTIVITY_PIER <- quantdata %>%
  drop_na(ACTIVITY_PIER) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ACTIVITY_PIER)) %>%
  mutate(Activity = "Visiting a pier, marina")

attract_engagement <- rbind(ACTIVITY_PLYGND, ACTIVITY_ZOOAQU, ACTIVITY_LSNPRK, ACTIVITY_HSTSIT, ACTIVITY_BEACH, ACTIVITY_PIER)

plot_attract_engagement <- ggplot(attract_engagement, aes(x=Activity, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



####  *- Behaviors ####

# Reverse code behaviors so they measure pro-environmental tendencies
quantdata$BEHAVIOR_FERTLZ.R <- ifelse(is.na(quantdata$BEHAVIOR_FERTLZ), NA, 
                                      ifelse(quantdata$BEHAVIOR_FERTLZ == 0, 4,
                                             ifelse(quantdata$BEHAVIOR_FERTLZ == 1, 3,
                                                    ifelse(quantdata$BEHAVIOR_FERTLZ == 2, 2,
                                                           ifelse(quantdata$BEHAVIOR_FERTLZ == 3, 1,
                                                                  ifelse(quantdata$BEHAVIOR_FERTLZ == 4, 0,NA))))))
quantdata$BEHAVIOR_WATER.R <- ifelse(is.na(quantdata$BEHAVIOR_WATER), NA, 
                                     ifelse(quantdata$BEHAVIOR_WATER == 0, 4,
                                            ifelse(quantdata$BEHAVIOR_WATER == 1, 3,
                                                   ifelse(quantdata$BEHAVIOR_WATER == 2, 2,
                                                          ifelse(quantdata$BEHAVIOR_WATER == 3, 1,
                                                                 ifelse(quantdata$BEHAVIOR_WATER == 4, 0,NA))))))
quantdata$BEHAVIOR_CLPPNG.R <- ifelse(is.na(quantdata$BEHAVIOR_CLPPNG), NA, 
                                      ifelse(quantdata$BEHAVIOR_CLPPNG == 0, 4,
                                             ifelse(quantdata$BEHAVIOR_CLPPNG == 1, 3,
                                                    ifelse(quantdata$BEHAVIOR_CLPPNG == 2, 2,
                                                           ifelse(quantdata$BEHAVIOR_CLPPNG == 3, 1,
                                                                  ifelse(quantdata$BEHAVIOR_CLPPNG == 4, 0,NA))))))
quantdata$BEHAVIOR_GRNINF2 <- quantdata$BEHAVIOR_GRNINF
quantdata$BEHAVIOR_NODRIV2 <- quantdata$BEHAVIOR_NODRIV
quantdata$BEHAVIOR_RECYCL2 <- quantdata$BEHAVIOR_RECYCL
quantdata$BEHAVIOR_PLNTFF2 <- quantdata$BEHAVIOR_PLNTFF
quantdata$BEHAVIOR_RESTOR2 <- quantdata$BEHAVIOR_RESTOR

quantdata <- quantdata %>%
  mutate(BEHAVIOR_GENPEB = apply(quantdata[,210:213], MARGIN = 1, FUN = mean, na.rm = TRUE),
         BEHAVIOR_GENPEB = na_if(BEHAVIOR_GENPEB, -Inf),
         BEHAVIOR_GENPEB = ifelse(BEHAVIOR_GENPEB=="NaN",NA,BEHAVIOR_GENPEB),
         BEHAVIOR_HOWPEB = apply(quantdata[,206:209], MARGIN = 1, FUN = mean, na.rm = TRUE),
         BEHAVIOR_HOWPEB = na_if(BEHAVIOR_HOWPEB, -Inf),
         BEHAVIOR_HOWPEB = ifelse(BEHAVIOR_HOWPEB=="NaN",NA,BEHAVIOR_HOWPEB))


#### *-- General PEB ####

BEHAVIOR_NODRIV <- qualdata %>%
  drop_na(BEHAVIOR_NODRIV) %>% 
  group_by(AUDIENCE, BEHAVIOR_NODRIV) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Choosing to walk, bike, carpool instead of driving") %>%
  rename(Frequency = BEHAVIOR_NODRIV)
BEHAVIOR_RECYCL <- qualdata %>%
  drop_na(BEHAVIOR_RECYCL) %>% 
  group_by(AUDIENCE, BEHAVIOR_RECYCL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Recycling household products") %>%
  rename(Frequency = BEHAVIOR_RECYCL)
BEHAVIOR_PLNTFF <- qualdata %>%
  drop_na(BEHAVIOR_PLNTFF) %>% 
  group_by(AUDIENCE, BEHAVIOR_PLNTFF) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Planting, maintaining native or FF plants") %>%
  rename(Frequency = BEHAVIOR_PLNTFF)
BEHAVIOR_RESTOR <- qualdata %>%
  drop_na(BEHAVIOR_RESTOR) %>% 
  group_by(AUDIENCE, BEHAVIOR_RESTOR) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Restoring natural habitats") %>%
  rename(Frequency = BEHAVIOR_RESTOR)

generalpeb <- rbind(BEHAVIOR_NODRIV, BEHAVIOR_RECYCL, BEHAVIOR_PLNTFF, BEHAVIOR_RESTOR)

plot_generalpeb <- ggplot(generalpeb, aes(x=Behavior, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Never" = "#8EC5E7",
                               "A few times" = "#3E9CD5",
                               "About once a month" = "#006FB2",
                               "About once a week" = "#004F7E",
                               "Multiple times per week" = "#003351")) +
  scale_x_discrete(labels = function(Behavior) str_wrap(Behavior, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


GENPEB_AVG <- quantdata %>%
  drop_na(BEHAVIOR_GENPEB) %>% 
  group_by(AUDIENCE) %>%
  summarise(PEB_AVG = mean(BEHAVIOR_GENPEB))


plot_generalpeb_avg <- ggplot(quantdata, aes(BEHAVIOR_GENPEB, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.199438) +
  geom_vline(xintercept = 1.576887) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())



BEHAVIOR_NODRIV <- quantdata %>%
  drop_na(BEHAVIOR_NODRIV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_NODRIV)) %>%
  mutate(Behavior = "Choosing to walk, bike, carpool instead of driving")
BEHAVIOR_RECYCL <- quantdata %>%
  drop_na(BEHAVIOR_RECYCL) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_RECYCL)) %>%
  mutate(Behavior = "Recycling household products")
BEHAVIOR_PLNTFF <- quantdata %>%
  drop_na(BEHAVIOR_PLNTFF) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_PLNTFF)) %>%
  mutate(Behavior = "Planting, maintaining native or FF plants")
BEHAVIOR_RESTOR <- quantdata %>%
  drop_na(BEHAVIOR_RESTOR) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_RESTOR)) %>%
  mutate(Behavior = "Restoring natural habitats")

behaviors <- rbind(BEHAVIOR_NODRIV, BEHAVIOR_RECYCL, BEHAVIOR_PLNTFF, BEHAVIOR_RESTOR)

plot_behaviors <- ggplot(behaviors, aes(x=Behavior, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Behavior) str_wrap(Behavior, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


#### *--- Norms ####

NORM_NODRIV <- quantdata %>%
  drop_na(BEHAVIOR_NODRIV, NORM_NODRIV) %>% 
  group_by(AUDIENCE, BEHAVIOR_NODRIV) %>%
  summarise(n = n(),
            Mean = mean(NORM_NODRIV),
            SD = sd(NORM_NODRIV)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_NODRIV") %>%
  rename(Score = BEHAVIOR_NODRIV)
NORM_RECYCL <- quantdata %>%
  drop_na(BEHAVIOR_RECYCL, NORM_RECYCL) %>% 
  group_by(AUDIENCE, BEHAVIOR_RECYCL) %>%
  summarise(n = n(),
            Mean = mean(NORM_RECYCL),
            SD = sd(NORM_RECYCL)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_RECYCL") %>%
  rename(Score = BEHAVIOR_RECYCL)
NORM_PLNTFF <- quantdata %>%
  drop_na(BEHAVIOR_PLNTFF, NORM_PLNTFF) %>% 
  group_by(AUDIENCE, BEHAVIOR_PLNTFF) %>%
  summarise(n = n(),
            Mean = mean(NORM_PLNTFF),
            SD = sd(NORM_PLNTFF)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_PLNTFF") %>%
  rename(Score = BEHAVIOR_PLNTFF)
NORM_RESTOR <- quantdata %>%
  drop_na(BEHAVIOR_RESTOR, NORM_RESTOR) %>% 
  group_by(AUDIENCE, BEHAVIOR_RESTOR) %>%
  summarise(n = n(),
            Mean = mean(NORM_RESTOR),
            SD = sd(NORM_RESTOR)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_RESTOR") %>%
  rename(Score = BEHAVIOR_RESTOR)

generalnorms <- rbind(NORM_NODRIV, NORM_RECYCL, NORM_PLNTFF, NORM_RESTOR)

plot_generalnorms <- ggplot(generalnorms, aes(x=Score, y=Mean, colour = AUDIENCE, group = AUDIENCE)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x=Score, ymax=Mean+SD, ymin=Mean-SD), position = position_dodge(width = 0.9)) +
  ylab("Mean") +
  ylim(-2,2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  facet_wrap(~AUDIENCE) + 
  facet_grid(~Behavior) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


#### *-- Homeowner PEB ####

qualhomeowners <- subset(qualdata, OWNERSHIP == "Own" & (PROPERTY == "Single-family home" | 
                                                           PROPERTY == "Townhome or duplex" | 
                                                           PROPERTY == "Condominium"))
quanthomeowners <- subset(quantdata, OWNERSHIP == "Own" & (PROPERTY == "Single-family home" | 
                                                             PROPERTY == "Townhome or duplex" | 
                                                             PROPERTY == "Condominium"))

BEHAVIOR_FERTLZ <- qualhomeowners %>%
  drop_na(BEHAVIOR_FERTLZ) %>% 
  group_by(AUDIENCE, BEHAVIOR_FERTLZ) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Fertilizing lawn, garden") %>%
  rename(Frequency = BEHAVIOR_FERTLZ)
BEHAVIOR_WATER <- qualhomeowners %>%
  drop_na(BEHAVIOR_WATER) %>% 
  group_by(AUDIENCE, BEHAVIOR_WATER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Watering lawn, garden") %>%
  rename(Frequency = BEHAVIOR_WATER)
BEHAVIOR_CLPPNG <- qualhomeowners %>%
  drop_na(BEHAVIOR_CLPPNG) %>% 
  group_by(AUDIENCE, BEHAVIOR_CLPPNG) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Blowing grass clippings onto street") %>%
  rename(Frequency = BEHAVIOR_CLPPNG)
BEHAVIOR_GRNINF <- qualhomeowners %>%
  drop_na(BEHAVIOR_GRNINF) %>% 
  group_by(AUDIENCE, BEHAVIOR_GRNINF) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "Maintaining a rain barrel, rain garden, swale") %>%
  rename(Frequency = BEHAVIOR_GRNINF)


homeownerpeb <- rbind(BEHAVIOR_FERTLZ, BEHAVIOR_WATER, BEHAVIOR_CLPPNG, BEHAVIOR_GRNINF)

plot_homeownerpeb <- ggplot(homeownerpeb, aes(x=Behavior, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Never" = "#8EC5E7",
                               "A few times" = "#3E9CD5",
                               "About once a month" = "#006FB2",
                               "About once a week" = "#004F7E",
                               "Multiple times per week" = "#003351")) +
  scale_x_discrete(labels = function(Behavior) str_wrap(Behavior, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


HOWPEB_AVG <- quanthomeowners %>%
  drop_na(BEHAVIOR_HOWPEB) %>% 
  group_by(AUDIENCE) %>%
  summarise(PEB_AVG = mean(BEHAVIOR_HOWPEB))


plot_homeownerpeb_avg <- ggplot(quanthomeowners, aes(BEHAVIOR_HOWPEB, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.653974) +
  geom_vline(xintercept = 2.289608) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())



BEHAVIOR_FERTLZ <- quanthomeowners %>%
  drop_na(BEHAVIOR_FERTLZ) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_FERTLZ)) %>%
  mutate(Behavior = "Fertilizing lawn, garden")
BEHAVIOR_WATER <- quanthomeowners %>%
  drop_na(BEHAVIOR_WATER) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_WATER)) %>%
  mutate(Behavior = "Watering lawn, garden")
BEHAVIOR_CLPPNG <- quanthomeowners %>%
  drop_na(BEHAVIOR_CLPPNG) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_CLPPNG)) %>%
  mutate(Behavior = "Blowing grass clippings onto street")
BEHAVIOR_GRNINF <- quanthomeowners %>%
  drop_na(BEHAVIOR_GRNINF) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(BEHAVIOR_GRNINF)) %>%
  mutate(Behavior = "Maintaining a rain barrel, rain garden, swale")


behaviors <- rbind(BEHAVIOR_FERTLZ, BEHAVIOR_WATER, BEHAVIOR_CLPPNG, BEHAVIOR_GRNINF)

plot_behaviors <- ggplot(behaviors, aes(x=Behavior, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Behavior) str_wrap(Behavior, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



#### *--- Norms ####

NORM_FERTLZ <- quanthomeowners %>%
  drop_na(BEHAVIOR_FERTLZ, NORM_FERTLZ) %>% 
  group_by(AUDIENCE, BEHAVIOR_FERTLZ) %>%
  summarise(n = n(),
            Mean = mean(NORM_FERTLZ),
            SD = sd(NORM_FERTLZ)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_FERTLZ") %>%
  rename(Score = BEHAVIOR_FERTLZ)
NORM_WATER <- quanthomeowners %>%
  drop_na(BEHAVIOR_WATER, NORM_WATER) %>% 
  group_by(AUDIENCE, BEHAVIOR_WATER) %>%
  summarise(n = n(),
            Mean = mean(NORM_WATER),
            SD = sd(NORM_WATER)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_WATER") %>%
  rename(Score = BEHAVIOR_WATER)
NORM_CLPPNG <- quanthomeowners %>%
  drop_na(BEHAVIOR_CLPPNG, NORM_CLPPNG) %>% 
  group_by(AUDIENCE, BEHAVIOR_CLPPNG) %>%
  summarise(n = n(),
            Mean = mean(NORM_CLPPNG),
            SD = sd(NORM_CLPPNG)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_CLPPNG") %>%
  rename(Score = BEHAVIOR_CLPPNG)
NORM_GRNINF <- quanthomeowners %>%
  drop_na(BEHAVIOR_GRNINF, NORM_GRNINF) %>% 
  group_by(AUDIENCE, BEHAVIOR_GRNINF) %>%
  summarise(n = n(),
            Mean = mean(NORM_GRNINF),
            SD = sd(NORM_GRNINF)) %>%
  mutate(pct = n/sum(n)*100,
         Behavior = "NORM_GRNINF") %>%
  rename(Score = BEHAVIOR_GRNINF)

homeownernorms <- rbind(NORM_FERTLZ, NORM_WATER, NORM_CLPPNG, NORM_GRNINF)

plot_homeownernorms <- ggplot(homeownernorms, aes(x=Score, y=Mean, colour = AUDIENCE, group = AUDIENCE)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x=Score, ymax=Mean+SD, ymin=Mean-SD), position = position_dodge(width = 0.9)) +
  ylab("Mean") +
  ylim(-2,2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  facet_wrap(~AUDIENCE) + 
  facet_grid(~Behavior) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



#### *--- Fertilizer ####

FERTLZ_SUMMER <- quanthomeowners %>%
  drop_na(FERTLZ_SUMMER) %>% 
  group_by(AUDIENCE, FERTLZ_SUMMER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  rename(FertilizeSummer = FERTLZ_SUMMER)
FERTLZ_KNOWLEDGE <- quanthomeowners %>%
  drop_na(FERTLZ_SUMMER, KNOWLEDGE_FRTSSN) %>% 
  group_by(AUDIENCE, FERTLZ_SUMMER, KNOWLEDGE_FRTSSN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  rename(FertilizeSummer = FERTLZ_SUMMER,
         Answer = KNOWLEDGE_FRTSSN)




########## WHAT THEY WANT ##################

# Set factor levels for qualitative answers
qualdata <- qualdata %>%
  mutate(PRIORITY_ACSNAT = ifelse(PRIORITY_ACSNAT=="", NA, PRIORITY_ACSNAT), 
         PRIORITY_ACSNAT = factor(PRIORITY_ACSNAT, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_TRASH = ifelse(PRIORITY_TRASH=="", NA, PRIORITY_TRASH), 
         PRIORITY_TRASH = factor(PRIORITY_TRASH, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_AIRQTY = ifelse(PRIORITY_AIRQTY=="", NA, PRIORITY_AIRQTY), 
         PRIORITY_AIRQTY = factor(PRIORITY_AIRQTY, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_WTRQTY = ifelse(PRIORITY_WTRQTY=="", NA, PRIORITY_WTRQTY), 
         PRIORITY_WTRQTY = factor(PRIORITY_WTRQTY, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_JOBINC = ifelse(PRIORITY_JOBINC=="", NA, PRIORITY_JOBINC), 
         PRIORITY_JOBINC = factor(PRIORITY_JOBINC, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_WAGINC = ifelse(PRIORITY_WAGINC=="", NA, PRIORITY_WAGINC), 
         PRIORITY_WAGINC = factor(PRIORITY_WAGINC, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_CSTLIV = ifelse(PRIORITY_CSTLIV=="", NA, PRIORITY_CSTLIV), 
         PRIORITY_CSTLIV = factor(PRIORITY_CSTLIV, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_JOBDIV = ifelse(PRIORITY_JOBDIV=="", NA, PRIORITY_JOBDIV), 
         PRIORITY_JOBDIV = factor(PRIORITY_JOBDIV, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_PEDCYC = ifelse(PRIORITY_PEDCYC=="", NA, PRIORITY_PEDCYC), 
         PRIORITY_PEDCYC = factor(PRIORITY_PEDCYC, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_RODBRG = ifelse(PRIORITY_RODBRG=="", NA, PRIORITY_RODBRG), 
         PRIORITY_RODBRG = factor(PRIORITY_RODBRG, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_UTILTY = ifelse(PRIORITY_UTILTY=="", NA, PRIORITY_UTILTY), 
         PRIORITY_UTILTY = factor(PRIORITY_UTILTY, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_FLDPRT = ifelse(PRIORITY_FLDPRT=="", NA, PRIORITY_FLDPRT), 
         PRIORITY_FLDPRT = factor(PRIORITY_FLDPRT, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_ACSEDU = ifelse(PRIORITY_ACSEDU=="", NA, PRIORITY_ACSEDU), 
         PRIORITY_ACSEDU = factor(PRIORITY_ACSEDU, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_ACSHLT = ifelse(PRIORITY_ACSHLT=="", NA, PRIORITY_ACSHLT), 
         PRIORITY_ACSHLT = factor(PRIORITY_ACSHLT, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_HOUSNG = ifelse(PRIORITY_HOUSNG=="", NA, PRIORITY_HOUSNG), 
         PRIORITY_HOUSNG = factor(PRIORITY_HOUSNG, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")),
         PRIORITY_PUBTRN = ifelse(PRIORITY_PUBTRN=="", NA, PRIORITY_PUBTRN), 
         PRIORITY_PUBTRN = factor(PRIORITY_PUBTRN, levels = c("Not a priority","Low priority","Moderate priority","High priority","Top priority")))

PRIORITY_ACSNAT <- qualdata %>%
  drop_na(PRIORITY_ACSNAT) %>% 
  group_by(AUDIENCE, PRIORITY_ACSNAT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "ACSNAT") %>%
  rename(Ranking = PRIORITY_ACSNAT)
PRIORITY_TRASH <- qualdata %>%
  drop_na(PRIORITY_TRASH) %>% 
  group_by(AUDIENCE, PRIORITY_TRASH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "TRASH") %>%
  rename(Ranking = PRIORITY_TRASH)
PRIORITY_AIRQTY <- qualdata %>%
  drop_na(PRIORITY_AIRQTY) %>% 
  group_by(AUDIENCE, PRIORITY_AIRQTY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "AIRQTY") %>%
  rename(Ranking = PRIORITY_AIRQTY)
PRIORITY_WTRQTY <- qualdata %>%
  drop_na(PRIORITY_WTRQTY) %>% 
  group_by(AUDIENCE, PRIORITY_WTRQTY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "WTRQTY") %>%
  rename(Ranking = PRIORITY_WTRQTY)
PRIORITY_JOBINC <- qualdata %>%
  drop_na(PRIORITY_JOBINC) %>% 
  group_by(AUDIENCE, PRIORITY_JOBINC) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "JOBINC") %>%
  rename(Ranking = PRIORITY_JOBINC)
PRIORITY_WAGINC <- qualdata %>%
  drop_na(PRIORITY_WAGINC) %>% 
  group_by(AUDIENCE, PRIORITY_WAGINC) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "WAGINC") %>%
  rename(Ranking = PRIORITY_WAGINC)
PRIORITY_CSTLIV <- qualdata %>%
  drop_na(PRIORITY_CSTLIV) %>% 
  group_by(AUDIENCE, PRIORITY_CSTLIV) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "CSTLIV") %>%
  rename(Ranking = PRIORITY_CSTLIV)
PRIORITY_JOBDIV <- qualdata %>%
  drop_na(PRIORITY_JOBDIV) %>% 
  group_by(AUDIENCE, PRIORITY_JOBDIV) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "JOBDIV") %>%
  rename(Ranking = PRIORITY_JOBDIV)
PRIORITY_PEDCYC <- qualdata %>%
  drop_na(PRIORITY_PEDCYC) %>% 
  group_by(AUDIENCE, PRIORITY_PEDCYC) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "PEDCYC") %>%
  rename(Ranking = PRIORITY_PEDCYC)
PRIORITY_RODBRG <- qualdata %>%
  drop_na(PRIORITY_RODBRG) %>% 
  group_by(AUDIENCE, PRIORITY_RODBRG) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "RODBRG") %>%
  rename(Ranking = PRIORITY_RODBRG)
PRIORITY_UTILTY <- qualdata %>%
  drop_na(PRIORITY_UTILTY) %>% 
  group_by(AUDIENCE, PRIORITY_UTILTY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "UTILTY") %>%
  rename(Ranking = PRIORITY_UTILTY)
PRIORITY_FLDPRT <- qualdata %>%
  drop_na(PRIORITY_FLDPRT) %>% 
  group_by(AUDIENCE, PRIORITY_FLDPRT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "FLDPRT") %>%
  rename(Ranking = PRIORITY_FLDPRT)
PRIORITY_ACSEDU <- qualdata %>%
  drop_na(PRIORITY_ACSEDU) %>% 
  group_by(AUDIENCE, PRIORITY_ACSEDU) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "ACSEDU") %>%
  rename(Ranking = PRIORITY_ACSEDU)
PRIORITY_ACSHLT <- qualdata %>%
  drop_na(PRIORITY_ACSHLT) %>% 
  group_by(AUDIENCE, PRIORITY_ACSHLT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "ACSHLT") %>%
  rename(Ranking = PRIORITY_ACSHLT)
PRIORITY_HOUSNG <- qualdata %>%
  drop_na(PRIORITY_HOUSNG) %>% 
  group_by(AUDIENCE, PRIORITY_HOUSNG) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "HOUSNG") %>%
  rename(Ranking = PRIORITY_HOUSNG)
PRIORITY_PUBTRN <- qualdata %>%
  drop_na(PRIORITY_PUBTRN) %>% 
  group_by(AUDIENCE, PRIORITY_PUBTRN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Priority = "PUBTRN") %>%
  rename(Ranking = PRIORITY_PUBTRN)

priorities_indiv <- rbind(PRIORITY_ACSNAT, PRIORITY_TRASH, PRIORITY_AIRQTY, PRIORITY_WTRQTY, PRIORITY_JOBINC, PRIORITY_WAGINC, PRIORITY_CSTLIV, PRIORITY_JOBDIV, PRIORITY_PEDCYC, PRIORITY_RODBRG, PRIORITY_UTILTY, PRIORITY_FLDPRT, PRIORITY_ACSEDU, PRIORITY_ACSHLT, PRIORITY_HOUSNG, PRIORITY_PUBTRN)


plot_priorities_indiv <- ggplot(priorities_indiv, aes(x=Priority, y=pct, fill=Ranking)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Not a priority" = "#8EC5E7",
                               "Low priority" = "#3E9CD5",
                               "Moderate priority" = "#006FB2",
                               "High priority" = "#004F7E",
                               "Top priority" = "#003351")) +
  scale_x_discrete(labels = function(Priority) str_wrap(Priority, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



PRIORITY_ACSNAT <- quantdata %>%
  drop_na(PRIORITY_ACSNAT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_ACSNAT)) %>%
  mutate(Priority = "ACSNAT")
PRIORITY_TRASH <- quantdata %>%
  drop_na(PRIORITY_TRASH) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_TRASH)) %>%
  mutate(Priority = "TRASH")
PRIORITY_AIRQTY <- quantdata %>%
  drop_na(PRIORITY_AIRQTY) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_AIRQTY)) %>%
  mutate(Priority = "AIRQTY")
PRIORITY_WTRQTY <- quantdata %>%
  drop_na(PRIORITY_WTRQTY) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_WTRQTY)) %>%
  mutate(Priority = "WTRQTY")
PRIORITY_JOBINC <- quantdata %>%
  drop_na(PRIORITY_JOBINC) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_JOBINC)) %>%
  mutate(Priority = "JOBINC")
PRIORITY_WAGINC <- quantdata %>%
  drop_na(PRIORITY_WAGINC) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_WAGINC)) %>%
  mutate(Priority = "WAGINC")
PRIORITY_CSTLIV <- quantdata %>%
  drop_na(PRIORITY_CSTLIV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_CSTLIV)) %>%
  mutate(Priority = "CSTLIV")
PRIORITY_JOBDIV <- quantdata %>%
  drop_na(PRIORITY_JOBDIV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_JOBDIV)) %>%
  mutate(Priority = "JOBDIV")
PRIORITY_PEDCYC <- quantdata %>%
  drop_na(PRIORITY_PEDCYC) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_PEDCYC)) %>%
  mutate(Priority = "PEDCYC")
PRIORITY_RODBRG <- quantdata %>%
  drop_na(PRIORITY_RODBRG) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_RODBRG)) %>%
  mutate(Priority = "RODBRG")
PRIORITY_UTILTY <- quantdata %>%
  drop_na(PRIORITY_UTILTY) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_UTILTY)) %>%
  mutate(Priority = "UTILTY")
PRIORITY_FLDPRT <- quantdata %>%
  drop_na(PRIORITY_FLDPRT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_FLDPRT)) %>%
  mutate(Priority = "FLDPRT")
PRIORITY_ACSEDU <- quantdata %>%
  drop_na(PRIORITY_ACSEDU) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_ACSEDU)) %>%
  mutate(Priority = "ACSEDU")
PRIORITY_ACSHLT <- quantdata %>%
  drop_na(PRIORITY_ACSHLT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_ACSHLT)) %>%
  mutate(Priority = "ACSHLT")
PRIORITY_HOUSNG <- quantdata %>%
  drop_na(PRIORITY_HOUSNG) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_HOUSNG)) %>%
  mutate(Priority = "HOUSNG")
PRIORITY_PUBTRN <- quantdata %>%
  drop_na(PRIORITY_PUBTRN) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_PUBTRN)) %>%
  mutate(Priority = "PUBTRN")

priorities_indiv <- rbind(PRIORITY_ACSNAT, PRIORITY_TRASH, PRIORITY_AIRQTY, PRIORITY_WTRQTY, PRIORITY_JOBINC, PRIORITY_WAGINC, PRIORITY_CSTLIV, PRIORITY_JOBDIV, PRIORITY_PEDCYC, PRIORITY_RODBRG, PRIORITY_UTILTY, PRIORITY_FLDPRT, PRIORITY_ACSEDU, PRIORITY_ACSHLT, PRIORITY_HOUSNG, PRIORITY_PUBTRN)

plot_priorities_indiv <- ggplot(priorities_indiv, aes(x=Priority, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



PRIORITIZE_ENVIRON <- quantdata %>%
  drop_na(PRIORITIZE_ENVIRON) %>% 
  group_by(AUDIENCE, PRIORITIZE_ENVIRON) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

PRIORITY_ENVIRON <- quantdata %>%
  drop_na(PRIORITY_ENVIRON) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_ENVIRON))

plot_ENVIRON <- ggplot(quantdata, aes(PRIORITY_ENVIRON, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.872191) +
  geom_vline(xintercept = 2.498343) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


PRIORITY_ECONOMY <- quantdata %>%
  drop_na(PRIORITY_ECONOMY) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_ECONOMY))

plot_ECONOMY <- ggplot(quantdata, aes(PRIORITY_ECONOMY, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.324438) +
  geom_vline(xintercept = 2.712831) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

PRIORITY_INFRAST <- quantdata %>%
  drop_na(PRIORITY_INFRAST) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_INFRAST))

plot_INFRAST <- ggplot(quantdata, aes(PRIORITY_INFRAST, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.764045) +
  geom_vline(xintercept = 2.597538) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())

PRIORITY_SOCSERV <- quantdata %>%
  drop_na(PRIORITY_SOCSERV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PRIORITY_SOCSERV))

plot_SOCSERV <- ggplot(quantdata, aes(PRIORITY_SOCSERV, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.450843) +
  geom_vline(xintercept = 2.545928) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())




########## HOW THEY FEEL ##################


####  *- Nature Relatedness ####

NRELATEDNESS <- quantdata %>%
  drop_na(NRELATEDNESS) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(NRELATEDNESS))

plot_NRELATEDNESS <- ggplot(quantdata, aes(NRELATEDNESS, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 3.187266) +
  geom_vline(xintercept = 2.339471) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Place Identity ####

PIDENTITY <- quantdata %>%
  drop_na(PIDENTITY) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(PIDENTITY))

plot_PIDENTITY <- ggplot(quantdata, aes(PIDENTITY, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 3.063670) +
  geom_vline(xintercept = 2.540881) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Solastalgia ####

SOLASTALGIA <- quantdata %>%
  drop_na(SOLASTALGIA) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(SOLASTALGIA))

plot_SOLASTALGIA <- ggplot(quantdata, aes(SOLASTALGIA, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.467897) +
  geom_vline(xintercept = 1.833693) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Hope Pathways ####


HOPEPATH_SELFEFF <- quantdata %>%
  drop_na(HOPEPATH_SELFEFF) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(HOPEPATH_SELFEFF))

plot_HOPEPATH_SELFEFF <- ggplot(quantdata, aes(HOPEPATH_SELFEFF, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.370787) +
  geom_vline(xintercept = 1.566981) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


HOPEPATH_RESPEFF <- quantdata %>%
  drop_na(HOPEPATH_RESPEFF) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(HOPEPATH_RESPEFF))

plot_HOPEPATH_RESPEFF <- ggplot(quantdata, aes(HOPEPATH_RESPEFF, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.716292) +
  geom_vline(xintercept = 2.696698) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Mental Health Index ####

cordata <- subset(quantdata, select = c("MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION","MHI_SCORE","MHI","SWELLBEING"))
MHIcor <- cor(cordata, use = "complete.obs")

SWELLBEING <- quantdata %>%
  drop_na(SWELLBEING) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(SWELLBEING))

plot_SWELLBEING <- ggplot(quantdata, aes(SWELLBEING, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 3.665725) +
  geom_vline(xintercept = 3.286660) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Eco-anxiety ####

cordata <- subset(quantdata, select = c("ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7"))
EAcor <- cor(cordata, use = "complete.obs")

ECOANXIETY_SCORE <- quantdata %>%
  drop_na(ECOANXIETY_SCORE) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ECOANXIETY_SCORE))

plot_ECOANXIETY_SCORE <- ggplot(quantdata, aes(ECOANXIETY_SCORE, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1/1.5, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 3.721591) +
  geom_vline(xintercept = 4.614583) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


ECOANXIETY_CAT <- quantdata %>%
  drop_na(ECOANXIETY_CAT) %>% 
  group_by(AUDIENCE, ECOANXIETY_CAT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)


plot_ECOANXIETY_CAT <- ggplot(ECOANXIETY_CAT, aes(x = "", y = pct, fill = ECOANXIETY_CAT)) +
  geom_col(color = "white") +
  #geom_text(aes(label = round(pct, digits = 0)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_fill_manual(values = c("In-network" = "#00806E",
  #"Public" = "#000000")) +
  facet_wrap(~AUDIENCE) + 
  theme_void()




####  *- Environmental Justice ####

qualdata <- qualdata %>%
  mutate(JUSTICE_DIST = factor(JUSTICE_DIST, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         JUSTICE_PRO = factor(JUSTICE_PRO, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         JUSTICE_REC = factor(JUSTICE_REC, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         JUSTICE_AGN = factor(JUSTICE_AGN, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")))


JUSTICE_DIST <- qualdata %>%
  drop_na(JUSTICE_DIST) %>% 
  group_by(AUDIENCE, JUSTICE_DIST) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "JUSTICE_DIST") %>%
  rename(Frequency = JUSTICE_DIST)
JUSTICE_PRO <- qualdata %>%
  drop_na(JUSTICE_PRO) %>% 
  group_by(AUDIENCE, JUSTICE_PRO) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "JUSTICE_PRO") %>%
  rename(Frequency = JUSTICE_PRO)
JUSTICE_REC <- qualdata %>%
  drop_na(JUSTICE_REC) %>% 
  group_by(AUDIENCE, JUSTICE_REC) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "JUSTICE_REC") %>%
  rename(Frequency = JUSTICE_REC)
JUSTICE_AGN <- qualdata %>%
  drop_na(JUSTICE_AGN) %>% 
  group_by(AUDIENCE, JUSTICE_AGN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "JUSTICE_AGN") %>%
  rename(Frequency = JUSTICE_AGN)

justice <- rbind(JUSTICE_DIST, JUSTICE_PRO, JUSTICE_REC, JUSTICE_AGN)

plot_justice <- ggplot(justice, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Not true at all" = "#8EC5E7",
                               "A little true" = "#3E9CD5",
                               "Moderately true" = "#006FB2",
                               "Very true" = "#004F7E",
                               "Completely true" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())

JUSTICE <- quantdata %>%
  drop_na(JUSTICE) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(JUSTICE))

plot_justice_avg <- ggplot(quantdata, aes(JUSTICE, colour = AUDIENCE, fill = AUDIENCE)) +
  geom_density(adjust = 1, alpha = 0.5) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_fill_manual(values = c("In-network" = "#00806E",
                               "Public" = "#000000")) +
  geom_vline(xintercept = 2.440075) +
  geom_vline(xintercept = 2.661001) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())


####  *- Environmental Change ####

qualdata <- qualdata %>%
  mutate(ATTITUDE_CHNGHS = factor(ATTITUDE_CHNGHS, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_CHNGFW = factor(ATTITUDE_CHNGFW, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_CHNGRN = factor(ATTITUDE_CHNGRN, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_CHNGTP = factor(ATTITUDE_CHNGTP, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_CCTHRT = factor(ATTITUDE_CCTHRT, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")))


ATTITUDE_CHNGHS <- qualdata %>%
  drop_na(ATTITUDE_CHNGHS) %>% 
  group_by(AUDIENCE, ATTITUDE_CHNGHS) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CHNGHS") %>%
  rename(Frequency = ATTITUDE_CHNGHS)
ATTITUDE_CHNGFW <- qualdata %>%
  drop_na(ATTITUDE_CHNGFW) %>% 
  group_by(AUDIENCE, ATTITUDE_CHNGFW) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CHNGFW") %>%
  rename(Frequency = ATTITUDE_CHNGFW)
ATTITUDE_CHNGRN <- qualdata %>%
  drop_na(ATTITUDE_CHNGRN) %>% 
  group_by(AUDIENCE, ATTITUDE_CHNGRN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CHNGRN") %>%
  rename(Frequency = ATTITUDE_CHNGRN)
ATTITUDE_CHNGTP <- qualdata %>%
  drop_na(ATTITUDE_CHNGTP) %>% 
  group_by(AUDIENCE, ATTITUDE_CHNGTP) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CHNGTP") %>%
  rename(Frequency = ATTITUDE_CHNGTP)
ATTITUDE_CCTHRT <- qualdata %>%
  drop_na(ATTITUDE_CCTHRT) %>% 
  group_by(AUDIENCE, ATTITUDE_CCTHRT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CCTHRT") %>%
  rename(Frequency = ATTITUDE_CCTHRT)

change <- rbind(ATTITUDE_CHNGHS, ATTITUDE_CHNGFW, ATTITUDE_CHNGRN, ATTITUDE_CHNGTP, ATTITUDE_CCTHRT)

plot_change <- ggplot(change, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Not true at all" = "#8EC5E7",
                               "A little true" = "#3E9CD5",
                               "Moderately true" = "#006FB2",
                               "Very true" = "#004F7E",
                               "Completely true" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


ATTITUDE_CCTHRT <- quantdata %>%
  drop_na(ATTITUDE_CCTHRT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_CCTHRT)) %>%
  mutate(Activity = "CCTHRT")
ATTITUDE_CHNGHS <- quantdata %>%
  drop_na(ATTITUDE_CHNGHS) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_CHNGHS)) %>%
  mutate(Activity = "CHNGHS")
ATTITUDE_CHNGFW <- quantdata %>%
  drop_na(ATTITUDE_CHNGFW) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_CHNGFW)) %>%
  mutate(Activity = "CHNGFW")
ATTITUDE_CHNGRN <- quantdata %>%
  drop_na(ATTITUDE_CHNGRN) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_CHNGRN)) %>%
  mutate(Activity = "CHNGRN")
ATTITUDE_CHNGTP <- quantdata %>%
  drop_na(ATTITUDE_CHNGTP) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_CHNGTP)) %>%
  mutate(Activity = "CHNGTP")

change_means <- rbind(ATTITUDE_CCTHRT, ATTITUDE_CHNGHS, ATTITUDE_CHNGFW, ATTITUDE_CHNGRN, ATTITUDE_CHNGTP)

plot_change_means <- ggplot(change_means, aes(x=Activity, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



####  *- Other Attitudes ####


qualdata <- qualdata %>%
  mutate(ATTITUDE_BAYLNK = factor(ATTITUDE_BAYLNK, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_BAYIMP = factor(ATTITUDE_BAYIMP, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_BIODIV = factor(ATTITUDE_BIODIV, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_OPPSAT = factor(ATTITUDE_OPPSAT, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_SAFOUT = factor(ATTITUDE_SAFOUT, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_OUTWEL = factor(ATTITUDE_OUTWEL, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")),
         ATTITUDE_SWMSAF = factor(ATTITUDE_SWMSAF, levels = c("Not true at all","A little true","Moderately true","Very true","Completely true")))


ATTITUDE_BAYLNK <- qualdata %>%
  drop_na(ATTITUDE_BAYLNK) %>% 
  group_by(AUDIENCE, ATTITUDE_BAYLNK) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "BAYLNK") %>%
  rename(Frequency = ATTITUDE_BAYLNK)
ATTITUDE_BAYIMP <- qualdata %>%
  drop_na(ATTITUDE_BAYIMP) %>% 
  group_by(AUDIENCE, ATTITUDE_BAYIMP) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "BAYIMP") %>%
  rename(Frequency = ATTITUDE_BAYIMP)
ATTITUDE_BIODIV <- qualdata %>%
  drop_na(ATTITUDE_BIODIV) %>% 
  group_by(AUDIENCE, ATTITUDE_BIODIV) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "BIODIV") %>%
  rename(Frequency = ATTITUDE_BIODIV)
ATTITUDE_OPPSAT <- qualdata %>%
  drop_na(ATTITUDE_OPPSAT) %>% 
  group_by(AUDIENCE, ATTITUDE_OPPSAT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "OPPSAT") %>%
  rename(Frequency = ATTITUDE_OPPSAT)
ATTITUDE_SAFOUT <- qualdata %>%
  drop_na(ATTITUDE_SAFOUT) %>% 
  group_by(AUDIENCE, ATTITUDE_SAFOUT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "SAFOUT") %>%
  rename(Frequency = ATTITUDE_SAFOUT)
ATTITUDE_OUTWEL <- qualdata %>%
  drop_na(ATTITUDE_OUTWEL) %>% 
  group_by(AUDIENCE, ATTITUDE_OUTWEL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "OUTWEL") %>%
  rename(Frequency = ATTITUDE_OUTWEL)
ATTITUDE_SWMSAF <- qualdata %>%
  drop_na(ATTITUDE_SWMSAF) %>% 
  group_by(AUDIENCE, ATTITUDE_SWMSAF) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Attitude = "SWMSAF") %>%
  rename(Frequency = ATTITUDE_SWMSAF)

attitudes <- rbind(ATTITUDE_BAYIMP,ATTITUDE_BAYLNK,ATTITUDE_BIODIV,ATTITUDE_OPPSAT,ATTITUDE_OUTWEL,ATTITUDE_SAFOUT,ATTITUDE_SWMSAF)

plot_attitudes <- ggplot(attitudes, aes(x=Attitude, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("Not true at all" = "#8EC5E7",
                               "A little true" = "#3E9CD5",
                               "Moderately true" = "#006FB2",
                               "Very true" = "#004F7E",
                               "Completely true" = "#003351")) +
  scale_x_discrete(labels = function(Attitude) str_wrap(Attitude, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


ATTITUDE_BAYIMP <- quantdata %>%
  drop_na(ATTITUDE_BAYIMP) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_BAYIMP)) %>%
  mutate(Attitude = "BAYIMP")
ATTITUDE_BAYLNK <- quantdata %>%
  drop_na(ATTITUDE_BAYLNK) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_BAYLNK)) %>%
  mutate(Attitude = "BAYLNK")
ATTITUDE_BIODIV <- quantdata %>%
  drop_na(ATTITUDE_BIODIV) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_BIODIV)) %>%
  mutate(Attitude = "BIODIV")
ATTITUDE_OPPSAT <- quantdata %>%
  drop_na(ATTITUDE_OPPSAT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_OPPSAT)) %>%
  mutate(Attitude = "OPPSAT")
ATTITUDE_OUTWEL <- quantdata %>%
  drop_na(ATTITUDE_OUTWEL) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_OUTWEL)) %>%
  mutate(Attitude = "OUTWEL")
ATTITUDE_SAFOUT <- quantdata %>%
  drop_na(ATTITUDE_SAFOUT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_SAFOUT)) %>%
  mutate(Attitude = "SAFOUT")
ATTITUDE_SWMSAF <- quantdata %>%
  drop_na(ATTITUDE_SWMSAF) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(ATTITUDE_SWMSAF)) %>%
  mutate(Attitude = "SWMSAF")

attitudes_means <- rbind(ATTITUDE_BAYIMP,ATTITUDE_BAYLNK,ATTITUDE_BIODIV,ATTITUDE_OPPSAT,ATTITUDE_OUTWEL,ATTITUDE_SAFOUT,ATTITUDE_SWMSAF)

plot_attitudes_means <- ggplot(attitudes_means, aes(x=Attitude, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,4) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



####  *- Social Capital ####


qualdata <- qualdata %>%
  mutate(GROUP_ARTCLT = factor(GROUP_ARTCLT, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_ENVPRT = factor(GROUP_ENVPRT, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_HOANBH = factor(GROUP_HOANBH, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_POLTCL = factor(GROUP_POLTCL, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_SPIRIT = factor(GROUP_SPIRIT, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_CHLPAR = factor(GROUP_CHLPAR, levels = c("No, not involved","Yes, a little involved","Yes, very involved")),
         GROUP_SPORTS = factor(GROUP_SPORTS, levels = c("No, not involved","Yes, a little involved","Yes, very involved")))


GROUP_ARTCLT <- qualdata %>%
  drop_na(GROUP_ARTCLT) %>% 
  group_by(AUDIENCE, GROUP_ARTCLT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "ARTCLT") %>%
  rename(Frequency = GROUP_ARTCLT)
GROUP_ENVPRT <- qualdata %>%
  drop_na(GROUP_ENVPRT) %>% 
  group_by(AUDIENCE, GROUP_ENVPRT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "ENVPRT") %>%
  rename(Frequency = GROUP_ENVPRT)
GROUP_HOANBH <- qualdata %>%
  drop_na(GROUP_HOANBH) %>% 
  group_by(AUDIENCE, GROUP_HOANBH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "HOANBH") %>%
  rename(Frequency = GROUP_HOANBH)
GROUP_POLTCL <- qualdata %>%
  drop_na(GROUP_POLTCL) %>% 
  group_by(AUDIENCE, GROUP_POLTCL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "POLTCL") %>%
  rename(Frequency = GROUP_POLTCL)
GROUP_SPIRIT <- qualdata %>%
  drop_na(GROUP_SPIRIT) %>% 
  group_by(AUDIENCE, GROUP_SPIRIT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "SPIRIT") %>%
  rename(Frequency = GROUP_SPIRIT)
GROUP_CHLPAR <- qualdata %>%
  drop_na(GROUP_CHLPAR) %>% 
  group_by(AUDIENCE, GROUP_CHLPAR) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "CHLPAR") %>%
  rename(Frequency = GROUP_CHLPAR)
GROUP_SPORTS <- qualdata %>%
  drop_na(GROUP_SPORTS) %>% 
  group_by(AUDIENCE, GROUP_SPORTS) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         Activity = "SPORTS") %>%
  rename(Frequency = GROUP_SPORTS)


scapital <- rbind(GROUP_ARTCLT,GROUP_CHLPAR,GROUP_ENVPRT,GROUP_HOANBH,GROUP_POLTCL,GROUP_SPIRIT,GROUP_SPORTS)

plot_scapital <- ggplot(scapital, aes(x=Activity, y=pct, fill=Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Respondents (%)") +
  scale_fill_manual(values = c("No, not involved" = "#8EC5E7",
                               "Yes, a little involved" = "#006FB2",
                               "Yes, very involved" = "#003351")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())


GROUP_ARTCLT <- quantdata %>%
  drop_na(GROUP_ARTCLT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_ARTCLT)) %>%
  mutate(Activity = "ARTCLT")
GROUP_CHLPAR <- quantdata %>%
  drop_na(GROUP_CHLPAR) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_CHLPAR)) %>%
  mutate(Activity = "CHLPAR")
GROUP_ENVPRT <- quantdata %>%
  drop_na(GROUP_ENVPRT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_ENVPRT)) %>%
  mutate(Activity = "ENVPRT")
GROUP_HOANBH <- quantdata %>%
  drop_na(GROUP_HOANBH) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_HOANBH)) %>%
  mutate(Activity = "HOANBH")
GROUP_POLTCL <- quantdata %>%
  drop_na(GROUP_POLTCL) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_POLTCL)) %>%
  mutate(Activity = "POLTCL")
GROUP_SPIRIT <- quantdata %>%
  drop_na(GROUP_SPIRIT) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_SPIRIT)) %>%
  mutate(Activity = "SPIRIT")
GROUP_SPORTS <- quantdata %>%
  drop_na(GROUP_SPORTS) %>% 
  group_by(AUDIENCE) %>%
  summarise(Mean = mean(GROUP_SPORTS)) %>%
  mutate(Activity = "SPORTS")

scapital_means <- rbind(GROUP_ARTCLT,GROUP_CHLPAR,GROUP_ENVPRT,GROUP_HOANBH,GROUP_POLTCL,GROUP_SPIRIT,GROUP_SPORTS)

plot_scapital_means <- ggplot(scapital_means, aes(x=Activity, y=Mean, colour = AUDIENCE)) +
  geom_point() +
  ylab("Mean") +
  ylim(0,2) +
  scale_color_manual(values = c("In-network" = "#00806E",
                                "Public" = "#000000")) +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 10)) +
  facet_wrap(~AUDIENCE) + 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank(),
        axis.ticks.x = element_blank())



########## AUDIENCE CHARACTERISTICS ##################


####  *- Underserved Communities ####

# Merge data
under_coords <- read.csv("tbcs-2024/survey_data/raw/underserved_coords.csv")
under_coords <- under_coords %>%
  remove_rownames() %>%
  column_to_rownames(var = "ResponseId")

qualdata <- merge(qualdata, under_coords, by = "row.names", all = TRUE)
quantdata <- merge(quantdata, under_coords, by = "row.names", all = TRUE)

qualdata <- qualdata %>%
  mutate(AGE = ifelse(AGE == "", NA, AGE),
         AGE = factor(AGE, levels = c("18 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 or older")),
         GENDER = ifelse(GENDER == "", NA, GENDER),
         GENDER = factor(GENDER, levels = c("Male","Female","Non-binary")),
         EDUCATION = ifelse(EDUCATION == "", NA, EDUCATION),
         EDUCATION = factor(EDUCATION, levels = c("Did not complete high school","High school diploma or GED","Vocational or Trade School degree","Associate degree","Bachelor's degree","Master's degree","Doctoral degree")),
         HHINCOME = ifelse(HHINCOME == "", NA, HHINCOME),
         HHINCOME = factor(HHINCOME, levels = c("Less than $25,000","$25,000 - $49,999","$50,000 - $74,999","$75,000 - $99,999","$100,000 or more")),
         EMPLOYED = ifelse(EMPLOYED == "", NA, EMPLOYED),
         EMPLOYED = factor(EMPLOYED, levels = c("Unemployed","Part-time","Full-time")),
         RACE = ifelse(RACE == "", NA, RACE),
         RACE = factor(RACE, levels = c("Asian","Black","Hispanic","White","Other","Multiple")),
         POLITICS = ifelse(POLITICS == "", NA, POLITICS),
         POLITICS = factor(POLITICS, levels = c("Strongly liberal","Moderately liberal","Slightly liberal","Neutral","Slightly conservative","Moderately conservative","Strongly conservative")),
         YEARS = ifelse(YEARS == "", NA, YEARS),
         YEARS = factor(YEARS, levels = c("Less than a year","1 - 4 years","5 - 9 years","10 - 19 years","20 years or more")),
         PROPERTY = ifelse(PROPERTY == "", NA, PROPERTY),
         PROPERTY = factor(PROPERTY, levels = c("Single-family home","Townhome or duplex","Condominium","Apartment","Mobile home")),
         OWNERSHIP = ifelse(OWNERSHIP == "", NA, OWNERSHIP),
         OWNERSHIP = factor(OWNERSHIP, levels = c("Own","Rent","Other")),
         REFERRAL = ifelse(REFERRAL == "", NA, REFERRAL),
         REFERRAL = factor(REFERRAL, levels = c("Panel","Mail","TBEP","Environmental","Recreation","Education","Government","Community","Local News","Connection","Unknown")),
         UNDERSERVED = ifelse(is.na(UNDERSERVED), "No Coordinate Data", "Yes"),
         COMMUNITY = ifelse(REFERRAL == "Mail" | UNDERSERVED == "Yes", "Underserved", "Other/Unknown"),
         COMMUNITY = factor(COMMUNITY, levels = c("Underserved","Other/Unknown")))

quantdata <- quantdata %>%
  mutate(UNDERSERVED = ifelse(is.na(UNDERSERVED), "No Coordinate Data", "Yes"),
         COMMUNITY = ifelse(REFERRAL == "Mail" | UNDERSERVED == "Yes", "Underserved", "Other/Unknown"),
         COMMUNITY = factor(COMMUNITY, levels = c("Underserved","Other/Unknown")))


COMMUNITY <- qualdata %>%
  drop_na(COMMUNITY) %>% 
  group_by(AUDIENCE, COMMUNITY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_COMMUNITY <- ggplot(COMMUNITY, aes(x = "", y = pct, fill = COMMUNITY)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()


####  *- Environmental Justice ####

JUSTICE <- subset(quantdata, AUDIENCE == "Public") %>%
  drop_na(JUSTICE) %>% 
  group_by(COMMUNITY) %>%
  summarise(Mean = mean(JUSTICE))

plot_justice_avg <- ggplot(quantdata, aes(JUSTICE, colour = COMMUNITY, fill = COMMUNITY)) +
  geom_density(adjust = 1, alpha = 0.5) +
  scale_color_manual(values = c("Underserved" = "#962C14",
                                "Other/Unknown" = "#000000")) +
  scale_fill_manual(values = c("Underserved" = "#962C14",
                               "Other/Unknown" = "#000000")) +
  geom_vline(xintercept = 2.506231) +
  geom_vline(xintercept = 2.700197) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        #axis.ticks.x = element_blank(),
        panel.grid  = element_blank())




####  *- Demographics ####

AGE <- qualdata %>%
  drop_na(AGE) %>% 
  group_by(AUDIENCE, AGE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_AGE <- ggplot(AGE, aes(x = "", y = pct, fill = AGE)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

GENDER <- qualdata %>%
  drop_na(GENDER) %>% 
  group_by(AUDIENCE, GENDER) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_GENDER <- ggplot(GENDER, aes(x = "", y = pct, fill = GENDER)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

EDUCATION <- qualdata %>%
  drop_na(EDUCATION) %>% 
  group_by(AUDIENCE, EDUCATION) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_EDUCATION <- ggplot(EDUCATION, aes(x = "", y = pct, fill = EDUCATION)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

HHINCOME <- qualdata %>%
  drop_na(HHINCOME) %>% 
  group_by(AUDIENCE, HHINCOME) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_HHINCOME <- ggplot(HHINCOME, aes(x = "", y = pct, fill = HHINCOME)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

EMPLOYED <- qualdata %>%
  drop_na(EMPLOYED) %>% 
  group_by(AUDIENCE, EMPLOYED) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_EMPLOYED <- ggplot(EMPLOYED, aes(x = "", y = pct, fill = EMPLOYED)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

EMPLOYED_RETIRED <- qualdata %>%
  drop_na(EMPLOYED, EMPLOYED_RETIRED) %>% 
  group_by(AUDIENCE, EMPLOYED, EMPLOYED_RETIRED) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_EMPLOYED_RETIRED <- ggplot(EMPLOYED_RETIRED, aes(x = "", y = pct, fill = EMPLOYED_RETIRED)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

EMPLOYED_STUDENT <- qualdata %>%
  drop_na(EMPLOYED_STUDENT) %>% 
  group_by(AUDIENCE, EMPLOYED_STUDENT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_EMPLOYED_STUDENT <- ggplot(EMPLOYED_STUDENT, aes(x = "", y = pct, fill = EMPLOYED_STUDENT)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

RACE <- qualdata %>%
  drop_na(RACE) %>% 
  group_by(AUDIENCE, RACE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_RACE <- ggplot(RACE, aes(x = "", y = pct, fill = RACE)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

POLITICS <- qualdata %>%
  drop_na(POLITICS) %>% 
  group_by(AUDIENCE, POLITICS) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_POLITICS <- ggplot(POLITICS, aes(x = "", y = pct, fill = POLITICS)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

YEARS <- qualdata %>%
  drop_na(YEARS) %>% 
  group_by(AUDIENCE, YEARS) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_YEARS <- ggplot(YEARS, aes(x = "", y = pct, fill = YEARS)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

PROPERTY <- qualdata %>%
  drop_na(PROPERTY) %>% 
  group_by(AUDIENCE, PROPERTY) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_PROPERTY <- ggplot(PROPERTY, aes(x = "", y = pct, fill = PROPERTY)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

OWNERSHIP <- qualdata %>%
  drop_na(OWNERSHIP) %>% 
  group_by(AUDIENCE, OWNERSHIP) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_OWNERSHIP <- ggplot(OWNERSHIP, aes(x = "", y = pct, fill = OWNERSHIP)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

REFERRAL <- qualdata %>%
  drop_na(REFERRAL) %>% 
  group_by(AUDIENCE, REFERRAL) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100)

plot_REFERRAL <- ggplot(REFERRAL, aes(x = "", y = pct, fill = REFERRAL)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~AUDIENCE) + 
  theme_void()

