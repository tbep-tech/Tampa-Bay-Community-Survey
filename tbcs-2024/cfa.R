library(lavaan)
library(MVN)
library(FactoMineR)
library(psych)
library(dplyr)
library(tidyr)
library(knitr)
library(semPlot)
library(psy)

# Load quantitative data
mydata <- read.csv("tbcs-2024/survey_data/final/tbcs_quant_2024.csv")
fulldata <- mydata[,-1]
rownames(fulldata) <- mydata[,1]

# variables that are going to be checked:
#   NR
#   PIDENTITY
#   SOLASTALGIA
#   HOPEPATH
#   MHI
#   ECOANXIETY
#   JUSTICE

psychdata <- fulldata[c(48:53,55:57,59:67,70:74,80:85,88:94,96:99)]


##### LET'S VERIFY WE CAN THESE VARIABLES FOR FACTOR ANALYSES (KMO tests)
# Tabachnick & Fidell (2001)

# KMO TEST FOR PCA WARRANT (ACCEPT IF > 0.5) #
KMO(psychdata)

##### THESE RESULTS ARE QUITE GOOD, SO JUSTIFIED IN USING THEM FOR FACTOR ANALYSIS
##### WE'LL EMPHASIZE THE USE OF ROBUST WEIGHTED LEAST SQUARES ESTIMATORS FOR NON-NORMAL CATEGORICAL/ORDINAL DATA (WLSMV) 


############ NATURE RELATEDNESS ###############

NRcormatrix <- cor(psychdata[, c("NR_IMPPRT","NR_CONNCT","NR_SPIRIT","NR_NOTICE","NR_VACSPT","NR_AFFECT")], method = "pearson", use = "complete.obs")

modelNR <- '
nrelated =~ NR_IMPPRT + NR_CONNCT + NR_SPIRIT + NR_NOTICE + NR_VACSPT + NR_AFFECT
'

# DATA WITH NAs ALLOWED
fitnrelated <- cfa(model=modelNR, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitnrelated, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitnrelated, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.985   TLI = 0.976    RMSEA = 0.059 / 0.154

parameterEstimates(fitnrelated, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$NR_IMPPRT,psychdata$NR_CONNCT,psychdata$NR_SPIRIT,psychdata$NR_NOTICE,psychdata$NR_VACSPT,psychdata$NR_AFFECT)
cronbach(X)


############ PLACE IDENTITY ###############

PIcormatrix <- cor(psychdata[, c("PIDENTITY_IDENTY","PIDENTITY_SPCIAL","PIDENTITY_ATTCHD")], method = "pearson", use = "complete.obs")

modelPI <- '
pidentity =~ PIDENTITY_IDENTY + PIDENTITY_SPCIAL + PIDENTITY_ATTCHD
'

# DATA WITH NAs ALLOWED
fitpidentity <- cfa(model=modelPI, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitpidentity, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitpidentity, fit.measures=TRUE, standardized=TRUE)
#CFI = 1.000   TLI = 1.000    RMSEA = 0.000 / NA

parameterEstimates(fitpidentity, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$PIDENTITY_IDENTY,psychdata$PIDENTITY_SPCIAL,psychdata$PIDENTITY_ATTCHD)
cronbach(X)



############ SOLASTALGIA ###############

SOcormatrix <- cor(psychdata[, c("SOLASTALGIA_BELONG","SOLASTALGIA_LOSSES","SOLASTALGIA_ASHAMD","SOLASTALGIA_DISAPR","SOLASTALGIA_UQLOSS","SOLASTALGIA_PEACEQ","SOLASTALGIA_DEGRAD","SOLASTALGIA_LFSTYL","SOLASTALGIA_FLEAVE")], method = "pearson", use = "complete.obs")

modelSO <- '
solastalgia =~ SOLASTALGIA_BELONG + SOLASTALGIA_LOSSES + SOLASTALGIA_ASHAMD + SOLASTALGIA_DISAPR + SOLASTALGIA_UQLOSS + SOLASTALGIA_PEACEQ + SOLASTALGIA_DEGRAD + SOLASTALGIA_LFSTYL + SOLASTALGIA_FLEAVE
'

# DATA WITH NAs ALLOWED
fitsolastalgia <- cfa(model=modelSO, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitsolastalgia, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitsolastalgia, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.965   TLI = 0.954    RMSEA = 0.068 / 0.001
#RMSEA isn't great, remove items
parameterEstimates(fitsolastalgia, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$SOLASTALGIA_BELONG,psychdata$SOLASTALGIA_LOSSES,psychdata$SOLASTALGIA_ASHAMD,psychdata$SOLASTALGIA_DISAPR,psychdata$SOLASTALGIA_UQLOSS,psychdata$SOLASTALGIA_PEACEQ,psychdata$SOLASTALGIA_DEGRAD,psychdata$SOLASTALGIA_LFSTYL,psychdata$SOLASTALGIA_FLEAVE)
cronbach(X)

modelSO <- '
solastalgia =~  SOLASTALGIA_LOSSES + SOLASTALGIA_ASHAMD + SOLASTALGIA_DISAPR + SOLASTALGIA_UQLOSS + SOLASTALGIA_PEACEQ + SOLASTALGIA_DEGRAD + SOLASTALGIA_LFSTYL
'
fitsolastalgia <- cfa(model=modelSO, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitsolastalgia, whatLabels = "est", layout = "tree")
summary(fitsolastalgia, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.989   TLI = 0.983    RMSEA = 0.048 / 0.564

parameterEstimates(fitsolastalgia, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$SOLASTALGIA_LOSSES,psychdata$SOLASTALGIA_ASHAMD,psychdata$SOLASTALGIA_DISAPR,psychdata$SOLASTALGIA_UQLOSS,psychdata$SOLASTALGIA_PEACEQ,psychdata$SOLASTALGIA_DEGRAD,psychdata$SOLASTALGIA_LFSTYL)
cronbach(X)




############ HOPE PATHWAYS ###############

HOcormatrix <- cor(psychdata[, c("HOPEPATH_WAYFIX","HOPEPATH_KNOWLG","HOPEPATH_CANFIX","HOPEPATH_RESOLV","HOPEPATH_ENOUGH")], method = "pearson", use = "complete.obs")

modelHO <- '
hopepath =~ HOPEPATH_WAYFIX + HOPEPATH_KNOWLG + HOPEPATH_CANFIX + HOPEPATH_RESOLV + HOPEPATH_ENOUGH
'

# DATA WITH NAs ALLOWED
fithopepath <- cfa(model=modelHO, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fithopepath, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fithopepath, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.513   TLI = 0.026    RMSEA = 0.249 / 0.000
#Very poor fit, restructure
parameterEstimates(fithopepath, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$HOPEPATH_WAYFIX,psychdata$HOPEPATH_KNOWLG,psychdata$HOPEPATH_CANFIX,psychdata$HOPEPATH_RESOLV,psychdata$HOPEPATH_ENOUGH)
cronbach(X)



############ JUSTICE ###############

JUcormatrix <- cor(psychdata[, c("JUSTICE_DIST","JUSTICE_PRO","JUSTICE_REC","JUSTICE_AGN")], method = "pearson", use = "complete.obs")

modelJU <- '
justice =~ JUSTICE_DIST + JUSTICE_PRO + JUSTICE_REC + JUSTICE_AGN
'

# DATA WITH NAs ALLOWED
fitjustice <- cfa(model=modelJU, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitjustice, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitjustice, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.958   TLI = 0.873    RMSEA = 0.140 / 0.000
# Not very good, try to reduce
parameterEstimates(fitjustice, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$JUSTICE_DIST,psychdata$JUSTICE_PRO,psychdata$JUSTICE_REC,psychdata$JUSTICE_AGN)
cronbach(X)


modelJU <- '
justice =~ JUSTICE_DIST + JUSTICE_PRO + JUSTICE_REC
'

# DATA WITH NAs ALLOWED
fitjustice <- cfa(model=modelJU, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitjustice, whatLabels = "est", layout = "tree")
summary(fitjustice, fit.measures=TRUE, standardized=TRUE)
#CFI = 1.000   TLI = 1.000    RMSEA = 0.000 / NA
parameterEstimates(fitjustice, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$JUSTICE_DIST,psychdata$JUSTICE_PRO,psychdata$JUSTICE_REC)
cronbach(X)





############ MHI ###############

MHcormatrix <- cor(psychdata[, c("MHI_A1","MHI_A2","MHI_D1","MHI_D2","MHI_D3","LIFESATISFACTION")], method = "pearson", use = "complete.obs")

modelMH <- '
mhi =~ MHI_A1 + MHI_A2 + MHI_D1 + MHI_D2 + MHI_D3
'

# DATA WITH NAs ALLOWED
fitmhi <- cfa(model=modelMH, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitmhi, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitmhi, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.867   TLI = 0.734    RMSEA = 0.183 / 0.000

parameterEstimates(fitmhi, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$MHI_A1,psychdata$MHI_A2,psychdata$MHI_D1,psychdata$MHI_D2,psychdata$MHI_D3)
cronbach(X)



modelSW <- '
SW =~ MHI_A1 + MHI_A2 + MHI_D1 + MHI_D2 + MHI_D3 + LIFESATISFACTION
'

# DATA WITH NAs ALLOWED
fitsw <- cfa(model=modelSW, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitsw, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitsw, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.866   TLI = 0.776    RMSEA = 0.161 / 0.000

parameterEstimates(fitsw, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$MHI_A1,psychdata$MHI_A2,psychdata$MHI_D1,psychdata$MHI_D2,psychdata$MHI_D3,psychdata$LIFESATISFACTION)
cronbach(X)




############ ECOANXIETY ###############

EAcormatrix <- cor(psychdata[, c("ECOANXIETY_1","ECOANXIETY_2","ECOANXIETY_3","ECOANXIETY_4","ECOANXIETY_5","ECOANXIETY_6","ECOANXIETY_7")], method = "pearson", use = "complete.obs")

modelEA <- '
ecoanxiety =~ ECOANXIETY_1 + ECOANXIETY_2 + ECOANXIETY_3 + ECOANXIETY_4 + ECOANXIETY_5 + ECOANXIETY_6 + ECOANXIETY_7
'

# DATA WITH NAs ALLOWED
fitecoanxiety <- cfa(model=modelEA, data=psychdata, meanstructure=TRUE, std.lv=TRUE, sample.nobs = 1238, estimator="WLSMV")
semPaths(fitecoanxiety, whatLabels = "est", layout = "tree")

#CFI > 0.9 = OK Fit
#TLI > 0.9 = OK Fit
#RMSEA < 0.1 = OK Fit & p > 0.05 = Model has a "close fit"
summary(fitecoanxiety, fit.measures=TRUE, standardized=TRUE)
#CFI = 0.954   TLI = 0.931    RMSEA = 0.064 / 0.031

parameterEstimates(fitecoanxiety, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, Estimate=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

X <- cbind(psychdata$ECOANXIETY_1,psychdata$ECOANXIETY_2,psychdata$ECOANXIETY_3,psychdata$ECOANXIETY_4,psychdata$ECOANXIETY_5,psychdata$ECOANXIETY_6,psychdata$ECOANXIETY_7)
cronbach(X)


