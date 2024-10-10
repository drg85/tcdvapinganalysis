#Code outline for TCD vaping analysis, davidrobert.grimes@tcd.ie
#See README for details

library(foreign)

#Enter location of Vaping master data
location = "Z:/DropBox/RStudioStuff/Vaping analysis/vapedata.sav"

#location = "C:/Users/david/Dropbox/RStudioStuff/Vaping analysis/vapedata.sav"
#Raw import from SPSS: 
dataset = suppressWarnings(read.spss(location,
                                     use.value.labels = TRUE,
                                     to.data.frame = TRUE))

#Extract relevant data
vapedata <- dataset[, c("Gender3cats", "Studentorstaff3cats", "Lifevaping2cats","Smoking2cats","Dualusers2cats")]

# Rename the columns 
colnames(vapedata) <- c("gender", "role", "lifevape", "current","dual")

#convert Lifevaping and Smoking to 1/0 (current column is current smoking)
vapedata$lifevape <- ifelse(vapedata$lifevape == "Yes", 1, 0)
vapedata$current <- ifelse(vapedata$current == "Yes", 1, 0)

#convert role to 3 factor
vapedata$role <- factor(vapedata$role, levels = c("Undergraduate", "Postgraduate","Staff"))


#Data cleaning for dual smoking entry (optional)
# Add "Not Relevant" as a level
levels(vapedata$dual) <- c(levels(vapedata$dual), "Not Relevant")

# Replace NA values with "Not Relevant"
vapedata$dual[is.na(vapedata$dual)] <- "Not Relevant"


#convert gender to factor
vapedata$gender <- factor(vapedata$gender, levels = c("Male incl trans", "Female incl trans","Other/prefer not to say"))


#current and former vaping derivation:
breakdown <- dataset[, c("Vaping30days3cats")]
currentvape <- breakdown
former <- breakdown
currentvape <- ifelse(currentvape == "Yes, current vaper", 1, 0)
currentdf <- data.frame(currentvape = currentvape)
former <- ifelse(former == "No, vaped before but not in 30 days", 1, 0)
formerdf <- data.frame(former = former)

##now we will import perceptions 
perceptions <- dataset[, c("Addictive2catsCombinedDisagreeneither", "Generallysafe2catsCombinedAgreeneither", "Safer3catsAgreefirst","Increasessmoking3catsDisagreefirst", "Easier3catsAgreefirst","Environment2catsCombinedDisagreeneither")]
colnames(perceptions) <- c("addict", "safe", "safer", "increase","drugs","env")
perceptions2 <- perceptions

#Convert agreement to binary predictor
perceptions$addict <- ifelse(perceptions$addict == "Agree/strongly agree", 1, 0)
perceptions$safe <- ifelse(perceptions$safe == "Agree/strongly agree or neither agree nor disagree", 1, 0)
perceptions$safer <- ifelse(perceptions$safer == "Agree/strongly agree", 1, 0)
perceptions$increase <- ifelse(perceptions$increase == "Agree/strongly agree", 1, 0)
perceptions$drugs <- ifelse(perceptions$drugs == "Agree/strongly agree", 1, 0)
perceptions$env <- ifelse(perceptions$env == "Agree/strongly agree", 1, 0)

#meld to one 
perfixed <- cbind(perceptions,currentdf,formerdf)
fullvapedata <- cbind(vapedata,perfixed)
fullvapeclean <- na.omit(fullvapedata)
unclean <- fullvapedata
fullvapedata <- fullvapeclean


#Full regression analysis (logistic regression) for role, gender, current smoking status AND perceptions 
#for lifetime vapers, use lifevape. For current, use currentvape etc.... 

logmodel_per <- glm(lifevape ~ role + gender + current +
                      addict + safe + safer + increase + drugs + env,
                    data = fullvapedata, family = binomial)


# Get odds ratios by exponentiating the coefficients
odds_ratios <- exp(coef(logmodel_per))

# Get the confidence intervals for the coefficients
conf_intervals <- confint(logmodel_per)

# Exponentiate the confidence intervals to get them for the odds ratios
odds_ratio_conf_intervals <- exp(conf_intervals)


# Combine the odds ratios and their confidence intervals
odds_ratios_with_ci <- cbind(Odds_Ratio = odds_ratios, 
                             Lower_95_CI = odds_ratio_conf_intervals[, 1], 
                             Upper_95_CI = odds_ratio_conf_intervals[, 2])




#Raw Univariates
#change currentvape to lifevape etc as desired, and use addict, safe, safer, etc in predictor list! 


univar_noadjust <- glm(currentvape ~ addict, data = fullvapedata, family = binomial)

orun <- exp(coef(univar_noadjust))
ciun <- exp(confint(univar_noadjust))
orciun <- cbind(Odds_Ratio = orun, 
                Lower_95_CI = ciun[, 1], 
                Upper_95_CI = ciun[, 2])




#Extra: distinguishing between dual and vape only
#need to count dual and non-dual...


# Filter rows where ever == 1
current_subset <- fullvapeclean[fullvapeclean$currentvape == 1, ]
current_subset$dual <- ifelse(current_subset$dual == "Dual vaper", 1, 0)

#remove current smoking predictor from demographic predictors. include all else!

fullsubsetlog <- glm(dual ~ role + gender + addict + safe + safer + increase + drugs + env,
                     data = current_subset, family = binomial)


dualor <- exp(coef(fullsubsetlog))
dualci <- exp(confint(fullsubsetlog))
dualallin <- cbind(Odds_Ratio = dualor, 
                   Lower_95_CI = dualci[, 1], 
                   Upper_95_CI = dualci[, 2])

#singlechangecode! 
unidual <- glm(dual ~ addict, data = current_subset, family = binomial)

unidualor <- exp(coef(unidual))
unidualci <- exp(confint(unidual))
unidualallin <- cbind(Odds_Ratio = unidualor, 
                      Lower_95_CI = unidualci[, 1], 
                      Upper_95_CI = unidualci[, 2])



