##########################
# Business Understanding #
##########################

# CMS rates hospitals in the US on a scale of 1-5 with the objective to make it easier for patients and consumers to compare the quality of hospitals.
# The ratings directly influence the choice of the hospital made by consumers and may have a significant impact on the revenue earned by hospitals.
# Thus, it is extremely important for hospitals to understand the methodology used by CMS for calculating the ratings so that they can work on
# improving the factors that influence them.
# This project is focused on developing an approach to calculate hospital ratings and using it to identify areas of improvement for
# certain hospitals. It will also require a thorough understanding of the rating system developed by CMS.

####################
# Business Problem #
####################

# The aim of analysis is to understand the methodology used by CMS for calculating the ratings and identify
# the factors influencing the ratings for hospitals, so that they can work on improving the factors that influence them.
# Recommend ways to improve the rating for Evanston Hospital to improve their current star rating of 3/5*

######################
# Data Understanding #
######################

# 'Hospital_Revised_FlatFiles_20161110'
#  CSV Files
# 1	Readmissions and Deaths - Hospital.csv	                                                readmission.csv
# 2	Readmissions and Deaths - Hospital.csv +   Complications - Hospital.csv	                mortality.csv
# 3	Healthcare Associated Infections - Hospital.csv +   Complications - Hospital.csv	      safety.csv
# 4	HCAHPS - Hospital.csv	                                                                  experience.csv
# 5	Outpatient Imaging Efficiency - Hopital.csv	                                            medical.csv
# 6	Timely and Effective Care - Hospital.csv	                                              timeliness.csv
# 7	Timely and Effective Care - Hospital.csv	                                              effectiveness.csv

#############################
# Exploratory Data Analysis #
#############################
# Perform the Univariate analysis for all the groups.
# Perform Bi-variate analysis for all the groups.

#############
# Modelling #
#############

# Part 1 - Supervised Learning-Based Rating
# Part 2 - Factor analysis and Clustering-Based Rating (Unsupervised)
# Part 3 - Provider analysis - Recommendations for Hospitals

# Let us load the data and create the groups as above:
# Copied the required raw files to the Groups location.
# 1. "Readmissions and Deaths - Hospital.csv"
# 2. "Complications - Hospital.csv"
# 3. "Healthcare Associated Infections - Hospital.csv"
# 4. "HCAHPS - Hospital.csv"
# 5. "Outpatient Imaging Efficiency - Hospital.csv"
# 6. "Timely and Effective Care - Hospital.csv"

## Set the path
setwd("G:/HCCapstone/Groups")

###################################### Data Prepartion, cleaning and Supervised Modelling ###########################################
# Data set contains 58 excel files, 2 PDF files
## out of this for this assignment we require 6 files & it has suffix as "_Hospital"

### Load the files into dataframes:
# 1. Load the data - replace Not Available, Not Applicable with NA  (Suffix _Raw dataframes )
# 2. Split xxxx_rawdata frames into 2 data frames [xxxx_hosp, xxxx_meas)
# 3. Rename columns - Standardize names across
# 4. Reorder columns to match all files
# 5. Standardize the measures- some measures with positive zscores and some measures with negative zscores.
# 6. Impute the outliers

# Libraries to be used

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(corrplot)
library(gridExtra)
library(ggthemes)

############## Functions To be Used ##############

# Function to replace "Not Available" to NA's
replace_NA <- function(x,y,z) {
  x[which(x == y)] <- z
  return(x)
}

func_numeric <- function(x) {
  x <- as.numeric(x)
  return(x)
}

func_rename <- function(x) {
  x %>% rename_at(vars(-Provider.ID),function(y) paste0(y,"_score"))
}

# Function to scale the values.
negative_zscore <- function(i){return((mean(i,na.rm = T) - i)/(sd(i,na.rm = T)))}

positive_zscore <- function(i){return((i - mean(i,na.rm = T))/(sd(i,na.rm = T)))}

#### Function to treat the outliers

treat_outliers <- function(df){
  for (colmn in 2:(ncol(df))){
    qtl = quantile(df[, colmn], probs = seq(0, 1, 0.00001), na.rm = T)
    df[, colmn][which(df[, colmn] <= qtl[0.00125*length(qtl)])] <- qtl[0.00125*length(qtl)]
    df[, colmn][which(df[, colmn] >= qtl[0.99875*length(qtl)])] <- qtl[0.99875*length(qtl)]
  }
  return(df)
}


## 1. Readmission - Load "Readmissions and Deaths - Hospital.csv" file into read_raw

read_rawdata <- read.csv("Readmissions and Deaths - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
dim(read_rawdata)  ## 67452 rows and 18 columns
#[1] 67452    18

str(read_rawdata)
unique(read_rawdata$Score)

# We will filter only those columns which are needed as per the mentor.
read_meas_list = c("READM_30_AMI","READM_30_CABG","READM_30_COPD","READM_30_HF","READM_30_HIP_KNEE","READM_30_HOSP_WIDE","READM_30_PN","READM_30_STK")
read_hosp <- read_rawdata[,c(1:8)]
read_hosp <- read_hosp[!duplicated(read_hosp),]
read_meas <- read_rawdata[,c(1,10,13)]
read_meas$Score <- func_numeric(read_meas$Score)
read_meas <- subset(read_meas, Measure.ID %in% read_meas_list)
str(read_meas)
read_meas_score <- spread(read_meas, Measure.ID, Score)
str(read_meas_score)
read_meas_score <- func_rename(read_meas_score)
str(read_meas_score)
readmission <- read_meas_score
# # We will use negative zscore scaling as high readmissions implies the Hospital is not doing well in terms of patient treatment quality.
readmission[, 2:ncol(readmission)] <- sapply(readmission[, -1], negative_zscore)
str(readmission)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
readmission <- treat_outliers(readmission)
read_master <- merge(read_hosp, readmission, by="Provider.ID")
dim(read_master)
# [1] 4818  16
str(read_master)
write.csv(readmission, "cleaned_readmission_data.csv")


## 2. Mortality - Load 2 Files "Readmissions and Deaths - Hospital.csv + Complications - Hospital.csv" into morality dataframe
mort_rawdata1 <- read_rawdata
mort_rawdata2 <- read.csv("Complications - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
identical(names(mort_rawdata1), names(mort_rawdata2))
# [1] TRUE
mort_rawdata <- rbind(mort_rawdata1, mort_rawdata2)
mort_meas_list = c("MORT_30_AMI", "MORT_30_CABG","MORT_30_COPD","MORT_30_HF","MORT_30_PN", "MORT_30_STK","PSI_4_SURG_COMP")
mort_hosp <- mort_rawdata[,c(1:8)]
mort_hosp = mort_hosp[!duplicated(mort_hosp),]
mort_meas <- mort_rawdata[,c(1,10,13)]
mort_meas <- subset(mort_meas, Measure.ID %in% mort_meas_list)
mort_meas$Score <- func_numeric(mort_meas$Score)
mort_meas_score <- spread(mort_meas, Measure.ID, Score)
mort_meas_score <- func_rename(mort_meas_score)
str(mort_meas_score)
mortality <- mort_meas_score
# Mortality indicates the death rate, higher the number worser is the hospital or provider.
# Since it is related to death rate we will use negative z-score formula.
mortality[, 2:ncol(mortality)] <- sapply(mortality[, -1], negative_zscore)
str(mortality)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
mortality <- treat_outliers(mortality)
mort_master <- merge(mort_hosp, mortality, by="Provider.ID")
dim(mort_master)
#[1] 4818   15
str(mort_master)
write.csv(mortality, "cleaned_mortality_data.csv")

## 3. Safety - Load 2 files "Healthcare Associated Infections - Hospital.csv + Complications - Hospital.csv" into safety dataframe
safe_rawdata1 <- mort_rawdata
safe_rawdata2 <- read.csv("Healthcare Associated Infections - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
safe_rawdata1 = safe_rawdata1[,c(1:8,10,13)]
safe_rawdata2 = safe_rawdata2[,c(1:8,10,12)]
identical(names(safe_rawdata1), names(safe_rawdata2))
# [1] TRUE
safe_rawdata <- rbind(safe_rawdata1, safe_rawdata2)
safe_meas_list = c("HAI_1_SIR","HAI_2_SIR","HAI_3_SIR","HAI_4_SIR","HAI_5_SIR","HAI_6_SIR", "COMP_HIP_KNEE","PSI_90_SAFETY")
safe_hosp <- safe_rawdata[,c(1:8)]
safe_hosp = safe_hosp[!duplicated(safe_hosp),]
safe_meas <- safe_rawdata[,c(1,9:10)]
safe_meas <- subset(safe_meas, Measure.ID %in% safe_meas_list)
safe_meas$Score <- func_numeric(safe_meas$Score)
safe_meas_score <- spread(safe_meas, Measure.ID, Score)
safe_meas_score <- func_rename(safe_meas_score)
safety <- safe_meas_score
# The HAI measures are related to infections contracted by the patients during their stay in the hospital
# we will negative zscore here as well
safety[, 2:ncol(safety)] <- sapply(safety[, -1], negative_zscore)
str(safety)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
safety <- treat_outliers(safety)
safe_master <- merge(safe_hosp, safety, "Provider.ID")
dim(safe_master)
# [1] 4818   16
write.csv(safety, "cleaned_safety_data.csv")

## 4. Experience - Load file "HCAHPS - Hospital.csv" into experience data rame
expe_rawdata <- read.csv("HCAHPS - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
expe_meas_list = c("H_CLEAN_LINEAR_SCORE","H_COMP_1_LINEAR_SCORE","H_COMP_2_LINEAR_SCORE","H_COMP_3_LINEAR_SCORE","H_COMP_4_LINEAR_SCORE","H_COMP_5_LINEAR_SCORE", "H_COMP_6_LINEAR_SCORE", "H_COMP_7_LINEAR_SCORE", "H_HSP_RATING_LINEAR_SCORE","H_QUIET_LINEAR_SCORE" ,"H_RECMND_LINEAR_SCORE" )
names(expe_rawdata)[names(expe_rawdata) == "HCAHPS.Question"] <- "Measure.Name"
names(expe_rawdata)[names(expe_rawdata) == "HCAHPS.Measure.ID"] <- "Measure.ID"
names(expe_rawdata)[names(expe_rawdata) == "HCAHPS.Linear.Mean.Value"] <- "Score"
expe_hosp <- expe_rawdata[,c(1:8)]
expe_hosp = expe_hosp[!duplicated(expe_hosp),]
expe_meas <- expe_rawdata[,c(1,9,16)]
expe_meas <- subset(expe_meas, Measure.ID %in% expe_meas_list)
expe_meas$Score <- func_numeric(expe_meas$Score)
expe_meas_score <- spread(expe_meas, Measure.ID, Score)
experience <- expe_meas_score
# It measures cleanliness, patient hospitality and doctors/staff communication,
# hospital environment etc. We will use positive zscore here
experience[, 2:ncol(experience)] <- sapply(experience[, -1], positive_zscore)
str(experience)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
experience <- treat_outliers(experience)
expe_master <- merge(expe_hosp, experience, by="Provider.ID")
dim(expe_master)
# [1] 4818   19
write.csv(experience, "cleaned_experience_data.csv")


## 5. Medical - Load file "Outpatient Imaging Efficiency - Hopital.csv" into medical data frame
medi_rawdata <- read.csv("Outpatient Imaging Efficiency - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
medi_meas_list = c("OP_10","OP_11","OP_13","OP_14","OP_8" )
medi_hosp <- medi_rawdata[,c(1:8)]
medi_hosp = medi_hosp[!duplicated(medi_hosp),]
medi_meas <- medi_rawdata[,c(1,9,11)]
medi_meas <- subset(medi_meas, Measure.ID %in% medi_meas_list)
medi_meas$Score <- as.numeric(medi_meas$Score)
medi_meas_score <- spread(medi_meas, Measure.ID, Score)
medi_meas_score <- func_rename(medi_meas_score)
medical <- medi_meas_score
# Unecessary usage of imaging tests, lower the better. We will use the negative zscore
medical[, 2:ncol(medical)] <- sapply(medical[, -1], negative_zscore)
str(medical)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
medical <- treat_outliers(medical)
medi_master <- merge(medi_hosp, medical, by="Provider.ID")
dim(medi_master)
# [1] 4818   13
write.csv(medical, "cleaned_medical_data.csv")

## 6. Timeliness - Load file "Timely and Effective Care - Hospital.csv" into timeliness data frame
time_rawdata <- read.csv("Timely and Effective Care - Hospital.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
time_meas_list = c("ED_1b","ED_2b","OP_18b","OP_20","OP_21","OP_3b","OP_5")
time_hosp <- time_rawdata[,c(1:8)]
time_hosp = time_hosp[!duplicated(time_hosp),]
time_meas <- time_rawdata[,c(1,10,12)]
time_meas <- subset(time_meas, Measure.ID %in% time_meas_list)
time_meas$Score <- as.numeric(time_meas$Score)
time_meas_score <- spread(time_meas, Measure.ID, Score)
time_meas_score <- func_rename(time_meas_score)
timeliness <- time_meas_score
# All the measures in timeliness indicate the average time the patient had to wait
# before being attended by the doctors or concerned specialists. We will use negative zscore
timeliness[, 2:ncol(timeliness)] <- sapply(timeliness[, -1], negative_zscore)
str(timeliness)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
timeliness <- treat_outliers(timeliness)
time_master <- merge(time_hosp, timeliness, by="Provider.ID")
dim(time_master)
# [1] 4818   15
write.csv(timeliness, "cleaned_timeliness_data.csv")

## 7. Timeliness - Load file "Timely and Effective Care - Hospital.csv" into timeliness data frame
effe_rawdata <- time_rawdata
effe_meas_list = c("CAC_3", "IMM_2","IMM_3_OP_27_FAC_ADHPCT", "OP_22", "OP_23" ,  "OP_29", "OP_30" , "OP_4","PC_01" ,"STK_4","STK_5","STK_6" ,"STK_8" ,"VTE_1","VTE_2","VTE_3","VTE_5","VTE_6")
effe_hosp <- effe_rawdata[,c(1:8)]
effe_hosp = effe_hosp[!duplicated(effe_hosp),]
effe_meas <- effe_rawdata[,c(1,10,12)]
effe_meas <- subset(effe_meas, Measure.ID %in% effe_meas_list)
effe_meas$Score <- as.numeric(effe_meas$Score)
effe_meas_score <- spread(effe_meas, Measure.ID, Score)
effe_meas_score <- func_rename(effe_meas_score)
effectiveness <- effe_meas_score
# Effectiveness has some columns for which the value higher is better, and few the score lower is better
# We will both postive and negative zscores here for the filtered columns
positive_measures <- c(2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18)
negative_measures <- c(5, 10, 19)
effectiveness[, positive_measures] <- sapply(effectiveness[, positive_measures], positive_zscore)
effectiveness[, negative_measures] <- sapply(effectiveness[, negative_measures], negative_zscore)
str(effectiveness)
# Outlier treatment: According to the CMS documentation, they've performed the outlier treatment for
# measures at the 0.125th and the 99.875th percentiles
effectiveness <- treat_outliers(effectiveness)
effe_master <- merge(effe_hosp, effectiveness, by="Provider.ID")
dim(effe_master)
# [1] 4818   26
write.csv(effectiveness, "cleaned_effectiveness_data.csv")


merge1 <- merge(read_master, mort_master)
merge2 <- merge(merge1, safe_master)
merge3 <- merge(merge2, expe_master)
merge4 <- merge(merge3, medi_master)
merge5 <- merge(merge4, time_master)
merge6 <- merge(merge5, effe_master)
str(merge6)
dim(merge6)
# [1] 4818   72

master_data_x <- merge6

############# Raw data is ready with all required 64 measures and 8 general columns ###

## 2. Data Cleaning
## Remove duplicate Data
master_data_x = master_data_x[!duplicated(master_data_x),]
dim(master_data_x)  ## No duplicates found
# [1] 4818 72

## Remove NA Values
sapply(master_data_x, function(x) sum(length(which(is.na(x)))))

# Provider.ID                Hospital.Name               Address                       City                        State
# 0                            0                            0                            0                            0
# ZIP.Code                   County.Name                 Phone.Number                  READM_30_AMI_score          READM_30_CABG_score
# 0                            0                            0                            2655                         3791
# READM_30_COPD_score        READM_30_HF_score           READM_30_HIP_KNEE_score       READM_30_HOSP_WIDE_score    READM_30_PN_score
# 1170                         1168                         2087                         423                          729
# READM_30_STK_score         MORT_30_AMI_score           MORT_30_CABG_score            MORT_30_COPD_score          MORT_30_HF_score
# 2210                         2430                         3780                         1227                         1200
# MORT_30_PN_score           MORT_30_STK_score           PSI_4_SURG_COMP_score         COMP_HIP_KNEE_score         HAI_1_SIR_score
# 730                         2142                         3000                          2104                         2443
# HAI_2_SIR_score            HAI_3_SIR_score             HAI_4_SIR_score               HAI_5_SIR_score             HAI_6_SIR_score
# 1929                         2775                         3962                         2988                         1546
# PSI_90_SAFETY_score        H_CLEAN_LINEAR_SCORE        H_COMP_1_LINEAR_SCORE         H_COMP_2_LINEAR_SCORE       H_COMP_3_LINEAR_SCORE
# 1594                         1310                         1310                         1310                         1310
# H_COMP_4_LINEAR_SCORE      H_COMP_5_LINEAR_SCORE       H_COMP_6_LINEAR_SCORE         H_COMP_7_LINEAR_SCORE       H_HSP_RATING_LINEAR_SCORE
# 1310                         1310                         1310                         1310                         1310
# H_QUIET_LINEAR_SCORE       H_RECMND_LINEAR_SCORE       OP_10_score                   OP_11_score                 OP_13_score
# 1310                         1310                         1189                         1469                         2585
# OP_14_score                OP_8_score                  ED_1b_score                   ED_2b_score                 OP_18b_score
# 2514                         3294                         1222                         1238                         1234
# OP_20_score                OP_21_score                 OP_3b_score                   OP_5_score                  CAC_3_score
# 1223                         1505                         4425                         2574                         4643
# IMM_2_score                IMM_3_OP_27_FAC_ADHPCT_score OP_22_score                  OP_23_score                 OP_29_score
# 1034                          711                         1543                         3608                         2087
# OP_30_score                OP_4_score                  PC_01_score                   STK_4_score                 STK_5_score
# 2191                         2599                         2296                         3919                         3276
# STK_6_score                STK_8_score                  VTE_1_score                  VTE_2_score                 VTE_3_score
# 2239                         2454                         1200                         1883                         2332
# VTE_5_score                VTE_6_score
# 2587                         3560
# >

sum(sapply(master_data_x, function(x) sum(length(which(is.na(x)))))  )
# [1] 131127

## The Score columns are having 131127 NA values, score columns are very important for our further analysis. As these are all independent(X) variables
## we will deal with the NA cleaning after merging with Dependent variable (ratings- y) in later phase

str(master_data_x)

##############################
## Load Dependent Variable data from Hospital Genral Information.csv file
hospital_ratings  <- read.csv("Hospital General Information.csv", stringsAsFactors = FALSE, na.strings=c("Not Available","Not Applicable"))
dim(hospital_ratings)  ## 4818 rows and 28 columns  - For each hospital one row is existing
# [1] 4818   28
str(hospital_ratings)

master_data_y <- hospital_ratings

sum(is.na(hospital_ratings$Hospital.overall.rating))  ## 321 records having NA values
# [1] 1170

# We need only the Provide.ID and Hospital.overall.rating columns
master_data_y <- master_data_y[,c(1,13)]
str(master_data_y)
dim(master_data_y)  ## 4818 rows and 2 columns (Provider id , Hospital CMS rating)
# [1] 4818 2

## Merge X (independent) & Y (dependent) variables to one using Provider id
dim(master_data_x)
# [1] 4818   72
master_data <- merge(master_data_x,master_data_y, by="Provider.ID")
dim(master_data) ## 4818 rows & 73 columns
#[1] 4818   73

### Data cleaning Part 2
## Remove all the record having NA in Hospital.overall.rating column

sum(is.na(master_data$Hospital.overall.rating))  ## 1170 records having NA values
# [1] 1170

master_data_with_na <- master_data[is.na(master_data$Hospital.overall.rating), ]
master_data_without_na <- master_data[!is.na(master_data$Hospital.overall.rating), ]
dim(master_data_without_na)  ## 3648 rows & 73 columns  (4818 total records)
# [1] 3648   73
dim(master_data_with_na) ## 1170 rows and 73 columns
# [1] 1170   73

## NA values across all coulumns
sapply(master_data_without_na, function(x) sum(is.na(x)))
dim(master_data_without_na)  ## 3648 rows and 73 columns
# [1] 3648   73

# Remove all columns having more than 50% NA in the dataset .. which are not going to yield any outcome
# which is close to 1824 NA values in any x measure.. will remove the measure
## The following measures having >50% of its data as NA
## READM_30_CABG - 2623 | PSI_4_SURG_COMP - 1831 | HAI_4_SIR - 2804 | HAI_5_SIR - 1860 | OP_3b - 3267 |
## STK_4 - 2762 | STK_5 - 2133 | VTE_6 - 2417

output <- names(which(sapply(master_data_without_na, function(x) sum(is.na(x)) < 1824)))
output

master_data_without_na  <- master_data_without_na[, output]
str(master_data_without_na)

## Replace NA with median values of the measure
names(master_data_without_na)
meas <- names(master_data_without_na[,9:ncol(master_data_without_na)])
for (i in meas){
  print(i)
  print(median(master_data_without_na[,i],na.rm=TRUE))
  med <- median(master_data_without_na[,i],na.rm=TRUE)
  master_data_without_na[i][is.na(master_data_without_na[i])] = med
}

## check for NA values after replacing NA with Median
sapply(master_data_without_na, function(x) sum(is.na(x)))

# No NA's found.

dim(master_data_without_na)
# [1] 3648   61
str(master_data_without_na)

## Remove hospital information which is not required
master_data_without_na <- master_data_without_na[,-c(2:8)]
master_data_without_na$Hospital.overall.rating <- as.factor(master_data_without_na$Hospital.overall.rating)
cleaned_master_data <- master_data_without_na
dim(cleaned_master_data)
# [1] 3648   54

####################################################################################################################################

#################################### Perform EDA ###################################################################################

library(Information)
# let us check the hospital_ratings dataset
summary(factor(hospital_ratings$Hospital.Type))

# Acute Care Hospitals                 Childrens          Critical Access Hospitals
#             3382                        99                      1337

# Acute care hospitals are the highest in the dataset, followed by Critical Access Hospitals and Childrens hospitals.

# Let us verify the hospitals ratings for each type for hospitals.
table(hospital_ratings$Hospital.Type, hospital_ratings$Hospital.overall.rating)

#                              1    2    3    4    5
# Acute Care Hospitals       117  659 1426  749  110
# Childrens                    0    0    0    0    0
# Critical Access Hospitals    0   25  346  215    1

# Since the Childrens hospital has no ratings, we will filter these records from the dataset.
# we will remove the Critical Access Hospitals as well, as there are no ratings for 1 and very few for 5.

hospital_ratings <- filter(hospital_ratings, Hospital.Type == "Acute Care Hospitals")
hospital_ratings$Hospital.Type <- factor(as.character(hospital_ratings$Hospital.Type))
table(hospital_ratings$Hospital.Type, hospital_ratings$Hospital.overall.rating)

#                               1    2    3    4    5
# Acute Care Hospitals        117  659 1426  749  110

# Let us check the Overall rating
unique(hospital_ratings$Hospital.overall.rating)
# [1]  3  2 NA  4  5  1

hospital_ratings$Hospital.overall.rating <- func_numeric(hospital_ratings$Hospital.overall.rating)

# Let's check the Ownership type for the hospitals.
table(hospital_ratings$Hospital.Ownership)


# Government - Federal        Government - Hospital District or Authority        Government - Local
# 38                                         281                                         183
# Government - State                      Physician                                 Proprietary
# 54                                          63                                         723
# Tribal               Voluntary non-profit - Church                Voluntary non-profit - Other
# 4                                         284                                         369
# Voluntary non-profit - Private
# 1383

# We see the Private hospitals have the data regarding the ratings in the dataset.

# Let us remove the NA's values-
hospital_ratings <- hospital_ratings[!is.na(hospital_ratings$Hospital.overall.rating), ]
dim(hospital_ratings)
# [1] 3061 28


# Univariate analysis - Segmented:
# Let us Segment across different groups categorical variables and compare their average overall ratings

avg_by_state <- hospital_ratings %>% group_by(State) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), provider_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_state
head(avg_by_state, 10)

# A tibble: 53 x 3
#   State     avg_rating    provider_count
#   <chr>       <dbl>          <int>
#    SD          4.2              15
#    WI          3.69             65
#    DE          3.67              6
#    ID          3.67             12
#    IN          3.59             80
#    MT          3.58             12
#    NH          3.54             13
#    KS          3.52             44
#    MN          3.52             48
#    CO          3.5              44

# We see that South Dakota(SD) has the highest average of 4.2 but the frequency is not the highest.
# we see that Indiana has an average of 3.59 and has the highest frequency.
# We see that Wisconsin has an average of 3.69 and a good count of providers.


graph_by_prov_state <- head(avg_by_state,10) %>%
  ggplot(aes(x = State, y = avg_rating, fill = State,
             label = paste("(",round(avg_rating,2),",",provider_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 2, vjust = -1) +
  theme_classic() +
  xlab("Hospitals by State") +  ylab("Average rating") +
  ggtitle("Average Ratings by State")

graph_by_prov_state

avg_by_hosp_ownership <- hospital_ratings %>% group_by(Hospital.Ownership) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_hosp_ownership

# A tibble: 10 x 3
# Hospital.Ownership                              avg_rating hospital_count
# <chr>                                              <dbl>    <int>
# 1 Physician                                         4.10    31
# 2 Voluntary non-profit - Church                     3.15   275
# 3 Voluntary non-profit - Other                      3.11   348
# 4 Voluntary non-profit - Private                    3.07  1284
# 5 Government - Hospital District or Authority       2.96   261
# 6 Government - Federal                              2.93    15
# 7 Proprietary                                       2.91   633
# 8 Government - Local                                2.79   168
# 9 Government - State                                2.64    44
# 10 Tribal                                           2.5      2


graph_by_hosp_owner <- avg_by_hosp_ownership %>%
  ggplot(aes(x = Hospital.Ownership, y = avg_rating, fill = Hospital.Ownership,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 2, vjust = -1) +
  theme_classic() +
  xlab("Hospitals by Onwership") +  ylab("Average rating") +
  ggtitle("Average Ratings by Onwership")

graph_by_hosp_owner

# we see good average ratings for "Physician" owned hospitals and voluntary non-profit owned hospitals
# Comparitively low ratings for tribal, Government -local owned hospitals.
library(cowplot)
grid_plot <- plot_grid(graph_by_prov_state, graph_by_hosp_owner)
grid_plot

# Average of Readmission Ratings
avg_by_readmission <- hospital_ratings %>% group_by(Readmission.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_readmission

# A tibble: 4 x 3
# Readmission.national.comparison       avg_rating hospital_count
# <chr>                                   <dbl>          <int>
# 1 NA                                    3.92            124
# 2 Above the National average            3.57            791
# 3 Same as the National average          3.03           1299
# 4 Below the National average            2.38            847

# Here we see that the average ratings of hospitals which have above the national average for readmissions is about 3.57 and around 811 hospitals rank here
# A considerable number of hospitals have average same as national average and their average rating is around 3.12
# Those which are below the national average have an average rating around 2.38.
# Above and Below national average hospitals are similar in number with respect to the readmissions count.

graph_by_readmission <- avg_by_readmission %>%
  ggplot(aes(x = Readmission.national.comparison, y = avg_rating, fill = Readmission.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Readmission Comparison ") +  ylab("Average rating") +
  ggtitle("Average Ratings by Readmissions")

graph_by_readmission

readmission_gp <- round(avg_by_readmission$avg_rating[2] - avg_by_readmission$avg_rating[4],2)

# Average Mortality ratings
avg_by_mortality <- hospital_ratings %>% group_by(Mortality.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_mortality

# A tibble: 4 x 3
# Mortality.national.comparison     avg_rating hospital_count
# <chr>                                <dbl>          <int>
# 1 NA                                  3.63            194
# 2 Above the National average          3.29            400
# 3 Same as the National average        3.01           2124
# 4 Below the National average          2.48            343

# Excluding the NA's, around 400 hospitals have mortality rate above the national average
# A considerable number of hospitals (2124) have mortality rate same as the National average
# Few hospitals (343) have the mortality rate below the National average.

graph_by_mortality <- avg_by_mortality %>%
  ggplot(aes(x = Mortality.national.comparison, y = avg_rating, fill = Mortality.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Morality Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Mortality")

graph_by_mortality

mortality_gp <- round(avg_by_mortality$avg_rating[2] - avg_by_mortality$avg_rating[4], 2)

# Average safety ratings
avg_by_safety <- hospital_ratings %>% group_by(Safety.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_safety

# # A tibble: 4 x 3
# Safety.of.care.national.comparison    avg_rating hospital_count
# <chr>                                     <dbl>   <int>
# 1 Above the National average               3.44   804
# 2 NA                                       3.12   205
# 3 Same as the National average             3.09  1379
# 4 Below the National average               2.36   673

# Here we see that the average ratings of hospitals which have above the national average for safety of care is about 3.44 and around 804 hospitals rank here
# A considerable number of hospitals have average same as national average and their average rating is around 3.09
# Those which are below the national average have an average rating around 2.36.

graph_by_safety <- avg_by_safety %>%
  ggplot(aes(x = Safety.of.care.national.comparison, y = avg_rating, fill = Safety.of.care.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Safety of care Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Safety")

graph_by_safety

safety_gp <- round(avg_by_safety$avg_rating[1] - avg_by_safety$avg_rating[4],2)

# Average experience ratings

avg_by_experience <- hospital_ratings %>% group_by(Patient.experience.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_experience

# # A tibble: 4 x 3
# Patient.experience.national.comparison    avg_rating hospital_count
# <chr>                                         <dbl>   <int>
# 1 Above the National average                   3.69   887
# 2 Same as the National average                 3.11   996
# 3 NA                                           2.86    99
# 4 Below the National average                   2.42  1079

graph_by_experience <- avg_by_experience %>%
  ggplot(aes(x = Patient.experience.national.comparison, y = avg_rating, fill = Patient.experience.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Patient Experience Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Experience")

graph_by_experience

experience_gp <- round(avg_by_experience$avg_rating[1] - avg_by_experience$avg_rating[4],2)

# Average medical ratings
avg_by_medical <- hospital_ratings %>% group_by(Efficient.use.of.medical.imaging.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_medical

# # A tibble: 4 x 3
# Efficient.use.of.medical.imaging.national.comparison    avg_rating hospital_count
# <chr>                                                       <dbl>   <int>
# 1 NA                                                         3.09   556
# 2 Same as the National average                               3.04  1806
# 3 Above the National average                                 3.03   359
# 4 Below the National average                                 2.86   340

# there's not much difference between the average ratings of above and same as
# national average hospitals by medical group variables

graph_by_medical <- avg_by_medical %>%
  ggplot(aes(x = Efficient.use.of.medical.imaging.national.comparison, y = avg_rating, fill = Efficient.use.of.medical.imaging.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 2, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Use of Medical Imaging Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Medical")

graph_by_medical

medical_gp <- round(avg_by_medical$avg_rating[3] - avg_by_medical$avg_rating[4],2)

# Average timeliness ratings
avg_by_timeliness <- hospital_ratings %>% group_by(Timeliness.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_timeliness

# # A tibble: 4 x 3
# Timeliness.of.care.national.comparison    avg_rating hospital_count
# <chr>                                         <dbl>   <int>
# 1 NA                                           3.62   151
# 2 Above the National average                   3.22   826
# 3 Same as the National average                 3.12  1185
# 4 Below the National average                   2.62   899

# timeliness has a significant effect on the average rating (from 2.62 to 3.22)

graph_by_timeliness <- avg_by_timeliness %>%
  ggplot(aes(x = Timeliness.of.care.national.comparison, y = avg_rating, fill = Timeliness.of.care.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Timeliness of care Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Timeliness")

graph_by_timeliness

timeliness_gp <- round(avg_by_timeliness$avg_rating[2] - avg_by_timeliness$avg_rating[4],2)

# Average effectiveness ratings

avg_by_effectiveness <- hospital_ratings %>% group_by(Effectiveness.of.care.national.comparison) %>%
  summarise(avg_rating=mean(Hospital.overall.rating, na.rm = T), hospital_count = n()) %>%
  arrange(desc(avg_rating))
avg_by_effectiveness

# # A tibble: 4 x 3
# Effectiveness.of.care.national.comparison     avg_rating hospital_count
# <chr>                                              <dbl> <int>
# 1 Above the National average                      3.08   991
# 2 Same as the National average                    3.07  1665
# 3 Below the National average                      2.72   405

graph_by_effectiveness <- avg_by_effectiveness %>%
  ggplot(aes(x = Effectiveness.of.care.national.comparison, y = avg_rating, fill = Effectiveness.of.care.national.comparison,
             label = paste("(",round(avg_rating,2),",",hospital_count,")"), vjust=-2)) +
  geom_bar(stat = "identity") + geom_text(size = 3, vjust = -0.5) +
  theme_classic() +
  xlab("Hospitals by National Effectiveness of care Comparison") +  ylab("Average rating") +
  ggtitle("Average Ratings by Effectiveness")

graph_by_effectiveness

effectiveness_gp <- round(avg_by_effectiveness$avg_rating[1] - avg_by_effectiveness$avg_rating[3],2)

# Average Star ratings by each group and its impact over the ratings
group_impact <- data.frame(readmission = readmission_gp, mortality = mortality_gp,safety = safety_gp, experience = experience_gp,
                           medical = medical_gp, timeliness = timeliness_gp, effectiveness = effectiveness_gp)

group_impacts_in_order <- t(group_impact[order(group_impact, decreasing=T)])
group_impacts_in_order

# [,1]
# experience    1.27
# readmission   1.19
# safety        1.08
# mortality     0.81
# timeliness    0.60
# effectiveness 0.36
# medical       0.17

# As per the cms measures the top 4 experience, readmission, safety and mortality have a 22% of weightage
# and the last 3 groups timeliness, effectiveness, medical have 4% of weightage

grid_plot1 <- plot_grid(graph_by_readmission, graph_by_mortality, graph_by_safety, graph_by_experience,graph_by_medical, graph_by_timeliness, graph_by_effectiveness)
grid_plot1

######################### Bivariate analysis ###############################

# correlation of readmission
readmission <- read_master
str(readmission)
correlation_readmission <- round(cor(readmission[, -c(1:8)], use="pairwise.complete.obs"), 2)
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
corrplot(correlation_readmission, method="shade",type = "full", col = col1(20), addCoef.col = "maroon")
write.csv(correlation_readmission, "correlation_readmission.csv")

# Visual Observations:
# The correlation measures for READM_30_HIP_KNEE_score is very low accross all the variables.
# The correlation measures for READM_30_CABG_score is very low accross all the variables.
# READM_30_HOSP_WIDE_score has very good correlation measures accross all the variables.

# Now let us check, how the measures have an impact on the ratings.
readmission <- readmission[, -c(2:8)]
readmission_rating <- merge(readmission, master_data_y, by="Provider.ID")

readmission_group_summary <- readmission_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
readmission_group_summary

# # A tibble: 6 x 9
# Hospital.overall.rating READM_30_AMI_score READM_30_CABG_score READM_30_COPD_score READM_30_HF_score READM_30_HIP_KNEE_score READM_30_HOSP_WIDE_score READM_30_PN_score READM_30_STK_score
# <int>              <dbl>               <dbl>               <dbl>             <dbl>                   <dbl>                    <dbl>             <dbl>              <dbl>
# 1                 -0.588              -0.579              -1.01            -1.14                   -0.309                   -1.75              -1.24              -0.946
# 2                 -0.413              -0.214              -0.392           -0.480                  -0.283                   -0.787             -0.609             -0.323
# 3                  0.0501             -0.0370              0.0135          -0.00873                -0.0543                  -0.0208             0.0322             0.0372
# 4                  0.362               0.313               0.346            0.436                   0.202                    0.612              0.409              0.327
# 5                  1.01                0.454               0.965            1.26                    0.883                    1.66               0.850              0.663
# NA                -0.698               0.228              -0.0357          -0.0303                 -0.00712                  0.00494            0.123              0.155

# We notice that, the ratings increase as all scores of the measures for readmission increase.
# Hence, all the readmission measures have an important role in predicting the ratings.

# correlation of mortality
mortality <- mort_master
str(mortality)
correlation_mortality <- round(cor(mortality[, -c(1:8)], use="pairwise.complete.obs"), 2)
corrplot(correlation_mortality, method="shade",type = "full", col = col1(20), addCoef.col = "maroon")
write.csv(correlation_mortality, "correlation_mortality.csv")
# The correlation values are too low compared to the correlation values for readmission.
# With low correlation values, we see HF, PN and STK scores have some significance.
# Probably these measures would have an impact in the ratings.

# Now let us check, how the measures have an impact on the ratings.
mortality <- mortality[, -c(2:8)]
mortality_rating <- merge(mortality, master_data_y, by="Provider.ID")
mortality_group_summary <- mortality_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
mortality_group_summary

# # A tibble: 6 x 8
# Hospital.overall.rating MORT_30_AMI_score MORT_30_CABG_score MORT_30_COPD_score MORT_30_HF_score MORT_30_PN_score MORT_30_STK_score PSI_4_SURG_COMP_score
# <int>             <dbl>              <dbl>              <dbl>            <dbl>            <dbl>             <dbl>                 <dbl>
#   1       	    -0.436              -0.226             -0.411            0.0892          -0.503            -0.554                -0.601
#   2           	-0.230              -0.118             -0.199           -0.104           -0.339            -0.0885               -0.240
#   3           	 0.00607            -0.0151            -0.0153          -0.0783          -0.0513           -0.0146                0.0177
#   4           	 0.235               0.133              0.180            0.210            0.376             0.168                 0.290
#   5           	 0.809               0.394              0.597            0.753            0.899             0.395                 0.443
#   NA          	-0.234              -0.136              0.0583          -0.251           -0.0476           -0.132                 1.40

# We Can see that the rating increases as each measure score increases
# but for HF score, it behaves differently.
# AMI, HF, ON have an effect on the ratings.

# correlation of safety
safety <- safe_master
str(safety)
correlation_safety <- round(cor(safety[, -c(1:8)], use="pairwise.complete.obs"), 2)
corrplot(correlation_safety, method="shade",type = "full", col = col1(20), addCoef.col = "maroon")
write.csv(correlation_safety, "correlation_safety.csv")
# COMP_HIP_KNEE_score has a slight negative correlation between HAI_1,2,3 and slight +ve correlation for HAI_4,5,6 and PSI.
# HAI-1, 2 and 3 are correlated compared to the other measures, all are not correlated with HAI-5, HAI-6, PSI and COMP-HIP-KNEE score
# HAI-1 has correlation with HAI-5.

# Now let us check, how the measures have an impact on the ratings.
safety <- safety[, -(2:8)]
safety_rating <- merge(safety, master_data_y, by="Provider.ID")
safety_group_summary <- safety_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
safety_group_summary

# # A tibble: 6 x 9
# Hospital.overall.rating COMP_HIP_KNEE_score HAI_1_SIR_score HAI_2_SIR_score HAI_3_SIR_score HAI_4_SIR_score HAI_5_SIR_score HAI_6_SIR_score PSI_90_SAFETY_score
# <int>                        <dbl>           <dbl>           <dbl>           <dbl>           <dbl>           <dbl>           <dbl>               <dbl>
#   1                         -0.204          -0.445          -0.526          -0.173          -0.395         -0.341          -0.0257              -1.88
#   2                         -0.149          -0.172          -0.109          -0.0459          0.0362        -0.0710         -0.100               -0.531
#   3                         -0.0765          0.0489          0.0654          0.0119          0.0434         0.00232        -0.0240               0.0652
#   4                          0.125           0.177           0.0745          0.0613         -0.0488         0.152           0.00476              0.471
#   5                          0.846           0.232           0.120          -0.0285          0.166          0.280           0.510                0.942
#  NA                          0.153          -0.0970         -0.0262          0.0660          0.0439        -0.0714          0.403                0.0689

# As per the above summary, it looks like COMP_HIP_KNEE_score, PSI_90_SAFETY_score have a significant impact on the ratings.

# correlation of experience
experience <- expe_master
correlation_experience <- round(cor(experience[, -c(1:8)], use="pairwise.complete.obs"), 2)
par(pin=c(5,5))              ##  (width, height) in inches
par(omi=c(0,0.5,0.5,0.5))        ## (bottom, left, top, right)  in inches
corrplot(correlation_experience, method="shade",type = "full", col = col1(20), addCoef.col = "maroon", tl.cex = 0.7, tl.offset = 0.5, number.cex = 0.8, tl.pos = 't')
write.csv(correlation_experience, "correlation_experience.csv")
# we can visualize that all the variables are positively correlated with each other.
# This implies that the overall hospital experience, which is all about the hospitality and clear communication, helps improve the ratings.
# All the measures play a vital role in the ratings.

# Now let us check, how the measures have an impact on the ratings.
experience <- experience[, -c(2:8)]
experience_rating <- merge(experience, master_data_y, by="Provider.ID")
experience_group_summary <- experience_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
experience_group_summary
# # A tibble: 6 x 12
# Hospital.overall.rating H_CLEAN_LINEAR_~ H_COMP_1_LINEAR~ H_COMP_2_LINEAR~ H_COMP_3_LINEAR~ H_COMP_4_LINEAR~ H_COMP_5_LINEAR~ H_COMP_6_LINEAR~ H_COMP_7_LINEAR~ H_HSP_RATING_LI~ H_QUIET_LINEAR_~ H_RECMND_LINEAR~
#   <int>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
#     1 	          -1.28            -1.57           -1.17            -1.64             -1.48            -1.31            -1.32            -1.37            -1.53            -1.09            -1.31
#     2 	          -0.705           -0.790          -0.709           -0.806            -0.767           -0.739           -0.661           -0.784           -0.790           -0.578           -0.737
#     3 	          -0.0223          -0.0303         -0.00778         -0.00465          -0.0252          -0.0574          -0.0351          -0.0777          -0.0649          -0.0224          -0.0874
#     4 	           0.530            0.602           0.504            0.566             0.577            0.577            0.524            0.620            0.617            0.364            0.600
#     5 	           0.922            1.14            0.822            1.06              1.05             1.02             0.883            1.25             1.38             0.958            1.37
#    NA 	           0.378            0.473           0.410            0.613             0.449            0.570            0.425            0.605            0.554            0.729            0.500

# This implies all the measures in experience are highly important measures.

# Correlation of medical
medical <- medi_master
correlation_medical <- round(cor(medical[, -c(1:8)], use="pairwise.complete.obs"), 2)
corrplot(correlation_medical, method="shade",type = "full", col = col1(20), addCoef.col = "maroon")
write.csv(correlation_medical, "correlation_medical.csv")
# Only OP-10 and OP_11 have some good correlation.
# Rest all the correlations are negligible.

# Now let us check, how the measures have an impact on the ratings.
medical <- medical[, -c(2:8)]
medical_rating <- merge(medical, master_data_y, by="Provider.ID")
medical_group_summary <- medical_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
medical_group_summary

# # A tibble: 6 x 6
# Hospital.overall.rating OP_10_score OP_11_score OP_13_score OP_14_score OP_8_score
# <int>                       <dbl>       <dbl>       <dbl>       <dbl>      <dbl>
#   1                        0.0750      0.132      0.0318      -0.685     0.130
#   2                       -0.0888     -0.0955    -0.0360      -0.273     0.0312
#   3                       -0.0114     -0.0134     0.0318       0.0235   -0.0176
#   4                        0.133       0.110     -0.0156       0.100     0.00368
#   5                        0.131       0.240     -0.135        0.317     0.0970
#  NA                       -0.213      -0.272      0.00673      1.06     -0.612

# Only OP_14 seems significant, but the overall weight of medical group itself is quite less

# Correlation of timeliness
timeliness <- time_master
correlation_timeliness <- round(cor(timeliness[, -c(1:8)], use="pairwise.complete.obs"), 2)
corrplot(correlation_timeliness, method="shade",type = "full", col = col1(20), addCoef.col = "maroon")
write.csv(correlation_timeliness, "correlation_timeliness.csv")
# All ED scores have good correlation with OP_18b, OP_20, and OP_21.
# Even OP_18b, 20 and 21 have good correlations with each other.

# Now let us check, how the measures have an impact on the ratings.
timeliness <- timeliness[, -c(2:8)]
timeliness_rating <- merge(timeliness, master_data_y, by="Provider.ID")
timeliness_group_summary <- timeliness_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
timeliness_group_summary

# # A tibble: 6 x 8
# Hospital.overall.rating ED_1b_score ED_2b_score OP_18b_score OP_20_score OP_21_score OP_3b_score OP_5_score
# <int>                       <dbl>       <dbl>        <dbl>       <dbl>       <dbl>       <dbl>      <dbl>
#   1                         -1.40      -1.30        -0.982      -1.01       -0.763      -0.403     -0.841
#   2                         -0.514     -0.508       -0.432      -0.266      -0.294      -0.137     -0.0914
#   3                          0.101      0.0991       0.0551      0.0390      0.0297     -0.0210    -0.0155
#   4                          0.242      0.206        0.0933      0.152       0.236       0.182      0.204
#   5                          0.301      0.174       -0.0454      0.125       0.263       0.542      0.341
#  NA                          0.393      0.499        0.747       0.288       0.160      -0.355     -0.181

# The above implies ED_1b and OP_21, OP_3b, OP_5 are the most important measures
# Since the overall weightage of the group is only 4%, the impact might be less.

# Correlation of effectiveness
effectiveness <- effe_master
correlation_effectiveness <- round(cor(effectiveness[, -c(1:8)], use="pairwise.complete.obs"), 2)
par(pin=c(75,75))              ##  (width, height) in inches
par(omi=c(0,0.8,0.8,0.8))        ## (bottom, left, top, right)  in inches
corrplot(correlation_effectiveness, method="shade",type = "full", col = col1(20), addCoef.col = "maroon", tl.cex = 0.8, tl.offset = 0.7, number.cex = 0.7,
         tl.pos = 'b')
write.csv(correlation_effectiveness, "correlation_effectiveness.csv")

# We can see very good correlations for STK_4, STK5,STK_6, STK_8, VTE_1, VTE_2 and VTE_6.
# CAC_3 and IMM_2, STK_6, STK_8, VTE_1 are positively correlated.

# Now let us check, how the measures have an impact on the ratings.
effectiveness <- effectiveness[, -c(2:8)]
effectiveness_rating <- merge(effectiveness, master_data_y, by="Provider.ID")
effectiveness_group_summary <- effectiveness_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_all(funs(mean(., na.rm = T)))
effectiveness_group_summary
write.csv(effectiveness_group_summary, "effectiveness_rating_summary.csv")

# We see a mixed behaviour here, ratings increase as measures increase for few and viceversa.
# This implies that VTE (1, 2, 3, 5) measures are correlated to each other, they might play role in the ratings.
# Similar observations are there for STK measures

################################ EDA Completed #########################################################################################

############################## Random Forest Modelling ##########################################
library(caTools)
library(rpart)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)

# We will use the cleaned master data as data set-
# cleaned_master_data

### split the train and test set
set.seed(123)

data= sample.split(cleaned_master_data,SplitRatio = 0.3)
train =subset(cleaned_master_data,data==FALSE)
test  =subset(cleaned_master_data,data==TRUE)
dim(train)
# [1] 2570   54

dim(test)
# [1] 1078   54

str(train)

# We will use mtry=20, we will use ntree = 500,1000 and 1500 as per the mentor's suggestions.
model_rf1 <- randomForest(Hospital.overall.rating~., data=train, promiximity=FALSE, ntree=500, mtry=20, do.trace=TRUE, na.action=na.omit)
test1 <- predict(model_rf1, newdata=test)
table(test1, test$Hospital.overall.rating)

# test1   1   2   3   4   5
#     1  15   6   0   0   0
#     2  17 138  17   0   0
#     3   0  68 466  65   0
#     4   0   0  21 228  24
#     5   0   0   0   0  13

summary(model_rf1)
conf_matrix1 <- confusionMatrix(test1, test$Hospital.overall.rating, positive="Yes")
conf_matrix1

# Confusion Matrix and Statistics
#
#                   Reference
# Prediction   1   2   3   4   5
#          1  15   6   0   0   0
#          2  17 138  17   0   0
#          3   0  68 466  65   0
#          4   0   0  21 228  24
#          5   0   0   0   0  13
#
# Overall Statistics
#
# Accuracy : 0.7978
# 95% CI : (0.7725, 0.8214)
# No Information Rate : 0.4675
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.6835
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.46875   0.6509   0.9246   0.7782  0.35135
# Specificity           0.99426   0.9607   0.7683   0.9427  1.00000
# Pos Pred Value        0.71429   0.8023   0.7780   0.8352  1.00000
# Neg Pred Value        0.98392   0.9183   0.9207   0.9193  0.97746
# Prevalence            0.02968   0.1967   0.4675   0.2718  0.03432
# Detection Rate        0.01391   0.1280   0.4323   0.2115  0.01206
# Detection Prevalence  0.01948   0.1596   0.5557   0.2532  0.01206
# Balanced Accuracy     0.73151   0.8058   0.8464   0.8604  0.67568

# Accuracy is 79.8%

model_rf2 <- randomForest(Hospital.overall.rating~., data=train, promiximity=FALSE, ntree=1000, mtry=20, do.trace=TRUE, na.action=na.omit)
test2 <- predict(model_rf2, newdata=test)
table(test2, test$Hospital.overall.rating)
summary(model_rf2)
conf_matrix2 <- confusionMatrix(test2, test$Hospital.overall.rating, positive="Yes")
conf_matrix2

# Confusion Matrix and Statistics
#
#                 Reference
# Prediction   1   2   3   4   5
#          1  13   6   0   0   0
#          2  19 136  16   0   0
#          3   0  70 464  61   0
#          4   0   0  24 232  23
#          5   0   0   0   0  14
#
# Overall Statistics
#
# Accuracy : 0.7968
# 95% CI : (0.7716, 0.8205)
# No Information Rate : 0.4675
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.6823
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                        Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.40625   0.6415   0.9206   0.7918  0.37838
# Specificity           0.99426   0.9596   0.7718   0.9401  1.00000
# Pos Pred Value        0.68421   0.7953   0.7798   0.8315  1.00000
# Neg Pred Value        0.98206   0.9162   0.9172   0.9237  0.97838
# Prevalence            0.02968   0.1967   0.4675   0.2718  0.03432
# Detection Rate        0.01206   0.1262   0.4304   0.2152  0.01299
# Detection Prevalence  0.01763   0.1586   0.5519   0.2588  0.01299
# Balanced Accuracy     0.70026   0.8005   0.8462   0.8660  0.68919

# Accuracy is 79.7%

model_rf3 <- randomForest(Hospital.overall.rating~., data=train, promiximity=FALSE, ntree=1500, mtry=20, do.trace=TRUE, na.action=na.omit)
test3 <- predict(model_rf3, newdata=test)
table(test3, test$Hospital.overall.rating)
summary(model_rf3)
conf_matrix3 <- confusionMatrix(test3, test$Hospital.overall.rating, positive="Yes")
conf_matrix3

# Confusion Matrix and Statistics
#
#                   Reference
# Prediction   1   2   3   4   5
#          1  13   6   0   0   0
#          2  19 139  14   0   0
#          3   0  67 468  62   0
#          4   0   0  22 231  23
#          5   0   0   0   0  14
#
# Overall Statistics
#
# Accuracy : 0.8024
# 95% CI : (0.7774, 0.8258)
# No Information Rate : 0.4675
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.6909
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.40625   0.6557   0.9286   0.7884  0.37838
# Specificity           0.99426   0.9619   0.7753   0.9427  1.00000
# Pos Pred Value        0.68421   0.8081   0.7839   0.8370  1.00000
# Neg Pred Value        0.98206   0.9194   0.9252   0.9227  0.97838
# Prevalence            0.02968   0.1967   0.4675   0.2718  0.03432
# Detection Rate        0.01206   0.1289   0.4341   0.2143  0.01299
# Detection Prevalence  0.01763   0.1596   0.5538   0.2560  0.01299
# Balanced Accuracy     0.70026   0.8088   0.8519   0.8655  0.68919

# Accuracy: 80.24%

# We observe 80.24% accuracy with this model, all the classes have good specificity and sensitivity.

print(model_rf3)

# Call:
#   randomForest(formula = Hospital.overall.rating ~ ., data = train,      promiximity = FALSE, ntree = 1500, mtry = 20, do.trace = TRUE,      na.action = na.omit)
# Type of random forest: classification
# Number of trees: 1500
# No. of variables tried at each split: 20
#
# OOB estimate of  error rate: 19.38%
# Confusion matrix:
#    1   2    3   4  5 class.error
# 1 31  54    0   0  0  0.63529412
# 2  5 332  135   0  0  0.29661017
# 3  0  42 1174  52  0  0.07413249
# 4  0   0  169 500  2  0.25484352
# 5  0   0    0  39 35  0.52702703

# This plot provides us class errors.
plot(model_rf3)

plot(margin(model_rf3,test$Hospital.overall.rating))
# We see here most of the values >0 mean that the majority was right, hence this has predicted correct

# This plot provides us all the variables as per their importance in the model to predict the ratings.
varImpPlot(model_rf3)

# This provides the top 10 measures which are highly important as per the variance Importance measures.
varImpPlot(model_rf3, main="Variable Importance of measures using MeanDecreaseGini", sort=T, n.var=10)

# We have got a good accuracy for this model and as per this model the top 10 variables as per their importance are
# 1.READM_30_HOSP_WIDE 2.PSI_90_SAFETY 3.MORT_30_PN 4.H_HSP_RATING_LINEAR_MEAN 5.H_COMP_7_LINEAR_MEAN
# 6.H_RECMND_LINEAR_MEAN 7.H_COMP_3_LINEAR_MEAN 8.MORT_30_HF 9.H_COMP_1_LINEAR_MEAN 10.MORT_30_COPD
# Overall the model is doing good.

######################### Data Modeling and Evaluation End ##########################################

##################################### Factor Analysis ###########################
# We will perform Factor analysis for each of the seven groups.
# install.packages("psych")
# install.packages("Information")

library(psych)

# As per the mentor suggestions we will first calculate the measures for effectiveness group and then create a function to perform the same for all the groups.
# effectiveness scores
effectiveness <- effe_master[,-c(2:8)]
str(effectiveness)
# remove the Provider.ID column
eff <- effectiveness[, -1]
str(eff)

#scale the data
eff <- as.data.frame(scale(eff))
str(eff)
# Using psych package, apply factor analysis on eff
# This command was provided by the mentor. We have to use the method=ML (maximum likelihood) and scores="tenBerge"
Eff.fa <- fa(eff, method="ml", scores = "tenBerge")

# finding the weights
eff_weights <- Eff.fa$weights
eff_weights
sum(eff_weights)
# [1] 1.353024
# The sum value is supposed to be 1
eff_weights <- eff_weights/sum(eff_weights)
sum(eff_weights)
# CMS says a hospital needs to have at least 3 measures per group
# Rows with attribute values having more than 3 NA's are removed(Removing all such hospitals)

eff$remove <- 0

for (x in 1:nrow(eff)){
  if (sum(!is.na(eff[x, ])) <= 3) {eff[x, c("remove")] = 1}
  else {eff[x, c("remove")] = 0}
}

round(sum(eff$remove)/nrow(eff) * 100,2)
# [1] 22.31
# Thus, about 22% hospitals are not valid as per CMS restrictions.

row_num <- which(!eff$remove)


eff <- eff[which(!eff$remove), ]
sum(eff$remove)
# [1] 0
eff <- eff[, -ncol(eff)]
str(eff)

# We will replace the NA's with the median values.
median_na <- function(x){
  x[which(is.na(x))] <- median(x, na.rm=T)
  return(x)
}

eff <- data.frame(sapply(eff, median_na))

# calculating group scores
head(eff_weights)

# Multiply each attribute with corresponding weights obtained using Eff.fa$weights
eff <- eff * eff_weights
str(eff)

# For each hospital, calculate average score for effectiveness
# Avg -> summed_score/number_of _measures
str(eff)

eff = eff %>% mutate(score=round(rowSums(.)/length(eff),3))

effectiveness <- effectiveness[row_num, ]
effectiveness$score <- eff$score
effectiveness_scores <- effectiveness[, c("Provider.ID", "score")]
str(effectiveness_scores)
# let us rename the column with appropriate measure name.
names(effectiveness_scores)[2]<-paste("effe_score")
str(effectiveness_scores)

################################################# effectiveness scores complete #################################################################

# Since we need to repeat this process for the rest of the measures let us create a function

function_group_score <- function(data) {
  # remove the provide.ID column
  df <- data[, -1]

  #scale the data
  df <- as.data.frame(scale(df))
  str(df)

  # Using psych package, apply factor analysis on data
  # This command was provided by the mentor. We have to use the method=ML (maximum likelihood) and scores="tenBerge"
  df.fa <- fa(df, method="ml", scores = "tenBerge")

  # finding the weights
  df_weights <- df.fa$weights
  sum(df_weights)

  df_weights <- df_weights/sum(df_weights)
  sum(df_weights)

  # CMS says a hospital needs to have at least 3 measures per group
  # Rows with attribute values having more than 3 NA's are removed(Removing all such hospitals)
  df$remove <- 0

  for (i in 1:nrow(df)){
    if (sum(!is.na(df[i, ])) <= 3) {df[i, c("remove")] = 1}
    else {df[i, c("remove")] = 0}
  }

  row_num <- which(!df$remove)

  df <- df[which(!df$remove), ]
  sum(df$remove)
  df <- df[, -ncol(df)]
  str(df)

  # We will replace the NA's with the median values.
  median_na <- function(x){
    x[which(is.na(x))] <- median(x, na.rm=T)
    return(x)
    }

  df <- data.frame(sapply(df, median_na))

  # calculating group scores
  # Multiply each attribute with corresponding weights obtained using df.fa$weights
  df <- df * df_weights

  # For each hospital, calculate average score for effectiveness
  # Avg -> summed_score/number_of _measures

  df = df %>% mutate(score=round(rowSums(.)/length(df),3))

  data <- data[row_num, ]
  data$score <- df$score
  data_scores <- data[, c("Provider.ID", "score")]
  # let us rename the column with appropriate measure name.
  names(data_scores)[2] <- paste("df_score")

  return(data_scores)
}

##############################################

# readmission scores
data <- read_master[,-c(2:8)]
readmission_scores <- function_group_score(data)
str(readmission_scores)
names(readmission_scores)[2] <- "radm_score"
head(readmission_scores)

# mortality scores
data <- mort_master[,-c(2:8)]
mortality_scores <- function_group_score(data)
str(mortality_scores)
names(mortality_scores)[2] <- "mort_score"
head(mortality_scores)

# safety scores
data <- safe_master[,-c(2:8)]
safety_scores <- function_group_score(data)
str(safety_scores)
names(safety_scores)[2] <- "safety_score"
str(safety_scores)

# experience scores
data <- expe_master[,-c(2:8)]
experience_scores <- function_group_score(data)
str(experience_scores)
names(experience_scores)[2] <- "expe_score"
str(experience_scores)

# medical scores
data <- medi_master[,-c(2:8)]
medical_scores <- function_group_score(data)
str(medical_scores)
names(medical_scores)[2] <- "medi_score"
str(medical_scores)

# timeliness scores
data <- time_master[,-c(2:8)]
timeliness_scores <- function_group_score(data)
str(timeliness_scores)
names(timeliness_scores)[2] <- "time_score"
str(timeliness_scores)

# Merge all the group scores into a master data frame group_scores using providerID
merge1 = merge(readmission_scores, mortality_scores, by = "Provider.ID")
merge2 = merge(merge1, safety_scores, by = "Provider.ID")
merge3 = merge(merge2, experience_scores, by = "Provider.ID")
merge4 = merge(merge3, medical_scores, by = "Provider.ID")
merge5 = merge(merge4, timeliness_scores, by = "Provider.ID")
merge6 = merge(merge5, effectiveness_scores, by = "Provider.ID")
group_scores <- merge6

##########################################3############  group scores calculation complete  ###############################################################

########################################## calculating the final scores ##################################################################
# Multiply each group_scores with corresponding measure weights given by CMS and sum
# them to form final_score=sum(each_column * cms_weights)/7

str(group_scores)
cms_weights <- c(readmission = 0.22, mortality = 0.22,safety = 0.22, experience = 0.22, medical = 0.04, timeliness = 0.04, effectiveness = 0.04)

group_scores[,"final_score"] <-
  apply(group_scores[, -c(1,ncol(group_scores))], 1, function(x)
    round(sum(x * cms_weights,na.rm = T),3))
str(group_scores)
head(group_scores)

# > head(group_scores)
#     Provider.ID radm_score mort_score safety_score expe_score medi_score time_score effe_score final_score
# 1       10001     -0.040     -0.021       -0.104     -0.033      0.033     -0.043      0.008      -0.046
# 2       10005      0.002     -0.164       -0.005     -0.017     -0.070      0.071      0.022      -0.040
# 3       10006      0.006     -0.182       -0.047     -0.039      0.137     -0.006      0.017      -0.052
# 4       10011      0.069     -0.009       -0.052     -0.030     -0.080     -0.229     -0.088      -0.014
# 5       10012     -0.013     -0.162        0.060      0.017     -0.112      0.075      0.009      -0.024
# 6       10016      0.004     -0.069        0.032     -0.007     -0.271     -0.048      0.023      -0.021
# >

final_score_df <- group_scores[, c(1, ncol(group_scores))]
summary(final_score_df$final_score)

# > summary(final_score_df$final_score)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -0.261000 -0.025000  0.000000 -0.002328  0.024000  0.250000


# Removing the final_score column from group_scores
str(group_scores)
group_scores <- group_scores[,-9]

########################################## calculating the final scores complete ##################################################################

########################################## Unsupervised Modelling using - kmeans clustering  ######################################################

# The values for nstart and ncluster are as per the mentor's suggestions.
score <- kmeans(final_score_df$final_score, 5, nstart = 100)
summary(score)
summary(factor(score$cluster))
str(score)
final_score_df$cluster_id <- score$cluster

f = final_score_df %>% group_by(cluster_id) %>%
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))
f

# We notice that after arranging the avg_score column in descending order, the cluster_id values are different.
# we now need to reassign the cluster ratings according to the average rating
# The top most one should be as follows, 5, 4, 3, 2, 1
# We will adjust this by creating a new clusterid column and assign the values to it.

str(f)

final_score_df$newcluster_id <-
  if_else(
    final_score_df$cluster_id == f$cluster_id[1],
    5,
    if_else(
      final_score_df$cluster_id == f$cluster_id[2],
      4,
      if_else(
        final_score_df$cluster_id == f$cluster_id[3],
        3,
        if_else(final_score_df$cluster_id == f$cluster_id[4], 2, 1)
      )
    )
  )

final_score_df %>% group_by(newcluster_id) %>%
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))

# # A tibble: 5 x 2
# cluster_id avg_score
# <int>       <dbl>
#   5        0.0905
#   4        0.0287
#   3       -0.00972
#   2       -0.0558
#   1       -0.142

str(final_score_df)

final_score_df$newcluster_id <- as.factor(final_score_df$newcluster_id)
summary(final_score_df$newcluster_id)

# > summary(final_score_df$newcluster_id)
#   1   2   3   4   5
#  48 430 973 849 114

# We see that there are around 114 providers who have top 5 rating and as usual most of the providers are in the median 3 rating.
# Around 849 providers are in top 4 rating.

summary(final_score_df$final_score)

# Visualising the final_score accross all the ratings
head(final_score_df)

final_plot <- ggplot(final_score_df, aes(x=newcluster_id, y=final_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Final Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.2, 0.2, 0.05))
final_plot

# Let us plot each group_scores against the final_score_df
# Merging the group_scores with final_scores_df
all_scores <- merge(group_scores, final_score_df, by="Provider.ID")

# all_scores group by readmission
summary(all_scores$radm_score)

readm_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=radm_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Readmission Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-1.0, 1.0, 0.25))
readm_plot

# all_scores group by mortality
summary(all_scores$mort_score)

mort_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=mort_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Mortality Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.2, 0.3, 0.05))
mort_plot

# all_scores group by safety
summary(all_scores$safety_score)

safe_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=safety_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("safety scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.3, 0.2, 0.04))
safe_plot

# all_scores group by experience
summary(all_scores$expe_score)

expe_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=expe_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Experience Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.4, 0.25, 0.05))
expe_plot

# all_scores group by medical
summary(all_scores$medi_score)

medi_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=medi_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Medical Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.5, 0.2, 0.05))
medi_plot

# all_scores group by timeliness
summary(all_scores$time_score)

time_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=time_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Timeliness Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.9, 0.3, 0.08))
time_plot

# all_scores group by effectiveness
summary(all_scores$effe_score)

effe_plot <- ggplot(all_scores, aes(x=factor(as.character(newcluster_id)), y=effe_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("Effectiveness Scores") + theme_light() +
  scale_y_continuous(breaks=100*seq(-0.2, 0.1, 0.01))
effe_plot


grid_plot2 <- plot_grid(readm_plot, mort_plot, safe_plot, expe_plot,medi_plot, time_plot, effe_plot,
                    labels=c("Readmission", "Mortality", "Safety","Experience","Medical", "Timeliness", "Effectiveness"))
grid_plot2

# Now let us compare the median of all the groups scores with the ratings
median_group_scores <- all_scores[,-c(1,10)] %>% group_by(newcluster_id) %>%
  summarise_all(.funs=(median), na.rm=T)
median_group_scores


# A tibble: 5 x 8
# newcluster_id radm_score mort_score safety_score expe_score medi_score time_score effe_score final_score
# <fct>              <dbl>      <dbl>        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>       <dbl>
# 1                 -0.353    -0.0405      -0.052      -0.086     0.0155    -0.0695      0.011      -0.126
# 2                 -0.048    -0.046       -0.031      -0.063     0.006     -0.038       0.012      -0.053
# 3                 -0.006    -0.008        0.001      -0.021     0.02       0.0165      0.013      -0.009
# 4                  0.024     0.03         0.027       0.023     0.044      0.029       0.016       0.027
# 5                  0.174     0.066        0.0415      0.037     0.056      0.0365      0.019       0.077

                                                                                                                                                                #                                                                                                                                                                 >                                                                                                                                                                              >
# We can clearly see the rating increases as the measure scores increase.

mean_group_scores <- all_scores[,-c(1,10)] %>% group_by(newcluster_id) %>%
  summarise_all(.funs=(mean), na.rm=T)
mean_group_scores

# newcluster_id   radm_score mort_score safety_score expe_score medi_score time_score effe_score final_score
# <fct>              <dbl>      <dbl>        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>       <dbl>
#   1               -0.342     -0.0501      -0.0654     -0.103   -0.0380     -0.104     -0.00231    -0.142
#   2               -0.0711    -0.0455      -0.0393     -0.0689  -0.0244     -0.0621     0.00560    -0.0558
#   3               -0.00925   -0.00680     -0.00325    -0.0220  -0.000215   -0.00609    0.00622    -0.00972
#   4                0.0353     0.0352       0.0224      0.0222   0.0302      0.0186     0.0109      0.0287
#   5                0.220      0.0650       0.0300      0.0449   0.0432      0.0199     0.0170      0.0905

# We can clearly see the rating increases as the measure scores increase.

############################################################  Validation of Star Ratings #################################################################
# master_data_y is the hospital_ratings dataset we will use it here.

str(master_data_y)

# Let us convert the ratings column to factor
master_data_y$Hospital.overall.rating <- factor(master_data_y$Hospital.overall.rating)

# merging the final_score_df with the master_data_y using Provider.ID
final_score_df <- merge(final_score_df, master_data_y, by="Provider.ID")
str(final_score_df)
summary(final_score_df$newcluster_id)

# Accuracy is determined by comparing the overall ratings of CMS(general.csv) by creating the confusion matrix.
final = table(final_score_df$newcluster_id, final_score_df$Hospital.overall.rating)
final

conf_matrix4 <- confusionMatrix(final)
conf_matrix4

# Confusion Matrix and Statistics
#
#
# 1   2   3   4   5
# 1  22  22   2   2   0
# 2  77 253  93   7   0
# 3   2 243 618 109   1
# 4   0  19 377 422  31
# 5   0   1  14  73  26
#
# Overall Statistics
#
# Accuracy : 0.5555
# 95% CI : (0.5354, 0.5755)
# No Information Rate : 0.4573
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.3508
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                       Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity          0.217822   0.4703   0.5598   0.6884  0.44828
# Specificity          0.988759   0.9057   0.7290   0.7629  0.96265
# Pos Pred Value       0.458333   0.5884   0.6351   0.4971  0.22807
# Neg Pred Value       0.966610   0.8564   0.6627   0.8780  0.98609
# Prevalence           0.041839   0.2229   0.4573   0.2539  0.02403
# Detection Rate       0.009114   0.1048   0.2560   0.1748  0.01077
# Detection Prevalence 0.019884   0.1781   0.4031   0.3517  0.04722
# Balanced Accuracy    0.603290   0.6880   0.6444   0.7257  0.70546

# Accuracy : 55.55%

# We see the accuracy is between 50 to 60%, and the model has good scope as it is not overfitting the data.

#################################### End of Factor Analysis and Un-supervised Clustering ###############################################################

#################################### Provider Analysis for Evanston Hospital ###########################################################################
# Using the group_scores dataset
# all_scores

str(all_scores)

# Provider: Evanston Hospital Provider.ID = 140010
# Check the group scores for the provider.ID = 140010
provider_140010_gp_scores <- all_scores[ which(all_scores$Provider.ID == 140010), ]
provider_140010_gp_scores

#       Provider.ID radm_score mort_score safety_score expe_score medi_score time_score effe_score final_score cluster_id newcluster_id
# 637      140010      0.019      0.278       -0.022     -0.002     -0.013      0.068      0.015       0.063          3             5

str(master_data_y)
provider_140010_cms_rating = master_data_y[ which(master_data_y$Provider.ID == 140010),]
str(provider_140010_cms_rating)
as.numeric(provider_140010_cms_rating$Hospital.overall.rating)
# [1] 3
# The current rating of the provider is 3 in the hospital general info provided by CMS.

as.numeric(provider_140010_gp_scores$newcluster_id)
# [1] 5
# The rating as per the kmeans model is 5, which is a +2 error

value=provider_140010_gp_scores$final_score*100
value

# Let us compare the provider's final score with the overall scores.
summary(all_scores$final_score)

final_plot <- ggplot(all_scores, aes(x=newcluster_id, y=final_score*100,fill=newcluster_id)) +
  geom_boxplot() +
  xlab("Star Ratings") + ylab("Final Scores with the Provider Final Score") + theme_light() +
  geom_hline(aes(yintercept=value), color="blue") + geom_text(aes(0,value,label = value, vjust=-1,hjust= -1)) +
  scale_y_continuous(breaks=100*seq(-0.2, 0.2, 0.05))
final_plot

# let us see how the provider score looks in all the group measures.
f1 <- provider_140010_gp_scores$final_score * 100
v1 <- provider_140010_gp_scores$radm_score * 100
v2 <- provider_140010_gp_scores$mort_score * 100
v3 <- provider_140010_gp_scores$safety_score * 100
v4 <- provider_140010_gp_scores$expe_score * 100
v5 <- provider_140010_gp_scores$medi_score * 100
v6 <- provider_140010_gp_scores$time_score * 100
v7 <- provider_140010_gp_scores$effe_score * 100

f <- final_plot + geom_hline(yintercept=f1, col="black")
r <- readm_plot + geom_hline(yintercept=v1, col="maroon") + geom_text(aes(0,v1,label = v1, vjust=-1,hjust= -1))
m <- mort_plot + geom_hline(yintercept=v2, col="maroon") + geom_text(aes(0,v2,label = v2, vjust=1,hjust= -1))
s <- safe_plot + geom_hline(yintercept=v3, col="maroon") + geom_text(aes(0,v3,label = v3, vjust=-1,hjust= -1))
ex <- expe_plot + geom_hline(yintercept=v4, col="maroon") + geom_text(aes(0,v4,label = v4, vjust=-1,hjust= -1))
md <- medi_plot + geom_hline(yintercept=v5, col="maroon") + geom_text(aes(0,v5,label = v5, vjust=-1,hjust= -1))
t <- time_plot + geom_hline(yintercept=v6, col="maroon") + geom_text(aes(0,v6,label = v6, vjust=-1,hjust= -1))
ef <- effe_plot + geom_hline(yintercept=v7, col="maroon") + geom_text(aes(0,v7,label = v7, vjust=-1,hjust= -1))

# Let us plot all the above graphs with the provider's y intercept for each group's score.
grid_plot3 <- plot_grid(f,r,m,s,ex,md,t,ef,
                       labels=c('Final','Readmission','Mortality','Safety','Experience','Medical','Timeliness','Effectiveness'),
                       ncol = 2, nrow = 4)
grid_plot3

# On comparing the average scores of groups with the provider's score, we find that:
# We see that the final score of the provider is above final score of rating 4.
# 1. The final score is higher than the average score for rating=4 but lower than for rating=5
# 2. Readmissions score is between avg of ratings 3 and 4
# 3. Mortality score is better than average for rating=5
# 4. Safety score is between the avg score for rating = 2 and 1
# 5. Experience score is above the average of ratings 3 and below the average of 4
# 6. Medical score is very bad compared to all the average ratings scores.
# 7. Timeliness score is better than average for rating 5
# 8. Effectiveness score is better than average for rating for 4 and below the average of rating 5

# We can observe that the overall ratings accross Final, Readminssion, Mortality, Timeliness and effectiveness
# is very good compared.
# The areas of improvement are needed in Safety, Experience and Medical.
# Though medical has very less weightage, let us focus on drilling down Safety, Experience and Medical scores,
# which have 22% weightage.

# Now let us view how the median values of each group is looking with respect to the providers scores.
str(all_scores)
all_median_scores <- round(summarise_all(all_scores[,-c(1,10,11)], .funs=(median), na.rm=T),3)
all_median_scores
all_median_scores <- cbind(id = "median_score", all_median_scores)

new_prov <- provider_140010_gp_scores[,-c(1,10,11)]
new_prov <- cbind(id = "provider_score", new_prov)

all_median_scores<- rbind(all_median_scores, new_prov)
all_median_scores

#                 id  radm_score mort_score safety_score exp_score medi_score time_score effe_score final_score
# 1     median_score      0.002      0.004        0.007    -0.007      0.030      0.016      0.014       0.000
# 637 provider_score      0.019      0.278       -0.022    -0.002     -0.013      0.068      0.015       0.063
str(all_scores)

str(all_scores)
all_mean_scores <- round(summarise_all(all_scores[,-c(1,10,11)], .funs=(mean), na.rm=T),3)
all_mean_scores
all_mean_scores <- cbind(id = "mean_score", all_mean_scores)

new_prov <- provider_140010_gp_scores[,-c(1,10,11)]
new_prov <- cbind(id = "provider_score", new_prov)

all_mean_scores<- rbind(all_mean_scores, new_prov)
all_mean_scores


#   id            radm_score mort_score safety_score expe_score medi_score time_score effe_score final_score
# mean_score      0.000      0.004        0.000     -0.013      0.007     -0.008      0.008      -0.002
# provider_score  0.019      0.278       -0.022     -0.002     -0.013      0.068      0.015       0.063

# The safety scores are lower than the overall average score, experience score is a bit okay with respect
# to the overall average, but the medical scores are very low.

# let us start the drill down in this order- Safety, Experience and Medical
# Let us plot the safety scores graph and identify the concern points

## 1.Safety
#-----------#

# we will plot the provider score in the y intercept.
h1 <- provider_140010_gp_scores$safety_score * 100
safe_plot + geom_hline(yintercept=h1, col="maroon") + geom_text(aes(0,h1,label = h1, vjust=-1,hjust= -1))

safety_scores <- safe_master[,-c(2:8)]
summary(safety_scores)

# Replace the NA's with median values
median_na <- function(x){
  x[which(is.na(x))] <- median(x, na.rm=T)
  return(x)
}

safety_scores[, 2:ncol(safety_scores)] <- sapply(safety_scores[, -1], median_na)
summary(safety_scores)

# The safety scores for the provider
provider_safe_scores <- round(safety_scores[ which(safety_scores$Provider.ID == 140010), ],3)
provider_safe_scores

# Provider.ID COMP_HIP_KNEE_score HAI_1_SIR_score HAI_2_SIR_score HAI_3_SIR_score HAI_4_SIR_score HAI_5_SIR_score HAI_6_SIR_score PSI_90_SAFETY_score
# 140010              -0.462          -0.112          -0.214          -0.244           1.018           0.191          -0.291               -3.23

# Let us calculate the average scores and compare them
average_safety_scores <- round(summarise_all(safety_scores[,-1], .funs=(mean), na.rm=T),3)
average_safety_scores <- cbind(id = "mean_score", average_safety_scores)
average_safety_scores

#         id        COMP_HIP_KNEE_score HAI_1_SIR_score HAI_2_SIR_score HAI_3_SIR_score HAI_4_SIR_score HAI_5_SIR_score HAI_6_SIR_score PSI_90_SAFETY_score
# mean_score               0.033           0.077           0.055           0.095           0.166           0.117           0.012               0.029

prov_s <- round(safety_scores[ which(safety_scores$Provider.ID == 140010),],3)
prov_s <- cbind(id = "prov_s_score", prov_s[,-1])
prov_s

#           id    Provider.ID   COMP_HIP_KNEE_score HAI_1_SIR_score HAI_2_SIR_score HAI_3_SIR_score HAI_4_SIR_score HAI_5_SIR_score HAI_6_SIR_score PSI_90_SAFETY_score
# prov_s_score      140010              -0.462          -0.112          -0.214          -0.244           1.018           0.191          -0.291               -3.23

average_safety_scores <- rbind(average_safety_scores, prov_s)
average_safety_scores

#       id            COMP_HIP_KNEE_score HAI_1_SIR_score HAI_2_SIR_score HAI_3_SIR_score HAI_4_SIR_score HAI_5_SIR_score HAI_6_SIR_score PSI_90_SAFETY_score
# mean_score               0.033           0.077           0.055           0.095           0.166           0.117           0.012               0.029
# prov_s_score            -0.462          -0.112          -0.214          -0.244           1.018           0.191          -0.291              -3.230

# Except the HAI_4_SIR and HAI_5_SIR measures all the other measures are way below the average scores.

# Let us visualize how these low score measures are performing with respect to the ratings

# merging safety scores with all_scores
str(all_scores)
safety_scores <- merge(safety_scores, all_scores, by="Provider.ID")
str(safety_scores)

# Measure: COMP_HIP_KNEE_score
summary(safety_scores$COMP_HIP_KNEE_score)

s1 <- prov_s$COMP_HIP_KNEE_score * 100

CHK_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=COMP_HIP_KNEE_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("COMP_HIP_KNEE_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
safe_chk_p <- CHK_plot + geom_hline(yintercept=s1, col="black") + geom_text(aes(0,s1,label = s1, vjust=-1,hjust= -1))
safe_chk_p

# The provider's score for COMP_HIP_KNEE_score is way below the average scores for all the ratings.
# This is a cause of concern, the provider has to improve on this score to improve their ratings.

# Measure: HAI_1_SIR_score

summary(safety_scores$HAI_1_SIR_score)
s2 <- prov_s$HAI_1_SIR_score * 100

HAI1_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_1_SIR_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("HAI_1_SIR_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-7, 1, 0.5))
safe_hai1_p <- HAI1_plot + geom_hline(yintercept=s2, col="black") + geom_text(aes(0,s2,label = s2, vjust=-1,hjust= -1))
safe_hai1_p

# HAI_1_SIR score of the provider is lower than the average scores for ratings 3, 4, 5 and
# just below the average score for rating 2.

# Measure: HAI_2_SIR_score

summary(safety_scores$HAI_2_SIR_score)
s3 <- prov_s$HAI_2_SIR_score * 100

HAI2_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_2_SIR_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("HAI_2_SIR_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-5, 1, 0.5))
safe_hai2_p <- HAI2_plot + geom_hline(yintercept=s3, col="black") + geom_text(aes(0,s3,label = s3, vjust=-1,hjust= -1))
safe_hai2_p

# HAI_2_SIR score of the provider is lower than the average scores for ratings 3, 4, 5 and
# just below the average scores for ratings 1 & 2.

# Measure: HAI_3_SIR_score

summary(safety_scores$HAI_3_SIR_score)
s4 <- prov_s$HAI_3_SIR_score * 100

HAI3_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_3_SIR_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("HAI_3_SIR_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-5, 1, 0.5))
safe_hai3_p <- HAI3_plot + geom_hline(yintercept=s4, col="black") + geom_text(aes(0,s4,label = s4, vjust=-1,hjust= -1))
safe_hai3_p

# HAI_3_SIR score of the provider is way below the average scores for all the ratings.
# This is a cause of concern, the provider has to improve on this score to improve their ratings.

# Measure: HAI_6_SIR_score

summary(safety_scores$HAI_6_SIR_score)
s5 <- prov_s$HAI_6_SIR_score * 100

HAI6_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_6_SIR_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("HAI_6_SIR_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-5, 1, 0.5))
safe_hai6_p <- HAI6_plot + geom_hline(yintercept=s5, col="black") + geom_text(aes(0,s5,label = s5, vjust=-1,hjust= -1))
safe_hai6_p

# HAI_6_SIR score of the provider is below the average scores for all the ratings.
# This is a cause of concern, the provider has to improve on this score to improve their ratings.

# Measure: PSI_90_SAFETY_score

summary(safety_scores$PSI_90_SAFETY_score)
s6 <- prov_s$PSI_90_SAFETY_score * 100

PSI90_plot <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=PSI_90_SAFETY_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("PSI_90_SAFETY_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-5, 2, 0.75))
safe_psi90_p <- PSI90_plot + geom_hline(yintercept=s6, col="black") + geom_text(aes(0,s6,label = s6, vjust=-1,hjust= -1))
safe_psi90_p

# PSI_90_SAFETY score of the provider is way below the average scores for all the ratings.
# The provider is doing poorly in this measure.
# This is a cause of concern, the provider has to improve on this score to improve their ratings.

# Let us plot all the safety graphs together.
grid_plot4 <- plot_grid(safe_chk_p,safe_hai1_p,safe_hai2_p,safe_hai3_p,safe_hai6_p,safe_psi90_p,
                        labels=c('COMP_HIP_KNEE_score','HAI_1_SIR_score','HAI_2_SIR_score','HAI_3_SIR_score','HAI_6_SIR_score','PSI_90_SAFETY_score'),
                        ncol = 2, nrow = 3)
grid_plot4


## 2.Experience
#--------------#
# we will plot the provider score in the y intercept.

h2 <- provider_140010_gp_scores$expe_score * 100
expe_plot + geom_hline(yintercept=h2, col="maroon") + geom_text(aes(0,h2,label = h2, vjust=-1,hjust= -1))

expe_scores <- expe_master[,-c(2:8)]
summary(expe_scores)

# Replace the NA's with median values
expe_scores[, 2:ncol(expe_scores)] <- sapply(expe_scores[, -1], median_na)
summary(expe_scores)

# The experience scores for the provider
provider_expe_scores <- round(expe_scores[ which(expe_scores$Provider.ID == 140010), ],3)
provider_expe_scores

# Provider.ID H_CLEAN_LINEAR_SCORE H_COMP_1_LINEAR_SCORE H_COMP_2_LINEAR_SCORE H_COMP_3_LINEAR_SCORE H_COMP_4_LINEAR_SCORE H_COMP_5_LINEAR_SCORE H_COMP_6_LINEAR_SCORE
# 140010                0.183                -0.129                -0.368                 -0.29                 -0.22                -0.648                -0.815
# H_COMP_7_LINEAR_SCORE H_HSP_RATING_LINEAR_SCORE H_QUIET_LINEAR_SCORE H_RECMND_LINEAR_SCORE
# -0.183                     0.084               -0.205                  0.45

# Let us calculate the average scores and compare them
average_expe_scores <- round(summarise_all(expe_scores[,-1], .funs=(mean), na.rm=T),3)
average_expe_scores <- cbind(id = "mean_score", average_expe_scores)
average_expe_scores

#         id 			H_CLEAN_LINEAR_SCORE H_COMP_1_LINEAR_SCORE H_COMP_2_LINEAR_SCORE H_COMP_3_LINEAR_SCORE H_COMP_4_LINEAR_SCORE H_COMP_5_LINEAR_SCORE H_COMP_6_LINEAR_SCORE
# mean_score                -0.02                 0.073                 0.012                -0.017                 0.045                 0.014                 0.008
# H_COMP_7_LINEAR_SCORE H_HSP_RATING_LINEAR_SCORE H_QUIET_LINEAR_SCORE H_RECMND_LINEAR_SCORE
# 0.045                     0.024               -0.002                 0.061

prov_e <- round(expe_scores[ which(expe_scores$Provider.ID == 140010),],3)
prov_e <- cbind(id = "prov_e_score", prov_e[,-1])
prov_e

#           id 		H_CLEAN_LINEAR_SCORE H_COMP_1_LINEAR_SCORE H_COMP_2_LINEAR_SCORE H_COMP_3_LINEAR_SCORE H_COMP_4_LINEAR_SCORE H_COMP_5_LINEAR_SCORE H_COMP_6_LINEAR_SCORE
# prov_e_score                0.183                -0.129                -0.368                 -0.29                 -0.22                -0.648                -0.815
# H_COMP_7_LINEAR_SCORE H_HSP_RATING_LINEAR_SCORE H_QUIET_LINEAR_SCORE H_RECMND_LINEAR_SCORE
# -0.183                     0.084               -0.205                  0.45

average_expe_scores <- rbind(average_expe_scores, prov_e)
average_expe_scores

#         id 			H_CLEAN_LINEAR_SCORE H_COMP_1_LINEAR_SCORE H_COMP_2_LINEAR_SCORE H_COMP_3_LINEAR_SCORE H_COMP_4_LINEAR_SCORE H_COMP_5_LINEAR_SCORE H_COMP_6_LINEAR_SCORE
#   mean_score               -0.020                 0.073                 0.012                -0.017                 0.045                 0.014                 0.008
# prov_e_score                0.183                -0.129                -0.368                -0.290                -0.220                -0.648                -0.815
# H_COMP_7_LINEAR_SCORE H_HSP_RATING_LINEAR_SCORE H_QUIET_LINEAR_SCORE H_RECMND_LINEAR_SCORE
#    0.045                     0.024               -0.002                 0.061
#   -0.183                     0.084               -0.205                 0.450

# The experience measures are very important.
# We notice that the provider's scores are very low compared to the average scores of H_COMP_1_LINEAR_SCORE, H_COMP_2_LINEAR_SCORE, H_COMP_3_LINEAR_SCORE,
# H_COMP_4_LINEAR_SCORE, H_COMP_5_LINEAR_SCORE, H_COMP_6_LINEAR_SCORE, H_COMP_7_LINEAR_SCORE, H_QUIET_LINEAR_SCORE.
# The scores for H_CLEAN_LINEAR_SCORE and H_RECMND_LINEAR_SCORE are very good compared to the average scores.

# Let us visualize how these low score measures are performing with respect to the ratings

# merging expe scores with all_scores
str(all_scores)
expe_scores <- merge(expe_scores, all_scores, by="Provider.ID")
str(expe_scores)

# Measure: H_COMP_1_LINEAR_SCORE
summary(expe_scores$H_COMP_1_LINEAR_SCORE)

e1 <- prov_e$H_COMP_1_LINEAR_SCORE * 100

HC1L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_1_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_1_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc1l_p <- HC1L_plot + geom_hline(yintercept=e1, col="black") + geom_text(aes(0,e1,label = e1, vjust=-1,hjust= -1))
expe_hc1l_p

# The H_COMP_1_LINEAR_SCORE of the provider (-0.129) is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.

# Measure: H_COMP_2_LINEAR_SCORE
summary(expe_scores$H_COMP_2_LINEAR_SCORE)

e2 <- prov_e$H_COMP_2_LINEAR_SCORE * 100

HC2L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_2_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_2_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc2l_p <- HC2L_plot + geom_hline(yintercept=e2, col="black") + geom_text(aes(0,e2,label = e2, vjust=-1,hjust= -1))
expe_hc2l_p

# The H_COMP_2_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.

# Measure: H_COMP_3_LINEAR_SCORE
summary(expe_scores$H_COMP_3_LINEAR_SCORE)

e3 <- prov_e$H_COMP_3_LINEAR_SCORE * 100

HC3L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_3_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_3_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc3l_p <- HC3L_plot + geom_hline(yintercept=e3, col="black") + geom_text(aes(0,e3,label = e3, vjust=-1,hjust= -1))
expe_hc3l_p

# The H_COMP_3_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.

# Measure: H_COMP_4_LINEAR_SCORE
summary(expe_scores$H_COMP_4_LINEAR_SCORE)

e4 <- prov_e$H_COMP_4_LINEAR_SCORE * 100

HC4L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_4_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_4_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc4l_p <- HC4L_plot + geom_hline(yintercept=e4, col="black") + geom_text(aes(0,e4,label = e4, vjust=-1,hjust= -1))
expe_hc4l_p

# Measure: H_COMP_5_LINEAR_SCORE
summary(expe_scores$H_COMP_5_LINEAR_SCORE)

e5 <- prov_e$H_COMP_5_LINEAR_SCORE * 100

HC5L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_5_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_5_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc5l_p <- HC5L_plot + geom_hline(yintercept=e5, col="black") + geom_text(aes(0,e5,label = e5, vjust=-1,hjust= -1))
expe_hc5l_p

# The H_COMP_5_LINEAR_SCORE of the provider is above the ratings 1 and is equal to the avaerage score of rating 2 and lower than the average score of ratings 3, 4 & 5.

# Measure: H_COMP_6_LINEAR_SCORE
summary(expe_scores$H_COMP_6_LINEAR_SCORE)

e6 <- prov_e$H_COMP_6_LINEAR_SCORE * 100

HC6L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_6_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_6_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc6l_p <- HC6L_plot + geom_hline(yintercept=e6, col="black") + geom_text(aes(0,e6,label = e6, vjust=-1,hjust= -1))
expe_hc6l_p

# The H_COMP_6_LINEAR_SCORE of the provider is above the ratings 1 and lower than the average score of ratings 2, 3, 4 & 5.

# Measure: H_COMP_7_LINEAR_SCORE
summary(expe_scores$H_COMP_7_LINEAR_SCORE)

e7 <- prov_e$H_COMP_7_LINEAR_SCORE * 100

HC7L_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_COMP_7_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_COMP_7_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
expe_hc7l_p <- HC7L_plot + geom_hline(yintercept=e7, col="black") + geom_text(aes(0,e7,label = e7, vjust=-1,hjust= -1))
expe_hc7l_p

# The H_COMP_7_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.

# Measure: H_QUIET_LINEAR_SCORE
summary(expe_scores$H_QUIET_LINEAR_SCORE)

e8 <- prov_e$H_QUIET_LINEAR_SCORE * 100

HQL_plot <- ggplot(expe_scores, aes(x=factor(as.character(newcluster_id)), y=H_QUIET_LINEAR_SCORE*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("H_QUIET_LINEAR_SCORE") + theme_light() +
  scale_y_continuous(breaks=100*seq(-3, 2, 0.5))
expe_hql_p <- HQL_plot + geom_hline(yintercept=e8, col="black") + geom_text(aes(0,e8,label = e8, vjust=-1,hjust= -1))
expe_hql_p

# The H_QUIET_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.

grid_plot5 <- plot_grid(expe_hc1l_p,expe_hc2l_p,expe_hc3l_p,expe_hc4l_p,expe_hc5l_p,expe_hc6l_p,expe_hc7l_p,expe_hql_p,
                        labels=c('H_COMP_1_LINEAR_SCORE','H_COMP_2_LINEAR_SCORE','H_COMP_3_LINEAR_SCORE','H_COMP_4_LINEAR_SCORE',
                                 'H_COMP_5_LINEAR_SCORE','H_COMP_6_LINEAR_SCORE','H_COMP_7_LINEAR_SCORE','H_QUIET_LINEAR_SCORE'),
                        ncol = 3, nrow = 3)
grid_plot5

## 3.Medical
#-----------#
# we will plot the provider score in the y intercept.

h3 <- provider_140010_gp_scores$medi_score * 100
medi_plot + geom_hline(yintercept=h3, col="maroon") + geom_text(aes(0,h3,label = h3, vjust=-1,hjust= -1))

medi_scores <- medi_master[,-c(2:8)]
summary(medi_scores)

medi_scores[, 2:ncol(medi_scores)] <- sapply(medi_scores[, -1], median_na)
summary(medi_scores)

# The medical scores for the provider
provider_medi_scores <- round(medi_scores[ which(medi_scores$Provider.ID == 140010), ],3)
provider_medi_scores

# Provider.ID OP_10_score OP_11_score OP_13_score OP_14_score OP_8_score
#   140010       0.261         0.2      -0.454       0.315      0.285

# Let us calculate the average scores and compare them
average_medi_scores <- round(summarise_all(medi_scores[,-1], .funs=(mean), na.rm=T),3)
average_medi_scores <- cbind(id = "mean_score", average_medi_scores)
average_medi_scores

#         id    OP_10_score OP_11_score OP_13_score OP_14_score OP_8_score
# mean_score       0.079       0.119       0.023       0.081      0.067

prov_m <- round(medi_scores[ which(medi_scores$Provider.ID == 140010),],3)
prov_m <- cbind(id = "prov_m_score", prov_m[,-1])
prov_m

#           id  OP_10_score OP_11_score OP_13_score OP_14_score OP_8_score
# prov_m_score       0.261         0.2      -0.454       0.315      0.285

average_medi_scores <- rbind(average_medi_scores, prov_m)
average_medi_scores

#          id   OP_10_score OP_11_score OP_13_score OP_14_score OP_8_score
#   mean_score     0.079       0.119       0.023       0.081      0.067
# prov_m_score     0.261       0.200      -0.454       0.315      0.285

# The Medical group have only 4% weightage.
# We see that the provider's scores are very high compared to average scores of all the measures, except OP_13_score.
# OP_13_score has very low score compared to the average score.
# We will check how the scores for this column are.

# Let us visualize how these low score measures are performing with respect to the ratings

# merging medi scores with all_scores
str(all_scores)
medi_scores <- merge(medi_scores, all_scores, by="Provider.ID")
str(medi_scores)

# Measure: OP_13_score
summary(medi_scores$OP_13_score)

m1 <- prov_m$OP_13_score * 100

OP13_plot <- ggplot(medi_scores, aes(x=factor(as.character(newcluster_id)), y=OP_13_score*100, fill=newcluster_id)) + geom_boxplot() +
  xlab("Star Ratings") + ylab("OP_13_score") + theme_light() +
  scale_y_continuous(breaks=100*seq(-4, 2.5, 0.5))
medi_op13_p <- OP13_plot + geom_hline(yintercept=m1, col="black") + geom_text(aes(0,m1,label = m1, vjust=-1,hjust= -1))
medi_op13_p

# OP_13 measure value is very low compared to the average score accorss the ratings.
# If the provider fix's this issue the medical scores will drastically improve.

# Summary of provider analysis:
# The provider has low scores in the groups safety, patient-experience and medical.
# The safety score of provider is -2.2 (on the scale of this plot safe_plot)
# The experience score is -0.2 (on this scale expe_plot)
# The medical score is -1.3  (on this scale medi_plot)

## 1.Safety Observations:
#########################
# The provider's score for COMP_HIP_KNEE_score is way below the average scores for all the ratings.
# This is a cause of concern, the provider has to improve on this area to improve their ratings.
# Rate of complications for hip/knee replacement patients
# Provider needs to take measures to simplify the procedure involved in the hip/knee replacement operations.

# HAI_2_SIR score of the provider is lower than the average scores for ratings 3, 4, 5 and above the average scores for ratings 1 & 2.
# Catheter associated urinary tract infections (CAUTI) in ICUs and select wards
# Provider needs to take measures to protect the patients from contracting these infections.

# HAI_3_SIR score of the provider is way below the average scores for all the ratings.
# Surgical Site Infection from colon surgery (SSI: Colon)
# Provider needs to take precuations to avoid these infections which are very sensitive and might lead to other severe problems for the patients.
# Provider needs to take measures to protect the patients from contracting these infections.

# HAI_6_SIR score of the provider is way below the average scores for all the ratings.
# Clostridium difficile (C.diff.) Laboratory identified Events (Intestinal Infections)
# Provider needs to take measures to protect the patients from contracting these infections.

# PSI_90_SAFETY score of the provider is way below the average scores for all the ratings.
# The provider is doing poorly in this measure.
# Serious complications, provider needs to take effort to resolve these safety issues which the
# patients are contracting due to various procedures.

## 2.Experience Observations:
#############################
# The experience measures are very important.
# We notice that the provider's scores are very low compared to the average scores of H_COMP_1_LINEAR_SCORE, H_COMP_2_LINEAR_SCORE, H_COMP_3_LINEAR_SCORE,
# H_COMP_4_LINEAR_SCORE, H_COMP_5_LINEAR_SCORE, H_COMP_6_LINEAR_SCORE, H_COMP_7_LINEAR_SCORE, H_QUIET_LINEAR_SCORE.
# The scores for H_CLEAN_LINEAR_SCORE and H_RECMND_LINEAR_SCORE are very good compared to the average scores.

# The H_COMP_1_LINEAR_SCORE of the provider (-0.129) is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.
# Nurse communication linear mean score - provider needs to work on the communication issues regarding Nurses.

# The H_COMP_2_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.
# Doctor communication linear mean score - provider needs to work on the communication issues regarding Doctors.

# The H_COMP_3_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.
# Staff responsiveness linear mean score - provider needs to work on the communication issues regarding Doctors.

# The H_COMP_5_LINEAR_SCORE of the provider is above the ratings 1 and is equal to the avaerage score of rating 2 and lower than the average score of ratings 3, 4 & 5.
# Communication about medicines linear mean score - provider needs to ensure the information regarding the medicines properly and clearly.

# The H_COMP_6_LINEAR_SCORE of the provider is above the ratings 1 and lower than the average score of ratings 2, 3, 4 & 5.
# Discharge information linear mean score - provider needs to ensure the discharge information is provided clearly to the patients.

# The H_COMP_7_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.
# Care transition linear mean score

# The H_QUIET_LINEAR_SCORE of the provider is above the ratings 1 &2 and is equal to the avaerage score of rating 3 and lower than the average score of ratings 4 & 5.
# Quietness linear mean score - needs to improve on the noise management in the hospital areas.

# Overall, in the experience measures the provider has to improve on the communications of Nurses/Doctos/Staff/Medicines/Quietness and Care transition.

## 3.Medical Observations:
#########################
# The Medical group have only 4% weightage.
# We see that the provider's scores are very high compared to average scores of all the measures, except OP_13_score.
# OP_13_score has very low score compared to the average score.
# We will check how the scores for this column are.
# OP_13 measure value is very low compared to the average score accorss the ratings.
# Outpatients who got cardiac imaging stress tests before low risk outpatient surgery - provider needs to avoid these unnecessary tests and follow the protocol standards.
# If the provider fix's this issue the medical scores will drastically improve.


############################################## Code ENDS #########################################################
