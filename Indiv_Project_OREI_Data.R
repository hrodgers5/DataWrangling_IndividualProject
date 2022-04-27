#This R Script was written by Hannah Rodgers. 
#It is used to clean up and transform soil data to get ready for regression analysis.

library(MASS)
library(readxl)
library(writexl)
library(patchwork)
library(ggpubr)
library(ggbiplot)
library(tidyverse)

#### DATA IMPORT ####
setwd("C:/Users/hanna/Desktop/GRAD PROJECTS/OREI")

#read  files
OREI_soil <- read_excel("OREI_05.2021.xlsx", sheet = "Soil_Data")
OREI_treatments <- read_excel("OREI_05.2021.xlsx", sheet = "Treatment_Data_2021")
OREI_enzymes <- read_excel("OREI_05.2021.xlsx", sheet = "Enzymes")
OREI_PLFA <- read_excel("OREI_05.2021.xlsx", sheet = "PLFAs")

#units 'cheat sheet'
units <- t(read_excel("OREI_05.2021.xlsx", sheet = "Units"))

#### DATA CLEANUP ####
#merge the sheets
OREI_all <- OREI_treatments %>% 
  left_join(OREI_soil, by = 'Sample_ID') %>% 
  left_join(OREI_enzymes, by = 'Sample_ID') %>% 
  left_join(OREI_PLFA, by = 'Sample_ID')

OREI_all <- OREI_all %>% 
#remove fallow phase and inorganic fertilized plots
  filter(Rotation == "wheat", Treatment != "fertilizer") %>% 
#remove unwanted variables
 dplyr::select(-PER1, -ID, -InorganicC, -Treatment, -Compost_Rate, 
         -Crop, -Rotation, -H2O, -BulkDensity) %>% 
#save compost year as a factor
  mutate(Compost_Year = as.factor(Compost_Year))

#separate into the two groups
OREI_2016 <- subset(OREI_all, Compost_Year != "2020")
OREI_2020 <- subset(OREI_all, Compost_Year != "2016")

# CHECK REGRESSION ASSUMPTIONS #
####1. REMOVE OUTLIERS ####

#check each variable for outliers, and remove if <4
source("http://goo.gl/UUyEzD")
outlierKD(OREI_2020, total_bacteria)
no
yes

#removed outliers from 2016: yield, NO3, PMN, DON, MBC, CBH, PHOS, NAG, actino,  gram_pos, AMF, sapro_fungi, total_MB, total_fungi

#removed outliers from 2020: NO3, protein, MBC, MBN, SOC, N, NAG, BX, AG, SUL, 

#save no_outlier file to disk, then read it back in for next session
write_xlsx(OREI_2020, "OREI_2020_no_outliers.xlsx")
write_xlsx(OREI_2016, "OREI_2016_no_outliers.xlsx")

OREI_2020 <- as.data.frame(read_excel("OREI_2020_no_outliers.xlsx"))
OREI_2016 <- as.data.frame(read_excel("OREI_2016_no_outliers.xlsx"))

####2. CHECK FOR NORMALITY OF RESIDUALS ####

#this loop runs a linear model on compost ~ variable for each variable
#it checks normality of the residuals and saves the p.value

p.vals <- list()

for (i in names(OREI_2016[,7:38])) {
  mod <- lm(get(i) ~ compost, data = OREI_2016)
  p.vals[[i]] <- (shapiro.test(mod$residuals))$p.value
}

#OREI_2020 with non-normal (p<0.05): DOC, DON, PHOS, 
#OREI_2016: only PHOS

#to fix this, log transform non-normal ones using boxcox

#this function find the optimal box_cox transformation and returns the transformed data

box_cox_transform <- function(v) {
#calculates the boxcox plot and pulls out lambda
  bc <- boxcox(v ~ compost, data = OREI_2016)
  lambda <- bc$x[which.max(bc$y)]
#transforms the data using lambda and saves it
  v <- (v^lambda-1)/lambda
  return(v)
}

#run the function on all non-normal variables
OREI_2016$PHOS <- box_cox_transform(OREI_2016$PHOS)

#test normality of new data
p.vals.trans <- list()

for (i in names(OREI_2016[,7:38])) {
  mod <- lm(get(i) ~ compost, data = OREI_2016)
  p.vals.trans[[i]] <- (shapiro.test(mod$residuals))$p.value
}

#everything is normal now! Save again:
write_xlsx(OREI_2020, "OREI_2020_normal.xlsx")
write_xlsx(OREI_2016, "OREI_2016_normal.xlsx")

OREI_2020 <- as.data.frame(read_excel("OREI_2020_normal.xlsx"))
OREI_2016 <- as.data.frame(read_excel("OREI_2016_normal.xlsx"))

####3. F-test for lack of fit (checks linearity) ####

#this function runs a linear and quadratic model for each variable, and tests whether they differ significantly
F_test <- function(x) {
  mod <- lm(x ~ compost, data = OREI_2016)
  reduced<-lm(x ~ compost, data = OREI_2016)
  full<-lm(x ~ poly(compost,2), data = OREI_2016)
  return(anova(reduced, full)$"Pr(>F)")
}

#use sapply to run this function on each

p.vals.F.test <- list()

for (i in colnames(OREI_2016[,7:38])) {
  p.vals.F.test[[i]] <- F_test(OREI_2016[[i]])
}

#differs: 2020 DOC, protein, porosity, WFPS
#2016: all good!

#####4. check constancy of residuals (levene test? brown forsythe?) look at Liana's code ####
library(car)
leveneTest(mod, compost)

#### VISUALIZE DATA ####
p1 <- ggplot(data= OREI_normal, aes(x = compost, y = DOC))+
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_regline_equation() +
  labs (title = 'One Year After Compost Application', x = "Compost Rate (Mg/ha)", y = "DOC (mg/kg)")

p2 <- ggplot(data= OREI_normal, aes(compost, DOC)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_regline_equation() +
  labs (title = 'Five Years After Compost Application', x = "Compost Rate (Mg/ha)", y = "DOC (mg/kg)")

p1 + p2 #& scale_y_continuous(limits = c(0, 375)) 
