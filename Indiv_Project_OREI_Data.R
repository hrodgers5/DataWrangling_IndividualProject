#This R Script was written by Hannah Rodgers. It is used to clean up soil data to get ready for regression analysis.

library(readxl)
library(writexl)
library(patchwork)
library(ggpubr)
library(ggbiplot)
library(tidyverse)

####DATA CLEANUP####
#read  files
setwd("C:/Users/hanna/Desktop/GRAD PROJECTS/OREI")

OREI_soil <- read_excel("OREI_05.2021.xlsx", sheet = "Soil_Data")
OREI_treatments <- read_excel("OREI_05.2021.xlsx", sheet = "Treatment_Data_2021")
OREI_enzymes <- read_excel("OREI_05.2021.xlsx", sheet = "Enzymes")
OREI_PLFA <- read_excel("OREI_05.2021.xlsx", sheet = "PLFAs")

#use this sheet to check on units
units <- t(read_excel("OREI_05.2021.xlsx", sheet = "Units"))

#merge the sheets
OREI_all <- OREI_treatments %>% 
  left_join(OREI_soil, by = 'Sample_ID') %>% 
  left_join(OREI_enzymes, by = 'Sample_ID') %>% 
  left_join(OREI_PLFA, by = 'Sample_ID')

#remove some unwanted columns, then filter to just keep the wheat phase and no IF
OREI_all <- OREI_all %>% 
  filter(Rotation == "wheat", Treatment != "fertilizer") %>% 
  select(-PER1, -ID, -InorganicC, -Treatment, -Compost_Rate, 
         -Crop, -Rotation, -H2O, -BulkDensity) %>% 
  mutate(Compost_Year = as.factor(Compost_Year))

#### CHECK THE REGRESSION ASSUMPTIONS ####

#REMOVE OUTLIERS AND SAVE AS NEW DATASETS
#I removed outliers if there were <4 outliers
OREI_no_outliers <- OREI_all

source("http://goo.gl/UUyEzD")
outlierKD(OREI_no_outliers, DON)

#DATA WITH NO OUTLIERS: Yield, Tillers, Heads, WFPS, Porosity, POXC, BG, LAP, gram_neg, sapro_fungi

#DATA WITH >3 OUTLIERS, SO DIDN'T REMOVE: NO3 (5), DOC (4), DON (9), BX (4)

#DATA WITH OUTLIERS REMOVED: PMN (1), Protein (2), PMC (1), MBC_fumigated (3), MBN_fumigated (2), SOC (1), N (2), CBH (1), PHOS (1), NAG 2, AG (1), SUL (2),  actinomycetes (1), gram_pos (1), AMF (1), total_MB (1), total_fungi (1), total_bacteria (1)

#save this no outlier file to disk
write_xlsx(OREI_no_outliers, "OREI_2021_no_outliers.xlsx")

#TASK 2: CHECK REGRESSION ASSUMPTIONS, AND TRANSFORM DATA TO BE NORMAL

#### Check regression assumptions. Transform data with non-normal residuals using boxcox, then save that in new dataframe.

OREI_normal <- OREI_no_outliers
#box cox
library(MASS)

mod <- lm(total_bacteria ~ compost, data = OREI_no_outliers)

#test assumptions
shapiro.test(mod$residuals)

plot(mod$residuals)
boxcox(mod)

#normal residuals: Yield, Tillers, Heads, WFPS, Porosity, Protein, POXC, PMC, MBC_fum, MBN_fum, SOC, N, BG, CBH, AG, SUL, LAP, actinomycetes, gram_pos, AMF, sapro_fungi, total_MB, total_fungi, total_bacteria

#non-normal residuals: NO3, PMN, DOC, DON, PHOS, NAG, BX, gram_neg

#F-test for lack of fit (check linearity)

F_test <- function(x) {
  mod <- lm(x ~ compost, data = OREI_2020)
  reduced<-lm(x ~ compost, data = OREI_2020)
  full<-lm(x ~ poly(compost,2), data = OREI_2020)
  anova(reduced, full)
}

F_test(OREI_2020$total_fungi)


#next, check constancy of residuals (levene test? brown forsythe?) look at Liana's code
library(car)
leveneTest(mod, compost)
