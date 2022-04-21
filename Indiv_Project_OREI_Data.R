#This R Script was written by Hannah Rodgers. It is used to clean up soil data to get ready for regression analysis.

library(readxl)
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
         -Crop, -Rotation, -H20, -BulkDensity) %>% 
  mutate(Compost_Year = as.factor(Compost_Year))

#### CHECK THE REGRESSION ASSUMPTIONS ####

#REMOVE OUTLIERS AND SAVE AS NEW DATASETS
OREI_no_outliers <- OREI_all

source("http://goo.gl/UUyEzD")
outlierKD(OREI_no_outliers, WFPS)

#outliers removed: Yield 0, Tillers 0, Heads 0, WFPS, NO3, PMN, Porosity, Protein, POXC, DOC, DON, PMC, MBC_fumigated, MBN_fumigated, SOC, N, BG, CBH, PHOS, NAG, BX, AG, SUL, LAP, actinomycetes, gram_neg, gram_pos, AMF, sapro_fungi, total_MB, total_fungi, total_bacteria

#save these no outlier files to disk
write_xlsx(OREI_no_outliers, "OREI_2021_no_outliers.xlsx")

plot(y= OREI_2020$Porosity, x =OREI_2020$compost)

#TASK 2: CHECK REGRESSION ASSUMPTIONS, AND TRANSFORM DATA TO BE NORMAL

#### WORKING WITH LIANA
#box cox
library(MASS)

mod <- lm((DOC)^-0.3 ~ compost, data = OREI_2020)

#test assumptions
shapiro.test(mod$residuals)
boxcox(mod)

#F-test for lack of fit

F_test <- function(x) {
  mod <- lm(x ~ compost, data = OREI_2020)
  reduced<-lm(x ~ compost, data = OREI_2020)
  full<-lm(x ~ poly(compost,2), data = OREI_2020)
  anova(reduced, full)
}

F_test(OREI_2020$total_fungi)