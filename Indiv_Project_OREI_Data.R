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

#merge the sheets
OREI_all <- merge(OREI_soil, OREI_treatments, OREI_enzymes, OREI_PLFA, by = Sample_ID)

#use this sheet to check on units
units <- t(read_excel("OREI_05.2021.xlsx", sheet = "Units"))

#change characters to factor
OREI_treatments <- OREI_treatments %>% 
  mutate(Compost_Rate = as.factor(Compost_Rate)) %>% 
  mutate(Compost_Year = as.factor(Compost_Year))

#### CHECK THE REGRESSION ASSUMPTIONS ####

#REMOVE OUTLIERS AND SAVE AS NEW DATASETS
OREI_soil_no <- OREI_soil
OREI_enzymes_no <- OREI_enzymes
OREI_PLFA_no <- OREI_PLFA

source("http://goo.gl/UUyEzD")
outlierKD(OREI_enzymes_no, BX)

#outliers removed: 3 from BG, 2 CBH, 1 PHOS, 4 NAG, BX, AG, SUL, LAP, PER1

#save these no outlier files to disk
write_xlsx(OREI_enzymes_no, "no_outliers/OREI_enzymes_no_outliers.xlsx")
write_xlsx(OREI_PLFA_no, "no_outliers/OREI_PLFA_no_outliers.xlsx")
write_xlsx(OREI_soil_no, "no_outliers/OREI_soil_no_outliers.xlsx")

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