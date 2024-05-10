## PreFer Data Challenge: Cleaning Health, Personality, and Race & Ethnicity Modules
## Sophia Magalona 
## May 4, 2024 
## Puts all recoded variables in healthrace dataframe   

## Import packages 
install.packages("data.table")
install.packages("tidyverse")
install.packages("naniar")
install.packages("mice")
library("data.table")
library("tidyverse")
library("naniar")
library("mice")

## Load all packages to library and adjust options
pkgs <- c("data.table", "tidyverse", "naniar", "mice")
lapply(pkgs, library, character.only = TRUE)

## Set the working directory
setwd("~/Library/CloudStorage/OneDrive-JohnsHopkins/PhD program/Projects/PreFer Data Challenge")

## Read in the dataset: 
##  -- read in variable names 
##  -- use dput to put var names in ", format 
##  -- copy dput output to fread function 
## myvars = read.table("SM_vars.csv",sep=",",header=T)
## dput(names(myvars))
data_orig = fread("PreFer_train_data.csv")

# load the codebooks 
codebook   <- readr::read_csv("PreFer_codebook.csv", show_col_types = FALSE) # codebook (time-varying)
codebook_s <- readr::read_csv("PreFer_codebook_summary.csv", show_col_types = FALSE) # summary codebook (time-invariant)

## Filter dataset: 
##  -- make copy of orig dataframe
##  -- keep only those with outcome data (n=987) 
data <- data_orig %>% 
  subset(outcome_available == 1) 

testInheritedMethods()

## Create Health variables: 
##  -- h_self20 - rating of general health for 2020
##  -- h_comphi20 - has complementary health insurance 
##  -- h_allow20 - applied for health care allowance 
##  -- h_improv20 - health poorer or better, compared to last year
##  -- also checked for major illness/health probs (e.g. cancer) but very low prevalence
##  -- others to consider physical health/emotional problems hinder daily/social/work 
health <- data %>% 
  select(nomem_encr, ch20m004, ch20m239, ch20m263, ch20m089, ch20m005, ch20m020, ch20m021, ch20m022) 
health %>% tbl_summary()

##  -- rename and recode 
data <- data %>% 
  mutate(
    h_self20 = ch20m004, 
    h_allow20 = if_else(ch20m263 == 1, 0, ch20m263), 
    h_allow20 = if_else(ch20m263 > 1, 1, h_allow20), 
    h_comphi20 = if_else(ch20m239 == 2, 0, ch20m239),
    h_improv20 = ch20m005,
    h_daily20 = ch20m020, 
    h_social20 = ch20m021, 
    h_work20 = ch20m022
  ) 

## Create Personality variables: 
##  -- no priority vars for personality

## Create Religion and Ethnicity variables: 
##  -- re_raised20 - raised within a certain faith 
##  -- re_belong20 - belong to a certain faith 
##  -- check missing vars if available in previous years
religion <- data %>% 
  select(nomem_encr, cr20m134, cr20m135, cr20m143, cr20m144) 
health %>% tbl_summary()

##  -- recode/add value labels to vars
##  -- re_raised20 - raised within a certain faith 
data <- data %>% 
  mutate(
    re_raised20 = case_when(
      cr20m135 == 1 ~ 1, 
      !is.na(cr20m135) & cr20m135 >= 2 & cr20m135 <= 11 ~ 2, 
      cr20m135 == 12 ~ 3, 
      cr20m135 > 12 ~ 4, 
      cr20m134 == 2 ~ 0, 
      is.na(cr20m134) ~ NA, 
      is.na(cr20m135) & cr20m134 == 1 ~ NA, 
      TRUE ~ 0 
    )
  )

##  -- re_belong20 - belong to a certain faith
data <- data %>% 
  mutate(
    re_belong20 = case_when(
      cr20m144 == 1 ~ 1, 
      !is.na(cr20m144) & cr20m144 >= 2 & cr20m144 <= 9 ~ 2, 
      cr20m144 == 10 ~ 3, 
      cr20m144 > 10 ~ 4, 
      cr20m143 == 2 ~ 0, 
      is.na(cr20m143) ~ NA, 
      is.na(cr20m144) & cr20m143 == 1 ~ NA, 
      TRUE ~ 0 
    ),
  )

##  -- re_relig20 - extent describe self as religious person 
sum(!is.na(data$cr20m162)) ## missing 84 

data <- data %>% 
  mutate(re_relig20 = cr20m162)

## Select all vars to use from health and religion sections
healthrace <- data %>%               
  select(nomem_encr, h_self20:re_relig20) 
vis_miss(healthrace)
healthrace %>% tbl_summary()

