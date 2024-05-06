## PreFer Data Challenge: Cleaning Health, Personality, and Race & Ethnicity Modules
## Sophia Magalona 
## May 4, 2024 
## Puts all recoded variables in healthrace dataframe   

## Import packages 
install.packages("data.table")
install.packages("tidyverse")
install.packages("naniar")
library("data.table")
library("tidyverse")
library("naniar")

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

##  -- recode and add value labels 
data <- data %>% 
  mutate(
    h_self20 = factor(data$ch20m004,
                      levels = c(1:5),
                      labels = c("poor", 
                                 "moderate", 
                                 "good", 
                                 "very good", 
                                 "excellent"))
  ) 

data <- data %>% 
  mutate( 
    h_allow20 = if_else(ch20m263 == 1, 0, ch20m263), 
    h_allow20 = if_else(ch20m263 > 1, 1, h_allow20), 
    h_allow20 = factor(h_allow20, levels = c(0,1), labels = c("no", "yes"))
  )

data <- data %>% 
  mutate(
    h_comphi20 = if_else(ch20m239 == 2, 0, ch20m239),
    h_comphi20 = factor(h_comphi20, levels = c(0,1), labels = c("no", "yes"))
  )

data <- data %>% 
  mutate(
    h_improv20 = factor(data$ch20m005,
                        levels = c(1:5),
                        labels = c("considerably poorer", 
                                   "somewhat poorer", 
                                   "the same", 
                                   "somewhat better",
                                   "considerably better"))
  ) 

##  -- apply same labels to daily/social/work hindrance
data <- data %>% 
  mutate(
    h_daily20 = ch20m020, 
    h_social20 = ch20m021, 
    h_work20 = ch20m022
  )

transform_function <- function(data, ...) { 
    data <- mutate_at(data, vars(...), ~factor(., 
                                         levels = c(1:5), 
                                         labels = c("not at all", 
                                                    "hardly", 
                                                    "a bit", 
                                                    "quite a lot", 
                                                    "very much")))
 
return(data)
} 

data <- transform_function(data, h_daily20, h_social20, h_work20)

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
    ),
    re_raised20 = factor(re_raised20,
                         levels = c(0:4), 
                         labels = c("none", "Catholic", "Prostestant", "Muslim", "other"))
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
    re_belong20 = factor(re_belong20,
                         levels = c(0:4), 
                         labels = c("none", "Catholic", "Prostestant", "Muslim", "other"))
  )

##  -- re_relig20 - extent describe self as religious person 
sum(!is.na(data$cr20m162)) ## missing 84 

data <- data %>% 
  mutate(
    re_relig20 = factor(cr20m162,
                        levels = c(1:4), 
                        labels = c("certainly religious", 
                                   "somewhat religious", 
                                   "barely religious", 
                                   "certainly not religious"))
    )

healthrace <- data %>%               
  select(nomem_encr, h_self20:re_relig20) 
vis_miss(healthrace)
healthrace %>% tbl_summary()
