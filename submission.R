# This is an example script to generate the outcome variable given the input dataset.
# 
# This script should be modified to prepare your own submission that predicts 
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
# 
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
# 
# clean_df should be used to clean (preprocess) the data.
# 
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(gtsummary)
library(data.table)
library(tidyverse)
library(dplyr)
library(purrr)
library(here)
library("naniar")
library("mice")
library(randomForest)



clean_df <- function(df, background_df = NULL){
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline) leave only the "return df" command

  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): Optional input dataframe containing background data (e.g., from PreFer_train_background_data.csv or PreFer_fake_background_data.csv).

  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.
  
  df <- df %>%
          filter(outcome_available ==1)

  
    background <- df %>% 
      select(nomem_encr, birthyear_bg:woonvorm_2020)  %>%      # background variables 
      mutate(migration = factor(migration_background_bg,  # starting from fixed background variables 
                                levels = c(0, 101, 202, 102, 201), 
                                labels = c("Dutch background", 
                                           "first generation foreign, western background",
                                           "First generation foreign, non-western background",
                                           "Second generation foreign, Western background",
                                           "Second generation foreign, non-western background")), 
             age2020 = as.numeric(age_bg),  # respondent's age in Dec, 2020
             sex = factor(ifelse(gender_bg == 1, "male", "female")), 
             marital_status2020 = factor(burgstat_2020, 
                                         levels = c(1:5), 
                                         labels = c("married", 
                                                    "separated",
                                                    "divorced",
                                                    "widowed", 
                                                    "never-married")), 
             domestic_situation2020 = factor(woonvorm_2020, 
                                             levels = c(1:5), 
                                             labels = c("single", 
                                                        "(un)married cohabitation, without child(ren)",
                                                        "(Un)married co-habitation, with child(ren)",
                                                        "single with child(ren)",
                                                        "other")), 
             dwell_type2020 = factor(woning_2020, 
                                     levels = c(1:4), 
                                     labels = c("Self-owned dwelling", 
                                                "Rental dwelling", 
                                                "Sub-rented dwelling", 
                                                "Cost-free dwelling")),
             urban2020 = factor(sted_2020, 
                                levels = c(1:5), 
                                labels = c("Extremely urban",
                                           "Very urban", 
                                           "Moderately urban",
                                           "Slightly urban",
                                           "Not urban")), 
             occupation = factor(belbezig_2020, 
                                 levels = c(1:14), 
                                 labels = c("Paid employment",
                                            "Works or assists in family business", 
                                            "Autonomous professional, freelancer, or self-employed",
                                            "Job seeker following job loss", 
                                            "First-time job seeker",
                                            "Exempted from job seeking following job loss",
                                            "Exempted from job seeking following job loss", 
                                            "Takes care of the housekeeping",
                                            "Is pensioner ([voluntary] early retirement, old age pension scheme)",
                                            "Has (partial) work disability",
                                            "Performs unpaid work while retaining unemployment benefit",
                                            "Performs voluntary work", 
                                            "Does something else", 
                                            "Is too young to have an occupation")),
             education_cbs = factor(oplcat_2020, 
                                    levels = c(1:6),
                                    labels = c("primary school",
                                               "vmbo (intermediate secondary education, US: junior high school)",
                                               "havo/vwo (higher secondary education/preparatory university education, US: senior high school)",
                                               "mbo (intermediate vocational education, US: junior college)",
                                               "hbo (higher vocational education, US: college)", 
                                               "wo (university)" )), 
             livep = factor(partner_2020, 
                            levels = c(0, 1), 
                            labels = c("no", "yes")), 
             # continuous variables
             pinc2020 = as.numeric(brutoink_2020),
             hinc2020 = brutohh_f_2020)
    
    
    # Select only constructed variables
    background <- background %>% 
      select(nomem_encr, migration, age2020, sex, 
             marital_status2020, domestic_situation2020, dwell_type2020, 
             urban2020, occupation, education_cbs, 
             livep, pinc2020, hinc2020) 
    
    
    #######################################################
    # FAMILY & HOUSEHOLD
    fert <- df %>%
      mutate(family_rel =  factor(cf20m526, # how would you describe the relationship with your family 
                                  levels = c(1:5), 
                                  labels = c("very poor", "poor", "not poor", "good", "very good")), 
             child_dead =  factor(cf20m471,  # did you eve have a child(ren) passed away after being born)                 
                                  levels = c(1,2), 
                                  labels = c("yes", "no")), 
             child_ever =  factor(cf20m454, # did you ever have any children)
                                  levels = c(1,2), 
                                  labels = c("yes", "no")), 
             child_num = cf20m455)   %>%  # the number of children one has
      select(nomem_encr, family_rel, child_dead, child_ever, child_num) # only 4 new variables about fertility are added 
    
    ## FAMILY     
    family <- df %>% 
      mutate(more_children = case_when(  # want more children 
        cf20m130 < 3 & cf20m128 == 1 ~ 1, # 1) more soon - want more in the next two years (cf20m130<3, cf20m128==1)
        cf20m130 >= 3 & cf20m128 == 1 ~ 2, # 2) more later - want more in more than 2 years (cf20m130>=3, cf20m128==1)
        cf20m128 == 2 ~ 0, # 3) no more - cf20m128 == 2 
        cf20m128 == 3 ~ 3, # 4) don't know - cf20m128 == 3 
        is.na(cf20m128) ~ NA), 
        more_children = factor(more_children, 
                               levels = c(0:3), 
                               labels = c("more soon", "more later", "no more", "DK")),
        livepartner = case_when(
          cf20m025 == 1 ~ 1, # has partner and lives with 
          cf20m025 == 2 & cf20m024 == 1 ~ 2, # has partner but does not live with
          cf20m024 == 2 ~ 0, # no partner 
          is.na(cf20m024) ~ NA),
        livepartner = factor(livepartner, 
                             levels = c(0:2), 
                             labels = c("no partner", "live with partner", "does not live with partner")), 
        partnerage = 2020-cf20m026, # partner age (birth year of partner) 
        partnercountry = cf20m027, # birth country of partner 
        partnercountry = factor(partnercountry, 
                                levels = c(1:8), 
                                labels = c("Netherlands", "Turkey", "Morocco", "Netherlands Antilles", "Surinam", 
                                           "Indonesia", "other non-western country", "other western country")), 
        partnership = 2020-cf20m028, # how long together (year partnership began) 
        partnerlive = 2020-cf20m029, # how long living together (year began living with partner)
        married = case_when( # married to partner 
          cf20m030 == 1 ~ 1, 
          cf20m030 == 2 | is.na(cf20m030) ~ 2),
        marriedwhen = 2020-cf20m031, # how long married (year married to partner) 
        partnergender = case_when( # partner gender
          cf20m032 == 1 ~ 1, 
          cf20m032 == 2 ~ 2, 
          is.na(cf20m032) ~ 0), 
        partnergender = factor(partnergender, 
                               levels = c(0:2), 
                               labels = c("no partner", "male", "female")), 
        # next is only among those with partners (cf20m024==1)
        relationsatisfied = cf20m180, # how satisfied with relationship 0=entirely dissatisfied, 10 = entirely satisfied 
        # can't figure out skip pattern for next one 
        famlifesatisfied = cf20m181, # how satisfied with family life 0=entirely dissatisfied, 10 = entirely satisfied 
        # next only among those with partners (cf20m024==1)
        diffopinion1 = cf20m183, # diff opinion with partner re: raising children 
        diffopinion2 = cf20m184, # diff opinion with partner re: housework
        diffopinion3 = cf20m185, # diff opinion with partner re: spending leisure time
        diffopinion4 = cf20m186 # diff opinion with partner re: working (too much)
      ) %>% 
      select(nomem_encr, more_children:diffopinion4)
    
    # add value labels to yes/no 
    yesno_function <- function(data, ...) { 
      columns <- enquos(...) 
      for (col in columns) { 
        data <- mutate(data, !!col := factor(!!col, 
                                             levels = c(1:2), 
                                             labels = c("yes", "no")))
      }
      return(data)
    } 
    
    family <- yesno_function(family, married)
    
    # add value labels to differences in opinion 
    diffop_function <- function(data, ...) { 
      columns <- enquos(...) 
      for (col in columns) { 
        data <- mutate(data, !!col := factor(!!col, 
                                             levels = c(1:4), 
                                             labels = c("practically never", "occasionally", "often", "not applicable")))
      }
      return(data)
    } 
    
    family <- diffop_function(family, diffopinion1, diffopinion2, diffopinion3, diffopinion4)
    
    
    #######################################################
    # HEALTH
    
    ##  -- h_self20 - rating of general health for 2020
    ##  -- h_comphi20 - has complementary health insurance 
    ##  -- h_allow20 - applied for health care allowance 
    ##  -- h_improv20 - health poorer or better, compared to last year
    ##  -- also checked for major illness/health probs (e.g. cancer) but very low prevalence
    ##  -- others to consider physical health/emotional problems hinder daily/social/work 
    health <- df %>% 
      select(nomem_encr, ch20m004, ch20m239, ch20m263, ch20m089, ch20m005, ch20m020, ch20m021, ch20m022) 
    
    ##  -- rename and recode 
    health <- health %>% 
      mutate(
        h_self20 = ch20m004, 
        h_allow20 = if_else(ch20m263 == 1, 0, ch20m263), 
        h_allow20 = if_else(ch20m263 > 1, 1, h_allow20), 
        h_comphi20 = if_else(ch20m239 == 2, 0, ch20m239),
        h_improv20 = as.numeric(ch20m005),
        h_daily20 = as.numeric(ch20m020), 
        h_social20 = as.numeric(ch20m021), 
        h_work20 = as.numeric(ch20m022)
      )  %>%
      select(nomem_encr, h_self20, h_allow20, h_comphi20, h_improv20, h_daily20, h_social20, h_work20) # only  new variables about health are added 
    
    
    #######################################################
    # PERSONALITY, RELIGION, ETHNICITY
    
    ##  -- no priority vars for personality
    
    ## Create Religion and Ethnicity variables: 
    ##  -- re_raised20 - raised within a certain faith 
    ##  -- re_belong20 - belong to a certain faith 
    ##  -- check missing vars if available in previous years
    religion <- df %>% 
      select(nomem_encr, cr20m134, cr20m135, cr20m143, cr20m144, cr20m162) 
    
    ##  -- recode/add value labels to vars
    ##  -- re_raised20 - raised within a certain faith 
    religion <- religion %>% 
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
    religion <- religion %>% 
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
    
    
    religion <- religion %>% 
      mutate(re_relig20 = as.numeric(cr20m162))  %>%
      select(nomem_encr, re_raised20, re_belong20, re_relig20) # only  new variables about religion are added 
    
    #######################################################
    # ECONOMIC SITUATION (ASSETS, HOUSING, INCOME)
    
    
    # Select variables from a list generated in excel
    econsit_priority <- subset(df, select=c("nomem_encr", 
                                            "ca20g002", "ca20g012", "ca20g063", 
                                            "cd20m008", "cd20m034", "cd20m041", "cd20m043", "cd20m044", "cd20m045", "cd20m046", 
                                            "cd20m047", "cd20m052", "cd20m091", "cd20m092", 
                                            "ci20m005", "ci20m006", "ci20m007", 
                                            "ci20m140", "ci20m143", "ci20m165", 
                                            "ci20m206", "ci20m208", "ci20m210", 
                                            "ci20m236", "ci20m237", "ci20m238", "ci20m239", "ci20m241", "ci20m242", "ci20m245", 
                                            "ci20m246", "ci20m247", "ci20m248", "ci20m249", "ci20m250", "ci20m252", "ci20m253", "ci20m255", 
                                            "ci20m257", "ci20m258", "ci20m261", "ci20m301", "ci20m303", "ci20m330", 
                                            "ci20m355", "ci20m356", 
                                            "ci20m377", "ci20m378", "ci20m379", "ci20m380"))
    
    econsit_priority <- econsit_priority %>%
      mutate(resp_finmatters20 = factor(ca20g002, levels = c(0,1)),
             balance20 = ca20g012,
             loans20 = ca20g063, # could make negative since it's loans?
             minimum_desiredinc20 = as.numeric(ci20m236), # These variables give subjective reference points for respondent's income
             exp_vs_inc20 = factor(ci20m253, 
                                   levels = c(1:3), 
                                   labels = c("expenditure was higher than the income", 
                                              "expenditure was approximately equal to the income", 
                                              "expenditure was lower than the income")), 
             exp_vs_inc20_nobigexp = factor(ci20m255, 
                                            levels = c(1:3), 
                                            labels = c("expenditure was higher than the income", 
                                                       "expenditure was approximately equal to the income", 
                                                       "expenditure was lower than the income")),                              
             rate_econsit20 = factor(ci20m252,
                                     levels = c(1:5),
                                     labels = c("we are accumulating debts", 
                                                "we are somewhat eating into savings", 
                                                "we are just managing to make ends meet", 
                                                "we have a little bit of money to spare", 
                                                "we have a lot of money to spare")),
             expense500_20 = factor(ci20m355,
                                    levels = c(1:7),
                                    labels = c("very easy",
                                               "2nd easiest",
                                               "3rd easiest",
                                               "Neutral",
                                               "3rd hardest",
                                               "2nd hardest",
                                               "very hard")),
             livingcosts_20 = factor(ci20m356,
                                     levels = c(1:7),
                                     labels = c("very easy",
                                                "2nd easiest",
                                                "3rd easiest",
                                                "Neutral",
                                                "3rd hardest",
                                                "2nd hardest",
                                                "very hard")),
             difficultyinc = factor(ci20m378,
                                    levels = c(1:10)),
             childbene20 = as.numeric(ci20m140),                      # Annual payments (to and from respondent)
             healthcarebene20 = as.numeric(ci20m143),
             alimonychildreceive20 = as.numeric(ci20m165),
             alimonyspouse20 = as.numeric(ci20m206),
             alimonychildpay20 = as.numeric(ci20m208),
             paychildlivealone20 = as.numeric(ci20m210),
             childbudget20 = as.numeric(ci20m330),
             housingdebt20 = as.numeric(ci20m301),                   # Debts in 2019
             utilitiesdebt20 = as.numeric(ci20m303),
             rentamount20 = as.numeric(cd20m008),                   # Rent
             hhexpend_nextyear20 = factor(ci20m258,
                                          levels=c(1:5),
                                          labels=c("much higher than the income",
                                                   "higher than the income",
                                                   "approximately equal to the income",
                                                   "lower than the income",
                                                   "much lower than the income")),
             econsit_change20 = factor(ci20m261,
                                       levels=c(1:5),
                                       labels=c("will get much better",
                                                "will get slightly better",
                                                "will remain more or less the same",
                                                "will get a bit worse",
                                                "will get a lot worse")),
             econissue_endsmeet20 = factor(ci20m245, levels = c(0,1)),           # Current experiences/ability to pay
             econissue_replacebroken20 = factor(ci20m246, levels = c(0,1)),
             econissue_borrowmoney20 = factor(ci20m247, levels = c(0,1)),
             econissue_behindrent20 = factor(ci20m248, levels = c(0,1)),
             econissue_debtcollect20 = factor(ci20m249, levels = c(0,1)),
             econissue_socialsupport20 = factor(ci20m250, levels = c(0,1)),
             dwellrooms20 = as.numeric(cd20m034),                                          # HOUSING
             dwellissuesmall20 = factor(cd20m041, levels = c(0,1)),
             dwellissueheat20 = factor(cd20m043, levels = c(0,1)),
             dwellissueroof20 = factor(cd20m044, levels = c(0,1)),
             dwellissuedamp20 = factor(cd20m045, levels = c(0,1)),
             dwellissuerot20 = factor(cd20m046, levels = c(0,1)),
             dwellissuenoise20 = factor(cd20m047, levels = c(0,1)),
             dwellissuecrime20 = factor(cd20m052, levels = c(0,1)),
             dwellsatisfied20 = factor(cd20m091, levels = c(0:10)),
             dwellvicinsatisfied20 = factor(cd20m092, levels = c(0:10)),
             satisfy_life20 = factor(ci20m005, levels = c(0:10)),            # OVERALL SATISFACTION
             satisfy_econsit20 = factor(ci20m006, levels = c(0:10)),     
             satisfy_dutcheconsit20 = factor(ci20m007, levels = c(0:10)),   
             subj_happiness20 = factor(ci20m380, levels = c(0:10)),
             stablestopwork20 = factor(ci20m237, levels = c(0,1)),        # EXPECTED IN/STABILITY IN 2020
             stablestartwork20 = factor(ci20m238, levels = c(0,1)), 
             stablechgwork20 = factor(ci20m239, levels = c(0,1)), 
             stablebenefitinc20 = factor(ci20m241, levels = c(0,1)), 
             stablebenefitdec20 = factor(ci20m242, levels = c(0,1)),  
             stablelosejob20 = as.numeric(ci20m379),
             chgsincelasteyr20 = factor(ci20m377, levels = c(0,10)),                                 
      )  %>%
      select(nomem_encr, 
             resp_finmatters20, 
             balance20, 
             loans20, 
             minimum_desiredinc20	,
             exp_vs_inc20	,
             exp_vs_inc20_nobigexp	,
             rate_econsit20	,
             expense500_20	,
             livingcosts_20	,
             difficultyinc	,
             childbene20	,
             healthcarebene20	,
             alimonychildreceive20	,
             alimonyspouse20	,
             alimonychildpay20	,
             paychildlivealone20	,
             childbudget20	,
             housingdebt20	,
             utilitiesdebt20	,
             rentamount20	,
             hhexpend_nextyear20	,
             econsit_change20	,
             econissue_endsmeet20	,
             econissue_replacebroken20	,
             econissue_borrowmoney20	,
             econissue_behindrent20	,
             econissue_debtcollect20	,
             econissue_socialsupport20	,
             dwellrooms20	,
             dwellissuesmall20	,
             dwellissueheat20	,
             dwellissueroof20	,
             dwellissuedamp20	,
             dwellissuerot20	,
             dwellissuenoise20	,
             dwellissuecrime20	,
             dwellsatisfied20	,
             dwellvicinsatisfied20	,
             satisfy_life20	,
             satisfy_econsit20	,
             satisfy_dutcheconsit20	,
             subj_happiness20	,
             stablestopwork20	,
             stablestartwork20	,
             stablechgwork20	,
             stablebenefitinc20	,
             stablebenefitdec20	,
             stablelosejob20	,
             chgsincelasteyr20) # only  new variables about economic situation are added 
    
    
    
    ########################################################
    # POLITICS & VALUES
    
    # Select variables from a list generated in excel
    
    polival_priority <- subset(df, select=c("nomem_encr", 
                                            "cv20l023", "cv20l024", "cv20l040", "cv20l041", "cv20l101", "cv20l109", "cv20l110", "cv20l111", "cv20l112", 
                                            "cv20l113", "cv20l114", "cv20l115", "cv20l124", "cv20l125", "cv20l126", "cv20l127", "cv20l128", "cv20l129", 
                                            "cv20l130", "cv20l131", "cv20l132", "cv20l133", "cv20l134", "cv20l139", "cv20l140", "cv20l141", "cv20l142", 
                                            "cv20l143", "cv20l144", "cv20l145", "cv20l146", "cv20l151", "cv20l152", "cv20l153", "cv20l154"))
    
    
    # Categorical variables (scale from 0-10) with missing codes -9 Don't know
    
    polival_priority <- polival_priority %>%
      mutate( conf_edusys20 = factor(cv20l023, levels = c(0:10)),
              conf_healthsys20 = factor(cv20l024, levels = c(0:10)),    
              satis_edusys20 = factor(cv20l040, levels = c(0:10)),   
              satis_healthsys20 = factor(cv20l041, levels = c(0:10)),  
              womanwork_baby20 = factor(cv20l143,
                                        levels = c(1:3),
                                        labels = c("full-time",
                                                   "part-time",
                                                   "no job")),
              womanwork_yngchild20 = factor(cv20l144,
                                            levels = c(1:3),
                                            labels = c("full-time",
                                                       "part-time",
                                                       "no job")),          
              womanwork_chldprimary20 = factor(cv20l145,
                                               levels = c(1:3),
                                               labels = c("full-time",
                                                          "part-time",
                                                          "no job")),       
              womanwork_chldsecondary20 = factor(cv20l146,
                                                 levels = c(1:3),
                                                 labels = c("full-time",
                                                            "part-time",
                                                            "no job")),     
              workingmother20 = factor(cv20l109, 
                                       levels = c(1:5), 
                                       labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              motherworkchildsuffer20 = factor(cv20l110, 
                                               levels = c(1:5), 
                                               labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              motherworkfamsuffer20 = factor(cv20l111, 
                                             levels = c(1:5), 
                                             labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              twoparentincome20 = factor(cv20l112, 
                                         levels = c(1:5), 
                                         labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              fatherearn20 = factor(cv20l113, 
                                    levels = c(1:5), 
                                    labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              fathermorehousework20 = factor(cv20l114, 
                                             levels = c(1:5), 
                                             labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              fathermorechldcare20 = factor(cv20l115, 
                                            levels = c(1:5), 
                                            labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              marriedhappy20 = factor(cv20l124, 
                                      levels = c(1:5), 
                                      labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              parentsshouldmarry20 = factor(cv20l125, 
                                            levels = c(1:5), 
                                            labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              singleparentok20 = factor(cv20l126, 
                                        levels = c(1:5), 
                                        labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              cohabitok20 = factor(cv20l127, 
                                   levels = c(1:5), 
                                   labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              cohabitbeforemar20 = factor(cv20l128, 
                                          levels = c(1:5), 
                                          labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              divorceok20 = factor(cv20l129, 
                                   levels = c(1:5), 
                                   labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              divorceokwithkid20 = factor(cv20l130, 
                                          levels = c(1:5), 
                                          labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              childcareforparent20 = factor(cv20l131, 
                                            levels = c(1:5), 
                                            labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              elderparentcohab20 = factor(cv20l132, 
                                          levels = c(1:5), 
                                          labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              childvisit20 = factor(cv20l133, 
                                    levels = c(1:5), 
                                    labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              childcareforparent_leave20 = factor(cv20l134, 
                                                  levels = c(1:5), 
                                                  labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              dutyfirst20 = factor(cv20l139, 
                                   levels = c(1:5), 
                                   labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              workhardforjoy20 = factor(cv20l140, 
                                        levels = c(1:5), 
                                        labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              happywhenworkhard20 = factor(cv20l141, 
                                           levels = c(1:5), 
                                           labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              workfirst20 = factor(cv20l142, 
                                   levels = c(1:5), 
                                   labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              womennaturalparent20 = factor(cv20l151, 
                                            levels = c(1:5), 
                                            labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              boyedu_moreimp20 = factor(cv20l152, 
                                        levels = c(1:5), 
                                        labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              genderedparenting20 = factor(cv20l153, 
                                           levels = c(1:5), 
                                           labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree")),
              menshouldleadjobs20 = factor(cv20l154, 
                                           levels = c(1:5), 
                                           labels = c("fully disagree", "disagree", "neither agree nor disagree", "agree", "fully agree"))) %>%
      select(nomem_encr, 
             conf_edusys20	,
             conf_healthsys20	,
             satis_edusys20	,
             satis_healthsys20	,
             womanwork_baby20	,
             womanwork_yngchild20	,
             womanwork_chldprimary20	,
             womanwork_chldsecondary20	,
             workingmother20	,
             motherworkchildsuffer20	,
             motherworkfamsuffer20	,
             twoparentincome20	,
             fatherearn20	,
             fathermorehousework20	,
             fathermorechldcare20	,
             marriedhappy20	,
             parentsshouldmarry20	,
             singleparentok20	,
             cohabitok20	,
             cohabitbeforemar20	,
             divorceok20	,
             divorceokwithkid20	,
             childcareforparent20	,
             elderparentcohab20	,
             childvisit20	,
             childcareforparent_leave20	,
             dutyfirst20	,
             workhardforjoy20	,
             happywhenworkhard20	,
             workfirst20	,
             womennaturalparent20	,
             boyedu_moreimp20	,
             genderedparenting20	,
             menshouldleadjobs20) # only  new variables about politics + values are added 
    
    ########################################################
    # COMBINE CLEANED VARIABLES
    
    #put all data frames into list
    df_list <- list(background, health, religion, econsit_priority, polival_priority, fert, family)
    
    #merge all data frames in list
    df <- df_list %>% reduce(full_join, by='nomem_encr')
    
    return(df)
  }
  

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.
  
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).
  
  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  ## This script contains a bare minimum working example
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }
  
  # Load the model
  model <- readRDS(here("model.rds"))
  
  # Preprocess the fake / holdout data
  df <- clean_df(df)
  
  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  # Generate predictions from model
  predictions <- predict(model, 
                         na.roughfix(subset(df, select = vars_without_id)), 
                         type="response") 
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  # predictions <- ifelse(predictions > 0.5, 1, 0)   # ‘>’ not meaningful for factors
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return(df_predict)
}
