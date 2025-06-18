library(here)
library(showtext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(tidyboot)
library(lme4)
library(emmeans)
library(lmerTest)

showtext_auto()

font_add(family = "Lato", regular = here("fonts/Lato-Regular.ttf"))
theme_set(theme_classic(base_size = 15, base_family = "Lato"))

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))


## ----load-data-----------------------------------------------------------------------------------------------------------------------------------------
data <- read.csv(here("data/study-5/non_anonymized/study-5_data.csv"))

main_data <- data %>%
  filter(time == "first") %>%
  select(
    subject_id,
    scenario_id,
    partner_first_choice = partner_choice,
    participant_first_choice = participant_choice,
    successful_first_time = coordination
  ) %>% 
  left_join(
    data %>%
      filter(time == "second") %>%
      select(
        subject_id,
        scenario_id,
        partner_status,
        participant_second_choice = participant_choice,
        passed_attention_checks
      ),
    by = c("subject_id", "scenario_id")
  ) %>% 
  # Create derived variables
  mutate(
    coordination = successful_first_time,
    successful_first_time = as.logical(successful_first_time),
    participant_first_choice_generous = ifelse(participant_first_choice == "give", 1, 0),
    participant_second_choice_generous = ifelse(participant_second_choice == "give", 1, 0),
    relationship = ifelse(partner_status == "equal", "equal", "unequal"),
    strategy = case_when(
      # Alternating: if partner gave first and participant gives second, or
      # if partner received first and participant receives second
      (partner_first_choice == "give" & participant_second_choice == "give") |
      (partner_first_choice == "receive" & participant_second_choice == "receive") ~ "alternating",
      TRUE ~ "repeating"
    ), 
    strategy_alternating = ifelse(strategy == "alternating", 1, 0),
    first_choices = paste0(participant_first_choice, "_", partner_first_choice)
  ) %>%
  select(
    subject_id,
    scenario_id,
    partner_status,
    relationship,
    partner_first_choice,
    participant_first_choice,
    first_choices,
    participant_first_choice_generous,
    coordination,
    successful_first_time,
    participant_second_choice,
    participant_second_choice_generous,
    strategy,
    strategy_alternating,
    passed_attention_checks
  ) %>%
  arrange(subject_id, scenario_id) %>%
  mutate(
    partner_status = factor(partner_status, levels = c("equal", "lower", "higher")),
    relationship = factor(relationship, levels = c("equal", "unequal")),
    participant_first_choice = factor(participant_first_choice, levels = c("receive", "give")),
    partner_first_choice = factor(partner_first_choice, levels = c("receive", "give")),
    coordination = factor(coordination, levels = c("False", "True")),
    participant_second_choice = factor(participant_second_choice, levels = c("receive", "give")),
    strategy = factor(strategy, levels = c("alternating", "repeating")), 
    first_choices = factor(first_choices, levels = c("give_give", "give_receive", "receive_give", "receive_receive"))
  )


# Filter for participants who passed attention checks
main_data_filtered <- main_data %>%
  filter(passed_attention_checks > 0)

write_csv(main_data_filtered, here("data/study-5_tidy_data.csv"))

# Look at demographic info
length(unique(main_data_filtered$subject_id))
study_5_demographics <- read.csv(here("data/study-5/non_anonymized/study-5_demographics.csv"))
study_5_demographics %>% count(gender)
study_5_demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
main_model <- glmer(
  strategy_alternating ~ relationship + (1 | scenario_id) + (1 | subject_id),
  data = main_data_filtered,
  family = binomial(link = "logit"), 
  control = glmerControl(optimizer = "bobyqa"))
summary(main_model)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: strategy_alternating ~ relationship + (1 | scenario_id) + (1 |      subject_id)
# Data: main_data_filtered
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 1071.2   1090.5   -531.6   1063.2      914 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.4739 -0.5956 -0.4594  0.8868  2.1427 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# subject_id  (Intercept) 1.07     1.034   
# scenario_id (Intercept) 0.00     0.000   
# Number of obs: 918, groups:  subject_id, 153; scenario_id, 6
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -0.98038    0.12284  -7.981 1.45e-15 ***
#   relationship1  0.26783    0.08222   3.258  0.00112 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# relatinshp1 0.147 
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


## ------------------------------------------------------------------------------------------------------------------------------------------------------
emm <- emmeans(main_model, ~ relationship)
pairs(emm)

# contrast        estimate    SE  df z.ratio p.value
# equal - unequal    0.536 0.164 Inf   3.258  0.0011
# 
# Results are given on the log odds ratio (not the response) scale. 

## ----secondary-analysis--------------------------------------------------------------------------------------------------------------------------------
# Model 1: Base model
model1 <- glmer(
  participant_second_choice_generous ~ partner_first_choice * participant_first_choice +
    (1 | scenario_id) + (1 | subject_id),
  data = main_data_filtered,
  family = binomial(link = "logit")
)
summary(model1)


# Model 2: Including partner status
model2 <- glmer(
  participant_second_choice_generous ~ partner_first_choice * participant_first_choice +
    partner_first_choice:partner_status + partner_status +
    (1 | scenario_id) + (1 | subject_id),
  data = main_data_filtered,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)
summary(model2)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: participant_second_choice_generous ~ partner_first_choice * participant_first_choice +  
#   partner_first_choice:partner_status + partner_status + (1 |      scenario_id) + (1 | subject_id)
# Data: main_data_filtered
# Control: glmerControl(optimizer = "bobyqa")
# 
# AIC      BIC   logLik deviance df.resid 
# 1121.7   1169.9   -550.8   1101.7      908 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8124 -0.6767 -0.5272  0.6894  1.8967 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# subject_id  (Intercept) 0        0       
# scenario_id (Intercept) 0        0       
# Number of obs: 918, groups:  subject_id, 153; scenario_id, 6
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                     -0.07314    0.07515  -0.973  0.33040    
# partner_first_choice1                            0.85625    0.07515  11.394  < 2e-16 ***
#   participant_first_choice1                        0.01320    0.07645   0.173  0.86290    
# partner_status1                                 -0.05751    0.10121  -0.568  0.56984    
# partner_status2                                 -0.10474    0.10682  -0.981  0.32681    
# partner_first_choice1:participant_first_choice1 -0.09031    0.07645  -1.181  0.23746    
# partner_first_choice1:partner_status1           -0.30945    0.10121  -3.058  0.00223 ** 
#   partner_first_choice1:partner_status2            0.14262    0.10682   1.335  0.18182    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) prtn__1 prtc__1 prtn_1 prtn_2 p__1:__ p__1:_1
# prtnr_frs_1 -0.044                                              
# prtcpnt_f_1  0.223 -0.047                                       
# prtnr_stts1 -0.054 -0.017   0.103                               
# prtnr_stts2 -0.015 -0.034  -0.199  -0.483                       
# prtn__1:__1 -0.047  0.223  -0.046  -0.032 -0.003                
# prtnr__1:_1 -0.017 -0.054  -0.032  -0.039  0.053  0.103         
# prtnr__1:_2 -0.034 -0.015  -0.003   0.053 -0.051 -0.199  -0.483 
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

# Compare models using likelihood ratio test
anova(model1, model2)

# Data: main_data_filtered
# Models:
#   model1: participant_second_choice_generous ~ partner_first_choice * participant_first_choice + (1 | scenario_id) + (1 | subject_id)
# model2: participant_second_choice_generous ~ partner_first_choice * participant_first_choice + partner_first_choice:partner_status + partner_status + (1 | scenario_id) + (1 | subject_id)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model1    6 1125.4 1154.3 -556.68   1113.4                       
# model2   10 1121.7 1169.9 -550.85   1101.7 11.676  4    0.01993 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
