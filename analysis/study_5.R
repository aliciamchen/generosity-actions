library(here)
library(showtext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)
library(emmeans)
library(lmerTest)
library(car)

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
    participant_first_choice = factor(participant_first_choice, levels = c("give", "receive")),
    partner_first_choice = factor(partner_first_choice, levels = c("give", "receive")),
    coordination = factor(coordination, levels = c("False", "True")),
    participant_second_choice = factor(participant_second_choice, levels = c("give", "receive")),
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
  strategy_alternating ~ partner_status * coordination + (1 | scenario_id) + (1 | subject_id),
  data = main_data_filtered,
  family = binomial(link = "logit"), 
  control = glmerControl(optimizer = "bobyqa"))
summary(main_model)

## ------------------------------------------------------------------------------------------------------------------------------------------------------
emm <- emmeans(main_model, ~ partner_status * coordination)
summary(emm, infer = T)

# partner_status coordination emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# equal          False        -1.187 0.218 Inf    -1.615    -0.759  -5.432  <.0001
# lower          False        -0.925 0.211 Inf    -1.339    -0.511  -4.376  <.0001
# higher         False        -1.115 0.225 Inf    -1.555    -0.675  -4.963  <.0001
# equal          True         -0.229 0.211 Inf    -0.642     0.185  -1.083  0.2786
# lower          True         -1.599 0.250 Inf    -2.088    -1.110  -6.406  <.0001
# higher         True         -1.476 0.235 Inf    -1.936    -1.015  -6.283  <.0001

pairs(emm)

# contrast                   estimate    SE  df z.ratio p.value
# equal False - lower False   -0.2617 0.271 Inf  -0.965  0.9290
# equal False - higher False  -0.0717 0.283 Inf  -0.253  0.9999
# equal False - equal True    -0.9582 0.280 Inf  -3.417  0.0083
# equal False - lower True     0.4123 0.301 Inf   1.372  0.7442
# equal False - higher True    0.2891 0.287 Inf   1.008  0.9154
# lower False - higher False   0.1900 0.277 Inf   0.687  0.9834
# lower False - equal True    -0.6964 0.270 Inf  -2.580  0.1021
# lower False - lower True     0.6741 0.300 Inf   2.243  0.2179
# lower False - higher True    0.5508 0.282 Inf   1.952  0.3704
# higher False - equal True   -0.8864 0.281 Inf  -3.155  0.0200
# higher False - lower True    0.4841 0.308 Inf   1.573  0.6166
# higher False - higher True   0.3608 0.299 Inf   1.207  0.8337
# equal True - lower True      1.3705 0.300 Inf   4.562  0.0001
# equal True - higher True     1.2472 0.288 Inf   4.337  0.0002
# lower True - higher True    -0.1233 0.308 Inf  -0.400  0.9987
# 
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: tukey method for comparing a family of 6 estimates 

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
# partner_first_choice1                           -0.85625    0.07515 -11.394  < 2e-16 ***
#   participant_first_choice1                       -0.01320    0.07645  -0.173  0.86290    
# partner_status1                                 -0.05751    0.10121  -0.568  0.56984    
# partner_status2                                 -0.10474    0.10681  -0.981  0.32681    
# partner_first_choice1:participant_first_choice1 -0.09031    0.07645  -1.181  0.23746    
# partner_first_choice1:partner_status1            0.30945    0.10121   3.058  0.00223 ** 
#   partner_first_choice1:partner_status2           -0.14262    0.10681  -1.335  0.18182    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) prtn__1 prtc__1 prtn_1 prtn_2 p__1:__ p__1:_1
# prtnr_frs_1  0.044                                              
# prtcpnt_f_1 -0.223 -0.047                                       
# prtnr_stts1 -0.054  0.017  -0.103                               
# prtnr_stts2 -0.015  0.034   0.199  -0.483                       
# prtn__1:__1 -0.047 -0.223   0.046  -0.032 -0.003                
# prtnr__1:_1  0.017 -0.054  -0.032   0.039 -0.053 -0.103         
# prtnr__1:_2  0.034 -0.015  -0.003  -0.053  0.051  0.199  -0.483 
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

Anova(model2, type = "III")

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: participant_second_choice_generous
# Chisq Df Pr(>Chisq)    
# (Intercept)                                     0.9473  1   0.330403    
# partner_first_choice                          129.8196  1  < 2.2e-16 ***
#   participant_first_choice                        0.0298  1   0.862900    
# partner_status                                  2.3776  2   0.304581    
# partner_first_choice:participant_first_choice   1.3956  1   0.237461    
# partner_first_choice:partner_status             9.3753  2   0.009208 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


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
