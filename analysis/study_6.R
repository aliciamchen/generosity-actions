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
data <- read.csv(here("data/study-6_data.csv"))

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
    strategy_repeating = ifelse(strategy == "repeating", 1, 0),
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
    strategy_repeating,
    passed_attention_checks
  ) %>%
  arrange(subject_id, scenario_id) %>%
  mutate(
    partner_status = factor(partner_status, levels = c("equal", "lower", "higher")),
    relationship = factor(relationship, levels = c("equal", "unequal")),
    participant_first_choice = factor(participant_first_choice, levels = c("give", "receive")),
    partner_first_choice = factor(partner_first_choice, levels = c("give", "receive")),
    participant_second_choice = factor(participant_second_choice, levels = c("give", "receive")),
    strategy = factor(strategy, levels = c("alternating", "repeating")), 
    first_choices = factor(first_choices, levels = c("give_give", "give_receive", "receive_give", "receive_receive"))
  )


# Filter for participants who passed attention checks
main_data_filtered <- main_data %>%
  filter(passed_attention_checks > 0)

write_csv(main_data_filtered, here("data/study-6_tidy_data.csv"))

# Look at demographic info
length(unique(main_data_filtered$subject_id))
study_6_demographics <- read.csv(here("data/study-6_demographics.csv"))
study_6_demographics %>% count(gender)
study_6_demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
main_model <- glmer(
  strategy_repeating ~ partner_status * coordination + (1 | scenario_id) + (1 | subject_id),
  data = main_data_filtered,
  family = binomial(link = "logit"), 
  control = glmerControl(optimizer = "bobyqa"))
summary(main_model)

## ------------------------------------------------------------------------------------------------------------------------------------------------------
emm <- emmeans(main_model, ~ partner_status * coordination)
summary(emm, infer = T)

# partner_status coordination emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# equal          False         1.049 0.193 Inf     0.672     1.427   5.442  <.0001
# lower          False         1.055 0.202 Inf     0.659     1.451   5.217  <.0001
# higher         False         1.291 0.201 Inf     0.898     1.684   6.434  <.0001
# equal          True          0.138 0.199 Inf    -0.253     0.529   0.692  0.4890
# lower          True          1.175 0.209 Inf     0.766     1.585   5.626  <.0001
# higher         True          1.653 0.245 Inf     1.173     2.133   6.746  <.0001
# 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

pairs(emm)

# contrast                   estimate    SE  df z.ratio p.value
# equal False - lower False   -0.0056 0.260 Inf  -0.022  1.0000
# equal False - higher False  -0.2415 0.258 Inf  -0.937  0.9369
# equal False - equal True     0.9115 0.264 Inf   3.453  0.0073
# equal False - lower True    -0.1259 0.263 Inf  -0.478  0.9969
# equal False - higher True   -0.6034 0.292 Inf  -2.068  0.3040
# lower False - higher False  -0.2359 0.264 Inf  -0.894  0.9480
# lower False - equal True     0.9171 0.266 Inf   3.446  0.0075
# lower False - lower True    -0.1203 0.274 Inf  -0.439  0.9979
# lower False - higher True   -0.5978 0.298 Inf  -2.003  0.3406
# higher False - equal True    1.1530 0.264 Inf   4.361  0.0002
# higher False - lower True    0.1156 0.267 Inf   0.432  0.9981
# higher False - higher True  -0.3619 0.299 Inf  -1.211  0.8315
# equal True - lower True     -1.0374 0.272 Inf  -3.814  0.0019
# equal True - higher True    -1.5149 0.301 Inf  -5.039  <.0001
# lower True - higher True    -0.4774 0.301 Inf  -1.588  0.6065
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
# 1127.8   1176.4   -553.9   1107.8      938 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.2065 -0.6942 -0.4461  0.7241  2.2418 
# 
# Random effects:
#   Groups      Name        Variance Std.Dev.
# subject_id  (Intercept) 0.00000  0.0000  
# scenario_id (Intercept) 0.00558  0.0747  
# Number of obs: 948, groups:  subject_id, 158; scenario_id, 6
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                      0.01195    0.08047   0.149  0.88192    
# partner_first_choice1                           -0.90833    0.07463 -12.170  < 2e-16 ***
#   participant_first_choice1                       -0.15649    0.07640  -2.048  0.04053 *  
#   partner_status1                                  0.04998    0.10137   0.493  0.62199    
# partner_status2                                 -0.31224    0.10767  -2.900  0.00373 ** 
#   partner_first_choice1:participant_first_choice1 -0.12204    0.07587  -1.609  0.10772    
# partner_first_choice1:partner_status1            0.41571    0.10144   4.098 4.17e-05 ***
#   partner_first_choice1:partner_status2           -0.09971    0.10790  -0.924  0.35544    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) prtn__1 prtc__1 prtn_1 prtn_2 p__1:__ p__1:_1
# prtnr_frs_1  0.007                                              
# prtcpnt_f_1 -0.110 -0.032                                       
# prtnr_stts1 -0.091  0.014  -0.142                               
# prtnr_stts2 -0.023  0.071   0.222  -0.457                       
# prtn__1:__1 -0.036 -0.121   0.049  -0.055  0.082                
# prtnr__1:_1  0.014 -0.095  -0.050   0.016 -0.090 -0.141         
# prtnr__1:_2  0.066 -0.029   0.073  -0.088  0.095  0.227  -0.459 
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

Anova(model2, type = "III")

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: participant_second_choice_generous
# Chisq Df Pr(>Chisq)    
# (Intercept)                                     0.0221  1  0.8819169    
# partner_first_choice                          148.1207  1  < 2.2e-16 ***
#   participant_first_choice                        4.1957  1  0.0405254 *  
#   partner_status                                  9.2871  2  0.0096236 ** 
#   partner_first_choice:participant_first_choice   2.5874  1  0.1077154    
# partner_first_choice:partner_status            17.9537  2  0.0001263 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Compare models using likelihood ratio test
anova(model1, model2)

# Data: main_data_filtered
# Models:
#   model1: participant_second_choice_generous ~ partner_first_choice * participant_first_choice + (1 | scenario_id) + (1 | subject_id)
# model2: participant_second_choice_generous ~ partner_first_choice * participant_first_choice + partner_first_choice:partner_status + partner_status + (1 | scenario_id) + (1 | subject_id)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model1    6 1146.5 1175.6 -567.24   1134.5                         
# model2   10 1127.8 1176.4 -553.92   1107.8 26.642  4  2.348e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
