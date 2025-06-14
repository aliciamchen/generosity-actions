library(here)
library(tidyverse)
library(tidyboot)
library(lme4)
library(lmerTest)
library(emmeans)
library(glue)

# Options -----------------------------------------------------------------

options(warn = -1)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Load and tidy data ------------------------------------------------------


scenarios.diffs <- read.csv(here("data/scenarios_diffs.csv"))

d <-
  read.csv(here("data/study-4_data.csv")) %>%
  filter(understood == "yes", pass_attention == TRUE, story != "attention") %>%
  mutate(
    strategy = case_when(
      stage == "second" & first_meeting == response ~ "Precedent",
      stage == "second" &
        first_meeting != response ~ "Reciprocity"
    )
  ) %>%
  select(-response, -understood, -first_meeting, -pass_attention) %>%
  mutate_all(~ case_when(
    . == "less" ~ "Lower",
    . == "more" ~ "Higher",
    . == "equal" ~ "Equal", TRUE ~ .
  )) %>%
  rename(
    first_actual = altruistic_status,
  ) %>%
  group_by(subject_id, story) %>%
  fill(strategy, .direction = "up") %>%
  ungroup() %>%
  pivot_wider(
    names_from = stage,
    values_from = response_status,
    names_prefix = "response_"
  ) %>%
  rename(
    first_response = response_first,
    second_response = response_second
  ) %>%
  mutate(symmetric = ifelse(first_actual == "Equal", "Symmetric", "Asymmetric"), 
    Expectations = case_when(
      first_actual != first_response ~ "Inconsistent",
      first_actual == first_response ~ "Consistent"
    ),
    Expectations = factor(Expectations, levels = c("Consistent", "Inconsistent"))) %>%
  group_by(story) %>%
  left_join(scenarios.diffs) %>%
  pivot_wider(names_from = type, values_from = c("n", "diff", "ci_lower", "ci_upper", "mean"))

# Set levels for categorical variables
d$first_actual <-
  factor(d$first_actual, levels = c("Equal", "Higher", "Lower"))
d$first_response <-
  factor(d$first_response, levels = c("Equal", "Higher", "Lower"))
d$second_response <-
  factor(d$second_response, levels = c("Equal", "Higher", "Lower"))
d$strategy <-
  factor(d$strategy, levels = c("Reciprocity", "Precedent"))

write.csv(d, here("data/study-4_tidy_data.csv"), row.names = FALSE)


# Display demographics ----------------------------------------------------


d.demographics <- read.csv(here("data/study-4_demographics.csv"))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))


# Stats -------------------------------------------------------------------

# What happened the first time people interacted?
# Can their expectations be explained by relative cost/benefit?
mod <-
  glmer(
    data = d %>% filter(first_actual != "Equal"),
    first_response ~ diff_effort + (1 |
      subject_id) + (1 |
      story),
    family = "binomial"
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response ~ diff_effort + (1 | subject_id) + (1 | story)
# Data: d %>% filter(first_actual != "Equal")
#
# AIC      BIC   logLik deviance df.resid
# 1615.6   1636.5   -803.8   1607.6     1352
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -2.4675 -0.6880 -0.3489  0.6741  2.6581
#
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.1799   0.4242
# story      (Intercept) 1.2495   1.1178
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   0.2029     0.5760   0.352    0.725
# diff_effort  -0.1144     0.2661  -0.430    0.667
#
# Correlation of Fixed Effects:
#   (Intr)
# diff_effort -0.880

mod <-
  glmer(
    data = d %>% filter(first_actual != "Equal"),
    first_response ~ diff_benefit + (1 |
      subject_id) + (1 |
      story),
    family = "binomial"
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response ~ diff_benefit + (1 | subject_id) + (1 | story)
# Data: d %>% filter(first_actual != "Equal")
#
# AIC      BIC   logLik deviance df.resid
# 1613.7   1634.5   -802.8   1605.7     1352
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -2.3913 -0.6976 -0.3403  0.6716  2.7261
#
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.1807   0.4251
# story      (Intercept) 1.1139   1.0554
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   -0.7152     0.5349  -1.337    0.181
# diff_benefit   0.3826     0.2555   1.497    0.134
#
# Correlation of Fixed Effects:
#   (Intr)
# diff_beneft -0.874



# Next, replicate findings from previous studies

# People expect alternation when the relationship is symmetric, and repetition when the relationship is asymmetric.

mod <- glmer(
  data = d,
  strategy ~ first_actual + (1 |
    subject_id) + (1 |
    story),
  family = "binomial"
)

summary(mod)


emmeans(mod, pairwise ~ first_actual) %>% summary(infer = T)

# $emmeans
# first_actual emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# Equal        -1.395 0.214 Inf   -1.8138    -0.976  -6.528  <.0001
# Higher        0.228 0.207 Inf   -0.1775     0.634   1.102  0.2703
# Lower         0.467 0.207 Inf    0.0606     0.874   2.252  0.0243
#
# Results are given on the logit (not the response) scale.
# Confidence level used: 0.95
#
# $contrasts
# contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# Equal - Higher   -1.623 0.136 Inf    -1.942   -1.3039 -11.915  <.0001
# Equal - Lower    -1.862 0.138 Inf    -2.186   -1.5383 -13.479  <.0001
# Higher - Lower   -0.239 0.125 Inf    -0.531    0.0535  -1.915  0.1345
#
# Results are given on the log odds ratio (not the response) scale.
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# Add consistent / inconsistent as a predictor
mod <- glmer(
  data = d,
  strategy ~ first_actual * Expectations + (1 |
                               subject_id) + (1 |
                                                story),
  family = "binomial"
)

summary(mod)
emmeans(mod, pairwise ~ first_actual * Expectations) %>% summary(infer = T)

# $emmeans
# first_actual Expectations emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# Equal        Consistent   -1.413 0.219 Inf    -1.842   -0.9833  -6.450  <.0001
# Higher       Consistent    0.869 0.237 Inf     0.404    1.3337   3.664  0.0002
# Lower        Consistent    1.165 0.240 Inf     0.695    1.6342   4.862  <.0001
# Equal        Inconsistent nonEst    NA  NA        NA        NA      NA      NA
# Higher       Inconsistent -0.389 0.234 Inf    -0.847    0.0692  -1.664  0.0961
# Lower        Inconsistent -0.200 0.233 Inf    -0.657    0.2565  -0.859  0.3902
# 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                                 estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
# Equal Consistent - Higher Consistent       -2.281 0.176 Inf    -2.761    -1.802 -12.991  <.0001
# Equal Consistent - Lower Consistent        -2.577 0.180 Inf    -3.069    -2.086 -14.305  <.0001
# Equal Consistent - Equal Inconsistent      nonEst    NA  NA        NA        NA      NA      NA
# Equal Consistent - Higher Inconsistent     -1.024 0.166 Inf    -1.478    -0.570  -6.150  <.0001
# Equal Consistent - Lower Inconsistent      -1.212 0.166 Inf    -1.665    -0.760  -7.309  <.0001
# Higher Consistent - Lower Consistent       -0.296 0.202 Inf    -0.846     0.255  -1.465  0.5852
# Higher Consistent - Equal Inconsistent     nonEst    NA  NA        NA        NA      NA      NA
# Higher Consistent - Higher Inconsistent     1.258 0.197 Inf     0.719     1.796   6.369  <.0001
# Higher Consistent - Lower Inconsistent      1.069 0.186 Inf     0.562     1.577   5.746  <.0001
# Lower Consistent - Equal Inconsistent      nonEst    NA  NA        NA        NA      NA      NA
# Lower Consistent - Higher Inconsistent      1.553 0.190 Inf     1.035     2.071   8.180  <.0001
# Lower Consistent - Lower Inconsistent       1.365 0.201 Inf     0.818     1.912   6.805  <.0001
# Equal Inconsistent - Higher Inconsistent   nonEst    NA  NA        NA        NA      NA      NA
# Equal Inconsistent - Lower Inconsistent    nonEst    NA  NA        NA        NA      NA      NA
# Higher Inconsistent - Lower Inconsistent   -0.189 0.189 Inf    -0.705     0.328  -0.995  0.8577
# 
# Results are given on the log odds ratio (not the response) scale. 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 5 estimates 
# P value adjustment: tukey method for comparing a family of 5 estimates 




# Main preregistered hypothesis: in asymmetric relationships, people’s expectations for what
# happens the second time are explained by (1) expectations of tacit coordination
# (what they thought would happen the first time), and (2) expectations of precedent.


d.h1 <- d %>%
  filter(symmetric == "Asymmetric") %>%
  mutate(
    first_response = factor(first_response, levels = c("Lower", "Higher")),
    first_actual = factor(first_actual, levels = c("Lower", "Higher")),
    second_response = factor(second_response, levels = c("Higher", "Lower"))
  )

mod <- glmer(
  data = d.h1,
  second_response ~ first_response * first_actual + (1 |
    subject_id) + (1 |
    story),
  family = "binomial"
)

summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: second_response ~ first_response * first_actual + (1 | subject_id) +      (1 | story)
# Data: d.h1
#
# AIC      BIC   logLik deviance df.resid
# 1759.0   1790.3   -873.5   1747.0     1350
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -1.8471 -0.8955  0.5692  0.8472  1.6317
#
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.00000  0.0000
# story      (Intercept) 0.06232  0.2496
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                    0.10280    0.08232   1.249    0.212
# first_response1                0.50655    0.06370   7.952 1.84e-15 ***
#   first_actual1                  0.29489    0.05757   5.122 3.02e-07 ***
#   first_response1:first_actual1 -0.02998    0.05771  -0.519    0.603
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#   (Intr) frst_r1 frst_c1
# frst_rspns1 0.011
# first_actl1 0.006  0.052
# frst_rs1:_1 0.047  0.011   0.027
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')



# Predicting results of other studies -------------------------------------


# Study 2 -----------------------------------------------------------------


study_4_first_time_summary <- d %>%
  mutate(first_response = recode(first_response, "Higher" = 0, "Lower" = 1)) %>%
  group_by(story, diff_effort, ci_lower_effort, ci_upper_effort, diff_benefit, ci_lower_benefit, ci_upper_benefit) %>%
  tidyboot_mean(first_response, na.rm = TRUE)


study_2_first_second <- read.csv(here("data/study-2_tidy_data.csv")) %>%
  filter(relationship != "Equal", next_interaction != "None") %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(first_actual = relationship) %>%
  mutate(
    second_response = case_when(
      next_interaction == "Precedent" ~ first_actual,
      next_interaction == "Reciprocity" ~ ifelse(first_actual == "Higher", "Lower", "Higher"),
      next_interaction == "None" ~ "None"
    )
  )

studies_2_4_all <-
  left_join(
    study_2_first_second,
    study_4_first_time_summary,
    suffix = c("_2", "_4"),
    by = (c("story", "diff_benefit", "ci_lower_benefit", "ci_upper_benefit", "diff_effort", "ci_lower_effort", "ci_upper_effort"))
  ) %>%
  rename(expected.first.4 = empirical_stat, expected.next.2 = normalized_likert_rating) %>%
  mutate(
    first_actual = factor(first_actual, levels = c("Lower", "Higher"))
  ) %>%
  filter(second_response == "Lower")

mod <- lmer(
  data = studies_2_4_all,
  expected.next.2 ~ expected.first.4 * first_actual + (1 | subject_id) + (1 | story)
)

summary(mod)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: expected.next.2 ~ expected.first.4 * first_actual + (1 | subject_id) +      (1 | story)
#    Data: studies_2_4_all
#
# REML criterion at convergence: -719.8
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -3.3085 -0.5820 -0.0397  0.6753  2.8221
#
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.00000  0.00000
#  story      (Intercept) 0.00019  0.01378
#  Residual               0.02010  0.14177
# Number of obs: 701, groups:  subject_id, 59; story, 18
#
# Fixed effects:
#                                 Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                      0.37564    0.01453  16.19771  25.857 1.34e-14 ***
# expected.first.4                 0.24854    0.02628  16.17993   9.456 5.39e-08 ***
# first_actual1                    0.09379    0.01244 685.44630   7.539 1.50e-13 ***
# expected.first.4:first_actual1  -0.09213    0.02250 685.60749  -4.094 4.75e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#             (Intr) exp..4 frst_1
# expctd.fr.4 -0.902
# first_actl1  0.016 -0.016
# expct..4:_1 -0.016  0.018 -0.903
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


# Study 3 -----------------------------------------------------------------

study_3_first_second <- read.csv(here("data/study-3_tidy_data.csv")) %>%
  filter(relationship != "Equal", next_interaction != "None") %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(first_actual = relationship) %>%
  mutate(
    second_response = case_when(
      next_interaction == "Precedent" ~ first_actual,
      next_interaction == "Reciprocity" ~ ifelse(first_actual == "Higher", "Lower", "Higher"),
      next_interaction == "None" ~ "None"
    )
  )

studies_3_4_all <-
  left_join(
    study_3_first_second,
    study_4_first_time_summary,
    suffix = c("_3", "_4"),
    by = (c("story", "diff_benefit", "ci_lower_benefit", "ci_upper_benefit", "diff_effort", "ci_lower_effort", "ci_upper_effort"))
  ) %>%
  rename(expected.first.4 = empirical_stat, expected.next.3 = normalized_likert_rating) %>%
  mutate(
    first_actual = factor(first_actual, levels = c("Lower", "Higher"))
  ) %>%
  filter(second_response == "Lower")

mod <- lmer(
  data = studies_3_4_all,
  expected.next.3 ~ expected.first.4 * first_actual + (1 | subject_id) + (1 | story)
)

summary(mod)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: expected.next.3 ~ expected.first.4 * first_actual + (1 | subject_id) +      (1 | story)
#    Data: studies_3_4_all
#
# REML criterion at convergence: -694.4
#
# Scaled residuals:
#      Min       1Q   Median       3Q      Max
# -2.86058 -0.54419 -0.01503  0.60696  2.66935
#
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.000000 0.00000
#  story      (Intercept) 0.001108 0.03328
#  Residual               0.019738 0.14049
# Number of obs: 678, groups:  subject_id, 57; story, 18
#
# Fixed effects:
#                                 Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                      0.42286    0.02214  16.80160  19.096 7.93e-13 ***
# expected.first.4                 0.15303    0.03989  16.50593   3.836  0.00139 **
# first_actual1                    0.05363    0.01273 663.17095   4.214 2.85e-05 ***
# expected.first.4:first_actual1  -0.06150    0.02270 662.48019  -2.709  0.00693 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#             (Intr) exp..4 frst_1
# expctd.fr.4 -0.903
# first_actl1  0.012 -0.011
# expct..4:_1 -0.011  0.007 -0.905
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

# With dominance and prestige score ---------------------------------------

mod2 <- lmer(
  data = studies_3_4_all,
  expected.next.3 ~ (dominance_score + prestige_score + expected.first.4) * first_actual + (1 | subject_id) + (1 | story)
)

summary(mod2)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: expected.next.3 ~ (dominance_score + prestige_score + expected.first.4) *  
#     first_actual + (1 | subject_id) + (1 | story)
#    Data: studies_3_4_all
# 
# REML criterion at convergence: -659.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0323 -0.5802 -0.0153  0.6424  2.5525 
# 
# Random effects:
#  Groups     Name        Variance  Std.Dev.
#  subject_id (Intercept) 0.0000000 0.00000 
#  story      (Intercept) 0.0008452 0.02907 
#  Residual               0.0194478 0.13946 
# Number of obs: 678, groups:  subject_id, 57; story, 18
# 
# Fixed effects:
#                                  Estimate Std. Error         df t value Pr(>|t|)   
# (Intercept)                     2.846e-01  7.319e-02  1.411e+01   3.888  0.00162 **
# dominance_score                 5.085e-04  7.331e-04  1.396e+01   0.694  0.49931   
# prestige_score                  1.731e-03  1.006e-03  1.404e+01   1.720  0.10732   
# expected.first.4                1.548e-01  3.766e-02  1.434e+01   4.109  0.00101 **
# first_actual1                  -2.087e-02  4.498e-02  6.613e+02  -0.464  0.64281   
# dominance_score:first_actual1   1.391e-03  4.476e-04  6.586e+02   3.108  0.00196 **
# prestige_score:first_actual1    5.043e-04  6.174e-04  6.622e+02   0.817  0.41437   
# expected.first.4:first_actual1 -7.255e-02  2.327e-02  6.600e+02  -3.117  0.00190 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) dmnnc_ prstg_ exp..4 frst_1 dm_:_1 pr_:_1
# dominnc_scr -0.027                                          
# prestig_scr -0.922 -0.254                                   
# expctd.fr.4 -0.359 -0.215  0.179                            
# first_actl1 -0.001  0.026 -0.004 -0.015                     
# dmnnc_sc:_1  0.026 -0.002 -0.021 -0.024 -0.032              
# prstg_sc:_1 -0.004 -0.021  0.008  0.018 -0.922 -0.245       
# expct..4:_1 -0.015 -0.024  0.018  0.020 -0.358 -0.217  0.173
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

# Is the more complex model better?
anova(mod, mod2)

# Data: studies_3_4_all
# Models:
#   mod: expected.next.3 ~ expected.first.4 * first_actual + (1 | subject_id) + (1 | story)
# mod2: expected.next.3 ~ (dominance_score + prestige_score + expected.first.4) * first_actual + (1 | subject_id) + (1 | story)
# npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# mod     7 -706.93 -675.29 360.46  -720.93                        
# mod2   11 -716.36 -666.65 369.18  -738.36 17.434  4   0.001591 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
