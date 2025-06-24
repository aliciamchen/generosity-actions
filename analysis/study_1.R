library(here)
library(tidyverse)
library(tidyboot)
library(lme4)
library(lmerTest)
library(forcats)
library(glue)
library(emmeans)


# Options -----------------------------------------------------------------

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)


# Load and tidy data ------------------------------------------------------

d <-
  read.csv(here('data/study-1_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("repeating", "alternating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    relationship = factor(
      relationship,
      levels = c("no_info", "symmetric", "asymmetric"),
      labels = c("No info", "Symmetric", "Asymmetric")
    ),
    next_interaction = factor(
      next_interaction,
      levels = c("alternating", "repeating", "none"),
      labels = c("Reciprocity", "Precedent", "None")
    )
  ) %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating)

write_csv(d, here("data/study-1_tidy_data.csv"))

# Demographic info --------------------------------------------------------

print(length(unique(d$subject_id)))

d.demographics <-
  read.csv(here('data/study-1_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)


# Stats -------------------------------------------------------------------


mod <-
  lmer(likert_rating ~ 1 + next_interaction * relationship
       + (1 | story) + (1 | subject_id),
       data = d)

# mod <-
#   lmer(likert_rating ~ 1 + next_interaction * relationship
#        + (1 + next_interaction | story) + (1 + relationship | subject_id),
#        data = d)

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)
# next_interaction              3529.5 1764.74     2 3095.1 952.5011 <2e-16 ***
# relationship                    12.3    6.14     2 3104.8   3.3166 0.0364 *
# next_interaction:relationship  468.2  117.05     4 3095.1  63.1770 <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


emmeans(mod, revpairwise ~ next_interaction |
          relationship) %>% summary(infer = T)

# $emmeans
# relationship = No info:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# Reciprocity        4.61 0.0896 158     4.43     4.78  51.407  <.0001
# Precedent          4.75 0.0896 158     4.57     4.93  52.983  <.0001
# None               2.38 0.0897 158     2.20     2.56  26.529  <.0001
#
# relationship = Symmetric:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# Reciprocity        4.75 0.0897 158     4.58     4.93  52.992  <.0001
# Precedent          4.49 0.0897 158     4.31     4.67  50.044  <.0001
# None               2.16 0.0897 158     1.98     2.33  24.045  <.0001
#
# relationship = Asymmetric:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# Reciprocity        3.48 0.0896 158     3.30     3.65  38.799  <.0001
# Precedent          5.25 0.0898 159     5.07     5.43  58.455  <.0001
# None               2.57 0.0897 158     2.39     2.75  28.667  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# relationship = No info:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# Precedent - Reciprocity    0.141 0.102 3095  -0.0987   0.3811   1.381  0.3512
# None - Reciprocity        -2.227 0.102 3095  -2.4675  -1.9873 -21.755  <.0001
# None - Precedent          -2.369 0.102 3095  -2.6087  -2.1286 -23.135  <.0001
#
# relationship = Symmetric:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# Precedent - Reciprocity   -0.265 0.102 3095  -0.5048  -0.0243  -2.582  0.0267
# None - Reciprocity        -2.597 0.102 3095  -2.8371  -2.3566 -25.346  <.0001
# None - Precedent          -2.332 0.102 3095  -2.5725  -2.0920 -22.763  <.0001
#
# relationship = Asymmetric:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# Precedent - Reciprocity    1.771 0.102 3095   1.5312   2.0117  17.290  <.0001
# None - Reciprocity        -0.906 0.102 3095  -1.1456  -0.6654  -8.844  <.0001
# None - Precedent          -2.677 0.103 3095  -2.9174  -2.4366 -26.109  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates


# Do people expect a continued social interaction, both with and without the relationship?


emm <-
  mod %>% emmeans(~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("no", "yes", "yes"))


emmeans(emm, revpairwise ~ interaction_present |
          relationship_present) %>%
  summary(infer = T)

# $emmeans
# relationship_present = no:
#   interaction_present emmean     SE    df lower.CL upper.CL t.ratio p.value
# no                    2.38 0.0897 158.5     2.20     2.56  26.529  <.0001
# yes                   4.68 0.0736  72.6     4.53     4.82  63.567  <.0001
#
# relationship_present = yes:
#   interaction_present emmean     SE    df lower.CL upper.CL t.ratio p.value
# no                    2.36 0.0736  72.8     2.22     2.51  32.123  <.0001
# yes                   4.49 0.0641  41.8     4.36     4.62  70.114  <.0001
#
# Results are averaged over the levels of: relationship, next_interaction
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# relationship_present = no:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# yes - no     2.30 0.0887 3095     2.12     2.47  25.911  <.0001
#
# relationship_present = yes:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# yes - no     2.13 0.0627 3095     2.00     2.25  33.915  <.0001
#
# Results are averaged over the levels of: relationship, next_interaction
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95

# HEREEEEE


# Repeat analyses without control levels ----------------------------------


d_filtered <- d %>%
  filter(next_interaction != "None" & relationship != "No info")

mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 |
                                                                             subject_id),
            data = d_filtered)

summary(mod)


emmeans(mod, revpairwise ~ next_interaction | relationship) %>%
  summary(infer = T)


emm <-
  mod %>% emmeans(~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("no", "yes", "yes"))


emmeans(emm, revpairwise ~ interaction_present |
          relationship_present) %>%
  summary(infer = T)



# Repeat with normalized values -------------------------------------------


# With all levels
mod <-
  lmer(
    normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                        story) + (1 |
                                                                                    subject_id),
    data = d
  )

summary(mod)

anova(mod, type = "III")

emmeans(mod, revpairwise ~ next_interaction |
          relationship) %>% summary(infer = T)


# Do people expect a continued social interaction, both with and without the relationship?
emm <-
  mod %>% emmeans(~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("no", "yes", "yes"))


emmeans(emm, revpairwise ~ interaction_present |
          relationship_present) %>%
  summary(infer = T)

# Without control levels

mod <- lmer(
  normalized_likert_rating ~ next_interaction * relationship + (1 |
                                                                  story) + (1 |
                                                                              subject_id),
  data = d_filtered
)

summary(mod)


emmeans(mod, revpairwise ~ next_interaction | relationship) %>%
  summary(infer = T)


emm <-
  mod %>% emmeans(~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("no", "yes", "yes"))


emmeans(emm, revpairwise ~ interaction_present |
          relationship_present) %>%
  summary(infer = T)

