library(here)
library(tidyverse)
library(tidyboot)
library(emmeans)
library(lme4)
library(lmerTest)
library(forcats)
library(glue)

# Options -----------------------------------------------------------------

options(warn = -1)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Load and tidy data ------------------------------------------------------

scenarios.diffs <- read.csv(here("data/scenarios_diffs.csv"))

d <-
  read.csv(here('data/study-2_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
      levels = c("equal", "less", "more"),
      labels = c("Equal", "Lower", "Higher")
    ),
    next_interaction = factor(
      next_interaction,
      levels = c("alternating", "repeating", "none"),
      labels = c("Reciprocity", "Precedent", "None")
    )
  ) %>% 
  group_by(subject_id, story, relationship) %>%
  mutate(total_rating = sum(likert_rating, na.rm = T),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating) %>% 
  left_join(scenarios.diffs)


write_csv(d, here("data/study-2_tidy_data.csv"))

d.benefit.effort <- d %>% filter(relationship != "Equal", next_interaction != "None") %>% select(-normalized_likert_rating) %>%
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(prec_minus_rec = Precedent - Reciprocity,
         p_prec = (Precedent / (Precedent + Reciprocity))) %>%
  ungroup()

write_csv(d.benefit.effort, here("data/study-2_benefit_effort.csv"))


# Demographic info --------------------------------------------------------

print(length(unique(d$subject_id)))

d.demographics <- read.csv(here('data/study-2_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))


# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

anova(mod, type="III")

# First, replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("no", "yes", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

# $emmeans
# asymmetry_present = no:
#   next_interaction emmean     SE    df lower.CL upper.CL t.ratio p.value
# Reciprocity        4.98 0.0937 161.8     4.79     5.16  53.073  <.0001
# Precedent          4.48 0.0938 162.3     4.30     4.67  47.796  <.0001
# None               2.29 0.0938 162.3     2.10     2.47  24.385  <.0001
# 
# asymmetry_present = yes:
#   next_interaction emmean     SE    df lower.CL upper.CL t.ratio p.value
# Reciprocity        4.06 0.0784  79.8     3.90     4.22  51.802  <.0001
# Precedent          4.86 0.0784  79.9     4.71     5.02  62.045  <.0001
# None               2.59 0.0783  79.5     2.43     2.75  33.080  <.0001
# 
# Results are averaged over the levels of: relationship 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# asymmetry_present = no:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# Reciprocity - Precedent    0.491 0.1030 3090    0.249    0.732   4.764  <.0001
# Reciprocity - None         2.687 0.1030 3090    2.446    2.929  26.089  <.0001
# Precedent - None           2.197 0.1030 3090    1.955    2.438  21.310  <.0001
# 
# asymmetry_present = yes:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# Reciprocity - Precedent   -0.804 0.0729 3090   -0.975   -0.633 -11.039  <.0001
# Reciprocity - None         1.470 0.0728 3090    1.299    1.640  20.200  <.0001
# Precedent - None           2.274 0.0728 3090    2.103    2.445  31.244  <.0001
# 
# Results are averaged over the levels of: relationship 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 3 estimates 
# P value adjustment: tukey method for comparing a family of 3 estimates 


# What about equal/higher/lower?
mod %>% emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)

# $emmeans
# next_interaction = Reciprocity:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# Equal          4.98 0.0937 162     4.79     5.16  53.073  <.0001
# Lower          4.06 0.0940 164     3.88     4.25  43.187  <.0001
# Higher         4.06 0.0936 161     3.87     4.24  43.348  <.0001
# 
# next_interaction = Precedent:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# Equal          4.48 0.0938 162     4.30     4.67  47.796  <.0001
# Lower          4.86 0.0940 164     4.67     5.04  51.639  <.0001
# Higher         4.87 0.0937 161     4.69     5.06  51.997  <.0001
# 
# next_interaction = None:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# Equal          2.29 0.0938 162     2.10     2.47  24.385  <.0001
# Lower          2.60 0.0939 162     2.41     2.79  27.695  <.0001
# Higher         2.58 0.0935 160     2.40     2.77  27.586  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# next_interaction = Reciprocity:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Lower - Equal  -0.91398 0.103 3093  -1.1560   -0.672  -8.855  <.0001
# Higher - Equal -0.91664 0.103 3092  -1.1578   -0.676  -8.914  <.0001
# Higher - Lower -0.00266 0.103 3098  -0.2446    0.239  -0.026  0.9996
# 
# next_interaction = Precedent:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Lower - Equal   0.37134 0.103 3093   0.1291    0.614   3.595  0.0010
# Higher - Equal  0.38809 0.103 3092   0.1466    0.630   3.768  0.0005
# Higher - Lower  0.01675 0.103 3098  -0.2254    0.259   0.162  0.9856
# 
# next_interaction = None:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Lower - Equal   0.31183 0.103 3093   0.0700    0.554   3.023  0.0071
# Higher - Equal  0.29265 0.103 3092   0.0515    0.534   2.846  0.0124
# Higher - Lower -0.01918 0.103 3098  -0.2606    0.222  -0.186  0.9811
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# Conf-level adjustment: tukey method for comparing a family of 3 estimates 
# P value adjustment: tukey method for comparing a family of 3 estimates 


# Exploratory analyses ----------------------------------------------------

# benefit / effort

mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "benefit"))

summary(mod)

mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "effort"))

summary(mod)



mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "benefit"))

summary(mod)

mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "effort"))

summary(mod)








##################################################

## Repeat all analyses with normalized values

# With all levels
mod <- lmer(normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

anova(mod, type="III")

# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("no", "yes", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

mod %>% emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)


