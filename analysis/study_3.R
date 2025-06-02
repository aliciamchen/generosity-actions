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

# Concrete relationship info
concrete.relationships <- read.csv(here("analysis/concrete_relationships.csv")) %>%
  mutate(relationship = case_when(
    relationship == "more" ~ "Higher",
    relationship == "less" ~ "Lower",
    relationship == "equal" ~ "Equal"
  ))

concrete.relationship.dims <- read.csv(here("analysis/USA_dim_rel_30d.csv"), check.names = FALSE) %>% 
  rename(feature = 1) %>%
  pivot_longer(
    cols = -feature,
    names_to = "concrete_relationship",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = feature,
    values_from = value
  )

d <-
  read.csv(here('data/study-3_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
  left_join(scenarios.diffs) %>% 
  left_join(concrete.relationships, by = c("story", "relationship")) %>%
  left_join(
    concrete.relationship.dims %>%
      select(concrete_relationship, conflict = Conflict, coercion = Coercion, importance_society = `Importance for society`, importance_individuals = `Importance for individuals`, information_exchange = `Information Exchange`),
    by = "concrete_relationship"
  ) %>%
  mutate(dominance_score = (conflict + coercion) / 2,
         prestige_score = (importance_society + importance_individuals + information_exchange) / 3
  )


d.benefit.effort <- d %>% filter(relationship != "Equal", next_interaction != "None") %>% 
  select(-normalized_likert_rating) %>%
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(prec_minus_rec = Precedent - Reciprocity,
         p_prec = (Precedent / (Precedent + Reciprocity))) %>%
  ungroup()

d <- d %>% 
  pivot_wider(names_from = "type", values_from = c("n", "diff", "ci_lower", "ci_upper", "mean"))

write_csv(d, here("data/study-3_tidy_data.csv"))
write_csv(d.benefit.effort, here("data/study-3_benefit_effort.csv"))


# Demographic info --------------------------------------------------------

print(length(unique(d$subject_id)))

d.demographics <- read.csv(here('data/study-3_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))


# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

anova(mod, type="III")

# First, replicate asymmetric/symmetric results from 1 and 2
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("no", "yes", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

# $emmeans
# asymmetry_present = no:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# Reciprocity        5.13 0.1110 179     4.91     5.34  46.138  <.0001
# Precedent          4.88 0.1110 178     4.66     5.10  43.954  <.0001
# None               1.98 0.1110 179     1.76     2.20  17.839  <.0001
# 
# asymmetry_present = yes:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# Reciprocity        4.67 0.0971 105     4.47     4.86  48.072  <.0001
# Precedent          5.10 0.0971 105     4.91     5.29  52.534  <.0001
# None               2.14 0.0971 105     1.94     2.33  22.009  <.0001
# 
# Results are averaged over the levels of: relationship 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# asymmetry_present = no:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# Reciprocity - Precedent    0.246 0.1080 2982 -0.00637    0.499   2.286  0.0580
# Reciprocity - None         3.144 0.1080 2982  2.89114    3.396  29.177  <.0001
# Precedent - None           2.898 0.1080 2982  2.64524    3.150  26.913  <.0001
# 
# asymmetry_present = yes:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# Reciprocity - Precedent   -0.434 0.0762 2982 -0.61299   -0.256  -5.703  <.0001
# Reciprocity - None         2.531 0.0761 2982  2.35243    2.709  33.254  <.0001
# Precedent - None           2.965 0.0761 2982  2.78675    3.144  38.946  <.0001
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
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# Equal          5.13 0.111 179     4.91     5.34  46.138  <.0001
# Higher         4.67 0.111 181     4.45     4.89  41.938  <.0001
# Lower          4.66 0.111 177     4.45     4.88  42.092  <.0001
# 
# next_interaction = Precedent:
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# Equal          4.88 0.111 178     4.66     5.10  43.954  <.0001
# Higher         5.11 0.111 181     4.89     5.33  45.830  <.0001
# Lower          5.10 0.111 177     4.88     5.32  45.988  <.0001
# 
# next_interaction = None:
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# Equal          1.98 0.111 179     1.76     2.20  17.839  <.0001
# Higher         2.07 0.111 181     1.85     2.29  18.607  <.0001
# Lower          2.20 0.111 177     1.98     2.42  19.873  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $contrasts
# next_interaction = Reciprocity:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Higher - Equal  -0.4558 0.108 2992  -0.7095   -0.202  -4.212  0.0001
# Lower - Equal   -0.4611 0.108 2992  -0.7134   -0.209  -4.284  0.0001
# Lower - Higher  -0.0053 0.108 2991  -0.2585    0.248  -0.049  0.9987
# 
# next_interaction = Precedent:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Higher - Equal   0.2273 0.108 2992  -0.0263    0.481   2.101  0.0897
# Lower - Equal    0.2168 0.108 2992  -0.0354    0.469   2.016  0.1086
# Lower - Higher  -0.0105 0.108 2991  -0.2639    0.243  -0.097  0.9948
# 
# next_interaction = None:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# Higher - Equal   0.0901 0.108 2992  -0.1636    0.344   0.833  0.6825
# Lower - Equal    0.2189 0.108 2992  -0.0334    0.471   2.035  0.1042
# Lower - Higher   0.1288 0.108 2991  -0.1242    0.382   1.193  0.4572
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

# replicate asymmetric/symmetric results from prev studies
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("no", "yes", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

mod %>% emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)


