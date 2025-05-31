library(here)
library(tidyverse)
library(tidyboot)
library(emmeans)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)

# Options -----------------------------------------------------------------

options(warn = -1)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

action.colors <- c("#DD8D29", "#E2D200", "#D6D6D6")
relationship.colors <- c("#B87FFF", "#35ACFF", "#D6D6D6")



# Load and tidy data ------------------------------------------------------

validation.benefit <- read.csv(here("data/validation_benefit_diff.csv"))
validation.effort <- read.csv(here("data/validation_effort_diff.csv"))

concrete.relationships <- read.csv(here("analysis/concrete_relationships.csv")) %>%
  mutate(relationship = case_when(
    relationship == "more" ~ "higher",
    relationship == "less" ~ "lower",
    .default = relationship
  ))

concrete.relationship.dims <- read.csv(here("analysis/USA_dim_rel_30d.csv"), check.names = FALSE)

concrete.relationship.dims <- concrete.relationship.dims %>%
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
  read.csv(here("data/study-4_data.csv")) %>%
  filter(pass_attention == T, understood == "yes") %>%
  mutate(relationship = case_when(
    relationship == "more" ~ "higher",
    relationship == "less" ~ "lower",
    .default = relationship
  )) %>%
  pivot_longer(
    cols = c("repeating", "alternating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    next_interaction = fct_relevel(
      next_interaction,
      "repeating", "alternating", "none"
    ),
    relationship = fct_relevel(
      relationship,
      "higher", "equal", "lower"
    )
  ) %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  left_join(validation.benefit %>% select(story, diff), by = "story") %>%
  rename(benefit_diff = diff) %>%
  left_join(validation.effort %>% select(story, diff), by = "story") %>%
  rename(effort_diff = diff) %>%
  left_join(concrete.relationships, by = c("story", "relationship")) %>%
  left_join(
    concrete.relationship.dims %>%
      select(concrete_relationship, conflict = Conflict, coercion = Coercion, importance_society = `Importance for society`, importance_individuals = `Importance for individuals`, information_exchange = `Information Exchange`),
    by = "concrete_relationship"
  ) %>%
  mutate(dominance_score = (conflict + coercion) / 2,
    prestige_score = (importance_society + importance_individuals + information_exchange) / 3
  )




write.csv(d, here("data/study-4_tidy_data.csv"), row.names = F)

d.benefit.effort <- d %>%
  filter(relationship != "equal", next_interaction != "none") %>%
  select(-normalized_likert_rating) %>%
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(rep_minus_alt = repeating - alternating) %>%
  ungroup()


# Demographic info --------------------------------------------------------


d.demographics <- read.csv(here("data/study-4_demographics.csv"))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

print(length(unique(d$subject_id)))


# Stats -------------------------------------------------------------------

mod <- lmer(
  likert_rating ~ 1 + next_interaction * relationship + (1 |
    story) + (1 | subject_id),
  data = d
)

summary(mod)

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq Mean Sq NumDF  DenDF   F value    Pr(>F)
# next_interaction              5510.7 2755.35     2 2982.1 1396.2119 < 2.2e-16 ***
# relationship                     1.2    0.60     2 3001.8    0.3060    0.7364
# next_interaction:relationship   65.9   16.49     4 2982.1    8.3539 1.066e-06 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>%
  emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

# asymmetry_present = no:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating          4.88 0.1110 178     4.66     5.10  43.954  <.0001
# alternating        5.13 0.1110 179     4.91     5.34  46.138  <.0001
# none               1.98 0.1110 179     1.76     2.20  17.839  <.0001
#
# asymmetry_present = yes:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating          5.10 0.0971 105     4.91     5.29  52.534  <.0001
# alternating        4.67 0.0971 105     4.47     4.86  48.072  <.0001
# none               2.14 0.0971 105     1.94     2.33  22.009  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# asymmetry_present = no:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.246 0.1080 2982   -0.499  0.00637  -2.286  0.0580
# repeating - none           2.898 0.1080 2982    2.645  3.15018  26.913  <.0001
# alternating - none         3.144 0.1080 2982    2.891  3.39646  29.177  <.0001
#
# asymmetry_present = yes:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.434 0.0762 2982    0.256  0.61299   5.703  <.0001
# repeating - none           2.965 0.0761 2982    2.787  3.14382  38.946  <.0001
# alternating - none         2.531 0.0761 2982    2.352  2.70936  33.254  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# Main effects - difference between alternating and repeating based on the specific relationship
mod %>%
  emmeans(revpairwise ~ next_interaction | relationship) %>%
  summary(infer = T)

# $emmeans
# relationship = higher:
#   next_interaction emmean    SE  df lower.CL upper.CL t.ratio p.value
# repeating          5.11 0.111 181     4.89     5.33  45.830  <.0001
# alternating        4.67 0.111 181     4.45     4.89  41.938  <.0001
# none               2.07 0.111 181     1.85     2.29  18.607  <.0001
#
# relationship = equal:
#   next_interaction emmean    SE  df lower.CL upper.CL t.ratio p.value
# repeating          4.88 0.111 178     4.66     5.10  43.954  <.0001
# alternating        5.13 0.111 179     4.91     5.34  46.138  <.0001
# none               1.98 0.111 179     1.76     2.20  17.839  <.0001
#
# relationship = lower:
#   next_interaction emmean    SE  df lower.CL upper.CL t.ratio p.value
# repeating          5.10 0.111 177     4.88     5.32  45.988  <.0001
# alternating        4.66 0.111 177     4.45     4.88  42.092  <.0001
# none               2.20 0.111 177     1.98     2.42  19.873  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# relationship = higher:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating   -0.437 0.108 2982 -0.69097   -0.183  -4.035  0.0002
# none - repeating          -3.035 0.108 2982 -3.28889   -2.781 -28.022  <.0001
# none - alternating        -2.598 0.108 2982 -2.85169   -2.344 -24.005  <.0001
#
# relationship = equal:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating    0.246 0.108 2982 -0.00637    0.499   2.286  0.0580
# none - repeating          -2.898 0.108 2982 -3.15018   -2.645 -26.913  <.0001
# none - alternating        -3.144 0.108 2982 -3.39646   -2.891 -29.177  <.0001
#
# relationship = lower:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating   -0.432 0.107 2982 -0.68295   -0.181  -4.031  0.0002
# none - repeating          -2.896 0.107 2982 -3.14664   -2.645 -27.052  <.0001
# none - alternating        -2.464 0.107 2982 -2.71486   -2.213 -23.019  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# are there differences in expectations for repeating actions, based on the direction of the asymmetry?
mod %>%
  emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)

# $emmeans
# next_interaction = repeating:
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# higher         5.11 0.111 181     4.89     5.33  45.830  <.0001
# equal          4.88 0.111 178     4.66     5.10  43.954  <.0001
# lower          5.10 0.111 177     4.88     5.32  45.988  <.0001
#
# next_interaction = alternating:
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# higher         4.67 0.111 181     4.45     4.89  41.938  <.0001
# equal          5.13 0.111 179     4.91     5.34  46.138  <.0001
# lower          4.66 0.111 177     4.45     4.88  42.092  <.0001
#
# next_interaction = none:
#   relationship emmean    SE  df lower.CL upper.CL t.ratio p.value
# higher         2.07 0.111 181     1.85     2.29  18.607  <.0001
# equal          1.98 0.111 179     1.76     2.20  17.839  <.0001
# lower          2.20 0.111 177     1.98     2.42  19.873  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# next_interaction = repeating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher  -0.2273 0.108 2992  -0.4810   0.0263  -2.101  0.0897
# lower - higher  -0.0105 0.108 2991  -0.2639   0.2428  -0.097  0.9948
# lower - equal    0.2168 0.108 2992  -0.0354   0.4690   2.016  0.1086
#
# next_interaction = alternating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher   0.4558 0.108 2992   0.2021   0.7095   4.212  0.0001
# lower - higher  -0.0053 0.108 2991  -0.2585   0.2479  -0.049  0.9987
# lower - equal   -0.4611 0.108 2992  -0.7134  -0.2087  -4.284  0.0001
#
# next_interaction = none:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher  -0.0901 0.108 2992  -0.3438   0.1636  -0.833  0.6825
# lower - higher   0.1288 0.108 2991  -0.1242   0.3817   1.193  0.4572
# lower - equal    0.2189 0.108 2992  -0.0334   0.4711   2.035  0.1042
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

emmeans(emm_symmetry, pairwise ~ asymmetry_present | next_interaction) %>%
  summary(infer = T)

# $emmeans
# next_interaction = repeating:
#   asymmetry_present emmean     SE  df lower.CL upper.CL t.ratio p.value
# no                  4.88 0.1110 178     4.66     5.10  43.954  <.0001
# yes                 5.10 0.0971 105     4.91     5.29  52.534  <.0001
#
# next_interaction = alternating:
#   asymmetry_present emmean     SE  df lower.CL upper.CL t.ratio p.value
# no                  5.13 0.1110 179     4.91     5.34  46.138  <.0001
# yes                 4.67 0.0971 105     4.47     4.86  48.072  <.0001
#
# next_interaction = none:
#   asymmetry_present emmean     SE  df lower.CL upper.CL t.ratio p.value
# no                  1.98 0.1110 179     1.76     2.20  17.839  <.0001
# yes                 2.14 0.0971 105     1.94     2.33  22.009  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# next_interaction = repeating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes   -0.222 0.0934 2992   -0.405  -0.0390  -2.378  0.0175
#
# next_interaction = alternating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes    0.458 0.0934 2992    0.275   0.6416   4.906  <.0001
#
# next_interaction = none:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes   -0.154 0.0934 2992   -0.338   0.0287  -1.654  0.0983
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95



# interaction effects
contrast(emmeans(mod, ~ next_interaction * relationship), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# next_interaction_pairwise relationship_pairwise estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   higher - equal         0.68310 0.153 2982    0.384    0.983   4.473  <.0001
# repeating - none          higher - equal         0.13722 0.153 2982   -0.162    0.437   0.899  0.3690
# alternating - none        higher - equal        -0.54588 0.153 2982   -0.845   -0.246  -3.574  0.0004
# repeating - alternating   higher - lower         0.00523 0.152 2982   -0.293    0.304   0.034  0.9726
# repeating - none          higher - lower         0.13928 0.152 2982   -0.159    0.438   0.915  0.3604
# alternating - none        higher - lower         0.13405 0.152 2982   -0.164    0.433   0.881  0.3786
# repeating - alternating   equal - lower         -0.67787 0.152 2982   -0.976   -0.380  -4.463  <.0001
# repeating - none          equal - lower          0.00206 0.152 2982   -0.296    0.300   0.014  0.9892
# alternating - none        equal - lower          0.67993 0.152 2982    0.382    0.978   4.477  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95


contrast(emmeans(emm_symmetry, pairwise ~ next_interaction * asymmetry_present), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# benefit / effort

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
  data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
  data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
  data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)


mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
  data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

# dominance / prestige

mod <- lmer(rep_minus_alt ~ 1 + dominance_score + (1 | story) + (1 | subject_id),
  data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + prestige_score + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)


mod <- lmer(rep_minus_alt ~ 1 + dominance_score  + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + prestige_score  + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

# try putting predictors together

mod <- lmer(rep_minus_alt ~ 1 + dominance_score + prestige_score + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)


mod <- lmer(rep_minus_alt ~ 1 + dominance_score + prestige_score + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

# try all predictors

mod <- lmer(rep_minus_alt ~ 1 + dominance_score + prestige_score + effort_diff + benefit_diff +  (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod)

mod_simple <- lmer(rep_minus_alt ~ 1 + effort_diff + benefit_diff +  (1 | story) + (1 | subject_id),
                   data = d.benefit.effort %>% filter(relationship == "higher")
)

summary(mod_simple)

anova(mod_simple, mod)

mod <- lmer(rep_minus_alt ~ 1 + dominance_score + prestige_score + effort_diff + benefit_diff +  (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod)

mod_simple <- lmer(rep_minus_alt ~ 1 + effort_diff + benefit_diff +  (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower")
)

summary(mod_simple)

anova(mod_simple, mod)

##################################################

## Repeat all analyses with normalized values

# With all levels
mod <- lmer(
  normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
    story) + (1 | subject_id),
  data = d
)

summary(mod)

anova(mod, type = "III")

# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>%
  emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)


# Main effects - difference between alternating and repeating based on the specific relationship
mod %>%
  emmeans(revpairwise ~ next_interaction | relationship) %>%
  summary(infer = T)


# are there differences in expectations for repeating actions, based on the direction of the asymmetry?
mod %>%
  emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)


emmeans(emm_symmetry, pairwise ~ asymmetry_present | next_interaction) %>%
  summary(infer = T)


# interaction effects
contrast(emmeans(mod, ~ next_interaction * relationship), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

contrast(emmeans(emm_symmetry, pairwise ~ next_interaction * asymmetry_present), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)



# Plots -------------------------------------------------------------



# Dominance and prestige


d.b.e.means <- d.benefit.effort %>%
  group_by(story, relationship, benefit_diff, effort_diff, dominance_score, prestige_score) %>%
  tidyboot_mean(rep_minus_alt, na.rm = TRUE)

ggplot(d.b.e.means, aes(x = prestige_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = prestige_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "prestige score",
    y = "precedent minus reciprocity"
  ) + 
  facet_wrap(~ relationship, ncol = 2) 

ggplot(d.b.e.means, aes(x = dominance_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = dominance_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "dominance score",
    y = "precedent minus reciprocity"
  ) + 
  facet_wrap(~ relationship, ncol = 2) 



ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = prestige_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = prestige_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "prestige score",
    y = "repeated minus alternated",
    title = "Generous actor higher-ranked"
  )

ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = prestige_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = prestige_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "prestige score",
    y = "repeated minus alternated",
    title = "Generous actor lower-ranked"
  )

ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = dominance_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = dominance_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "dominance score",
    y = "repeated minus alternated",
    title = "Generous actor higher-ranked"
  )

ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = dominance_score, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = dominance_score, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "dominance score",
    y = "repeated minus alternated",
    title = "Generous actor lower-ranked"
  )

f <- ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "relative benefit to recipient",
    y = "repeated minus alternated",
    title = "A is lower status"
  )

f

# ggsave(here("figures/outputs/1b_benefit_lower.pdf"),
#        width = 8.7,
#        height = 4)

f <- ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "relative benefit to recipient",
    y = "repeated minus alternated",
    title = "A is higher status"
  )

f

# ggsave(here("figures/outputs/1b_benefit_higher.pdf"),
#        width = 8.7,
#        height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = effort_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "relative effort",
    y = "repeated minus alternated",
    title = "A is lower status"
  )

f

# ggsave(here("figures/outputs/1b_effort_lower.pdf"),
#        width = 8.7,
#        height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = effort_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax = ci_upper), size = 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "relative effort",
    y = "repeated minus alternated",
    title = "A is higher status"
  )

f

# ggsave(here("figures/outputs/1b_effort_higher.pdf"),
#        width = 8.7,
#        height = 4)
