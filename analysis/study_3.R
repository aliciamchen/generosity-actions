library(here)
library(tidyverse)
library(tidyboot)
library(emmeans)
library(lme4)
library(lmerTest)
library(forcats)
library(glue)
library(effectsize)
library(BayesFactor)

source(here("analysis/stats_helpers.R"))
set_stats_file("study_3")

# Options -----------------------------------------------------------------

options(warn = -1)

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

n_subjects <- length(unique(d$subject_id))
print(n_subjects)

d.demographics <- read.csv(here('data/study-3_demographics.csv'))
d.demographics |> count(gender)
d.demographics |> summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

write_demographics("studyThree", d.demographics, n_subjects)


# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

cat("\n--- Standardized Parameters ---\n")
print(standardize_parameters(mod))

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
emm_rel <- emmeans(mod, ~ relationship | next_interaction)
contrast(emm_rel, method = "revpairwise") %>% summary(infer = T)

cat("\n--- Effect Sizes (Cohen's d) for relationship contrasts ---\n")
s3_effect_sizes <- eff_size(emm_rel, sigma = sigma(mod), edf = df.residual(mod))
print(s3_effect_sizes)

# Export EMMs and contrasts to tex
s3_sym_emms <- summary(emmeans(emm_symmetry, ~ next_interaction | asymmetry_present))
s3_sym_cons <- summary(emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present), infer = TRUE, adjust = "none")$contrasts

for (asym in c("no", "yes")) {
  asym_label <- switch(asym, no = "Equal", yes = "Asym")
  for (ni in c("Reciprocity", "Precedent", "None")) {
    ni_label <- switch(ni, Reciprocity = "Recip", Precedent = "Prec", None = "None")
    row <- s3_sym_emms |> filter(asymmetry_present == asym, next_interaction == ni)
    write_emm(paste0("studyThree", ni_label, asym_label), row)
  }
}

row <- s3_sym_cons |> filter(asymmetry_present == "yes", contrast == "Reciprocity - Precedent")
write_contrast("studyThreeRecipVsPrecAsym", row, stat_type = "t")
row <- s3_sym_cons |> filter(asymmetry_present == "no", contrast == "Reciprocity - Precedent")
write_contrast("studyThreeRecipVsPrecEqual", row, stat_type = "t")

# Higher vs Lower on Precedent
s3_rel_emms <- summary(emm_rel)
s3_rel_cons <- summary(contrast(emm_rel, method = "revpairwise"), infer = TRUE, adjust = "none")

for (rel in c("Equal", "Lower", "Higher")) {
  row <- s3_rel_emms |> filter(next_interaction == "Precedent", relationship == rel)
  write_emm(paste0("studyThreePrec", rel), row)
}
row <- s3_rel_cons |> filter(next_interaction == "Precedent", contrast == "Lower - Higher")
write_contrast("studyThreePrecLowerVsHigher", row, stat_type = "t")

# Cohen's d for Precedent vs Reciprocity within Asym and Equal
s3_sym_es <- eff_size(
  emmeans(emm_symmetry, ~ next_interaction | asymmetry_present),
  sigma = sigma(mod), edf = df.residual(mod)
)
s3_sym_es_df <- as.data.frame(s3_sym_es)
for (asym in c("no", "yes")) {
  asym_label <- switch(asym, no = "Equal", yes = "Asym")
  es_row <- s3_sym_es_df |> filter(
    asymmetry_present == asym,
    grepl("Reciprocity - Precedent|Precedent - Reciprocity", contrast)
  )
  if (nrow(es_row) > 0) {
    write_cohens_d(paste0("studyThreeRecipVsPrec", asym_label), es_row$effect.size[1])
  }
}

# Cohen's d for Precedent Higher vs Lower
s3_es_df <- as.data.frame(s3_effect_sizes)
es_row <- s3_es_df |> filter(next_interaction == "Precedent", grepl("Higher - Lower|Lower - Higher", contrast))
if (nrow(es_row) > 0) {
  write_cohens_d("studyThreePrecLowerVsHigher", es_row$effect.size[1])
}

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


# Bayes Factor for Higher vs Lower null on Precedent
cat("\n--- Bayes Factors for key null results ---\n")
d_bf_prec <- d %>% filter(next_interaction == "Precedent", relationship != "Equal") %>%
  select(likert_rating, relationship, subject_id, story) %>%
  drop_na() %>% as.data.frame()
d_bf_prec$subject_id <- factor(d_bf_prec$subject_id)
d_bf_prec$story <- factor(d_bf_prec$story)
bf_prec <- lmBF(likert_rating ~ relationship + subject_id + story,
                data = d_bf_prec,
                whichRandom = c("subject_id", "story"))
bf_prec_null <- lmBF(likert_rating ~ subject_id + story,
                     data = d_bf_prec,
                     whichRandom = c("subject_id", "story"))
cat("BF01 for Higher vs Lower on Precedent (Study 3):\n")
print(1 / (bf_prec / bf_prec_null))
write_bf("studyThreePrecHigherVsLower", extractBF(bf_prec / bf_prec_null)$bf |> {\(x) 1/x}())

# Exploratory analyses ----------------------------------------------------

# benefit / effort

mod_higher_benefit <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "benefit"))
summary(mod_higher_benefit)
export_lmer_coef("studyThreeHigherBenefit", mod_higher_benefit)

mod_higher_effort <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "effort"))
summary(mod_higher_effort)
export_lmer_coef("studyThreeHigherEffort", mod_higher_effort)

mod_lower_benefit <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "benefit"))
summary(mod_lower_benefit)
export_lmer_coef("studyThreeLowerBenefit", mod_lower_benefit)

mod_lower_effort <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "effort"))
summary(mod_lower_effort)
export_lmer_coef("studyThreeLowerEffort", mod_lower_effort)



# BF for cost/benefit not moderating precedent
d_bf_cost <- d %>% filter(relationship != "Equal", next_interaction == "Precedent") %>%
  select(likert_rating, diff_effort, diff_benefit, subject_id, story) %>%
  drop_na() %>% as.data.frame()
d_bf_cost$subject_id <- factor(d_bf_cost$subject_id)
d_bf_cost$story <- factor(d_bf_cost$story)
# For effort
bf_effort <- lmBF(likert_rating ~ diff_effort + subject_id + story,
                  data = d_bf_cost,
                  whichRandom = c("subject_id", "story"))
bf_effort_null <- lmBF(likert_rating ~ subject_id + story,
                       data = d_bf_cost,
                       whichRandom = c("subject_id", "story"))
cat("BF01 for effort not moderating precedent (Study 3):\n")
print(1 / (bf_effort / bf_effort_null))
write_bf("studyThreeEffortModPrec", extractBF(bf_effort / bf_effort_null)$bf |> {\(x) 1/x}())

# For benefit
bf_benefit <- lmBF(likert_rating ~ diff_benefit + subject_id + story,
                   data = d_bf_cost,
                   whichRandom = c("subject_id", "story"))
bf_benefit_null <- lmBF(likert_rating ~ subject_id + story,
                        data = d_bf_cost,
                        whichRandom = c("subject_id", "story"))
cat("BF01 for benefit not moderating precedent (Study 3):\n")
print(1 / (bf_benefit / bf_benefit_null))
write_bf("studyThreeBenefitModPrec", extractBF(bf_benefit / bf_benefit_null)$bf |> {\(x) 1/x}())

# Full model with Study 2 benefit / effort

benefit.effort.2 <- read.csv(here("data/study-2_benefit_effort.csv")) %>%
  mutate(relationship = factor(relationship, levels = c("Equal", "Lower", "Higher")),
         study = "Study 2")

study.3.benefit.effort <- d.benefit.effort %>%
  mutate(study = "Study 3")

benefit.effort.all <- bind_rows(benefit.effort.2, study.3.benefit.effort)


mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id) + (1 | study),
            data = benefit.effort.all %>% filter(relationship == "Higher", type == "benefit"))

summary(mod)

mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id) + (1 | study),
            data = benefit.effort.all %>% filter(relationship == "Higher", type == "effort"))

summary(mod)



mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id) + (1 | study),
            data = benefit.effort.all %>% filter(relationship == "Lower", type == "benefit"))

summary(mod)

mod <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id) + (1 | study),
            data = benefit.effort.all %>% filter(relationship == "Lower", type == "effort"))

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


