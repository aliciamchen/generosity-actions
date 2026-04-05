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
set_stats_file("study_2")

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




d.benefit.effort <- d %>% filter(relationship != "Equal", next_interaction != "None") %>% select(-normalized_likert_rating) %>%
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(prec_minus_rec = Precedent - Reciprocity,
         p_prec = (Precedent / (Precedent + Reciprocity))) %>%
  ungroup()

d <- d %>% 
  pivot_wider(names_from = "type", values_from = c("n", "diff", "ci_lower", "ci_upper", "mean"))

write_csv(d, here("data/study-2_tidy_data.csv"))
write_csv(d.benefit.effort, here("data/study-2_benefit_effort.csv"))


# Demographic info --------------------------------------------------------

n_subjects <- length(unique(d$subject_id))
print(n_subjects)

d.demographics <- read.csv(here('data/study-2_demographics.csv'))
d.demographics |> count(gender)
d.demographics |> summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

write_demographics("studyTwo", d.demographics, n_subjects)


# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

cat("\n--- Standardized Parameters ---\n")
print(standardize_parameters(mod))

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
emm_rel <- emmeans(mod, ~ relationship | next_interaction)
contrast(emm_rel, method = "revpairwise") %>% summary(infer = T)

cat("\n--- Effect Sizes (Cohen's d) for relationship contrasts ---\n")
s2_effect_sizes <- eff_size(emm_rel, sigma = sigma(mod), edf = df.residual(mod))
print(s2_effect_sizes)

# Export EMMs and contrasts to tex
s2_sym_emms <- summary(emmeans(emm_symmetry, ~ next_interaction | asymmetry_present))
s2_sym_cons <- summary(emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present), infer = TRUE, adjust = "none")$contrasts

# Asymmetric (pooled Higher+Lower): Precedent vs Reciprocity
for (ni in c("Reciprocity", "Precedent", "None")) {
  ni_label <- switch(ni, Reciprocity = "Recip", Precedent = "Prec", None = "None")
  row <- s2_sym_emms |> filter(asymmetry_present == "yes", next_interaction == ni)
  write_emm(paste0("studyTwo", ni_label, "Asym"), row)
}
row <- s2_sym_cons |> filter(asymmetry_present == "yes", contrast == "Reciprocity - Precedent")
write_contrast("studyTwoRecipVsPrecAsym", row, stat_type = "t")

# Equal: Reciprocity vs Precedent
for (ni in c("Reciprocity", "Precedent", "None")) {
  ni_label <- switch(ni, Reciprocity = "Recip", Precedent = "Prec", None = "None")
  row <- s2_sym_emms |> filter(asymmetry_present == "no", next_interaction == ni)
  write_emm(paste0("studyTwo", ni_label, "Equal"), row)
}
row <- s2_sym_cons |> filter(asymmetry_present == "no", contrast == "Reciprocity - Precedent")
write_contrast("studyTwoRecipVsPrecEqual", row, stat_type = "t")

# Higher vs Lower on Precedent
s2_rel_emms <- summary(emm_rel)
s2_rel_cons <- summary(contrast(emm_rel, method = "revpairwise"), infer = TRUE, adjust = "none")

for (rel in c("Equal", "Lower", "Higher")) {
  row <- s2_rel_emms |> filter(next_interaction == "Precedent", relationship == rel)
  write_emm(paste0("studyTwoPrec", rel), row)
}
row <- s2_rel_cons |> filter(next_interaction == "Precedent", contrast == "Higher - Lower")
write_contrast("studyTwoPrecHigherVsLower", row, stat_type = "t")

# Cohen's d for Precedent Higher vs Lower
s2_es_df <- as.data.frame(s2_effect_sizes)
es_row <- s2_es_df |> filter(next_interaction == "Precedent", grepl("Higher - Lower|Lower - Higher", contrast))
if (nrow(es_row) > 0) {
  write_cohens_d("studyTwoPrecHigherVsLower", es_row$effect.size[1])
}

# Cohen's d for Precedent vs Reciprocity within Asym and Equal
s2_sym_es <- eff_size(
  emmeans(emm_symmetry, ~ next_interaction | asymmetry_present),
  sigma = sigma(mod), edf = df.residual(mod)
)
s2_sym_es_df <- as.data.frame(s2_sym_es)
for (asym in c("no", "yes")) {
  asym_label <- switch(asym, no = "Equal", yes = "Asym")
  es_row <- s2_sym_es_df |> filter(
    asymmetry_present == asym,
    grepl("Reciprocity - Precedent|Precedent - Reciprocity", contrast)
  )
  if (nrow(es_row) > 0) {
    write_cohens_d(paste0("studyTwoRecipVsPrec", asym_label), es_row$effect.size[1])
  }
}

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


# Bayes Factor for Higher vs Lower null on Precedent
cat("\n--- Bayes Factors for key null results ---\n")
# BF for no Higher vs Lower difference on precedent
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
s2_bf01_prec <- as.vector(1 / (bf_prec / bf_prec_null))
cat("BF01 for Higher vs Lower on Precedent (Study 2):\n")
print(s2_bf01_prec)
write_bf("studyTwoPrecHigherVsLower", extractBF(bf_prec / bf_prec_null)$bf |> {\(x) 1/x}())

# Exploratory analyses ----------------------------------------------------

# benefit / effort

mod_higher_benefit <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "benefit"))
summary(mod_higher_benefit)
export_lmer_coef("studyTwoHigherBenefit", mod_higher_benefit)

mod_higher_effort <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Higher", type == "effort"))
summary(mod_higher_effort)
export_lmer_coef("studyTwoHigherEffort", mod_higher_effort)

mod_lower_benefit <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "benefit"))
summary(mod_lower_benefit)
export_lmer_coef("studyTwoLowerBenefit", mod_lower_benefit)

mod_lower_effort <- lmer(p_prec ~ 1 + diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "Lower", type == "effort"))
summary(mod_lower_effort)
export_lmer_coef("studyTwoLowerEffort", mod_lower_effort)








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
cat("BF01 for effort not moderating precedent (Study 2):\n")
print(1 / (bf_effort / bf_effort_null))
write_bf("studyTwoEffortModPrec", extractBF(bf_effort / bf_effort_null)$bf |> {\(x) 1/x}())

# For benefit
bf_benefit <- lmBF(likert_rating ~ diff_benefit + subject_id + story,
                   data = d_bf_cost,
                   whichRandom = c("subject_id", "story"))
bf_benefit_null <- lmBF(likert_rating ~ subject_id + story,
                        data = d_bf_cost,
                        whichRandom = c("subject_id", "story"))
cat("BF01 for benefit not moderating precedent (Study 2):\n")
print(1 / (bf_benefit / bf_benefit_null))
write_bf("studyTwoBenefitModPrec", extractBF(bf_benefit / bf_benefit_null)$bf |> {\(x) 1/x}())

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


