library(here)
library(tidyverse)
library(tidyboot)
library(lme4)
library(lmerTest)
library(forcats)
library(glue)
library(emmeans)
library(effectsize)
library(ordinal)

source(here("analysis/stats_helpers.R"))
set_stats_file("study_1")

set.seed(67)

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

n_subjects <- length(unique(d$subject_id))
print(n_subjects)

d.demographics <-
  read.csv(here('data/study-1_demographics.csv'))
d.demographics |> count(gender)
d.demographics |> summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

write_demographics("studyOne", d.demographics, n_subjects)


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

cat("\n--- Standardized Parameters ---\n")
print(standardize_parameters(mod))

anova(mod, type = "III")

# Type III Analysis of Variance Table with Satterthwaite’s method
#                               Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)
# next_interaction              3529.5 1764.74     2 3095.1 952.5011 <2e-16 ***
# relationship                    12.3    6.14     2 3104.8   3.3166 0.0364 *
# next_interaction:relationship  468.2  117.05     4 3095.1  63.1770 <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ‘ 1


emm_contrasts <- emmeans(mod, revpairwise ~ next_interaction |
          relationship)
emm_contrasts %>% summary(infer = T)

cat("\n--- Effect Sizes (Cohen’s d) for contrasts ---\n")
s1_effect_sizes <- eff_size(emmeans(mod, ~ next_interaction | relationship), sigma = sigma(mod), edf = df.residual(mod), method = "revpairwise")
print(s1_effect_sizes)

# Export EMMs and contrasts to tex
s1_emms <- summary(emm_contrasts$emmeans)
s1_cons <- summary(emm_contrasts, infer = TRUE, adjust = "none")$contrasts

# EMMs by relationship × next_interaction
for (rel in c("No info", "Symmetric", "Asymmetric")) {
  rel_label <- switch(rel, "No info" = "NoInfo", Symmetric = "Sym", Asymmetric = "Asym")
  for (ni in c("Reciprocity", "Precedent", "None")) {
    ni_label <- switch(ni, Reciprocity = "Recip", Precedent = "Prec", None = "None")
    row <- s1_emms |> filter(relationship == rel, next_interaction == ni)
    write_emm(paste0("studyOne", ni_label, rel_label), row)
  }
}

# Key contrasts: Precedent - Reciprocity for each relationship
for (rel in c("No info", "Symmetric", "Asymmetric")) {
  rel_label <- switch(rel, "No info" = "NoInfo", Symmetric = "Sym", Asymmetric = "Asym")
  row <- s1_cons |> filter(relationship == rel, contrast == "Precedent - Reciprocity")
  write_contrast(paste0("studyOnePrecVsRecip", rel_label), row, stat_type = "t")
}

# Effect sizes (Cohen’s d) for Precedent - Reciprocity
s1_es_df <- as.data.frame(s1_effect_sizes)
for (rel in c("No info", "Symmetric", "Asymmetric")) {
  rel_label <- switch(rel, "No info" = "NoInfo", Symmetric = "Sym", Asymmetric = "Asym")
  es_rows <- s1_es_df |> filter(relationship == rel)
  # Precedent - Reciprocity is the first contrast within each relationship
  prec_recip_row <- es_rows |> filter(grepl("Precedent - Reciprocity|Reciprocity - Precedent", contrast))
  if (nrow(prec_recip_row) > 0) {
    write_cohens_d(paste0("studyOnePrecVsRecip", rel_label), prec_recip_row$effect.size[1])
  }
}

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

# Ordinal robustness check (ceiling effects)
cat("\n--- Ordinal Mixed Model Robustness Check ---\n")
d$ordered_rating <- ordered(d$likert_rating)
ord_mod <- clmm(ordered_rating ~ next_interaction * relationship + (1 | story) + (1 | subject_id), data = d)
cat("Ordinal model summary:\n")
summary(ord_mod)

# Export ordinal model interaction coefficients
write_section("Ordinal robustness check")
ord_coefs <- summary(ord_mod)$coefficients
# next_interaction1:relationship2 is the key interaction (precedent vs recip × asym vs sym)
ord_int_names <- c(
  "next_interaction1:relationship1" = "NIaRelA",
  "next_interaction2:relationship1" = "NIbRelA",
  "next_interaction1:relationship2" = "NIaRelB",
  "next_interaction2:relationship2" = "NIbRelB"
)
for (coef_name in names(ord_int_names)) {
  label <- ord_int_names[coef_name]
  row <- ord_coefs[coef_name, ]
  write_stat(paste0("studyOneOrd", label, "B"), row["Estimate"], digits = 2)
  write_stat(paste0("studyOneOrd", label, "Z"), row["z value"], digits = 2)
  write_p(paste0("studyOneOrd", label, "P"), row["Pr(>|z|)"])
}

# Do people expect a continued social interaction, both with and without the relationship?


emm <-
  mod %>% emmeans(~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("no", "yes", "yes"))


s1_interaction <- emmeans(emm, revpairwise ~ interaction_present |
          relationship_present) |>
  summary(infer = T)
s1_interaction

# Export continued-interaction contrast (no relationship info)
s1_int_cons <- s1_interaction$contrasts
row_no_rel <- s1_int_cons |> filter(relationship_present == "no")
write_contrast("studyOneInteractionNoRel", row_no_rel, stat_type = "t")

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


# Z-scored robustness check (ceiling effects) ------------------------------

cat("\n--- Z-Scored Robustness Check ---\n")
d <- d |>
  group_by(subject_id) |>
  mutate(z_likert_rating = scale(likert_rating)[, 1]) |>
  ungroup()

mod_z <- lmer(
  z_likert_rating ~ 1 + next_interaction * relationship + (1 | story) + (1 | subject_id),
  data = d
)

summary(mod_z)
anova(mod_z, type = "III")

s1_z_cons <- emmeans(mod_z, revpairwise ~ next_interaction | relationship) |>
  summary(infer = TRUE, adjust = "none")
print(s1_z_cons)

# Export z-scored contrasts
write_section("Z-scored robustness check")
for (rel in c("No info", "Symmetric", "Asymmetric")) {
  rel_label <- switch(rel, "No info" = "NoInfo", Symmetric = "Sym", Asymmetric = "Asym")
  row <- s1_z_cons$contrasts |> filter(relationship == rel, contrast == "Precedent - Reciprocity")
  write_contrast(paste0("studyOneZPrecVsRecip", rel_label), row, stat_type = "t")
}

# ANOVA for z-scored model
s1_z_anova <- anova(mod_z, type = "III")
write_stat("studyOneZInteractionF", s1_z_anova["next_interaction:relationship", "F value"], digits = 2)
write_stat("studyOneZInteractionP", formatC(s1_z_anova["next_interaction:relationship", "Pr(>F)"], format = "e", digits = 1))
