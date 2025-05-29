library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(forcats)
library(glue)
library(emmeans)


# Options -----------------------------------------------------------------

options(warn = -1)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

action.colors <- c("#DD8D29", "#E2D200", "#D6D6D6")


# Load and tidy data ------------------------------------------------------

d <-
  read.csv(here('data/1a_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("repeating", "alternating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    next_interaction = fct_relevel(next_interaction, "repeating", "alternating", "none"),
    relationship = fct_relevel(relationship, "asymmetric", "symmetric", "no_info")
  ) %>% # Then, normalize likert rating
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating)


# Demographic info --------------------------------------------------------

d.demographics <-
  read.csv(here('data/1a_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))


# Plots -------------------------------------------------------------------

d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(
    next_interaction = fct_relevel(next_interaction, "repeating", "alternating", "none"),
    relationship = fct_relevel(relationship, "asymmetric", "symmetric", "no_info")
  )


f = ggplot(
  data = d %>% filter(relationship == "asymmetric"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "asymmetric"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "asymmetric"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


ggsave(
  here("figures/outputs/1a_violin_asym_cont.pdf"),
  width = 6,
  height = 7.5
)


f = ggplot(
  data = d %>% filter(relationship == "symmetric"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "symmetric"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "symmetric"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


ggsave(
  here("figures/outputs/1a_violin_sym_cont.pdf"),
  width = 6,
  height = 7.5
)


f = ggplot(
  data = d %>% filter(relationship == "no_info"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "no_info"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "no_info"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/1a_violin_noinfo_cont.pdf"),
  width = 6,
  height = 7.5
)


# Stats -------------------------------------------------------------------


mod <-
  lmer(likert_rating ~ 1 + next_interaction * relationship
       + (1 | story) + (1 | subject_id),
       data = d)

summary(mod)

anova(mod, type="III")

# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq Mean Sq NumDF  DenDF  F value Pr(>F)
# next_interaction              3529.5 1764.74     2 3095.1 952.5011 <2e-16 ***
# relationship                    12.3    6.14     2 3104.8   3.3166 0.0364 *
# next_interaction:relationship  468.2  117.05     4 3095.1  63.1770 <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


emmeans(mod, revpairwise ~ next_interaction | relationship) %>% summary(infer = T)

# $emmeans
# relationship = asymmetric:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating          5.25 0.0898 159     5.07     5.43  58.455  <.0001
# alternating        3.48 0.0896 158     3.30     3.65  38.799  <.0001
# none               2.57 0.0897 158     2.39     2.75  28.667  <.0001
#
# relationship = symmetric:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating          4.49 0.0897 158     4.31     4.67  50.044  <.0001
# alternating        4.75 0.0897 158     4.58     4.93  52.992  <.0001
# none               2.16 0.0897 158     1.98     2.33  24.045  <.0001
#
# relationship = no_info:
#   next_interaction emmean     SE  df lower.CL upper.CL t.ratio p.value
# repeating          4.75 0.0896 158     4.57     4.93  52.983  <.0001
# alternating        4.61 0.0896 158     4.43     4.78  51.407  <.0001
# none               2.38 0.0897 158     2.20     2.56  26.529  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# relationship = asymmetric:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating   -1.771 0.102 3095  -2.0117  -1.5312 -17.290  <.0001
# none - repeating          -2.677 0.103 3095  -2.9174  -2.4366 -26.109  <.0001
# none - alternating        -0.906 0.102 3095  -1.1456  -0.6654  -8.844  <.0001
#
# relationship = symmetric:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating    0.265 0.102 3095   0.0243   0.5048   2.582  0.0267
# none - repeating          -2.332 0.102 3095  -2.5725  -2.0920 -22.763  <.0001
# none - alternating        -2.597 0.102 3095  -2.8371  -2.3566 -25.346  <.0001
#
# relationship = no_info:
#   contrast                estimate    SE   df lower.CL upper.CL t.ratio p.value
# alternating - repeating   -0.141 0.102 3095  -0.3811   0.0987  -1.381  0.3512
# none - repeating          -2.369 0.102 3095  -2.6087  -2.1286 -23.135  <.0001
# none - alternating        -2.227 0.102 3095  -2.4675  -1.9873 -21.755  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates


# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(emmeans(mod, ~ next_interaction * relationship), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# next_interaction_pairwise relationship_pairwise  estimate    SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   asymmetric - symmetric   2.0360 0.145 3095   1.7519    2.320  14.051  <.0001
# repeating - none          asymmetric - symmetric   0.3447 0.145 3095   0.0605    0.629   2.378  0.0175
# alternating - none        asymmetric - symmetric  -1.6913 0.145 3095  -1.9753   -1.407 -11.677  <.0001
# repeating - alternating   asymmetric - no_info     1.6302 0.145 3095   1.3463    1.914  11.259  <.0001
# repeating - none          asymmetric - no_info     0.3083 0.145 3095   0.0242    0.592   2.128  0.0334
# alternating - none        asymmetric - no_info    -1.3219 0.145 3095  -1.6058   -1.038  -9.130  <.0001
# repeating - alternating   symmetric - no_info     -0.4058 0.145 3095  -0.6897   -0.122  -2.803  0.0051
# repeating - none          symmetric - no_info     -0.0364 0.145 3095  -0.3204    0.248  -0.251  0.8017
# alternating - none        symmetric - no_info      0.3694 0.145 3095   0.0854    0.653   2.551  0.0108
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95


# Do people expect a continued social interaction, both with and without the relationship?
emm <-
  mod %>% emmeans( ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, revpairwise ~ interaction_present | relationship_present) %>%
  summary(infer = T)

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



# Repeat analyses without control levels ----------------------------------


d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 |
                                                                             subject_id),
            data = d_filtered)

summary(mod)

emmeans(mod, revpairwise ~ next_interaction | relationship) %>%
  summary(infer = T)

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(emmeans(mod, ~ next_interaction * relationship), interaction = c("pairwise", "pairwise")) %>%
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

anova(mod, type="III")

emmeans(mod, revpairwise ~ next_interaction | relationship) %>% summary(infer = T)


# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast(emmeans(mod, ~ next_interaction * relationship), interaction = c("pairwise", "pairwise")) %>%
  summary(infer = T)

# Do people expect a continued social interaction, both with and without the relationship?
emm <-
  mod %>% emmeans( ~ relationship * next_interaction) %>%
  add_grouping("interaction_present",
               "next_interaction",
               c("yes", "yes", "no")) %>%
  add_grouping("relationship_present", "relationship", c("yes", "yes", "no"))


emmeans(emm, revpairwise ~ interaction_present | relationship_present) %>%
  summary(infer = T)

# Without control levels

d_filtered <- d %>%
  filter(next_interaction != "none" & relationship != "no_info")

mod <- lmer(normalized_likert_rating ~ next_interaction * relationship + (1 |
                                                                 story) + (1 |
                                                                             subject_id),
            data = d_filtered)

summary(mod)


# Extra plots -------------------------------------------------------------


# Plot aggregated results on one plot
f = ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

# Plot grouped by story
d.means.story <-
  d %>% drop_na() %>%
  group_by(story, relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))


f = ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.story,
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.story,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "relationship", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") +
  facet_wrap(~ story)


f

