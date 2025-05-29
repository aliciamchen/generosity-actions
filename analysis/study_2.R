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

validation.benefit <- read.csv(here('data/validation_benefit_diff.csv'))
validation.effort <- read.csv(here('data/validation_effort_diff.csv'))

d <-
  read.csv(here('data/1b_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
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
    next_interaction = fct_relevel(next_interaction,
                                   "repeating", "alternating", "none"),
    relationship = fct_relevel(relationship,
                               "higher", "equal", "lower")
  ) %>%
  group_by(subject_id, story, relationship) %>%
  mutate(total_rating = sum(likert_rating, na.rm = T),
         normalized_likert_rating = likert_rating / total_rating) %>%
  select(-total_rating) %>%
  left_join(validation.benefit %>% select(story, diff), by = "story") %>%
  rename(benefit_diff = diff) %>%
  left_join(validation.effort %>% select(story, diff), by = "story") %>%
  rename(effort_diff = diff)

d.benefit.effort <- d %>% filter(relationship != "equal", next_interaction != "none") %>% select(-normalized_likert_rating) %>%
  pivot_wider(names_from = next_interaction, values_from = likert_rating) %>%
  group_by(story, subject_id) %>%
  mutate(rep_minus_alt = repeating - alternating) %>%
  ungroup()


# Demographic info --------------------------------------------------------


d.demographics <- read.csv(here('data/1b_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age))

print(length(unique(d$subject_id)))



# Plots -------------------------------------------------------------------


d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat) %>%
  mutate(next_interaction = fct_relevel(next_interaction,
                                        "repeating", "alternating", "none"))


f = ggplot(data = d %>% filter(relationship == "higher"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "higher"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "higher"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_higher_cont.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "equal"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_equal_cont.pdf"),
       width = 6,
       height = 7.5)


f = ggplot(data = d %>% filter(relationship == "lower"),
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "lower"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "lower"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = action.colors,
    name = "next interaction",
    breaks = c("repeating", "alternating")
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f
ggsave(here("figures/outputs/1b_violin_lower_cont.pdf"),
       width = 6,
       height = 7.5)



# Stats -------------------------------------------------------------------

mod <- lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                     story) + (1 | subject_id),
            data = d)

summary(mod)

anova(mod, type="III")

# replicate asymmetric/symmetric results from 1a
emm_symmetry <- mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

# $emmeans
# asymmetry_present = no:
#   next_interaction emmean     SE    df lower.CL upper.CL t.ratio p.value
# repeating          4.48 0.0938 162.3     4.30     4.67  47.796  <.0001
# alternating        4.98 0.0937 161.8     4.79     5.16  53.073  <.0001
# none               2.29 0.0938 162.3     2.10     2.47  24.385  <.0001
#
# asymmetry_present = yes:
#   next_interaction emmean     SE    df lower.CL upper.CL t.ratio p.value
# repeating          4.86 0.0784  79.9     4.71     5.02  62.045  <.0001
# alternating        4.06 0.0784  79.8     3.90     4.22  51.802  <.0001
# none               2.59 0.0783  79.5     2.43     2.75  33.080  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# asymmetry_present = no:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating   -0.491 0.1030 3090   -0.732   -0.249  -4.764  <.0001
# repeating - none           2.197 0.1031 3090    1.955    2.438  21.310  <.0001
# alternating - none         2.687 0.1030 3090    2.446    2.929  26.089  <.0001
#
# asymmetry_present = yes:
#   contrast                estimate     SE   df lower.CL upper.CL t.ratio p.value
# repeating - alternating    0.804 0.0729 3090    0.633    0.975  11.039  <.0001
# repeating - none           2.274 0.0728 3090    2.103    2.445  31.244  <.0001
# alternating - none         1.470 0.0728 3090    1.299    1.640  20.200  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

# Pairwise contrasts
mod %>% emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)

# $emmeans
# next_interaction = repeating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         4.87 0.0937 161     4.69     5.06  51.997  <.0001
# equal          4.48 0.0938 162     4.30     4.67  47.796  <.0001
# lower          4.86 0.0940 164     4.67     5.04  51.639  <.0001
#
# next_interaction = alternating:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         4.06 0.0936 161     3.87     4.24  43.348  <.0001
# equal          4.98 0.0937 162     4.79     5.16  53.073  <.0001
# lower          4.06 0.0940 164     3.88     4.25  43.187  <.0001
#
# next_interaction = none:
#   relationship emmean     SE  df lower.CL upper.CL t.ratio p.value
# higher         2.58 0.0935 160     2.40     2.77  27.586  <.0001
# equal          2.29 0.0938 162     2.10     2.47  24.385  <.0001
# lower          2.60 0.0939 162     2.41     2.79  27.695  <.0001
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# next_interaction = repeating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher -0.38809 0.103 3092   -0.630  -0.1466  -3.768  0.0005
# lower - higher -0.01675 0.103 3098   -0.259   0.2254  -0.162  0.9856
# lower - equal   0.37134 0.103 3093    0.129   0.6135   3.595  0.0010
#
# next_interaction = alternating:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher  0.91664 0.103 3092    0.676   1.1578   8.914  <.0001
# lower - higher  0.00266 0.103 3098   -0.239   0.2446   0.026  0.9996
# lower - equal  -0.91398 0.103 3093   -1.156  -0.6720  -8.855  <.0001
#
# next_interaction = none:
#   contrast       estimate    SE   df lower.CL upper.CL t.ratio p.value
# equal - higher -0.29265 0.103 3092   -0.534  -0.0515  -2.846  0.0124
# lower - higher  0.01918 0.103 3098   -0.222   0.2606   0.186  0.9811
# lower - equal   0.31183 0.103 3093    0.070   0.5537   3.023  0.0071
#
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
# Conf-level adjustment: tukey method for comparing a family of 3 estimates
# P value adjustment: tukey method for comparing a family of 3 estimates

emmeans(emm_symmetry, pairwise ~ asymmetry_present | next_interaction) %>%
  summary(infer = T)

# $emmeans
# next_interaction = repeating:
#   asymmetry_present emmean     SE    df lower.CL upper.CL t.ratio p.value
# no                  4.48 0.0938 162.3     4.30     4.67  47.796  <.0001
# yes                 4.86 0.0784  79.9     4.71     5.02  62.045  <.0001
#
# next_interaction = alternating:
#   asymmetry_present emmean     SE    df lower.CL upper.CL t.ratio p.value
# no                  4.98 0.0937 161.8     4.79     5.16  53.073  <.0001
# yes                 4.06 0.0784  79.8     3.90     4.22  51.802  <.0001
#
# next_interaction = none:
#   asymmetry_present emmean     SE    df lower.CL upper.CL t.ratio p.value
# no                  2.29 0.0938 162.3     2.10     2.47  24.385  <.0001
# yes                 2.59 0.0783  79.5     2.43     2.75  33.080  <.0001
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95
#
# $contrasts
# next_interaction = repeating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes   -0.380 0.0893 3091   -0.555   -0.205  -4.253  <.0001
#
# next_interaction = alternating:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes    0.915 0.0892 3091    0.740    1.090  10.264  <.0001
#
# next_interaction = none:
#   contrast estimate     SE   df lower.CL upper.CL t.ratio p.value
# no - yes   -0.302 0.0892 3091   -0.477   -0.127  -3.388  0.0007
#
# Results are averaged over the levels of: relationship
# Degrees-of-freedom method: kenward-roger
# Confidence level used: 0.95


# benefit / effort

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "higher"))

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + effort_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower"))

summary(mod)

mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "higher"))

summary(mod)


mod <- lmer(rep_minus_alt ~ 1 + benefit_diff + (1 | story) + (1 | subject_id),
            data = d.benefit.effort %>% filter(relationship == "lower"))

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
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emmeans(emm_symmetry, pairwise ~ next_interaction | asymmetry_present) %>%
  summary(infer = T)

# Pairwise contrasts
mod %>% emmeans(revpairwise ~ relationship | next_interaction) %>%
  summary(infer = T)


emmeans(emm_symmetry, pairwise ~ asymmetry_present | next_interaction) %>%
  summary(infer = T)



# Extra plots -------------------------------------------------------------


# Aggregated results on one plot
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
  scale_x_discrete(limits = c("higher", "lower", "equal")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

# grouped by story

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
  scale_x_discrete(limits = c("higher", "lower", "equal")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom") +
  facet_wrap(~story)

f

## Look at sample individual scenarios
stories = c('concerts', 'restaurant', 'family meals', 'meeting location')

for (s in stories) {

  f = ggplot(data = d %>% filter(relationship == "higher", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "higher", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "higher", story == s),
      mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.09
    ) +
    scale_fill_manual(
      values = action.colors,
      name = "next interaction",
      breaks = c("repeating", "alternating")
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of generous person", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")

  f

  ggsave(here(glue("figures/outputs/1b_higher_{s}.pdf")),
         width = 6,
         height = 7.5)

}


for (s in stories) {

  f = ggplot(data = d %>% filter(relationship == "lower", story == s),
             aes(x = relationship, y = likert_rating, fill = next_interaction)) +
    geom_violin(width = 1.16,
                bw = 0.43,
                position = position_dodge(width = 0.8)) +
    geom_point(
      d.means.story %>% filter(relationship == "lower", story == s),
      mapping = aes(x = relationship, y = likert_rating),
      size = 2.3,
      alpha = 1,
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      d.means.story %>% filter(relationship == "lower", story == s),
      mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.8),
      size = 1.5,
      width = 0.09
    ) +
    scale_fill_manual(
      values = action.colors,
      name = "next interaction",
      breaks = c("repeating", "alternating")
    ) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       limits = c(0.8, 7.2)) +
    labs(x = "status of generous person", y = "how likely?", fill = "next interaction", title = s) +
    theme(legend.position = "bottom")

  f

  ggsave(here(glue("figures/outputs/1b_lower_{s}.pdf")),
         width = 6,
         height = 7.5)

}


# Benefit / effort plots


d.b.e.means <- d.benefit.effort %>%
  group_by(story, relationship, benefit_diff, effort_diff) %>%
  tidyboot_mean(rep_minus_alt, na.rm = TRUE)

f <- ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "relative benefit to recipient",
       y = "repeated minus alternated",
       title = "study 1b, A is lower status")

f

ggsave(here("figures/outputs/1b_benefit_lower.pdf"),
       width = 8.7,
       height = 4)

f <- ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = benefit_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "relative benefit to recipient",
       y = "repeated minus alternated",
       title = "study 1b, A is higher status")

f

ggsave(here("figures/outputs/1b_benefit_higher.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "lower"), aes(x = effort_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "relative effort",
       y = "repeated minus alternated",
       title = "study 1b, A is lower status")

f

ggsave(here("figures/outputs/1b_effort_lower.pdf"),
       width = 8.7,
       height = 4)


f <- ggplot(d.b.e.means %>% filter(relationship == "higher"), aes(x = effort_diff, y = empirical_stat)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(mapping = aes(x = effort_diff, ymin = ci_lower, ymax=ci_upper), size= 1.5, width = 0.09, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "relative effort",
       y = "repeated minus alternated",
       title = "study 1b, A is higher status")

f

ggsave(here("figures/outputs/1b_effort_higher.pdf"),
       width = 8.7,
       height = 4)

