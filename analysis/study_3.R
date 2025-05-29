library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(forcats)
library(emmeans)
library(glue)
library(wesanderson)


# Options -----------------------------------------------------------------

options(warn = -1)

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
theme_set(theme_classic(base_size = 16))


# Load and tidy data ------------------------------------------------------


effort_diff <-
  read.csv(here("data/validation_effort_diff.csv")) %>% rename(effort_diff = diff) %>% select(c(story, effort_diff))
benefit_diff <-
  read.csv(here("data/validation_benefit_diff.csv")) %>% rename(benefit_diff = diff) %>% select(c(story, benefit_diff))

d <-
  read.csv(here('data/1c_data.csv')) %>% filter(understood == 'yes', pass_attention == TRUE, story != 'attention') %>%
  mutate(
    strategy = case_when(
      stage == "second" & first_meeting == response ~ 'repeating',
      stage == "second" &
        first_meeting != response ~ 'alternating'
    )
  ) %>%
  select(-response, -understood, -first_meeting, -pass_attention) %>%
  mutate_all(~ case_when(. == 'less' ~ 'lower', . == 'more' ~ 'higher', TRUE ~ .)) %>%
  rename(first_actual_higher = altruistic_status,
         strategy_repeating = strategy) %>%
  group_by(subject_id, story) %>%
  fill(strategy_repeating, .direction = "up") %>%
  ungroup() %>%
  pivot_wider(names_from = stage,
              values_from = response_status,
              names_prefix = "response_higher_") %>%
  rename(first_response_higher = response_higher_first,
         second_response_higher = response_higher_second) %>%
  mutate(symmetric = ifelse(first_actual_higher == "equal", "symmetric", "asymmetric")) %>%
  group_by(story) %>%
  left_join(effort_diff) %>%
  left_join(benefit_diff)

# Set levels for categorical variables
d$first_actual_higher <-
  factor(d$first_actual_higher, levels = c("higher", "lower", "equal"))
d$first_response_higher <-
  factor(d$first_response_higher, levels = c("higher", "lower", "equal"))
d$second_response_higher <-
  factor(d$second_response_higher, levels = c("lower", "higher", "equal"))
d$strategy_repeating <-
  factor(d$strategy_repeating, levels = c("alternating", "repeating"))

write.csv(d, here('data/1c_tidy_data.csv'), row.names = FALSE)


# Display demographics ----------------------------------------------------


d.demographics <- read.csv(here('data/1c_demographics.csv'))
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))



# Plots -------------------------------------------------------------------


### Plot with scenario on x axis, implicit coordination on y axis, x axis ordered by y axis

d.first.response <-
  d %>%
  mutate(first_response_higher = recode(
    first_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(story, effort_diff, benefit_diff) %>%
  tidyboot_mean(first_response_higher, na.rm = TRUE)


d.temp <- d.first.response %>%
  arrange(desc(mean))

levs <- unique(d.temp$story)

d.first.response$story <-
  factor(d.first.response$story, levels = levs)

# save d first response csv
write.csv(d.first.response, here('data/1c_implicit.csv'), row.names = FALSE)


f <-
  ggplot(d.first.response, aes(x = story, y = empirical_stat)) +
  geom_point(
    data = d.first.response,
    aes(x = story, y = empirical_stat),
    size = 2.4,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = d.first.response,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1.8,
    width = 0.3
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "story", y = "expectation of higher", title = 'implicit coordination expectations') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f

ggsave(
  here("figures/outputs/1c_scenarios_ordered.pdf"),
  width = 8,
  height = 3.5
)


## First time on x axis, second time on y axis, colored by actual first

d.second.response <-
  d %>% filter(symmetric == "asymmetric") %>%
  mutate(second_response_higher = recode(
    second_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(story, first_actual_higher) %>%
  tidyboot_mean(second_response_higher, na.rm = TRUE)

d.first.second <-
  left_join(
    d.first.response,
    d.second.response,
    suffix = c("_first", "_second"),
    by = (c("story"))
  )

f <-
  ggplot(
    d.first.second,
    aes(x = empirical_stat_first, y = empirical_stat_second, color = first_actual_higher)
  ) +
  geom_smooth(method = "lm",
              fill = "lightgray",
              linewidth = 1.3) +
  geom_point(size = 3.3,
             alpha = 0.3,
             stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_first, ymin = ci_lower_second, ymax = ci_upper_second),
    size = 1.5,
    width = 0.043,
    alpha = 0.3
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_second, xmin = ci_lower_first, xmax = ci_upper_first),
    size = 1.5,
    height = 0.043,
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("higher" = "#B87FFF", "lower" = "#35ACFF")) +
  labs(x = "first time higher", y = "second time higher", color = "observed_higher")

f

ggsave(here("figures/outputs/1c_first_second.pdf"),
       width = 6.75,
       height = 4)


## Are the effects of scenario consistent between 1b and 1c?

# import study 1b data
d.1b <- read.csv(here('data/1b_tidy_data.csv')) %>%
  filter(relationship != "equal", next_interaction != 'none') %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(observed_higher = relationship) %>%
  mutate(
    observed_higher = ifelse(observed_higher == 'less', 'lower', 'higher'),
    second_response_higher = case_when(
      next_interaction == 'repeating' ~ observed_higher,
      next_interaction == 'alternating' ~ ifelse(observed_higher == "higher", "lower", "higher"),
      next_interaction == 'none' ~ 'none'
    )
  )


d.1b.means <- d.1b %>%
  filter(second_response_higher == 'higher') %>%
  group_by(story, observed_higher) %>%
  tidyboot_mean(normalized_likert_rating, na.rm = T)


d.1b.1c <-
  left_join(d.1b.means,
            d.first.response,
            suffix = c("_1b", "_1c"),
            by = (c("story")))

f <-
  ggplot(d.1b.1c,
         aes(x = empirical_stat_1c, y = empirical_stat_1b, color = observed_higher)) +
  geom_point(size = 3.3,
             alpha = 0.3,
             stroke = 0) +
  geom_smooth(method = "lm",
              fill = "lightgray",
              linewidth = 1.3) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_1b, ymax = ci_upper_1b),
    size = 1.5,
    width = 0.043,
    alpha = 0.3
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_1b, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.014,
    alpha = 0.3
  ) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("higher" = "#B87FFF", "lower" = "#35ACFF")) +
  labs(x = "study 1c first time higher", y = "study 1b P(higher)")

f

ggsave(here("figures/outputs/1c_1b.pdf"),
       width = 6.6,
       height = 4)


# Stats -------------------------------------------------------------------

# What happened the first time people interacted?
# Can their expectations be explained by relative cost/benefit?
mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ effort_diff + (1 |
                                             subject_id) + (1 |
                                                              story),
    family = 'binomial'
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response_higher ~ effort_diff + (1 | subject_id) + (1 |      story)
# Data: d %>% filter(first_actual_higher != "equal")
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
# story      (Intercept) 1.2496   1.1179
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   0.2019     0.5760   0.351    0.726
# effort_diff  -0.1139     0.2661  -0.428    0.669
#
# Correlation of Fixed Effects:
#   (Intr)
# effort_diff -0.880

mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ benefit_diff + (1 |
                                              subject_id) + (1 |
                                                               story),
    family = 'binomial'
  )
summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: first_response_higher ~ benefit_diff + (1 | subject_id) + (1 |      story)
# Data: d %>% filter(first_actual_higher != "equal")
#
# AIC      BIC   logLik deviance df.resid
# 1613.9   1634.7   -802.9   1605.9     1352
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -2.3929 -0.6972 -0.3419  0.6716  2.7132
#
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.1808   0.4252
# story      (Intercept) 1.1271   1.0617
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   -0.7140     0.5571  -1.282    0.200
# benefit_diff   0.3785     0.2664   1.421    0.155
#
# Correlation of Fixed Effects:
#   (Intr)
# benefit_dff -0.883



# Next, replicate findings from studies 1a and 1b


# People expect alternation when the relationship is symmetric, and repetition when the relationship is asymmetric.

mod <- glmer(
  data = d,
  strategy_repeating ~ first_actual_higher + (1 |
                                                subject_id) + (1 |
                                                                 story),
  family =  'binomial'
)

summary(mod)


emm <- mod %>% emmeans(pairwise ~ first_actual_higher) %>%
  add_grouping("asymmetric", "first_actual_higher", c("yes", "yes", "no"))
emm
emmeans(emm, pairwise ~ asymmetric)

# $emmeans
# asymmetric emmean    SE  df asymp.LCL asymp.UCL
# no         -1.395 0.214 Inf   -1.8138    -0.976
# yes         0.348 0.198 Inf   -0.0396     0.735
#
# Results are averaged over the levels of: first_actual_higher
# Results are given on the logit (not the response) scale.
# Confidence level used: 0.95
#
# $contrasts
# contrast estimate    SE  df z.ratio p.value
# no - yes    -1.74 0.122 Inf -14.263  <.0001
#
# Results are averaged over the levels of: first_actual_higher
# Results are given on the log odds ratio (not the response) scale.


emmeans(mod, pairwise ~ first_actual_higher) %>% summary(infer = T)

# $emmeans
# first_actual_higher emmean    SE  df asymp.LCL asymp.UCL
# higher               0.228 0.207 Inf   -0.1774     0.634
# lower                0.467 0.207 Inf    0.0606     0.874
# equal               -1.395 0.214 Inf   -1.8138    -0.976
#
# Results are given on the logit (not the response) scale.
# Confidence level used: 0.95
#
# $contrasts
# contrast       estimate    SE  df z.ratio p.value
# higher - lower   -0.239 0.125 Inf  -1.915  0.1345
# higher - equal    1.623 0.136 Inf  11.915  <.0001
# lower - equal     1.862 0.138 Inf  13.479  <.0001
#
# Results are given on the log odds ratio (not the response) scale.
# P value adjustment: tukey method for comparing a family of 3 estimates


# Main preregistered hypothesis: in asymmetric relationships, people’s expectations for what
# happens the second time are explained by (1) expectations of tacit coordination
# (what they thought would happen the first time), and (2) expectations of precedent.


d.h1 <- d %>% filter(symmetric == 'asymmetric')
d.h1$first_response_higher <- factor(d.h1$first_response_higher, levels = c("higher", "lower"))
d.h1$first_actual_higher <- factor(d.h1$first_actual_higher, levels = c("higher", "lower"))
d.h1$second_response_higher <- factor(d.h1$second_response_higher, levels = c("lower", "higher"))

mod <- glmer(
  data = d.h1,
  second_response_higher ~ first_response_higher * first_actual_higher + (1 |
                                                                            subject_id) + (1 |
                                                                                             story),
  family =  'binomial'
)

summary(mod)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: second_response_higher ~ first_response_higher * first_actual_higher +      (1 | subject_id) + (1 | story)
# Data: d.h1
#
# AIC      BIC   logLik deviance df.resid
# 1759.0   1790.3   -873.5   1747.0     1350
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -1.6317 -0.8472 -0.5692  0.8955  1.8471
#
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject_id (Intercept) 0.00000  0.0000
# story      (Intercept) 0.06231  0.2496
# Number of obs: 1356, groups:  subject_id, 113; story, 18
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                                 -0.10279    0.08232  -1.249    0.212
# first_response_higher1                       0.50654    0.06370   7.952 1.84e-15 ***
#   first_actual_higher1                         0.29489    0.05757   5.122 3.02e-07 ***
#   first_response_higher1:first_actual_higher1  0.02998    0.05771   0.520    0.603
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#   (Intr) frst_r_1 frst_c_1
# frst_rspn_1 -0.011
# frst_ctl_h1 -0.006  0.052
# frst__1:__1  0.047 -0.011   -0.027
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')



##  In asymmetric relationships, people have strong intuitions of tacit coordination.

mod <-
  glmer(
    data = d %>% filter(first_actual_higher != "equal"),
    first_response_higher ~ (1 |
                               subject_id) + (1 | story),
    family = 'binomial'
  )

summary(mod)

emm <- emmeans(mod, specs = ~ 1)
summary(emm, null = 0, infer = c(TRUE, TRUE))

# across scenarios, they average out
# 1       emmean    SE  df asymp.LCL asymp.UCL z.ratio p.value
# overall -0.015 0.275 Inf    -0.554     0.524  -0.055  0.9565
#
# Results are given on the logit (not the response) scale.
# Confidence level used: 0.95



# Correlate 1c first time with 1b P(higher)
d.1b.1c.all <- left_join(d.1b, d.first.response) %>%
  rename(expected.first.1c = empirical_stat, expected.next.1b = normalized_likert_rating) %>%
  filter(second_response_higher == "higher")

mod <- lmer(
  data = d.1b.1c.all,
  expected.next.1b ~ expected.first.1c * observed_higher + (1 |
                                                              subject_id) + (1 | story)
)

summary(mod)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: expected.next.1b ~ expected.first.1c * observed_higher + (1 |      subject_id) + (1 | story)
#    Data: d.1b.1c.all
#
# REML criterion at convergence: -717
#
# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -2.8221 -0.6753  0.0397  0.5820  3.3085
#
# Random effects:
#  Groups     Name        Variance Std.Dev.
#  subject_id (Intercept) 0.00000  0.00000
#  story      (Intercept) 0.00019  0.01378
#  Residual               0.02010  0.14177
# Number of obs: 701, groups:  subject_id, 59; story, 18
#
# Fixed effects:
#                                     Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                        5.001e-01  6.264e-03 1.601e+01  79.837  < 2e-16 ***
# expected.first.1c                  1.243e-01  1.314e-02 1.618e+01   9.456 5.39e-08 ***
# observed_higher1                   4.772e-02  5.358e-03 6.850e+02   8.907  < 2e-16 ***
# expected.first.1c:observed_higher1 4.607e-02  1.125e-02 6.856e+02   4.094 4.75e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#             (Intr) exp..1 obsr_1
# expctd.fr.1 -0.005
# obsrvd_hgh1 -0.006  0.000
# expct..1:_1  0.000 -0.018 -0.005
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


# Extra plots -------------------------------------------------------------



## Is implicit coordination based on effort/benefit?

f <-
  ggplot(d.first.response, aes(x = effort_diff, y = empirical_stat)) +
  geom_point(
    size = 2.4,
    alpha = 0.7,
    color = "#00BFC4",
    stroke = 0
  ) +
  geom_errorbar(
    mapping = aes(x = effort_diff, ymin = ci_lower, ymax = ci_upper),
    size = 1.8,
    width = 0.07,
    alpha = 0.7,
    color = "#00BFC4"
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived effort difference in scenario", y = "expectations for high status", title = "implicit coordination effort")

f

ggsave(here("figures/outputs/1c_effort_corr.pdf"),
       width = 8,
       height = 2.8)

f <-
  ggplot(d.first.response, aes(x = benefit_diff, y = empirical_stat)) +
  geom_point(
    size = 2.4,
    alpha = 0.7,
    color = "#F8766D",
    stroke = 0
  ) +
  geom_errorbar(
    mapping = aes(x = benefit_diff, ymin = ci_lower, ymax = ci_upper),
    size = 1.8,
    width = 0.07,
    alpha = 0.7,
    color = "#F8766D"
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "perceived benefit difference in scenario", y = "expectations for high status", title = "implicit coordination benefit")


f

ggsave(here("figures/outputs/1c_benefit_corr.pdf"),
       width = 8,
       height = 2.8)


# Plot showing main hypothesis

h1.means <- d %>%
  filter(symmetric == 'asymmetric') %>%
  mutate(second_response_higher = recode(
    second_response_higher,
    "higher" = 1,
    "lower" = -1
  )) %>%
  group_by(first_response_higher, first_actual_higher) %>%
  tidyboot_mean(second_response_higher, na.rm = T)

p1 <-
  ggplot(
    h1.means,
    aes(x = first_response_higher, y = empirical_stat, color = first_actual_higher)
  ) +
  geom_point(
    mapping = aes(x = first_response_higher, y = empirical_stat),
    size = 2.5,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = first_response_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 2.2,
    width = 0
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "second response asymmetric relationships", y = "second_response_higher")

p1


# Do people expect repetition in asymmetric relationships, and alternating in asymmetric relationships?

h2.means <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(first_response_higher, first_actual_higher) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)


p2 <-
  ggplot(
    h2.means,
    aes(x = first_response_higher, y = empirical_stat, color = first_actual_higher)
  ) +
  geom_point(
    mapping = aes(x = first_response_higher, y = empirical_stat),
    size = 2.5,
    alpha = 1,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    mapping = aes(x = first_response_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.3),
    size = 1.8,
    width = 0.12
  ) +
  scale_color_manual(
    values = wes_palette(n = 3, name = "Cavalcanti1"),
    name = "first_actual_higher",
    breaks = c("higher", "lower", "equal")
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_discrete(limits = c("higher", "lower", "equal")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy (repeating vs alternating)", y = "strategy_repeating")

p2

# Strategy marginalized over first responses

h2.means.asym <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(symmetric) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)

p3 <-
  ggplot(h2.means.asym, aes(x = symmetric, y = empirical_stat)) +
  geom_point(
    mapping = aes(x = symmetric, y = empirical_stat),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = symmetric, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating")

p3

h2.means.asym <- d %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(first_actual_higher) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)

p4 <-
  ggplot(h2.means.asym, aes(x = first_actual_higher, y = empirical_stat)) +
  geom_point(
    mapping = aes(x = first_actual_higher, y = empirical_stat),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    mapping = aes(x = first_actual_higher, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(title = "strategy marginalized over first responses", y = "strategy_repeating")

p4

# What happens in equal status condition in 1b and 1c?
# Do they correspond with each other?
# Does this correlate with first time predicted in 1c?

# import study 1b data
d.1b.equal <- read.csv(here('data/1b_tidy_data.csv')) %>%
  filter(relationship == "equal", next_interaction != 'none') %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating) %>%
  ungroup() %>%
  rename(observed_higher = relationship)

d.1c.equal.means <- d %>%
  filter(first_actual_higher == "equal") %>%
  mutate(strategy_repeating = recode(
    strategy_repeating,
    "repeating" = 1,
    "alternating" = -1
  )) %>%
  group_by(story) %>%
  tidyboot_mean(strategy_repeating, na.rm = T)


d.1b.equal.means <- d.1b.equal %>%
  filter(next_interaction == "repeating") %>%
  rename(strategy_repeating = next_interaction) %>%
  group_by(story) %>%
  tidyboot_mean(normalized_likert_rating, na.rm = T)


d.1b.1c.equal <-
  left_join(
    d.1b.equal.means,
    d.first.response,
    suffix = c("_1b", "_1c"),
    by = (c("story"))
  )

d.1c.equal <-
  left_join(
    d.1c.equal.means,
    d.first.response,
    suffix = c("_2", "_1"),
    by = (c("story"))
  )


# Plots

f <-
  ggplot(d.1c.equal, aes(x = empirical_stat_1, y = empirical_stat_2)) +
  geom_point(size = 3.3,
             alpha = 0.7,
             stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1, ymin = ci_lower_2, ymax = ci_upper_2),
    size = 1.5,
    width = 0.043,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_2, xmin = ci_lower_1, xmax = ci_upper_1),
    size = 1.5,
    height = 0.03,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 1c repeating expectation", title = "equal status")

f

f <-
  ggplot(d.1b.1c.equal, aes(x = empirical_stat_1c, y = empirical_stat_1b)) +
  geom_point(size = 3.3,
             alpha = 0.7,
             stroke = 0) +
  geom_errorbar(
    mapping = aes(x = empirical_stat_1c, ymin = ci_lower_1b, ymax = ci_upper_1b),
    size = 1.5,
    width = 0.043,
    alpha = 0.6
  ) +
  geom_errorbarh(
    mapping = aes(y = empirical_stat_1b, xmin = ci_lower_1c, xmax = ci_upper_1c),
    size = 1.5,
    height = 0.014,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray") +
  scale_y_continuous(limits = c(0.2, 0.8)) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "study 1c first time higher", y = "study 1b P(repeating)", title = "equal status")

f
