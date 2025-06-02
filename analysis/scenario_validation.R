library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(forcats)
library(showtext)

showtext_auto()

font_add(family = "Lato", regular = here("fonts/Lato-Regular.ttf"))
theme_set(theme_classic(base_size = 15, base_family = "Lato"))

options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))


# Load data

d.benefit <- read.csv(here('data/validation_benefit_data.csv')) %>% 
  filter(pass_attention == T, understood == "yes") %>%
  rename("recipient" = expected_high_benefit, 
         "generous_actor" = expected_low_benefit) %>% 
  pivot_longer(
    cols = c("generous_actor", "recipient"),
    names_to = "partner",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention"))

print(length(unique(d.benefit$subject_id)))

d.effort <- read.csv(here('data/validation_effort_data.csv')) %>% 
  filter(pass_attention == T, understood == "yes") %>%
  rename("recipient" = expected_high_benefit, 
         "generous_actor" = expected_low_benefit) %>% 
  pivot_longer(
    cols = c("generous_actor", "recipient"),
    names_to = "partner",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention"))

print(length(unique(d.effort$subject_id)))



# Calculate diffs

diffs.benefit <- d.benefit %>%
  group_by(subject_id, story) %>%
  spread(partner, likert_rating) %>%
  mutate(diff = recipient - generous_actor, type = "benefit")

diffs.effort <- d.effort %>%
  group_by(subject_id, story) %>%
  spread(partner, likert_rating) %>%
  mutate(diff = generous_actor - recipient, type = "effort")

diffs <- bind_rows(diffs.benefit, diffs.effort)

diffs.summary <- diffs %>%
  group_by(story, type) %>%
  tidyboot_mean(diff, na.rm = TRUE) %>%
  rename(diff = empirical_stat)



# arrange descending
df.temp <- diffs.summary %>%
  filter(type == "benefit") %>%
  arrange(desc(diff))

levs <- unique(df.temp$story)

diffs.summary$story <- factor(diffs.summary$story, levels = levs)

write.csv(diffs.summary, here("data/scenarios_diffs.csv"), row.names = FALSE)



# Calculate absolute means for benefit and effort
# Are these scenarios coordination or zero sum?
benefit.long <- diffs.benefit %>% pivot_longer(
  names_to = "partner",
  values_to = "benefit",
  cols = c("recipient", "generous_actor")
)

effort.long <- diffs.effort %>% pivot_longer(
  names_to = "partner",
  values_to = "effort",
  cols = c("recipient", "generous_actor")
)


benefit.long$story <- factor(benefit.long$story, levels = levs)
effort.long$story <- factor(effort.long$story, levels = levs)

benefit.long.summary <- benefit.long %>%
  group_by(story, partner) %>%
  tidyboot_mean(benefit, na.rm = T) %>%
  rename(benefit = empirical_stat)

write.csv(benefit.long.summary,
          here("data/scenarios_benefit.csv"),
          row.names = FALSE)

effort.long.summary <- effort.long %>%
  group_by(story, partner) %>%
  tidyboot_mean(effort, na.rm = T) %>%
  rename(effort = empirical_stat)

write.csv(effort.long.summary,
          here("data/scenarios_effort.csv"),
          row.names = FALSE)



# PLOTS -------------------------------------------------------------------


# Diffs
ggplot(diffs, aes(x = story, y = diff, fill = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_violin(width = 2.0,
              bw = 0.43,
              position = position_dodge(width = 0.4), alpha = 1) +
  geom_point(
    data = diffs.summary,
    aes(x = story, y = diff),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = diffs.summary,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  labs(x = "Scenario", y = "Difference between the people interacting") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        legend.position = "none") + 
  facet_wrap(~str_to_title(type), nrow = 2)

ggsave(here("figures/scenario_diffs.pdf"), width = 8, height = 5)

# Benefit

ggplot(
  benefit.long,
  aes(x = story, y = benefit)
) +
  geom_violin(
    width = 2.0,
    bw = 0.43,
    position = position_dodge(width = 0.4), 
    fill = "#fbb4ae"
  ) +
  geom_point(
    data = benefit.long.summary,
    aes(x = story, y = benefit),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = benefit.long.summary,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "Scenario", y = "Benefit compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  facet_wrap(~partner, nrow = 2, labeller = labeller(partner = c("generous_actor" = "Generous actor", "recipient" = "Recipient")))

ggsave(here("figures/scenario_benefit.pdf"), width = 8, height = 5)

# Effort

ggplot(
  effort.long,
  aes(x = story, y = effort)
) +
  geom_violin(
    width = 2.0,
    bw = 0.43,
    position = position_dodge(width = 0.4),
    fill = "#b3cde3"
  ) +
  geom_point(
    data = effort.long.summary,
    aes(x = story, y = effort),
    size = 1,
    alpha = 1,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    data = effort.long.summary,
    aes(x = story, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.4),
    size = 1,
    width = 0.2
  ) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "Scenario", y = "Effort compared to not interacting") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  facet_wrap(~partner, nrow = 2, labeller = labeller(partner = c("generous_actor" = "Generous actor", "recipient" = "Recipient")))

ggsave(here("figures/scenario_effort.pdf"), width = 8, height = 5)

