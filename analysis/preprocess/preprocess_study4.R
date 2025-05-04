library(here)
library(tidyverse)
library(digest)

d <- read_csv(here('data/study-4/non_anonymized/study-4_data.csv'))
d.dem <- read_csv(here('data/study-4/non_anonymized/study-4_demographics.csv'))

# Anonymize participants
d$subject_id <- sapply(d$subject_id, digest)
d.dem$subject_id <- sapply(d.dem$subject_id, digest)

# switch coding of relationship for conversation
d %>%
  mutate(relationship = case_when(
    story == "conversation" & relationship == "less" ~ "more",
    story == "conversation" & relationship == "more" ~ "less",
    TRUE ~ relationship
  )) %>%
  write_csv(here('data/study-4_data.csv'))

d.dem %>%
  write_csv(here('data/study-4_demographics.csv'))