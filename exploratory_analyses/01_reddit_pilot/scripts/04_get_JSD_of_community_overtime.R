# get JSD distance for comments for arbitary bags
library(here)
library(tidyverse)

DOC_TOPIC_MODEL_PATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_redpill.csv")
POST_META_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/redpill2years_tidy.csv")
OUT_PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/01_reddit_pilot/data/post_JSD_redpill_community_overtime.csv")

source(here("exploratory_analyses/01_reddit_pilot/scripts/04_JSD_helpers.R"))

# get metadata
post_metadata <- read_csv(POST_META_DATA_PATH, guess_max = 1000000)  %>%
  select(subreddit, post_id, comment_id, author, created_utc) %>%
  mutate(created_bin = lubridate::round_date(created_utc, "week")) %>%
  unite(document, c(subreddit, post_id, comment_id), sep = "-")  %>%
  select(document, created_bin)

# get topic model 
post_topic_model <- read_csv(DOC_TOPIC_MODEL_PATH) 

# do the thing
mean_jsd_values <- get_JSD_of_post_bags(post_metadata, post_topic_model, "created_bin")
write_csv(mean_jsd_values, OUT_PAIRWISE_TOPIC_JSD)


