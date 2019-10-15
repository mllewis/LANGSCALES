# get JSD distance for comments for arbitary bags
library(here)
library(tidyverse)


DOC_TOPIC_MODEL_PATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_pittsburgh.csv")
POST_META_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/pittsburgh2years_tidy.csv")
OUT_PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/01_reddit_pilot/data/post_JSD_pittsburgh_nth_post.csv")

source(here("exploratory_analyses/01_reddit_pilot/scripts/04_JSD_helpers.R"))

# get metadata

# get topic model 
post_topic_model <- read_csv(DOC_TOPIC_MODEL_PATH) 

post_metadata <- read_csv(POST_META_DATA_PATH, guess_max = 1000000)  %>%
  select(subreddit, post_id, comment_id, author, created_utc) %>%
  mutate(created_bin = lubridate::round_date(created_utc, "week")) %>%
  unite(document, c(subreddit, post_id, comment_id), sep = "-")  %>%
  filter(document %in% post_topic_model$document) 

# get 1 distribution per week per person
post_metadata_grouped <- post_metadata %>%
  left_join(post_topic_model) %>%
  unite(author_time, c(author, created_bin), sep = "///", remove = F) %>% 
  select(author, created_bin, author_time, topic, gamma) %>%
  group_by(author, created_bin, author_time, topic)  %>%
  summarize(gamma = mean(gamma)) %>%
  group_by(author, created_bin) %>%
  nest() %>%
  group_by(author) %>%
  mutate(nth_post = 1:n()) %>%
  ungroup() %>%
  mutate(data = unlist(data))
  #unnest(cols = c(data))
  

JSD_pair_roll <- rollify(mean, window = 2)


post_metadata_grouped %>%
  group_by(author) %>%
  mutate(JSD_distance = JSD_pair_roll(data[1]$gamma))
  

# do the thing
mean_jsd_values <- get_JSD_of_post_bags(post_metadata, post_topic_model, "created_bin")
write_csv(mean_jsd_values, OUT_PAIRWISE_TOPIC_JSD)


