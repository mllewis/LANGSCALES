# get JSD distance for comments for nth post at user level and compare to community distance
library(here)
library(tidyverse)
library(philentropy)

DOC_TOPIC_MODEL_PATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_redpill.csv")
POST_META_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/redpill2years_tidy.csv")
OUT_PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/01_reddit_pilot/data/post_JSD_redpill_nth_post.csv")


# get topic model
post_topic_model <- read_csv(DOC_TOPIC_MODEL_PATH)

# get metadata
post_metadata <- read_csv(POST_META_DATA_PATH, guess_max = 1000000)  %>%
  select(subreddit, post_id, comment_id, author, created_utc) %>%
  mutate(created_bin = lubridate::round_date(created_utc, "week")) %>%
  unite(document, c(subreddit, post_id, comment_id), sep = "-")  %>%
  filter(document %in% post_topic_model$document)
hi <- this is %>%
  <- %>%
# get 1 distribution per week per person
post_metadata_one_per_week <- post_metadata %>%
  left_join(post_topic_model) %>%
  unite(author_time, c(author, created_bin), sep = "///", remove = F) %>%
  select(author, created_bin, author_time, topic, gamma) %>%
  group_by(author, created_bin, author_time, topic)  %>%
  summarize(gamma = mean(gamma))

post_metadata_grouped <-  post_metadata_one_per_week %>%
  group_by(author, created_bin) %>%
  nest() %>%
  group_by(author) %>%
  mutate(nth_post = 1:n()) %>%
  ungroup()

# get distance to previous post
JSD_bind <- function(p, q) {
  JSD(rbind(p$gamma, q$gamma))
}

safe_JSD <- possibly(JSD_bind, NA)

previous_JSD <-  post_metadata_grouped %>%
  group_by(author) %>%
  mutate(previous_data = lag(data),
         previous_author_JSD = map2_dbl(data, previous_data, ~safe_JSD(.x, .y))) %>%
  select(author,  created_bin, nth_post, previous_author_JSD)

# get distance to all others at same timepoint
get_within_bin_JSD <- function(df){
  df_wide <- spread(df, topic, gamma)

  JSD_distance_pairwise <-  JSD(as.matrix(df_wide[,-1]))

  diag(JSD_distance_pairwise) <- NA

  data.frame(author = df_wide$author,
             current_community_JSD= colMeans(JSD_distance_pairwise, na.rm = T), row.names = NULL)

}

community_JSD <- post_metadata_one_per_week %>%
  ungroup() %>%
  select(-author_time) %>%
  group_by(created_bin) %>%
  nest() %>%
  mutate(temp = map(data,get_within_bin_JSD)) %>%
  select(-data) %>%
  unnest()


previous_and_community_JSD <- full_join(previous_JSD, community_JSD)

write_csv(previous_and_community_JSD, OUT_PAIRWISE_TOPIC_JSD)



new_function <- function(what) {
  this %>%
    to %>% this
  <- is the
}
