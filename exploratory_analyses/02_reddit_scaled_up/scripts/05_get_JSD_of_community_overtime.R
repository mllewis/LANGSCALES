# get JSD distance for comments for arbitary bags
library(here)
library(tidyverse)

DOC_TOPIC_MODEL_PATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_redpill.csv")
POST_META_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/redpill2years_tidy.csv")
OUT_PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/01_reddit_pilot/data/post_JSD_redpill_community_overtime.csv")
LOCAL_PATH <-  here("/exploratory_analyses/02_reddit_scaled_up/data/")
source(here("exploratory_analyses/01_reddit_pilot/scripts/04_JSD_helpers.R"))

get_community_over_time_jsd <- function(subreddit, local_path){
  # get metadata
  post_meta_data_path <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_metadata <- read_csv(post_meta_data_path, guess_max = 1000000)  %>%
    select(subreddit, post_id, comment_id, author, created_utc) %>%
    mutate(created_bin = lubridate::round_date(created_utc, "week")) %>%
    unite(document, c(subreddit, post_id, comment_id), sep = "-")  %>%
    select(document, created_bin)

  # get topic model
  topic_model_path <- glue("{local_path}topic_models/{subreddit}_dt.csv")
  post_topic_model <- read_csv(topic_model_path)

  # do the thing
  out_pairwise_topic_jsd <- glue("{local_path}jsd_over_community_time/{subreddit}_jsd_over_community_time.csv")
  mean_jsd_values <- get_JSD_of_post_bags(post_metadata, post_topic_model, "created_bin")
  write_csv(mean_jsd_values, out_pairwise_topic_jsd)
}


target_subreddits <- glue("{LOCAL_PATH}raw/comments/")%>%
  list.files() %>%
  str_replace_all("_comments.csv","")

walk(target_subreddits, get_community_over_time_jsd,
     LOCAL_PATH)

