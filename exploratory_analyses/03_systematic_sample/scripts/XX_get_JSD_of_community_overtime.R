# get JSD distance for comments for arbitary bags
library(here)
library(tidyverse)
library(glue)
library(parallel)

NCLUSTERS <- 3
OUT_PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/03_systematic_sample/datapost_JSD_redpill_community_overtime.csv")
LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
source(here("exploratory_analyses/01_reddit_pilot/scripts/04_JSD_helpers.R"))

get_community_over_time_jsd <- function(subreddit, local_path){
  # get metadata
  post_meta_data_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_meta_data_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")

  if(file.exists(post_meta_data_path1)){
    post_meta_data_path <- post_meta_data_path1
  } else {
    post_meta_data_path <- post_meta_data_path2
  }
  post_metadata <- fread(post_meta_data_path) %>%
    select(subreddit, post_id, comment_id, author, created_utc) %>%
    mutate(created_bin = lubridate::round_date(as.POSIXct(created_utc), "week"),
           document = paste(subreddit, post_id, comment_id, sep = "-"))

  # get topic model
  topic_model_path <- glue("{local_path}topic_models/{subreddit}_dt.csv")
  post_topic_model <- fread(topic_model_path)

  # do the thing
  out_pairwise_topic_jsd <- glue("{local_path}jsd_over_community_time/{subreddit}_jsd_over_community_time.csv")
  mean_jsd_values <- get_JSD_of_post_bags(post_metadata, post_topic_model, "created_bin")
  fwrite(mean_jsd_values, out_pairwise_topic_jsd)
}


target_subreddits <- glue("{LOCAL_PATH}topic_models/") %>%
  list.files()%>%
  str_replace_all("_dt.csv","") %>%
  str_replace_all("_tw.csv","") %>%
  unique()

completed <- glue("{LOCAL_PATH}jsd_over_community_time/")%>%
  list.files() %>%
  str_replace_all("_jsd_over_community_time.csv","")

remaining <- setdiff(target_subreddits, completed )



# initial cluster
#cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  get_community_over_time_jsd(current_subreddit, LOCAL_PATH)
}

walk(remaining, get_community_over_time_jsd, LOCAL_PATH)

#parLapply(cluster,
#        1:length(remaining),
#        parallel_wrapper,
#        remaining,
#        LOCAL_PATH)


