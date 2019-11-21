# get JSD distance for comments for nth post at user level and compare to community distance
library(here)
library(tidyverse)
library(philentropy)
library(glue)
library(data.table)
library(dtplyr)
library(parallel)
library(dplyr, warn.conflicts = FALSE)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3

# get distance to previous post
JSD_bind <- function(p, q) {
  JSD(rbind(p$gamma, q$gamma))
}

safe_JSD <- possibly(JSD_bind, NA)

get_jsd_nth_post <- function(subreddit, local_path){
  print(subreddit)

  # get topic model
  doc_topic_model_path <- glue("{local_path}topic_models/{subreddit}_dt.csv")
  post_topic_model <- fread(doc_topic_model_path)

  # get metadata
  post_meta_data_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_meta_data_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(post_meta_data_path1)){
    post_meta_data_path <- post_meta_data_path1
  } else {
    post_meta_data_path <- post_meta_data_path2
  }

  try({

    post_metadata_one_per_week <- fread(post_meta_data_path) %>%
        select(subreddit, post_id, comment_id, author, created_utc, n_words) %>%
        mutate(created_bin = lubridate::round_date(as.POSIXct(created_utc), "week"),
               document = paste(subreddit, post_id, comment_id, sep = "-"))  %>%
        filter(document %in% post_topic_model$document) %>%
        group_by(subreddit, author, created_bin) %>%
        filter(n_words == max(n_words)) %>% # select longest post each week (randomly select one if there are two of equal length)
        slice(1) %>%
        left_join(post_topic_model)


  post_metadata_grouped <-  post_metadata_one_per_week %>%
    group_by(author, created_bin) %>%
    nest() %>%
    group_by(author) %>%
    mutate(nth_post = 1:n()) %>%
    ungroup()

  previous_JSD <-  post_metadata_grouped %>%
    group_by(author) %>%
    mutate(previous_data = lag(data),
           previous_author_JSD = map2_dbl(data, previous_data, ~safe_JSD(.x, .y))) %>%
    select(author,  created_bin, nth_post, previous_author_JSD)

  out_pairwise_topic_JSD <- glue("{local_path}jsd_nth_post/{subreddit}_previous_jsd_nth_post.csv")
  write_csv(previous_JSD, out_pairwise_topic_JSD)
  }, silent = T)
}

completed <- glue("{LOCAL_PATH}jsd_nth_post/")%>%
  list.files() %>%
  str_replace_all("_previous_jsd_nth_post.csv","")


target_subreddits <- glue("{LOCAL_PATH}topic_models/") %>%
  list.files()%>%
  str_replace_all("_dt.csv","") %>%
  str_replace_all("_tw.csv","") %>%
  unique()

remaining <- setdiff(target_subreddits, completed)

# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  get_jsd_nth_post(current_subreddit, LOCAL_PATH)
}

parLapply(cluster,
       1:length(remaining),
       parallel_wrapper,
       remaining,
       LOCAL_PATH)
