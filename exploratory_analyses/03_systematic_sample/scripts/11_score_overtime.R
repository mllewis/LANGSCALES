# what does the mean score look like over time

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)
library(parallel)


OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/score_overtime.csv")
LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3

get_score_overtime <- function(subreddit, local_path, outpath) {

  # get metadata
  post_meta_data_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_meta_data_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(post_meta_data_path1)){
    post_meta_data_path <- post_meta_data_path1
  } else {
    post_meta_data_path <- post_meta_data_path2
  }

try({

  post_metadata <- fread(post_meta_data_path) %>%
    lazy_dt()

  post_metadata_tidy <- post_metadata %>%
    filter(text_type == "comment") %>%
    mutate(created_utc = lubridate::as_datetime(created_utc),
           created_bin = lubridate::round_date(created_utc, "week")) %>%
    select(subreddit, post_id, created_bin, score, n_words)


  # churn looking at all comments
  comment_score_all <- post_metadata_tidy %>%
    group_by(created_bin, post_id) %>%
    summarize(mean_score = mean(score)) %>%
    group_by(created_bin) %>%
    summarize(mean_score = mean(mean_score)) %>%
    mutate(comment_length_type = "all") %>%
    as.data.table()

  # ony comments over 100 words
  comment_score_long <- post_metadata_tidy %>%
    filter(n_words > 100) %>%
    group_by(created_bin, post_id) %>%
    summarize(mean_score = mean(score)) %>%
    group_by(created_bin) %>%
    summarize(mean_score = mean(mean_score)) %>%
    mutate(comment_length_type = "all") %>%
    as.data.table()

  score_df <- bind_rows(comment_score_all, comment_score_long) %>%
    mutate(subreddit = subreddit)

  write_csv(score_df, outpath, append = T)


  }, silent = T)

}


target_subreddits <- glue("{LOCAL_PATH}topic_models/") %>%
  list.files()%>%
  str_replace_all("_dt.csv","") %>%
  str_replace_all("_tw.csv","") %>%
  unique()


# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path, outpath){
  current_subreddit <- all_subreddits[id]
  get_score_overtime(current_subreddit, this_local_path, outpath)
}

parLapply(cluster,
        1:length(target_subreddits),
        parallel_wrapper,
        target_subreddits,
        LOCAL_PATH,
        OUT_PATH)
