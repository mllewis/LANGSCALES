# within the same thread, how long does it take for a different person to respond

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)
library(parallel)


OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/thread_lag_overtime.csv")
LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3

get_thread_lag_overtime <- function(subreddit, local_path, outpath) {

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
    select(subreddit, post_id, author, created_utc, n_words) %>%
    mutate(created_utc = lubridate::as_datetime(created_utc))

  # churn looking at all comments
  comment_lag_all <- post_metadata_tidy %>%
    arrange(post_id, created_utc) %>%
    group_by(post_id) %>%
    mutate(next_author = lead(author, 1),
           previous_created_utc = lag(created_utc, 1)) %>%
    filter(next_author != author, # take the last post by user if they responsed twice
           !is.na(previous_created_utc)) %>%
    mutate(comment_lag = created_utc - previous_created_utc,
          created_bin = lubridate::round_date(created_utc, "week")) %>%
    group_by(subreddit, created_bin) %>%
    summarize(mean_comment_lag = mean(as.numeric(comment_lag)),
              n = n()) %>%
    mutate(comment_length_type = "all") %>%
    as.data.table()

  # ony comments over 100 words
  comment_lag_long <- post_metadata_tidy %>%
    filter(n_words > 100) %>%
    arrange(post_id, created_utc) %>%
    group_by(post_id) %>%
    mutate(next_author = lead(author, 1),
           previous_created_utc = lag(created_utc, 1)) %>%
    filter(next_author != author, # take the last post by user if they responsed twice
           !is.na(previous_created_utc)) %>%
    mutate(comment_lag = created_utc - previous_created_utc,
           created_bin = lubridate::round_date(created_utc, "week")) %>%
    group_by(subreddit, created_bin) %>%
    summarize(mean_comment_lag = mean(as.numeric(comment_lag)),
              n = n()) %>%
    mutate(comment_length_type = "long_only") %>%
    as.data.table()

  lag_df <- bind_rows(comment_lag_all, comment_lag_long)

  write_csv(lag_df, outpath, append = T)


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
  get_thread_lag_overtime(current_subreddit, this_local_path, outpath)
}

parLapply(cluster,
        1:length(target_subreddits),
        parallel_wrapper,
        target_subreddits,
        LOCAL_PATH,
        OUT_PATH)
