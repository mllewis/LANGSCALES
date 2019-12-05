# Get subreddit level time author variables

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_author_time_data.csv")


get_author_time_data <- function(df){

  this_lazy_df <- lazy_dt(df)
  author_longevity <- this_lazy_df %>%
    group_by(author) %>%
    mutate(total_time = as.numeric(max(created_utc) - min(created_utc))) %>%
    ungroup() %>%
    select(author, total_time) %>%
    summarize(author_longevity_mean = mean(total_time),
              author_sd_mean = sd(total_time),
              author_longevity_H = entropy(total_time)) %>%
    as.data.table()

  author_lag <- this_lazy_df %>%
    group_by(author) %>%
    arrange(author, created_utc) %>%
    mutate(previous_post_time = lag(created_utc, 1),
           time_lag = created_utc - previous_post_time) %>%
    summarize(mean_lag = as.numeric(mean(time_lag, na.rm = T))) %>%
    filter(mean_lag != "NaN") %>%
    summarize(author_lag_sd = sd(mean_lag),
              author_lag_H = entropy(mean_lag),
              author_lag_mean = mean(mean_lag)) %>%
    as.data.table()

  bind_cols(author_longevity, author_lag)
}



get_meta_for_one_subreddit <- function(subreddit, local_path, outpath){
  print(subreddit)
  inpath1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  inpath2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(inpath1)){
    inpath <- inpath1
  } else {
    inpath <- inpath2
  }

  tidy_data <- fread(inpath) %>%
    mutate(created_utc = lubridate::as_datetime(created_utc)) %>%
    select(-body)  %>%
    as.data.table() %>%
    filter(n_words >= 100)


  author_time_data <-  try(get_author_time_data(tidy_data), TRUE)

  if(inherits(author_time_data, "try-error")){
    author_time_data <- data.frame(author_longevity_mean = NA,
                                   author_sd_mean = NA,
                                   author_longevity_H = NA,
                                   author_lag_sd = NA,
                                   author_lag_H = NA,
                                   author_lag_mean = NA)
  } else {
    author_time_data <- author_time_data
  }

  author_time_meta_data <-
    bind_cols(author_time_data) %>%
    mutate(subreddit = subreddit) %>%
    select(subreddit, everything())


  write_csv(author_time_meta_data, outpath, append = T)

}


target_subreddits <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts.csv","") %>%
  str_replace_all("_tidy_comments_posts_f.csv","")


walk(target_subreddits, get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)


