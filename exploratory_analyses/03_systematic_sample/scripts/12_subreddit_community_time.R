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

overall_lag <- function(df){
  df %>%
    filter(text_type == "comment") %>%
    arrange(created_utc) %>%
    mutate(next_author = lead(author, 1),
           previous_created_utc = lag(created_utc, 1)) %>%
    filter(next_author != author, # take the last post by user if they responsed twice
           !is.na(previous_created_utc)) %>%
    mutate(comment_lag = created_utc - previous_created_utc) %>%
    summarize(mean = mean(comment_lag),
              sd = sd(comment_lag)) %>%
    mutate(coeff_var = as.numeric(sd)/as.numeric(mean)) %>%
    pull(coeff_var)
  #http://barabasi.com/f/233.pdf


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
    as.data.table()


  author_time_data <-  try(get_author_time_data(long_posts), TRUE)

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


walk(rev(target_subreddits)[14:97], get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)


