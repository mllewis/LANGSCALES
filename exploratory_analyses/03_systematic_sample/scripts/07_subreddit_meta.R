# get density of author postings in each subreddit

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_meta_data.csv")
get_author_entropy <- function(df){
  number_of_posts_per_author <-  count(df, author)$n
  n_authors <- length(number_of_posts_per_author)
  entropy(number_of_posts_per_author)/log(n_authors)
}

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
    lazy_dt() %>%
    mutate(created_utc = as.POSIXct(created_utc)) %>%
    select(-body)  %>%
    as.data.table()

  n_comments_total <-  count(tidy_data, text_type) %>%
    filter(text_type == "comment") %>%
    pull(n)

  n_posts_total <-  count(tidy_data, text_type) %>%
    filter(text_type == "post") %>%
    pull(n)

  long_posts <- tidy_data %>%
    filter(n_words >= 100) %>%
    mutate(n_words = as.numeric(n_words),
           score = as.numeric(score))

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

  all_meta <-  long_posts %>%
      summarize(
        author_H  = get_author_entropy(.),
        author_n = length(unique(.$author)),
        word_H =  entropy(.$n_words),
        word_mean_n = mean(.$n_words),
        word_sd = sd(.$n_words),
        word_total = sum(.$n_words),
        score_mean = mean(.$score),
        score_sd = sd(.$score),
        score_H = entropy(.$score),
        comment_long_n = n()) %>%
      mutate(
       comments_n_all = n_comments_total,
       posts_n_all = n_posts_total,
       comments_posts_ratio = n_comments_total/n_posts_total) %>%
    bind_cols(author_time_data) %>%
    mutate(subreddit = subreddit) %>%
    select(subreddit, everything())


  write_csv(all_meta, outpath, append = T)

}


target_subreddits <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts.csv","") %>%
  str_replace_all("_tidy_comments_posts_f.csv","")


walk(rev(target_subreddits)[14:97], get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)


