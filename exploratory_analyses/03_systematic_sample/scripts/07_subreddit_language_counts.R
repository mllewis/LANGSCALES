# get get word/people/score counts/entropy

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_counts_scores.csv")



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


  all_meta <-  long_posts %>%
      summarize(
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
    mutate(subreddit = subreddit) %>%
    select(subreddit, everything())


  write_csv(all_meta, outpath, append = T)

}


target_subreddits <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts.csv","") %>%
  str_replace_all("_tidy_comments_posts_f.csv","")


walk(target_subreddits, get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)


