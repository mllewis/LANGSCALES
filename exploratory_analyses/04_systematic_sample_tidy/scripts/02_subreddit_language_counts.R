# get get word/people/score counts/entropy

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
OUT_PATH <- here("exploratory_analyses/04_systematic_sample_tidy/data/subreddit_meta.csv")


get_meta_for_one_subreddit <- function(subreddit, local_path, outpath){
  print(subreddit)

  inpath1 <- glue("{local_path}tidy2/{subreddit}_tidy_comments_posts_clean.csv")
  inpath2 <- glue("{local_path}tidy2/{subreddit}_tidy_comments_posts_f_clean.csv")

  if(file.exists(inpath1)){
    inpath <- inpath1
  } else {
    inpath <- inpath2
  }

  tidy_data <- fread(inpath) %>%
    lazy_dt() %>%
    mutate(created_utc = as.POSIXct(created_utc),
           n_words = as.numeric(n_words),
           score = as.numeric(score),
           num_comments = as.numeric(num_comments)) %>%
    select(-body, -clean_body)

  n_comments_total <-  count(tidy_data, text_type) %>%
    filter(text_type == "comment") %>%
    as.data.table()%>%
    pull(n)

  n_posts_total <-  count(tidy_data, text_type) %>%
    filter(text_type == "post") %>%
    as.data.table()%>%
    pull(n)

  all_meta <-  tidy_data %>%
      filter(text_type == "comment") %>%
      summarize(
        author_n = length(unique(.$author)),
        word_H =  entropy(.$n_words),
        word_mean_n = mean(.$n_words),
        word_sd = sd(.$n_words),
        word_total = sum(.$n_words),
        score_mean = mean(.$score),
        score_sd = sd(.$score),
        score_H = entropy(.$score)) %>%
      mutate(
       comments_n_all = n_comments_total,
       posts_n_all = n_posts_total,
       comments_posts_ratio = n_comments_total/n_posts_total) %>%
    mutate(subreddit = subreddit) %>%
    select(subreddit, everything()) %>%
    as.data.table()


  write_csv(all_meta, outpath, append = T)

}


target_subreddits <- glue("{LOCAL_PATH}tidy2/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts_clean.csv","") %>%
  str_replace_all("_tidy_comments_posts_f_clean.csv","")


walk(target_subreddits, get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)


