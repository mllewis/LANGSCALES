# get density of author postings in each subreddit (entropy over author counts and gini coefficient)

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_author_inequality_long.csv")


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
    mutate(created_utc = as.POSIXct(created_utc)) %>%
    select(-body) %>%
    filter(n_words > 100)

  author_inequality  <-  tidy_data %>%
    group_by(author, text_type) %>%
    summarize(n = n()) %>%
    group_by(text_type) %>%
    summarize(n_authors = n(),
              author_H = entropy(n),
              gini_coeff = reldist::gini(n)) %>%
    mutate(normalized_author_H= author_H/log(n_authors)) %>%
    select(-n_authors) %>%
    gather("measure", "value", -text_type) %>%
    unite("measure", text_type:measure, sep = "_") %>%
    spread(measure, value) %>%
    mutate(subreddit = subreddit) %>%
    select(subreddit, everything())

  write_csv(author_inequality, outpath, append = T)

}


target_subreddits <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts.csv","") %>%
  str_replace_all("_tidy_comments_posts_f.csv","")


walk(target_subreddits, get_meta_for_one_subreddit, LOCAL_PATH, OUT_PATH)
