# get word counts by subreddit
library(here)
library(tidyverse)
library(glue)
library(data.table)
library(dtplyr)
library(tm)
library(tidytext)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"

get_word_counts_subreddit <- function(subreddit, local_path, nwords_per_post){
   print(subreddit)

  # tidy comments
  subreddit_path1 <- glue("{local_path}tidy2/{subreddit}_tidy_comments_posts_clean.csv")
  subreddit_path2 <- glue("{local_path}tidy2/{subreddit}_tidy_comments_posts_f_clean.csv")
  if(file.exists(subreddit_path1)){
    subreddit_path <- subreddit_path1
  } else {
    subreddit_path <- subreddit_path2
  }

    tidy_subreddit_data <- fread(subreddit_path) %>%
      lazy_dt()

    reddit_word_counts <- tidy_subreddit_data %>%
      filter(text_type == "comment") %>%
      select(subreddit, clean_body) %>%
      as.data.table() %>%
      unnest_tokens(word, clean_body) %>%
      count(subreddit, word) %>%
      arrange(-n)

    word_count_outpath <- glue("{local_path}word_counts2/{subreddit}_word_counts2.csv")
    write_csv(reddit_word_counts, word_count_outpath)
}



complete_subreddits <- glue("{LOCAL_PATH}tidy2/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts_clean.csv","") %>%
  str_replace_all("_tidy_comments_posts_f_clean.csv","") %>%
  unique()


walk(rev(complete_subreddits),
     get_word_counts_subreddit,
     LOCAL_PATH)
