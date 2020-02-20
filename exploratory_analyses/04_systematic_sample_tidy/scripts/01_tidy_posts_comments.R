# merge data from big query (comments + posts), tidy V2
library(tidyverse)
library(parallel)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(anytime)
library(glue)
library(here)
source(here("exploratory_analyses/04_systematic_sample_tidy/scripts/utils/reddit_tokenizer.R"))

LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 2
MIN_WORDS_PER_POST <- 50 # excluding stop words

tidy_raw_reddit_data <- function(subreddit, local_path, min_num_words){

    # posts
    post_in_path <- glue("{local_path}raw/posts/{subreddit}_posts.csv")
    raw_posts <-  fread(post_in_path, col.names = c("created_utc",	"subreddit",	"author",	"domain",	"url",
                                                    "num_comments",	"score",	"title",	"selftext",	"id"),
                        colClasses = list(integer = c(1, 6, 7),
                                          character = c(2:5, 8:10)),
                                          verbose = T, select = 1:10) %>%
      lazy_dt()

    raw_posts_dt <- raw_posts %>%
     mutate(created_utc = anytime(created_utc),
           link_id = paste0("t3_", id), # name is the same as the link_id, but many missing
           comment_id = link_id,
           parent_id = NA,
           text_type = "post") %>%
      rename(body = selftext,
             post_id = link_id) %>%
      filter(body != "[deleted]",
             body != "[removed]",
             author != "[deleted]") %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score)

    rm(raw_posts)

    clean_posts_dt <- raw_posts_dt %>%
      mutate(tokenized_list = map(body, tokenize_words_reddit),
             n_words = map_dbl(tokenized_list, ~unlist(.x) %>% length),
             clean_body1 = map_chr(tokenized_list, ~paste(.x, collapse = " ")),
             clean_body = map_chr(clean_body1, str_squish)) %>%
      select(-tokenized_list, -clean_body1) %>%
      as.data.table()

    # comments
    comment_in_path <- glue("{local_path}raw/comments/{subreddit}_comments.csv")
    raw_comments <- fread(comment_in_path,  col.names = c("body", "author",	"created_utc",
                                                          "link_id",	"parent_id",  "score",	"id",	"subreddit"),
                          verbose = T) %>%
      lazy_dt()

    raw_comments_dt <- raw_comments %>%
      rename(post_id = link_id) %>%
      filter(body != "[deleted]",
             body != "[removed]",
             body != "",
             author != "[deleted]") %>%
      mutate(created_utc = anytime(created_utc),
             comment_id = paste0("t1_", id),
             text_type = "comment",
             num_comments = NA,
             title = NA) %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score)

    rm(raw_comments)

    clean_comments_dt <- raw_comments_dt %>%
      mutate(tokenized_list = map(body, tokenize_words_reddit),
             n_words = map_dbl(tokenized_list, ~unlist(.x) %>% length)) %>%
      filter(n_words >= min_num_words) %>%
      mutate(clean_body1 = map_chr(tokenized_list, ~paste(.x, collapse = " ")),
             clean_body = map_chr(clean_body1, str_squish)) %>%
      select(-tokenized_list, -clean_body1) %>%
      as.data.table()



    # combine comments and posts
    comments_posts <- rbindlist(list(clean_posts_dt, clean_comments_dt)) %>%
      unique(by = "comment_id")  # some comments are duplicated with different retrieval times

    rm(raw_posts_dt); rm(raw_comments_dt); rm(clean_posts_dt); rm(clean_comments_dt)

    outpath <- glue("{local_path}tidy2/{subreddit}_tidy_comments_posts_clean.csv")
    fwrite(comments_posts, outpath)
}

small_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/raw/comments/")),
                                                 full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),
         file_name = str_replace(file_name, "_comments.csv", ""),
         file_size = file.info(as.character(path))$size) %>%
  select(-path) %>%
  arrange(-file_size) %>%
  filter(file_size > 1000000000) %>%
  pull(file_name)



complete_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/tidy2/")),
                                                 full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),
         file_name = str_replace(file_name, "_tidy_comments_posts_clean.csv", "")) %>%
  pull(file_name)

subreddits_to_do <- setdiff(small_subreddits, complete_subreddits)

# do the thing
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  tidy_raw_reddit_data(current_subreddit, this_local_path, MIN_WORDS_PER_POST)
}


parLapply(cluster,
         1:length(subreddits_to_do),
         parallel_wrapper,
         small_subreddits,
         LOCAL_PATH)


