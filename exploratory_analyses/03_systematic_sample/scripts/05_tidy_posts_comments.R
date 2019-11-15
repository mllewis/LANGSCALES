# merge data from big query (comments + posts), tidy
library(tidyverse)
library(parallel)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(anytime)
library(glue)
library(quanteda)
library(tokenizers)

LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 4
MIN_WORDS_PER_POST <- 100 # excluding stop words

local_path <- LOCAL_PATH
subreddit = "FortNiteBR"
subreddit = "loseit"


tidy_raw_reddit_data <- function(subreddit, local_path, min_num_words){

    # posts
    post_in_path <- glue("{local_path}raw/posts/{subreddit}_posts.csv")
    raw_posts <-  fread(post_in_path, col.names = c("created_utc",	"subreddit",	"author",	"domain",	"url",
                                                    "num_comments",	"score",	"title",	"selftext",	"id"),
                        colClasses = list(integer = c(1, 6, 7),
                                          character = c(2:5, 8:10)),
                                          verbose = T, select = 1:10) %>%
      lazy_dt()

    raw_posts_dt <-raw_posts %>%
     mutate(created_utc = anytime(created_utc),
           link_id = paste0("t3_", id), # name is the same as the link_id, but many missing
           comment_id = link_id,
           parent_id = NA,
           text_type = "post",
           n_words = NA) %>%
      rename(body = selftext,
             post_id = link_id) %>%
      filter(body != "[deleted]",
             body != "[removed]",
             author != "[deleted]") %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score, n_words) %>%
      as.data.table()

    rm(raw_posts)

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
             title, author, body, num_comments, score) %>%

      as.data.table()

    raw_comments_dt$n_words <- tokenize_words(raw_comments_dt$body) %>%
      map_dbl(length)
    raw_comments_dt_filtered <- raw_comments_dt[n_words >= min_num_words]

    rm(raw_comments)

    # combine comments and posts
    comments_posts <- rbindlist(list(raw_posts_dt, raw_comments_dt_filtered)) %>%
      unique(by = "comment_id")  # some comments are duplicated with different retrieval times

    rm(raw_posts_dt); rm(raw_comments_dt); rm(raw_comments_dt_filtered)

    outpath <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
    fwrite(comments_posts, outpath)
}

### SMALL FILES: DO THE THING (IN PARALLEL)
small_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/raw/comments/")),
                                                 full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),
         file_name = str_replace(file_name, "_comments.csv", ""),
         file_size = file.info(as.character(path))$size) %>%
  select(-path) %>%
  arrange(-file_size) %>%
  filter(file_size < 1000000000) %>%
  pull(file_name)

#walk(rev(small_subreddits), tidy_raw_reddit_data, LOCAL_PATH)
# initial cluster
#cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  tidy_raw_reddit_data(current_subreddit, this_local_path)
}

#parLapply(cluster,
 #        1:length(small_subreddits),
 #        parallel_wrapper,
 #        small_subreddits,
  #       LOCAL_PATH)

### LARGE FILES
large_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/raw/comments/")),
                                                 full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),
         file_name = str_replace(file_name, "_comments.csv", ""),
         file_size = file.info(as.character(path))$size) %>%
  select(-path) %>%
  arrange(-file_size) %>%
  filter(file_size >= 1000000000) %>%
  pull(file_name)


all_subreddits <- glue("{LOCAL_PATH}raw/comments/")%>%
  list.files() %>%
  str_replace_all("_comments.csv","")

completed <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts.csv","")

completed2 <- glue("{LOCAL_PATH}tidy/")%>%
  list.files() %>%
  str_replace_all("_tidy_comments_posts_f.csv","")

todo <- setdiff(all_subreddits, completed) %>% setdiff(completed2)
walk(todo[3:6], tidy_raw_reddit_data, LOCAL_PATH, MIN_WORDS_PER_POST)


#walk(rev(large_subreddits), tidy_raw_reddit_data, LOCAL_PATH)
