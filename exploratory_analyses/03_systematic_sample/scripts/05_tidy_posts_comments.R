# merge data from big query (comments + posts), tidy
library(tidyverse)
library(parallel)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(anytime)
library(here)
library(glue)

LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 4

subreddit <- "movies"
local_path <- LOCAL_PATH
subreddit = "ApexSquads"
subreddit = "Android"




tidy_raw_reddit_data <- function(subreddit, local_path){

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
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score) %>%
      filter(body != "[deleted]",
             body != "[removed]") %>%
      as.data.table()

    rm(raw_posts)

    # comments
    comment_in_path <- glue("{local_path}raw/comments/{subreddit}_comments.csv")
    raw_comments <- fread(comment_in_path,  col.names = c("body", "author",	"created_utc",
                                                          "link_id",	"parent_id",  "score",	"id",	"subreddit"),
                          verbose = T) %>%
      lazy_dt()

    raw_comments_dt <- raw_comments %>%
      mutate(created_utc = anytime(created_utc),
             comment_id = paste0("t1_", id),
             text_type = "comment",
             num_comments = NA,
             title = NA) %>%
      rename(post_id = link_id) %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score) %>%
      filter(body != "[deleted]",
             body != "[removed]") %>%
      as.data.table()

    rm(raw_comments)

    # combine comments and posts
    comments_posts <- rbindlist(list(raw_posts_dt, raw_comments_dt)) %>%
      unique(by = "comment_id") # some comments are duplicated with different retrieval times

    rm(raw_posts_dt); rm(raw_comments_dt)

    setDT(comments_posts)[, body := tolower(body)] # make everything lower
    setDT(comments_posts)[, body := gsub("\n|\\s+", " ", body, perl = TRUE)] # get rid of new line characters  and extra white space
    setDT(comments_posts)[, body := gsub('http\\S+\\s*', "L", body, perl = TRUE)] # replace links
    setDT(comments_posts)[, body := gsub("[[:punct:]]|’|\uFFFD|—", "", body, perl = TRUE)] # get rid of punctuation

    outpath <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
    fwrite(comments_posts, outpath)
}

### DO THE THING (IN PARALLEL)
target_subreddits <- glue("{LOCAL_PATH}raw/comments/")%>%
  list.files() %>%
  str_replace_all("_comments.csv","")

# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  tidy_raw_reddit_data(current_subreddit, this_local_path)
}

parLapply(cluster,
          1:length(target_subreddits),
          parallel_wrapper,
          target_subreddits,
          LOCAL_PATH)
