# merge data from big query (comments + posts), tidy
library(tidyverse)
library(anytime)
library(here)
library(glue)

LOCAL_PATH <-  here("/exploratory_analyses/02_reddit_scaled_up/data/")

clean_text <- function(text){
  text %>%
    tolower() %>%
    str_replace_all('\\S+https://\\S+', "L") %>% # remove links
    str_remove_all("[:punct:]") %>%
    str_trim() %>%
    str_squish()
}

tidy_raw_reddit_data <- function(subreddit, local_path){
    post_in_path <- glue("{local_path}raw/posts/{subreddit}_posts.csv")
    raw_posts <- read_csv(post_in_path, guess_max = 10000) %>% # nrows to use to guess cols
      mutate(created_utc = anytime(created_utc),
             link_id = paste0("t3_", id), # name is the same as the link_id, but many missing
             comment_id = link_id,
             parent_id = NA,
             text_type = "post") %>%
      rename(body = selftext,
             post_id = link_id) %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, created_utc,
             title, author, body, num_comments, score)

    comment_in_path <- glue("{local_path}raw/comments/{subreddit}_comments.csv")
    raw_comments <- read_csv(comment_in_path,  guess_max = 1000000) %>%
      mutate(created_utc = anytime(created_utc),
             comment_id = paste0("t1_", id),
             text_type = "comment") %>%
      rename(post_id = link_id) %>%
      select(subreddit, post_id, text_type, parent_id, comment_id,
              created_utc,  author, body, score)

    comments_posts <- bind_rows(raw_posts, raw_comments) %>%
      distinct(comment_id, .keep_all = T) %>%  # there are cases where the same comment is retreived multiple times; take one
      arrange(post_id, text_type, parent_id, comment_id, created_utc)  %>%
      mutate(body_clean = map_chr(body, clean_text)) # tthis is slow

    outpath <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
    write_csv(comments_posts, outpath)
}

### DO THE THING
target_subreddits <- glue("{LOCAL_PATH}raw/comments/")%>%
  list.files() %>%
  str_replace_all("_comments.csv","")

walk(target_subreddits, tidy_raw_reddit_data, LOCAL_PATH)
