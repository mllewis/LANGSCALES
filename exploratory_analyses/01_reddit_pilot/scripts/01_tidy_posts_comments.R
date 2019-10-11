# merge data from big query (comments + posts), tidy
library(tidyverse)
library(anytime)
library(here)


clean_text <- function(text){
  text %>%
    tolower() %>% 
    str_replace_all('\\S+https://\\S+', "L") %>% # remove links
    str_remove_all("[:punct:]") %>%
    str_trim() %>% 
    str_squish() 
}

tidy_raw_reddit_data <- function(post_path, comment_path, out_path){
    raw_posts <- read_csv(post_path, guess_max = 10000) %>% # nrows to use to guess cols 
      mutate(retrieved_on = anytime(retrieved_on), # converts from unix time
             created_utc = anytime(created_utc),
             link_id = paste0("t3_", id), # name is the same as the link_id, but many missing
             comment_id = link_id, 
             parent_id = NA,
             text_type = "post") %>% 
      mutate_at(c("author", "domain", "url", "title", "selftext", "thumbnail",
                  "is_self", "permalink", "author_flair_text", "link_id"), as.factor)  %>%
      select(-ups, -downs, -saved, -from_kind, -from,
             -stickied, -over_18, -subreddit_id, -hide_score, -link_flair_css_class,
             -author_flair_css_class, -archived, -from_id, -quarantine, -link_flair_text,
             -distinguished, -name, -id)  %>%
      rename(body = selftext) 
    
    raw_comments <- read_csv(comment_path,  guess_max = 1000000) %>%
      mutate(retrieved_on = anytime(retrieved_on),
             created_utc = anytime(created_utc),
             comment_id = paste0("t1_", id),
             text_type = "comment") %>%
      select(-ups, -distinguished, -author_flair_css_class, -author_flair_text,
             -score_hidden, -downs, -name, -subreddit_id, -archived, -id ) %>%
      mutate_at(c("author", "link_id", "parent_id", "comment_id"),
                as.factor) 
    
    comments_posts <- bind_rows(raw_posts, raw_comments) %>%
      rename(post_id = link_id) %>%
      select(subreddit, post_id, text_type, parent_id, comment_id, 
             created_utc, title, author, body, num_comments, score, everything()) %>%
      group_by(comment_id) %>%
      arrange(comment_id, retrieved_on) %>% # there are cases where the same comment is retreived multiple times
      slice(n()) %>%
      distinct() %>%
      arrange(post_id, text_type, parent_id, comment_id, created_utc)  %>%
      mutate(body_clean = map_chr(body, clean_text))
      
    write_csv(comments_posts, out_path)
}

### DO THE THING
POSTS_PATHS <- list(here("exploratory_analyses/01_reddit_pilot/data/redpill2years_posts.csv"),
                    here("exploratory_analyses/01_reddit_pilot/data/pittsburgh2years_posts.csv"))
COMMENTS_PATHS <- list(here("exploratory_analyses/01_reddit_pilot/data/redpill2years_comments.csv"),
                       here("exploratory_analyses/01_reddit_pilot/data/pittsburgh2years_comments.csv"))
OUTPATHS <-  list(here("exploratory_analyses/01_reddit_pilot/data/redpill2years_tidy.csv"),
                  here("exploratory_analyses/01_reddit_pilot/data/pittsburgh2years_tidy.csv"))

list(POSTS_PATHS[1], COMMENTS_PATHS[1], OUTPATHS[1]) %>%
  pmap(tidy_raw_reddit_data)
