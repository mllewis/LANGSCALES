# get author in/out (churn) over community time

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)
library(parallel)


OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/churn_overtime.csv")
LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3

get_churn_overtime <- function(subreddit, local_path, outpath) {

  # get metadata
  post_meta_data_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_meta_data_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(post_meta_data_path1)){
    post_meta_data_path <- post_meta_data_path1
  } else {
    post_meta_data_path <- post_meta_data_path2
  }

try({

  post_metadata <- fread(post_meta_data_path) #%>%
   # lazy_dt()

  post_metadata_tidy <- post_metadata %>%
    mutate(created_bin = lubridate::round_date(as.POSIXct(created_utc), "week")) %>%
    filter(text_type == "comment") %>%
    select(subreddit, author, created_bin, n_words)

  # churn looking at all comments
  in_out_all <- post_metadata_tidy %>%
    distinct(subreddit, author, created_bin) %>%
    group_by(subreddit, author) %>%
    mutate(single_commenters = (max(created_bin) == min(created_bin)),
           first =  created_bin == min(created_bin),
           last = created_bin == max(created_bin)) %>%
    filter(!single_commenters) %>%
    filter(last | first) %>%
    select(-single_commenters)

  churn_all <- in_out_all %>%
    group_by(created_bin) %>%
    summarize(in_sum = sum(first),
              out_sum = sum(last)) %>%
    mutate(inout_sum = in_sum + out_sum,
           in_churn = in_sum /inout_sum,
           comment_length_type = "all") %>%
    as.data.table()

  # churn looking at long comments only (n_words > 100)
  in_out_long <- post_metadata_tidy %>%
    filter(n_words > 100) %>%
    distinct(subreddit, author, created_bin) %>%
    group_by(subreddit, author) %>%
    mutate(single_commenters = (max(created_bin) == min(created_bin)),
           first =  created_bin == min(created_bin),
           last = created_bin == max(created_bin)) %>%
    filter(!single_commenters) %>%
    filter(last | first) %>%
    select(-single_commenters)

  churn_long<- in_out_long %>%
    group_by(created_bin) %>%
    summarize(in_sum = sum(first),
              out_sum = sum(last)) %>%
    mutate(inout_sum = in_sum + out_sum,
           in_churn = in_sum /inout_sum,
           comment_length_type = "long_only")  %>%
   as.data.table()

  churn_df <- bind_rows(churn_all, churn_long) %>%
    mutate(subreddit = subreddit) %>%
    select(subreddit, created_bin, in_churn, inout_sum, comment_length_type)

  write_csv(churn_df, outpath, append = T)


  }, silent = T)

}


target_subreddits <- glue("{LOCAL_PATH}topic_models/") %>%
  list.files()%>%
  str_replace_all("_dt.csv","") %>%
  str_replace_all("_tw.csv","") %>%
  unique()


# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, all_subreddits, this_local_path, outpath){
  current_subreddit <- all_subreddits[id]
  get_churn_overtime(current_subreddit, this_local_path, outpath)
}

parLapply(cluster,
        1:length(target_subreddits),
        parallel_wrapper,
        target_subreddits,
        LOCAL_PATH,
        OUT_PATH)


get_churn_overtime("gifs", LOCAL_PATH, OUT_PATH)



