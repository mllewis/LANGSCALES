# get entropy of longest post for each author per week over time

library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(entropy)
library(glue)
library(here)
library(parallel)


OUT_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_post_entropy_overtime.csv")
LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3

get_document_entropy_overtime <- function(subreddit, local_path, outpath) {

  # get metadata
  post_meta_data_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_meta_data_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(post_meta_data_path1)){
    post_meta_data_path <- post_meta_data_path1
  } else {
    post_meta_data_path <- post_meta_data_path2
  }

try({
  doc_topic_model_path <- glue("{local_path}topic_models/{subreddit}_dt.csv")
  post_topic_model <- fread(doc_topic_model_path)

  post_metadata <- fread(post_meta_data_path)

  # get 1 distribution per week per person
  post_metadata_tidy <- post_metadata %>%
    select(subreddit, post_id, comment_id, author, created_utc, n_words) %>%
    mutate(created_bin = lubridate::round_date(as.POSIXct(created_utc), "week"),
           document = paste(subreddit, post_id, comment_id, sep = "-"))  %>%
    filter(document %in% post_topic_model$document) %>%
    group_by(subreddit, author, created_bin) %>%
    filter(n_words == max(n_words)) %>% # select longest post each week (randomly select one if there are two of equal length)
    slice(1)


  post_metadata_one_per_week <- post_metadata_tidy %>%
    left_join(post_topic_model) %>%
    select(subreddit, author, created_bin, topic, gamma)

  by_week_entropy <- post_metadata_one_per_week %>%
    group_by(subreddit, author, created_bin) %>%
    summarize(document_entropy = entropy(gamma)) %>%
    group_by(subreddit, created_bin) %>%
    summarize(mean_document_entropy = mean(document_entropy),
              n = n())

  write_csv(by_week_entropy, outpath, append = T)


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
  get_document_entropy_overtime(current_subreddit, this_local_path, outpath)
}

parLapply(cluster,
        1:length(target_subreddits),
        parallel_wrapper,
        target_subreddits,
        LOCAL_PATH,
        OUT_PATH)




