# get JSD distance for comments for nth post at user level and compare to community distance
library(here)
library(tidyverse)
library(philentropy)
library(glue)


LOCAL_PATH <-  here("/exploratory_analyses/02_reddit_scaled_up/data/")

# get distance to previous post
JSD_bind <- function(p, q) {
  JSD(rbind(p$gamma, q$gamma))
}

# get distance to all others at same timepoint
get_within_bin_JSD <- function(df){
  df_wide <- spread(df, topic, gamma)

  JSD_distance_pairwise <-  JSD(as.matrix(df_wide[,-1]))

  if(nrow(df_wide) == 2 ) {
    overall_mean <-  df_wide[1]
  } else {
    diag(JSD_distance_pairwise) <- NA
    overall_mean <-  colMeans(JSD_distance_pairwise, na.rm = T)
  }

  data.frame(author = df_wide$author,
             current_community_JSD = overall_mean, row.names = NULL)

}

safe_JSD <- possibly(JSD_bind, NA)


get_jsd_nth_post <- function(subreddit, local_path){
  print(subreddit)

  # get topic model
  doc_topic_model_path <- glue("{local_path}topic_models/{subreddit}_dt.csv")
  post_topic_model <- read_csv(doc_topic_model_path)

  # get metadata
  post_meta_data_path <-glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  post_metadata <- read_csv(post_meta_data_path, guess_max = 1000000)  %>%
    select(subreddit, post_id, comment_id, author, created_utc) %>%
    mutate(created_bin = lubridate::round_date(created_utc, "week")) %>%
    unite(document, c(subreddit, post_id, comment_id), sep = "-")  %>%
    filter(document %in% post_topic_model$document)

  # get 1 distribution per week per person
  post_metadata_one_per_week <- post_metadata %>%
    left_join(post_topic_model) %>%
    unite(author_time, c(author, created_bin), sep = "///", remove = F) %>%
    select(author, created_bin, author_time, topic, gamma) %>%
    group_by(author, created_bin, author_time, topic)  %>%
    summarize(gamma = mean(gamma))

  post_metadata_grouped <-  post_metadata_one_per_week %>%
    group_by(author, created_bin) %>%
    nest() %>%
    group_by(author) %>%
    mutate(nth_post = 1:n()) %>%
    ungroup()

  previous_JSD <-  post_metadata_grouped %>%
    group_by(author) %>%
    mutate(previous_data = lag(data),
           previous_author_JSD = map2_dbl(data, previous_data, ~safe_JSD(.x, .y))) %>%
    select(author,  created_bin, nth_post, previous_author_JSD)

  community_JSD <- post_metadata_one_per_week %>%
    ungroup() %>%
    select(-author_time) %>%
    group_by(created_bin) %>%
    nest() %>%
    mutate(temp = map(data, get_within_bin_JSD)) %>%
    select(-data) %>%
    unnest()

  previous_and_community_JSD <- full_join(previous_JSD, community_JSD)

  out_pairwise_topic_JSD <- glue("{local_path}jsd_nth_post/{subreddit}_jsd_nth_post.csv")
  write_csv(previous_and_community_JSD, out_pairwise_topic_JSD)
}

target_subreddits <- glue("{LOCAL_PATH}raw/comments/") %>%
  list.files()%>%
  str_replace_all("_comments.csv","")


walk(target_subreddits[c(2,6,7)], get_jsd_nth_post,
     LOCAL_PATH)
