# get log author uniqueness
library(tidyverse)
library(data.table)
library(here)
library(tokenizers)
library(dtplyr)
library(tidyboot)
library(parallel)
library(glue)
library(tidytext)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 4
MIN_WORDS_PER_POST <- 100 # excluding stop words

local_path <- LOCAL_PATH
subreddit = "FortNiteBR"
subreddit = "SweatyPalms"

get_by_author_lau_data <- function(subreddit, local_path){

  # tidy comments
  subreddit_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  subreddit_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(subreddit_path1)){
    subreddit_path <- subreddit_path1
  } else {
    subreddit_path <- subreddit_path2
  }

  unnested_comments <- fread(subreddit_path) %>%
    filter(text_type == "comment",
           n_words >= 100) %>%
    unnest_tokens(word, body)  %>%
    lazy_dt() %>%
    select(subreddit, post_id, comment_id, author, created_utc, word) %>%
    mutate(created_bin = lubridate::round_date(as.POSIXct(created_utc), "week"))

  # get one comment per author per week (longest one)
  unnested_comment_one_per_week <- unnested_comments %>%
    count(subreddit, author, created_bin, comment_id, name = "n_total_words") %>% # get words per comment
    group_by(subreddit, author, created_bin) %>%
    filter(n_total_words == max(n_total_words)) %>%
    slice(1) %>% # takes first if tehre's two that are the same max length
    ungroup() %>%
    select(-n_total_words)

  unnested_comments_target <- unnested_comments %>%
    right_join(unnested_comment_one_per_week %>% select(comment_id ))

  rm(unnested_comments); rm(unnested_comment_one_per_week)
  # get log p of word in subreddit
  log_p_word <- unnested_comments_target %>%
    count(word) %>%
    mutate(log_p_w = log(n/sum(n))) %>%
    select(-n)

  # get lau by author and time bin
  lau_by_author_bin <- unnested_comments_target %>%
    left_join(log_p_word) %>%
    group_by(author, created_bin) %>%
    summarize(sum_log_p_w = sum(log_p_w),
              n_post_words = n()) %>%
    mutate(lau = sum_log_p_w/n_post_words) %>%
    arrange(author, created_bin)%>%
    group_by(author) %>%
    mutate(nth_comment = 1:n(),
           subreddit = subreddit) %>%
    as.data.table()

  lau_outpath <- glue("{local_path}lau_by_author_comment/{subreddit}_lau_author_comment.csv")
  write_csv(lau_by_author_bin, lau_outpath)

}



complete_subreddits <- glue("{local_path}lau_by_author_comment/")%>%
  list.files() %>%
  str_replace_all("_lau_author_comment.csv","") %>%
  unique()


target_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/raw/comments/")),
                                                full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),

         file_name = str_replace(file_name, "_comments.csv", ""),
         file_size = file.info(as.character(path))$size) %>%
  select(-path) %>%
  filter(!(file_name %in% complete_subreddits),
         !(file_name %in% c("loseit", "memes", "worldnews", "u_krystaltopaz",
                            "TryNotToLaughOrGrin", "titsmcgee", "BarebackGayPorn", "u_magicallysarah21", "teik_ihevoorik_ihe"))) %>%
  arrange(file_size) %>%
  slice(1:25) %>%
  pull(file_name)


# wrapper function
cluster <- makeCluster(NCLUSTERS, type = "FORK")
parallel_wrapper <- function(id, all_subreddits, this_local_path){
  current_subreddit <- all_subreddits[id]
  get_by_author_lau_data(current_subreddit, this_local_path)
}

parLapply(cluster,
        1:length(target_subreddits),
        parallel_wrapper,
        target_subreddits,
        LOCAL_PATH)

