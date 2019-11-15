# train topic models for comments using mallet package
library(here)
options( java.parameters = "-Xmx4g" )
library(XLConnect)
library(mallet)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tidytext)
library(glue)
library(parallel)
library(tm)

LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"
NCLUSTERS <- 3
MIN_WORDS_PER_POST <- 100 # excluding stop words
NTOPICS <- 50

get_topic_model_subreddit <- function(subreddit, local_path, ntopics, nwords_per_post){
   print(subreddit)

  # tidy comments
  subreddit_path1 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
  subreddit_path2 <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts_f.csv")
  if(file.exists(subreddit_path1)){
    subreddit_path <- subreddit_path1
  } else {
    subreddit_path <- subreddit_path2
  }

    tidy_subreddit_data <- fread(subreddit_path) %>%
      lazy_dt()

    reddit_text <- tidy_subreddit_data %>%
      filter(text_type == "comment",
             n_words >= nwords_per_post) %>%
      select(subreddit, post_id, comment_id, body) %>%
      mutate(body = removeWords(body, stopwords("english")),
             document = paste(subreddit, post_id, comment_id, sep = '-')) %>%
      select(document, body) %>%
      as.data.table()

    # create an empty file of "stopwords"
    file.create(empty_file <- tempfile())
    docs <- mallet.import(reddit_text$document,
                          reddit_text$body,
                          empty_file)

    # train model
    mallet_model <- MalletLDA(num.topics = ntopics)
    mallet_model$loadDocuments(docs)
    mallet_model$train(500)

    # save matrices
    document_topic_model <- tidy(mallet_model, matrix = "gamma")
    topic_word_model <- tidy(mallet_model, matrix = "beta")

    doc_topic_model_outpath <- glue("{local_path}topic_models/{subreddit}_dt.csv")
    write_csv(document_topic_model, doc_topic_model_outpath)

    topic_word_model_outpath <- glue("{local_path}topic_models/{subreddit}_tw.csv")
    write_csv(topic_word_model, topic_word_model_outpath)
}



complete_subreddits <- glue("{LOCAL_PATH}topic_models/")%>%
  list.files() %>%
  str_replace_all("_dt.csv","") %>%
  str_replace_all("_tw.csv","") %>%
  unique()


todo_subreddits <- data.frame(path = list.files(as.character(glue("{LOCAL_PATH}/raw/comments/")),
                                                 full.names = T)) %>%
  mutate(file_name = basename(as.character(path)),
         file_name = str_replace(file_name, "_comments.csv", ""),
         file_size = file.info(as.character(path))$size) %>%
  select(-path) %>%
  filter(!(file_name %in% complete_subreddits),
         !(file_name %in% c("loseit", "memes", "worldnews", "u_krystaltopaz",
                            "TryNotToLaughOrGrin", "titsmcgee", "BarebackGayPorn", "u_magicallysarah21"))) %>%
  arrange(-file_size) %>%
  pull(file_name)



# wrapper function
#cluster <- makeCluster(NCLUSTERS, type = "FORK")
parallel_wrapper <- function(id, all_subreddits, this_local_path, ntopics, minwords){
  current_subreddit <- all_subreddits[id]
  get_topic_model_subreddit(current_subreddit, this_local_path, ntopics, minwords)
}


#parLapply(cluster,
#        2:length(target_subreddits),
#        parallel_wrapper,
#        target_subreddits,
#        LOCAL_PATH,
#        NTOPICS,
#        MIN_WORDS_PER_POST)

walk(todo_subreddits[2],
     get_topic_model_subreddit,
     LOCAL_PATH,
     NTOPICS,
     MIN_WORDS_PER_POST)
