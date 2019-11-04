# train topic models for comments using mallet package
library(here)
library(mallet)
library(tidyverse)
library(tidytext)
library(glue)

LOCAL_PATH <-  here("/exploratory_analyses/02_reddit_scaled_up/data/")

MIN_WORDS_PER_POST <- 100 # excluding stop words
NTOPICS <- 50

get_topic_model_subreddit <- function(subreddit, local_path, ntopics, nwords_per_post){
    print(subreddit)
   # tidy comments
    subreddit_path <- glue("{local_path}tidy/{subreddit}_tidy_comments_posts.csv")
    tidy_subreddit_data <- read_csv(subreddit_path, guess_max = 1000000)  %>%
      select(-body)

    reddit_text <- tidy_subreddit_data %>%
      filter(author != "[deleted]",
             text_type == "comment",
             body_clean != "removed")

    # unnest tokens
    unnested_reddit <- reddit_text %>%
      select(subreddit, post_id, comment_id, body_clean) %>%
      unite(document, subreddit, post_id, comment_id, sep = "-") %>%
      distinct(document, .keep_all = T)  %>%
      unnest_tokens(word, body_clean)

    # remove stop words
    unnested_reddit_tidy <- unnested_reddit %>%
      anti_join(stop_words %>% filter(lexicon == "snowball"), by = "word") %>%
      mutate(word = str_replace(word, "'", ""))  %>%
      arrange(document)

    # collapse posts to single line; remove posts with fewer than MIN_WORDS_PER_POST words
    nested_reddit_tidy <- unnested_reddit_tidy %>%
      group_by(document) %>%
      summarize(text = paste(word, collapse = " ")) %>%
      right_join(unnested_reddit_tidy %>%
                   count(document) %>%
                   filter(n >= nwords_per_post) %>%
                   select(document))

    # create an empty file of "stopwords"
    file.create(empty_file <- tempfile())
    docs <- mallet.import(nested_reddit_tidy$document,
                          nested_reddit_tidy$text,
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

target_subreddits <- glue("{LOCAL_PATH}raw/comments/")%>%
  list.files() %>%
  str_replace_all("_comments.csv","")

walk(target_subreddits[17], get_topic_model_subreddit,
     LOCAL_PATH,
     NTOPICS,
     MIN_WORDS_PER_POST)
