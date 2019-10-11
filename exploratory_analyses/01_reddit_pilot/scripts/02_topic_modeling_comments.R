# train topic models for comments using mallet package
library(here)
library(mallet)
library(tidyverse)
library(tidytext)


SUBREDDIT_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/redpill2years_tidy.csv")
#SUBREDDIT_DATA_PATH <- here("exploratory_analyses/01_reddit_pilot/data/pittsburgh2years_tidy.csv")
DOC_TOPIC_MODEL_OUTPATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_redpill.csv")
TOPIC_WORD_MODEL_OUTPATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/tw_redpill.csv")
MIN_WORDS_PER_POST <- 100 # excluding stop words
NTOPICS <- 50

# tidy comments
tidy_pgh <- read_csv(SUBREDDIT_DATA_PATH, guess_max = 1000000)  %>%
  select(-body)

reddit_text <- tidy_pgh %>%
  filter(author != "[deleted]")  %>%
  select(1:8, 20) %>%
  filter(text_type == "comment",
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
               filter(n >= MIN_WORDS_PER_POST) %>% 
               select(document))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(nested_reddit_tidy$document, 
                      nested_reddit_tidy$text, 
                      empty_file)

# train model
mallet_model <- MalletLDA(num.topics = NTOPICS)
mallet_model$loadDocuments(docs)
mallet_model$train(500)

# save matrices
document_topic_model <- tidy(mallet_model, matrix = "gamma")
topic_word_model <- tidy(mallet_model, matrix = "beta")

write_csv(document_topic_model, DOC_TOPIC_MODEL_OUTPATH)
write_csv(topic_word_model, TOPIC_WORD_MODEL_OUTPATH)