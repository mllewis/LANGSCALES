# get word TYPE counts by subreddit
library(here)
library(tidyverse)
library(glue)
library(data.table)
library(dtplyr)
library(tm)
library(tidytext)


LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/word_counts2/"
OUTPATH <- here("exploratory_analyses/04_systematic_sample_tidy/data/word_type_counts.csv")

all_files <- list.files(LOCAL_PATH, full.names = T)

type_counts <- map_dbl(all_files, ~read_csv(.x) %>% nrow())

type_count_df <- data.frame(subreddit= all_files,
           n_word_types = type_counts) %>%
  mutate(subreddit = str_replace(subreddit, "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/word_counts2//", ""))

write_csv(type_count_df, OUTPATH)
