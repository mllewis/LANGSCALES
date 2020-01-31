# Use Piantasodi method to get frequency counts for two "corpora"
library(tidyverse)
library(here)



LOCAL_PATH <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/word_counts/"
OUTPATH <- "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/misc/all_word_counts.csv"

all_files <- list.files(LOCAL_PATH, full.names = T)

type_counts <- map_df(all_files, read_csv)

corpus_counts <- type_counts %>%
  rename(total_counts = n) %>%
  mutate(corpus_1_counts = map_dbl(total_counts, ~rbinom(1, .x, .5)),
         corpus_2_counts = total_counts - corpus_1_counts,
         corpus_1_counts = corpus_1_counts + 1, # to get rid of zeros
         corpus_2_counts = corpus_2_counts + 1)

write_csv(corpus_counts, OUTPATH)


