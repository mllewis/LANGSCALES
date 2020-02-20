
library(here)
library(poweRlaw)
library(parallel)

AUTHOR_COUNTS <-  here("exploratory_analyses/04_systematic_sample_tidy/data/subreddit_meta.csv")
author_count_measures <- read_csv(AUTHOR_COUNTS, col_names = c("subreddit","author_n","word_H","word_mean_n","word_sd","word_total","score_mean",
                                                               "score_sd","score_H","comments_n_all", "posts_n_all","comments_posts_ratio"))

