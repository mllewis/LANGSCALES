# get df of all subreddit-level vars
library(here)
library(tidyverse)


AUTHOR_COUNTS <-  here("exploratory_analyses/03_systematic_sample/data/subreddit_counts_scores.csv")
AUTHOR_TIME <- here("exploratory_analyses/03_systematic_sample/data/subreddit_author_time_data.csv")
LAG_PATH <- here("exploratory_analyses/03_systematic_sample/data/thread_lag_overtime.csv")
CHURN_PATH <- here("exploratory_analyses/03_systematic_sample/data/churn_overtime.csv")
AUTHOR_INEQ <- here("exploratory_analyses/03_systematic_sample/data/subreddit_author_inequality_all.csv")
AUTHOR_INEQ_LONG <- here("exploratory_analyses/03_systematic_sample/data/subreddit_author_inequality_long.csv")
SCORE_PATH <- here("exploratory_analyses/03_systematic_sample/data/score_overtime.csv")
ENTROPY_PATH <- here("exploratory_analyses/03_systematic_sample/data/subreddit_post_entropy_overtime.csv")
PAIRWISE_TOPIC_JSD <- "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/jsd_nth_post/"

TIDY_OUT <- here("exploratory_analyses/03_systematic_sample/data/tidy_subreddit_vars.csv")



# author counts
author_count_measures <- read_csv(AUTHOR_COUNTS, col_names = c("subreddit","author_n","word_H","word_mean_n","word_sd","word_total","score_mean",
                                                               "score_sd","score_H","comments_n_long","comments_n_all",
                                                               "posts_n_all","comments_posts_ratio")) %>%
  filter(author_n > 100) %>%
  arrange(-author_n) %>%
  mutate(author_rank = 1:n())

# author time
author_time <- read_csv(AUTHOR_TIME, col_names = c("subreddit", "author_longevity_mean", "author_sd_mean",
                                                   "author_longevity_H", "author_lag_sd", "author_lag_H",
                                                   "author_lag_mean"))

# lag
lag <- read_csv(LAG_PATH, col_names = c("subreddit", "created_bin", "lag_sec", "n", "comment_length_type")) %>%
  filter(comment_length_type == "all")

lag_mean <- lag %>%
  group_by(subreddit) %>%
  summarize(mean_lag = mean(lag_sec))

# churn
churn <- read_csv(CHURN_PATH, col_names = c("subreddit", "created_bin", "in_churn",
                                            "inout_sum", "comment_length_type")) %>%
  filter(comment_length_type == "all")

in_churn <- churn %>%
  group_by(subreddit) %>%
  summarize(mean_churn = mean(in_churn))

# author ineqality
author_ineq <- read_csv(AUTHOR_INEQ, col_names = c("subreddit", "comment_author_H", "comment_gini_coeff_all",
                                                   "comment_normalized_author_H", "post_author_H", "post_gini_coeff",
                                                   "post_normalized_author_H")) %>%
  select(subreddit, comment_gini_coeff_all)

author_ineq_long <- read_csv(AUTHOR_INEQ_LONG, col_names = c("subreddit", "comment_author_H",
                                                             "comment_gini_coeff_long",
                                                             "comment_normalized_author_H")) %>%
  select(subreddit, comment_gini_coeff_long)

# scores
scores<- read_csv(SCORE_PATH, col_names = c( "created_bin", "mean_score",
                                             "comment_length_type", "subreddit")) %>%
  filter(comment_length_type == "all")

score_mean <- scores %>%
  group_by(subreddit) %>%
  summarize(mean_score = mean(mean_score))

# Comment entropy
comment_entropy <- read_csv(ENTROPY_PATH, col_names = c( "subreddit", "created_bin",
                                                        "mean_document_entropy", "n"))


entropy_mean <- comment_entropy %>%
  group_by(subreddit) %>%
  summarize(mean_document_entropy = mean(mean_document_entropy))

# ## Nth similarity

nth_post_data <- map_df(list.files(PAIRWISE_TOPIC_JSD, full.names = T),
                        ~{read_csv(.x) %>% mutate(subreddit = .x)}) %>%
  mutate(subreddit = str_replace(subreddit, paste0(PAIRWISE_TOPIC_JSD, "/"), ""),
         subreddit = str_replace(subreddit, "_previous_jsd_nth_post.csv", ""))

over_individual_time <- nth_post_data  %>%
  select(author, nth_post, previous_author_JSD, subreddit) %>%
  gather("measure", "value", -nth_post, -subreddit, -author) %>%
  select(-measure) %>%
  filter(!is.na(value))

over_individual_time_ms <- over_individual_time %>%
  group_by(subreddit, nth_post) %>%
  summarize(mean_JSD = mean(value, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  filter(nth_post <= 50)

mean_pairwise_JSD <- over_individual_time_ms %>%
  group_by(subreddit) %>%
  summarize(mean_JSD = mean(mean_JSD))


### BIND EVERYTHING TOGETHER ###
all_measures <- author_count_measures %>%
  left_join(author_time) %>%
  left_join(lag_mean) %>%
  left_join(in_churn) %>%
  left_join(author_ineq) %>%
  left_join(author_ineq_long) %>%
  left_join(score_mean) %>%
  left_join(entropy_mean) %>%
  left_join(mean_pairwise_JSD)

write_csv(all_measures, TIDY_OUT)


