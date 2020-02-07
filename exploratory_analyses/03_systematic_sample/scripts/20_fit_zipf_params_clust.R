# User powerlaw package to estimate parameter
library(tidyverse)
library(poweRlaw)
library(parallel)

INFILE <-  "/data/molly/all_word_counts.csv"
NCLUSTERS <- 16
OUTFILE <- "/data/molly/estimated_zipf_params.csv"
#INFILE <-  "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/misc/all_word_counts.csv"
#NCLUSTERS <- 4
#OUTFILE <- here("exploratory_analyses/03_systematic_sample/data/estimated_zipf_params.csv")


all_counts <- read_csv(INFILE) %>%
  select(subreddit, total_counts)

nested_word_counts <- all_counts %>%
  group_by(subreddit) %>%
  arrange(-total_counts) %>%
  nest() %>%
  ungroup() %>%
  slice(70)

get_power_law_params <- function(current_subreddit, counts, this_outfile){
  reddit_power_law <- displ$new(counts)
  xmin_est_reddit <- estimate_xmin(reddit_power_law)
  reddit_power_law$setXmin(xmin_est_reddit)
  bootstrapped_p_value_reddit <- bootstrap_p(reddit_power_law,
                                             threads = 2,
                                             xmax = 1000000)
  params <- data.frame(subreddit = current_subreddit,
                       param = reddit_power_law$pars,
                       xmin =  xmin_est_reddit$xmin,
                       power_law_p = bootstrapped_p_value_reddit$p)
  write_csv(params, this_outfile, append = T)

}


# wrapper function
cluster <- makeCluster(NCLUSTERS, type = "FORK")
parallel_wrapper <- function(id, all_subreddits, df, outfile){
  current_subreddit_df <- df %>%
    filter(subreddit == all_subreddits[id])
  current_counts <- current_subreddit_df$data[[1]]$total_counts
  get_power_law_params(current_subreddit, current_counts, outfile)
}

parLapply(cluster,
          1,
          #1:nrow(nested_word_counts),
          parallel_wrapper,
          nested_word_counts$subreddit,
          nested_word_counts,
          OUTFILE)

