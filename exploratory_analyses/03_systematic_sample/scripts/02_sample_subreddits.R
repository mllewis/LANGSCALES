# within range (MIN_COMMUNITY_SIZE, MAX_COMMMUNITY_SIZE), cut in log space into 10 intervals
# sample 10 subreddits from each interval


library(here)
library(tidyverse)

MIN_COMMUNITY_SIZE <- 100
MAX_COMMMUNITY_SIZE <- 1500000
NINTERVALS <- 10
NSAMPLES <- 10
NAUTHOR_PATH <- here("exploratory_analyses/03_systematic_sample/data/commentsperauthor.csv")
TARGET_SUBREDDITS <- here("exploratory_analyses/03_systematic_sample/data/target_subreddits.csv")


nauthor_df <- read_csv(NAUTHOR_PATH)

nauthor_tidy <- nauthor_df %>%
   arrange(-nauthors) %>%
   filter(nauthors >= MIN_COMMUNITY_SIZE,
          nauthors <= MAX_COMMMUNITY_SIZE) %>%
   mutate(log_nauthors = log(nauthors),
          interval = cut(log_nauthors, NINTERVALS))

ggplot(nauthor_tidy, aes(x = nauthors)) +
   geom_histogram()

sampled_subreddits <- nauthor_tidy %>%
   group_by(interval) %>%
   sample_n(NSAMPLES) %>%
   arrange(-nauthors) %>%
   select(subreddit, nauthors, log_nauthors, interval)

write_csv(sampled_subreddits, TARGET_SUBREDDITS)

sampled_subreddits$subreddit2 <- paste0('\'', sampled_subreddits$subreddit, '\'')


paste(sampled_subreddits$subreddit2, collapse = ",")

sampled_subreddits[,5] %>%
   data.frame()




# ggplot(sampled_subreddits, aes(x = log(nauthors))) +
#    geom_histogram()
#
#
# author_sample_counts <- sampled_subreddits %>%
#    group_by(interval) %>%
#    summarize(mean_n_authors = mean(nauthors),
#              log_mean_n_authors = mean(log(nauthors)))
#
# author_sample_counts %>%
#    ggplot(aes(x = 1:10, y = log_mean_n_authors)) +
#    geom_point()


