# get JSD distance for comments
library(here)
library(philentropy)
library(tidyverse)

DOC_TOPIC_MODEL_PATH <- here("exploratory_analyses/01_reddit_pilot/data/topic_modeling/dt_redpill.csv")
PAIRWISE_TOPIC_JSD <- here("exploratory_analyses/01_reddit_pilot/data/post_JSD_redpill.csv")

jsd_distance_fast <- function(m) { #https://stackoverflow.com/questions/29050368/which-r-implementation-gives-the-fastest-jsd-matrix-computation
  ncol <- ncol(m)
  xlogx <- matrix(colSums(m * log(2 * m)), ncol, ncol)
  xlogx2 <- xlogx + t(xlogx)
  xlogx2[upper.tri(xlogx2, diag=TRUE)] <- 0
  
  xylogxy <- matrix(0, ncol, ncol)
  for (i in seq_len(ncol)[-1]) {
    j <- seq_len(i - 1L)
    xy <- m[, i] + m[, j, drop=FALSE]
    xylogxy[i, j] <- colSums(xy * log(xy))
  }
  
  sqrt(0.5 * (xlogx2 - xylogxy))
}

P <- 1:10/sum(1:10)
Q <- 20:29/sum(20:29)
x <- rbind(P,Q)

dt <- read_csv(DOC_TOPIC_MODEL_PATH) 

dt_wide <- dt %>%
  spread(topic, gamma)

documents <- dt_wide$document

dt_mat <- dt_wide %>%
 # slice(1:5) %>%
  select(-document) %>%
  as.matrix() %>%
  t()

pairwise_JSD <- jsd_distance_fast(dt_mat) 

pairwise_JSD[upper.tri(pairwise_JSD)] <- NA

pairwise_JSD_tidy <- pairwise_JSD %>%
  as.data.frame()  %>%
  mutate(document1 = documents) %>%
  select(document1, everything())

names(pairwise_JSD_tidy)  = c("document1", documents)
  
long_jsd1 <- gather(pairwise_JSD_tidy, 
                    "document2",
                    "jsd", 
                    -document1) %>%
  select(document1, document2, jsd) %>%
  filter(!is.na(jsd),
         document1 != document2) 

long_jsd2 <- long_jsd1 %>%
  rename(document2 = document1,
         document1 = document2) 

long_jsd_full <- bind_rows(long_jsd1, long_jsd2)

write_csv(long_jsd_full, PAIRWISE_TOPIC_JSD)
