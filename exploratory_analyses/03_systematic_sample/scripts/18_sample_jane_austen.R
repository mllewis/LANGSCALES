# get jane austen types by differen token lengths
library(tidyverse)
library(janeaustenr)
library(tidytext)
library(here)

OUTPATH <- here("exploratory_analyses/03_systematic_sample/data/jane_austen_tt_samples.csv")

jane_austen_tokens <- austen_books() %>%
  unnest_tokens("word", "text") %>%
  select(word)


n_token_values <- c(1:10 %o% 10^(2:5)) %>%
  as.list() %>%
  keep(. < nrow(jane_austen_tokens)) %>%
  unlist()

sample_ids <- 1:100

get_unique_types <- function(args, df){
  this_n_tokens <- args[[1]]
  this_sample_id <-  args[[2]]
  df %>%
    sample_n(this_n_tokens) %>%
    summarize(n_types = n_distinct(word)) %>%
    mutate(n_tokens = this_n_tokens,
           sample_id = this_sample_id)
}

types_vs_tokens <- n_token_values %>%
  cross2(sample_ids) %>%
  map_df(get_unique_types, jane_austen_tokens)

write_csv(types_vs_tokens, OUTPATH)



