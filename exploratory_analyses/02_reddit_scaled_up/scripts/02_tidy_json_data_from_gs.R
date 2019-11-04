# download files from google cloud and save as csv
library(here)
Sys.setenv("GCS_DEFAULT_BUCKET" = "langscale",
           "GCS_AUTH_FILE" = here("exploratory_analyses/02_reddit_scaled_up/scripts/langscale-e75cff32760e.json")) # must be before library is loaded
library(jsonlite)
library(tidyverse)
library(googleCloudStorageR)
library(glue)

TARGET_SUBREDDITS <- c('politics')

LOCAL_PATH <- here("exploratory_analyses/02_reddit_scaled_up/data/")

# objects in gcs bucket
objects <- gcs_list_objects() %>%
  pull(name)

download_one_json_file <- function(file, path){
   full_out_path <- glue("{path}raw/temp/{file}")
   gcs_get_object(file, saveToDisk = full_out_path)
}

read_write_delete_in_one_json_file <- function(file, path, outpath){
  print(file)
  full_in_path <- glue("{path}raw/temp/{file}")
  infile <- stream_in(file(full_in_path))
  write_csv(infile, outpath, append = TRUE)
  file.remove(full_in_path)
}



save_data_for_one_subreddit <- function(subreddit, all_objects, local_path) {
  print(subreddit)
  comment_out_path = glue("{local_path}raw/comments/{subreddit}_comments.csv")
  post_out_path = glue("{local_path}raw/posts/{subreddit}_posts.csv")

  comment_files <- all_objects[str_detect(all_objects, subreddit) & str_detect(all_objects, "comments_")]
  post_files <- all_objects[str_detect(all_objects, subreddit) & str_detect(all_objects, "posts_")]

  # download json files locally
  walk(comment_files, download_one_json_file, local_path)
  walk(post_files, download_one_json_file, local_path)

  # save jsons as csv (merge multiple jsons form same subreddit); delete jsons
  walk(comment_files, read_write_delete_in_one_json_file, local_path, comment_out_path)
  walk(post_files, read_write_delete_in_one_json_file, local_path, post_out_path)

}

walk(TARGET_SUBREDDITS[1], save_data_for_one_subreddit, objects, LOCAL_PATH)

