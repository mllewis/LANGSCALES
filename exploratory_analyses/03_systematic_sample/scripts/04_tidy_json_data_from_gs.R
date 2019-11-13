# download files from google cloud and save as csv
library(here)
Sys.setenv("GCS_DEFAULT_BUCKET" = "langscale",
           "GCS_AUTH_FILE" = here("exploratory_analyses/02_reddit_scaled_up/scripts/langscale-e75cff32760e.json")) # must be before library is loaded
library(jsonlite)
library(tidyverse)
library(googleCloudStorageR)
library(glue)

TARGET_SUBREDDITS <- c('gifs','mildlyinteresting','worldnews','movies','memes','dankmemes','WTF','trashy','FortNiteBR','BlackPeopleTwitter','Whatcouldgowrong','Tinder','insanepeoplefacebook','AskMen','GetMotivated','Art','therewasanattempt','JusticeServed','HistoryMemes','Eyebleach','Android','nostalgia','apple','UnethicalLifeProTips','fantasyfootball','loseit','destiny2','SweatyPalms','boottoobig','Justrolledintotheshop','Memes_Of_The_Dank','environment','wifesharing','FloridaMan','OverwatchUniversity','morbidquestions','CrackWatch','spotify','PraiseTheCameraMan','neoliberal','EpicSeven','whatcarshouldIbuy','transtimelines','hugeboobs','USPS','RedDeadOnline','saplings','Boobies','beauty','FinancialCareers','JustCause','spirituality','suspiciouslyspecific','StuffOnCats','earlsweatshirt','Tierzoo','mkbhd','transadorable','metaldetecting','MakeupAddictionCanada','Pee','streetphotography','TechnoProduction','areolas','Slut','newsokur','greatdanes','AdrianaChechik','Kalilinux','Celebsnudess','Chennai','kobo','DisneyTravel','numetal','Electricity','IdleEmpire','vce','BattlePaintings','panamacity','Morphs','GolfClashClans','BarebackGayPorn','WedgieGirls','regularshowmemes','Preset_Market','Spicy_Memes','fentanyl','ApexSquads','mculeaks','Violetdarkstorm','SexclusiveSelling','Mondo','beneater','teik_ihevoorik_ihe','lostgirl','titsmcgee','u_magicallysarah21','u_krystaltopaz','TryNotToLaughOrGrin','grimdefender')

LOCAL_PATH <- "/Volumes/wilbur_the_great/LANGSCALES_subreddit_sample/"

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

walk(TARGET_SUBREDDITS[2:100], save_data_for_one_subreddit, objects, LOCAL_PATH)

