# do google query of table already subset and save to google clouse
library(bigrquery)
library(glue)
library(tidyverse)

TARGET_SUBREDDITS <- c( 'DarkEnlightenment', 'nyc')
PROJECT <- "langscale"

#sql <- "SELECT body, author, created_utc, link_id, parent_id, score, id, subreddit
#FROM `fh-bigquery.reddit_comments.20*`
#WHERE _TABLE_SUFFIX BETWEEN '17_01' AND '19_05' AND
#subreddit IN ('slowcooking', 'askscience', 'politics', 'Piracy', 'Watchexchange', 'OkCupid', 'DarkEnlightenment', '3dshacks', 'fatFIRE', 'smallbusiness', 'kansascity',
#                    'philadelphia', 'chicago', 'Cleveland', 'akron') AND created_utc > 1504224000;" # september 1 2017


# I made langscale.redpilltemp.subreddit_comments and langscale.redpilltemp.subreddit_posts tables
# manually on the online interface with the above query (and then saving query as table)



save_to_gc_one_subreddit <- function(subreddit_name,  this_project){
     # sql_command_comments <- glue("SELECT * FROM `langscale.redpilltemp.subreddit_comments2` WHERE subreddit = '{subreddit_name}'")
     # one_subreddit_table_comments <- bq_project_query(this_project, sql_command_comments)
     # full_gs_path_comments <- glue("gs://{this_project}/comments_{subreddit_name}-*.json")
     # bq_table_save(one_subreddit_table_comments, full_gs_path_comments)

      sql_command_posts <- glue("SELECT * FROM `langscale.redpilltemp.subreddit_posts2` WHERE subreddit = '{subreddit_name}'")
      one_subreddit_table_posts <- bq_project_query(this_project, sql_command_posts)
      full_gs_path_posts <- glue("gs://{this_project}/posts_{subreddit_name}-*.json")
      bq_table_save(one_subreddit_table_posts, full_gs_path_posts)
}

walk(TARGET_SUBREDDITS[1], save_to_gc_one_subreddit, PROJECT)


