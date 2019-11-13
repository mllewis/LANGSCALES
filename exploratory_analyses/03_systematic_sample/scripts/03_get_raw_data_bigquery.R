# do google query of table already subset and save to google clouse
library(bigrquery)
library(glue)
library(tidyverse)

TARGET_SUBREDDITS <- c('gifs','mildlyinteresting','worldnews','movies','memes','dankmemes','WTF','trashy','FortNiteBR','BlackPeopleTwitter','Whatcouldgowrong','Tinder','insanepeoplefacebook','AskMen','GetMotivated','Art','therewasanattempt','JusticeServed','HistoryMemes','Eyebleach','Android','nostalgia','apple','UnethicalLifeProTips','fantasyfootball','loseit','destiny2','SweatyPalms','boottoobig','Justrolledintotheshop','Memes_Of_The_Dank','environment','wifesharing','FloridaMan','OverwatchUniversity','morbidquestions','CrackWatch','spotify','PraiseTheCameraMan','neoliberal','EpicSeven','whatcarshouldIbuy','transtimelines','hugeboobs','USPS','RedDeadOnline','saplings','Boobies','beauty','FinancialCareers','JustCause','spirituality','suspiciouslyspecific','StuffOnCats','earlsweatshirt','Tierzoo','mkbhd','transadorable','metaldetecting','MakeupAddictionCanada','Pee','streetphotography','TechnoProduction','areolas','Slut','newsokur','greatdanes','AdrianaChechik','Kalilinux','Celebsnudess','Chennai','kobo','DisneyTravel','numetal','Electricity','IdleEmpire','vce','BattlePaintings','panamacity','Morphs','GolfClashClans','BarebackGayPorn','WedgieGirls','regularshowmemes','Preset_Market','Spicy_Memes','fentanyl','ApexSquads','mculeaks','Violetdarkstorm','SexclusiveSelling','Mondo','beneater','teik_ihevoorik_ihe','lostgirl','titsmcgee','u_magicallysarah21','u_krystaltopaz','TryNotToLaughOrGrin','grimdefender')
PROJECT <- "langscale"


# I made langscale.sampled_reddits.sampledcomments and langscale.redpilltemp.subredditposts tables
# *manually* on the online interface with the below two queries (and then saving query as table)

### COMMENTS:
#SELECT body, author, created_utc, link_id, parent_id, score, id, subreddit
#FROM TABLE_QUERY([fh-bigquery:reddit_comments], "REGEXP_MATCH(table_id, '^2019_..$')"),
#TABLE_QUERY([fh-bigquery:reddit_comments], "REGEXP_MATCH(table_id, '^2018_..$')"),
#TABLE_QUERY([fh-bigquery:reddit_comments], "REGEXP_MATCH(table_id, '^2017_..$')"),
#WHERE subreddit IN ('gifs','mildlyinteresting','worldnews','movies','memes','dankmemes','WTF','trashy','FortNiteBR','BlackPeopleTwitter','Whatcouldgowrong','Tinder','insanepeoplefacebook','AskMen','GetMotivated','Art','therewasanattempt','JusticeServed','HistoryMemes','Eyebleach','Android','nostalgia','apple','UnethicalLifeProTips','fantasyfootball','loseit','destiny2','SweatyPalms','boottoobig','Justrolledintotheshop','Memes_Of_The_Dank','environment','wifesharing','FloridaMan','OverwatchUniversity','morbidquestions','CrackWatch','spotify','PraiseTheCameraMan','neoliberal','EpicSeven','whatcarshouldIbuy','transtimelines','hugeboobs','USPS','RedDeadOnline','saplings','Boobies','beauty','FinancialCareers','JustCause','spirituality','suspiciouslyspecific','StuffOnCats','earlsweatshirt','Tierzoo','mkbhd','transadorable','metaldetecting','MakeupAddictionCanada','Pee','streetphotography','TechnoProduction','areolas','Slut','newsokur','greatdanes','AdrianaChechik','Kalilinux','Celebsnudess','Chennai','kobo','DisneyTravel','numetal','Electricity','IdleEmpire','vce','BattlePaintings','panamacity','Morphs','GolfClashClans','BarebackGayPorn','WedgieGirls','regularshowmemes','Preset_Market','Spicy_Memes','fentanyl','ApexSquads','mculeaks','Violetdarkstorm','SexclusiveSelling','Mondo','beneater','teik_ihevoorik_ihe','lostgirl','titsmcgee','u_magicallysarah21','u_krystaltopaz','TryNotToLaughOrGrin','grimdefender') AND created_utc > 1504224000; # september 1 2017

### POSTS:
#SELECT created_utc, subreddit, author, domain, url, num_comments, score, title, selftext, id, from_id, name
#FROM `fh-bigquery.reddit_posts.20*`
#WHERE _TABLE_SUFFIX BETWEEN '17_01' AND '19_05' AND
#subreddit IN ('gifs','mildlyinteresting','worldnews','movies','memes','dankmemes','WTF','trashy','FortNiteBR','BlackPeopleTwitter','Whatcouldgowrong','Tinder','insanepeoplefacebook','AskMen','GetMotivated','Art','therewasanattempt','JusticeServed','HistoryMemes','Eyebleach','Android','nostalgia','apple','UnethicalLifeProTips','fantasyfootball','loseit','destiny2','SweatyPalms','boottoobig','Justrolledintotheshop','Memes_Of_The_Dank','environment','wifesharing','FloridaMan','OverwatchUniversity','morbidquestions','CrackWatch','spotify','PraiseTheCameraMan','neoliberal','EpicSeven','whatcarshouldIbuy','transtimelines','hugeboobs','USPS','RedDeadOnline','saplings','Boobies','beauty','FinancialCareers','JustCause','spirituality','suspiciouslyspecific','StuffOnCats','earlsweatshirt','Tierzoo','mkbhd','transadorable','metaldetecting','MakeupAddictionCanada','Pee','streetphotography','TechnoProduction','areolas','Slut','newsokur','greatdanes','AdrianaChechik','Kalilinux','Celebsnudess','Chennai','kobo','DisneyTravel','numetal','Electricity','IdleEmpire','vce','BattlePaintings','panamacity','Morphs','GolfClashClans','BarebackGayPorn','WedgieGirls','regularshowmemes','Preset_Market','Spicy_Memes','fentanyl','ApexSquads','mculeaks','Violetdarkstorm','SexclusiveSelling','Mondo','beneater','teik_ihevoorik_ihe','lostgirl','titsmcgee','u_magicallysarah21','u_krystaltopaz','TryNotToLaughOrGrin','grimdefender') AND created_utc > 1504224000;


save_to_gc_one_subreddit <- function(subreddit_name,  this_project){
      sql_command_comments <- glue("SELECT * FROM `langscale.sampled_reddits.sampledcomments` WHERE subreddit = '{subreddit_name}'")
      one_subreddit_table_comments <- bq_project_query(this_project, sql_command_comments)
      full_gs_path_comments <- glue("gs://{this_project}/comments_{subreddit_name}-*.json")
      bq_table_save(one_subreddit_table_comments, full_gs_path_comments)

      sql_command_posts <- glue("SELECT * FROM `langscale.sampled_reddits.sampledposts` WHERE subreddit = '{subreddit_name}'")
      one_subreddit_table_posts <- bq_project_query(this_project, sql_command_posts)
      full_gs_path_posts <- glue("gs://{this_project}/posts_{subreddit_name}-*.json")
      bq_table_save(one_subreddit_table_posts, full_gs_path_posts)
}

walk(TARGET_SUBREDDITS[51:100], save_to_gc_one_subreddit, PROJECT)


