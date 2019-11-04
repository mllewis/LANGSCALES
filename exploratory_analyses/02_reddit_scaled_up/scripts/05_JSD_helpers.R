library(philentropy)
library(data.table)

get_JSD_distance_for_one_bag <- function(group_data, group_name, topic_matrix){
  
  target_posts <- group_data %>%
    as.data.table() 
  
  setnames(target_posts, old = names(target_posts), new = "document")
  
  filtered_topic_data_matrix<- merge(topic_matrix, target_posts, all = F)  %>%
    select(-document) %>%
    as.matrix()
  
  if(nrow(filtered_topic_data_matrix) > 0){
    
    pairwise_JSD <- JSD(filtered_topic_data_matrix, unit = "log2")
    if(nrow(filtered_topic_data_matrix) > 2) {pairwise_JSD[upper.tri(pairwise_JSD, diag = T)] <- NA}
    
    data.frame(group = group_name,
               n = dim(filtered_topic_data_matrix)[1],
               mean_jsd = mean(pairwise_JSD, na.rm = T),
               sd_jsd = sd(pairwise_JSD, na.rm = T))
    
  } else {
    
    data.frame(group = group_name,
               n = dim(filtered_topic_data_matrix)[1],
               mean_jsd = NA,
               sd_jsd = NA)
  } 
}

get_JSD_of_post_bags <- function(meta, topic_model, target_bins){
  dt_wide <- topic_model %>%
    spread(topic, gamma) %>%
    data.table()
  
  setkey(dt_wide, document)
  
  grouped_posts <- split(meta$document,
                         meta[target_bins]) 
  
  map2_df(grouped_posts,
          names(grouped_posts), 
          get_JSD_distance_for_one_bag, 
          dt_wide)
}
