library(tidyverse)


################## merging surveys
merge_surveys <- function(survey_list) {
  
  #create a temporary list of survey_list
  survey_list_temp <- survey_list
  #extract common names to all the surveys
  var_list <- Reduce(intersect, lapply(survey_list, names))
  
  #filter out the questions asking about filling of the previous survey
  last_survey <- survey_list[[length(survey_list)]]
  Q2s <- last_survey %>% select(starts_with("Q2_"))
  #avoid the column created for Neen
  dd <- sapply(Q2s, function(x) !str_detect(attr(x, 'label'), "Neen") )
  real_Q2s <- Q2s[, dd]
  
  #filter out the respondents that have been faithful, i.e filled out all the previous surveys
  faithful_respondents <- which(apply(real_Q2s, 1, sum, na.rm = TRUE) == ncol(real_Q2s))
  
  #filter out the IP adresses of the faithful respondents
  faithful_respondents <- last_survey[faithful_respondents, ]
  
  #compute the intersection of all IPS across the surveys after replacing the last
  #survey with only the faithful respondents
  survey_list_temp[[length(survey_list_temp)]] <- faithful_respondents
  intersect_IPs <- Reduce(intersect, lapply(survey_list_temp, 
                                            function(x) {
                                              
                                              x$IPAddress
                                              
                                            }))
  
  # filter out the common columns and the faithful respondents
  ret_data_list <- lapply(survey_list, function(x) {
    
    y <- x %>% 
      filter(IPAddress %in% intersect_IPs) %>%
      select(all_of(var_list))
    
    y2 <- y[order(y$IPAddress), ]
    
    #compute the number of times each IP appeared
    y3 <- y2 %>% group_by(IPAddress) %>% 
      summarise(times = n())
    
    respondents_id <- unlist(lapply(1:nrow(y3), function(i) {
      
      rep(i, y3$times[i])
      
    }))
    
    y2$respondents_id <- respondents_id
    
    y2
    
  }) 
  #compute the number of respondents per list
  respondents_per_list <- lapply(ret_data_list, nrow)
  #turn the ret_data_list into a survey
  ret_data <-  do.call(rbind.data.frame, ret_data_list) %>% 
    mutate(Survey_id = rep(1:length(survey_list), times = respondents_per_list))
  return(ret_data)
  
}

#merge surveys 2 - 4
test_merge <- merge_surveys(survey_list_orig[2:4])
