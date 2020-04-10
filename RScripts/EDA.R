#### obtain number of respondents in total, per region and per province
N_respondents <- numeric(length = length(survey_list))
region_plots <- surv_sums  <- vector("list", length = length(survey_list))

for(i in 1:length(survey_list)) {
  
  if(i == 1) {
    
    surv_sums[[i]] <- survey_summaries(survey_list[[i]], "province", gender = "Q3")
    #extract the number of respondents
    N_respondents[i] <- surv_sums[[i]]$N_respondents
    #make the barchart
    region_plots[[i]] <- bar_chart(surv_sums[[i]]$pdata_region, x = "Region", y = "Number")
    
  } else {
    
    surv_sums[[i]] <- survey_summaries(survey_list[[i]], "province", gender = "Q4")
    #extract the number of respondents
    N_respondents[i] <- surv_sums[[i]]$N_respondents
    #make the barchart
    region_plots[[i]] <- bar_chart(surv_sums[[i]]$pdata_region, x = "Region", y = "Number")
  }
  
}