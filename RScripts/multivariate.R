prob_matrices <- lapply(c("Q4", "Diploma", "Q7", "age_range"), function(ovar) {
  
  mat_return <- cluster_matrix(survey_list[[3]], "Q91", ovar)
  
  colnames(mat_return)[ncol(mat_return)] <- paste("Missing", ovar, sep = "_")
  return(mat_return)
}) %>% do.call(cbind, .)

#filter out the missing row and columns
prob_matrices2 <- prob_matrices[!str_detect(row.names(prob_matrices), "Missing"), 
                                !str_detect(colnames(prob_matrices), "Missing_") ]

#filter out the Andere column
prob_matrices3 <- prob_matrices2[, !str_detect(colnames(prob_matrices2), "Andere")]

heatmap(t(prob_matrices3), Colv = NA, 
        scale = "none", 
        col = brewer.pal(11,"RdYlGn"), 
        RowSideColors = c(rep("purple", 6), rep("orange", 7)),
        main = "Ben je bereid om een COVID-19 app te gebruiken?", 
        margins = c(9, 4)
)