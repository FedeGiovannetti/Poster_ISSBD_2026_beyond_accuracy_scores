#### Helper Functions ####


summary_lm_table <- function(outcome, models, data, diagnostic_plots = FALSE) {
  
  options(scipen = 999)
  
  tabla_modelos <- data.frame()
  
  for (i in 1:length(models)) {
    
    mi_lm <- lm(models[i], data = data) 
    
    mi_lm_summary = mi_lm%>% 
      summary() 
    
    tabla_lm = mi_lm_summary%>% 
      broom::tidy()#%>% 
      # filter(term != "(Intercept)") 
    
    tabla_lm$rsquared = mi_lm_summary$r.squared
    
    tabla_modelos <- bind_rows(tabla_modelos, tabla_lm) %>% 
      
      mutate(across(where(is.numeric), ~round(.x, digits = 4))) 
    
    if (diagnostic_plots == TRUE) {
      
      plot(mi_lm, 1:2, main = models[i])
      
      (print((mi_lm$residuals)))
      
    }
    
  }
  
tabla_modelos$outcome <- c(outcome, rep("", nrow(tabla_modelos)-1))
tabla_modelos$rsquared[2:nrow(tabla_modelos)] <- ""
  
  tabla_modelos %>% 
    rename(predictor = "term") %>% 
    dplyr::select(outcome, predictor, everything()) %>% 
    return()
  
}




# Esta función me ordena las secuencias alfabéticamente


sort_sequence = function(sequence){
  
  paste0(str_sort(str_split(sequence, "")[[1]]), collapse = "")
  
}

any_in_sequence = function(response_sequence, sequence){
  
  split_response = as.character(str_split(response_sequence, "", simplify = T))
  
  split_sequence = as.character(str_split(sequence, "", simplify = T))
  
  any(split_response %in% split_sequence)
  
}

