
## These two functions are extracted from 
## Gao, C. X., Wang, S., Zhu, Y., Ziou, M., Teo, S. M., Smith, C. L., … Dwyer, D. (2024, February 25).
##   Ensemble clustering: A practical tutorial. https://doi.org/10.31234/osf.io/fq6e9
# 
# The name of the functions were slightly changed and aricode::ARI was implemented
# 
# 
# Function calculate RI for the ith and jth column of the data. 
# If users interested in calculating ARI, aricode::ARI function can be used instead. 

cal_ARI_ij<-function(i,j,data){
  return<-1
  if(i!=j){
    data<-data[,c(i,j)] %>% na.omit()
    return<-aricode::ARI(data[,1],data[,2])
  }
  return
}

# Function calculating RI for all pairs of columns in the data
pairwise_ARI<-function(cluster_results){
  # Calculate pairwise Rand index
  cal_ARI <- Vectorize(cal_ARI_ij, vectorize.args=list("i","j"))
  prw_ARI <- outer(1:length(cluster_results),1:length(cluster_results),
              cal_ARI,data=cluster_results)
  diag(prw_ARI) <- NA
  return(prw_ARI)
}



#### Function for showing final clustering results table for diagnostics

cluster_table_diag = function(BC, max_k){
  
  df_list <- list()
  
  for (i in 1:dim(BC)[4]) {
    
    majority <- majority_voting(as.data.frame(BC[, , 1, i]))
    
    majority_table <- table(majority)
    
    padded_majority_table <- matrix(c(majority_table, rep(NA, max_k - nrow(majority_table))), ncol = 1)
    
    
    df_list[[i]] <- padded_majority_table
    
    
  }
  
  
  clusters_df = as.data.frame(df_list) 
  
  colnames_construction = function(data){
    
    col_names = c()
    
    for (i in 1:ncol(data)) {
      
      
      col_names[i] = paste("k=",sum(!is.na(data[,i])), sep = "")
    }
    
    return(col_names)
    
  }
  
  colnames(clusters_df) <- colnames_construction(clusters_df)

  clusters_df = clusters_df%>%
    select(unique(colnames(clusters_df))) %>% 
    mutate(Clusters = c(1:max_k)) %>%
    select(Clusters, everything())
  # 
  return(clusters_df)

  
}


internal_ensemble_validity = function(BC, max_k){
  
  
  graph_heatmap(BC)

  graph_delta_area(BC) %>% invisible()
  # 
  cluster_table_diag(BC, max_k) %>% 
    custom_flextable("Cluster solutions for BC") %>% 
    print()
  
  # for (i in 1:(max_k-1)) {
  #   
  #   agreement<-pairwise_ARI(as.data.frame(BC[, , 1, i]))
  #   hist(as.vector(agreement),
  #        main = paste("Histogram of ARI Agreement for k = ", i+1))
  #   
  # }
  

  
  
}


## Ordering tasks in table

task_order <- function(x) {
  ordered_tasks <- c("ANT", "STROOP", "CORSI", "TOL", "KBIT")
  x[order(match(x, ordered_tasks))]
}



feature_rename = function(data){

    data %>% 
      mutate(Feature = case_when(
        Feature == "STROOP_correct_trials_perc_congruent" ~ "STROOP Desempeño ensayos congruentes",    
        Feature == "STROOP_correct_trials_perc_incongruent"  ~ "STROOP Desempeño ensayos incongruentes",
        Feature == "STROOP_median_rt_correct_congruent" ~ "STROOP TR ensayos congruentes",      
        Feature == "STROOP_median_rt_correct_incongruent" ~ "STROOP TR ensayos incongruentes",
        Feature == "CORSI_correct_trials_perc" ~ "CORSI Desempeño",
        Feature == "CORSI_rt_mean_correct" ~ "CORSI Tiempo de ejecución",              
        Feature == "TOL_correct_trials_perc" ~ "TOL Desempeño",           
        Feature == "TOL_rt_planning_mean_correct" ~ "TOL Tiempo de planificación"
        )
      )
   
}


feature_order = c("ANT_Accuracy congruent trials",
                  "ANT_Accuracy incongruent trials",
                  "ANT_Omitted congruent trials",
                  "ANT_Omitted incongruent trials",
                  "ANT_RT congruent trials",      
                  "ANT_RT incongruent trials",
                  "ANT_Alerting network",
                  "ANT_Orienting network",
                  "ANT_Executive network",
                  "CORSI_Accuracy",
                  "CORSI_RT correct trials",              
                  "CORSI_RT incorrect trials",
                  "TOL_Accuracy",           
                  "TOL_Planning time correct trials",      
                  "TOL_Planning time incorrect trials", 
                  "KBIT_Accuracy")

BC_imputation <- function(BC){
  
  Eknn <- apply(BC, 2:4, impute_knn, data = datos, seed = 999)
  
  
  eval.obj <- consensus_evaluate(data = datos, Eknn)
  
  trim.obj <- eval.obj$trim.obj
  
  Eknn <- trim.obj$E.new
  
  Ecomp <- purrr::map2(Eknn, 3, impute_missing, data = datos)
  
  return(Ecomp)
  
}

