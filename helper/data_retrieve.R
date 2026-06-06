library(tidyverse)
library(httr)

# En este script se presentan distintas funciones para cargar los datos
# tanto de forma local como desde github

#### Leer bases de datos individualmente desde local ####

read_data <- function(local_folder,task, df_type, time){
  
  ## Esta función permite cargar una base de datos a partir de la siguiente info:
  # task: tarea de interés ["STROOP", "ANT", "TOL", "CORSI", "KBIT"]
  # df_type: tipo de base ["original", "clean", "processed"]
  # time: momento de la toma de datos ["PRE", "POST"]
  
  # La base se carga en formato dataframe
  
  file_name <- paste(paste(task,
                           time,
                           "PICT2014",
                           df_type,
                           sep = "_"),
                     ".csv", sep = "")
  
  
  data <- read.csv(paste(local_folder,
                         "Data",
                         "Cognitive",
                         task,
                         df_type,
                         file_name,
                         sep = "/"))
  
  return(data)
  
}


#### Leer multiples bases de datos desde local ####


read_multiple_data <- function(local_folder, task, df_type, time){
  
  ## Esta función permite cargar varias bases de datos:
  # task: tarea de interés ["STROOP", "ANT", "TOL", "CORSI", "KBIT"]
  # df_type: tipo de base ["original", "clean", "processed"]
  # time: momento de la toma de datos ["PRE", "POST"]
  
  # En cada uno de los parámetros se puede poner todas las tareas que querramos
  # por ejemplo: c("TOL", "CORSI").
  # En los otros parámetros podemos indicar una sola característica o varias
  # Por ejemplo: c("processed") si queremos que todos los df sean de datos procesados
  # o c("processed", "clean") si queremos que el primero venga procesado y el segundo limpio
  # De esta forma se pueden hacer todas las combinaciones de tareas, tipo de dato y momento de evaluación
  # siempre que respetemos el orden en que ponemos los strings dentro de cada vector
  
  # La función devuelve un dataframe por tarea
  
  
  parameters = list(local_folder, task, df_type, time)
  
  all_data = pmap(parameters, read_data)
  
  names(all_data) <- task
    
  list2env(all_data, envir = .GlobalEnv)

  
  
}



#### Leer bases de datos individualmente desde github ####

read_data_github <- function(task, df_type, time, git_pat){
  
  ## Esta función permite cargar una base de datos desde github a partir de la siguiente info:
  # task: tarea de interés ["STROOP", "ANT", "TOL", "CORSI", "KBIT"]
  # df_type: tipo de base ["original", "clean", "processed"]
  # time: momento de la toma de datos ["PRE", "POST"]
  
  # La base se carga en formato dataframe
  
  # Set your GitHub Personal Access Token
  github_pat <- git_pat
  
  file_name <- paste(paste(task,
                           time,
                           "PICT2014",
                           df_type,
                           sep = "_"),
                     ".csv", sep = "")
  
  data_path <- paste("Data",
                     "Cognitive",
                     task,
                     df_type,
                     file_name,
                     sep = "/")
  
  
  
  # Make a GET request to the GitHub API
  x <- GET(url = paste0("https://api.github.com/repos/FedeGiovannetti/Datos_PICT_2014/contents/", data_path),
           authenticate("username", github_pat),
           add_headers(`Accept` = "application/vnd.github.v3.raw"))
  # 
  # 
  data =content(x, type="text/csv")
  # 
  return(data)
  
}



#### Leer multiples bases de datos desde github ####


read_multiple_data_github <- function(task, df_type, time, git_pat){
  
  ## Esta función permite cargar varias bases de datos:
  # task: tarea de interés ["STROOP", "ANT", "TOL", "CORSI", "KBIT"]
  # df_type: tipo de base ["original", "clean", "processed"]
  # time: momento de la toma de datos ["PRE", "POST"]
  
  # En cada uno de los parámetros se puede poner todas las tareas que querramos
  # por ejemplo: c("TOL", "CORSI").
  # En los otros parámetros podemos indicar una sola característica o varias
  # Por ejemplo: c("processed") si queremos que todos los df sean de datos procesados
  # o c("processed", "clean") si queremos que el primero venga procesado y el segundo limpio
  # De esta forma se pueden hacer todas las combinaciones de tareas, tipo de dato y momento de evaluación
  # siempre que respetemos el orden en que ponemos los strings dentro de cada vector
  
  # La función devuelve un dataframe por tarea
  
  
  parameters = list(task, df_type, time, git_pat)
  
  all_data = pmap(parameters, read_data_github)
  
  names(all_data) <- task
  
  list2env(all_data, envir = .GlobalEnv)
    
    

  
  
}

