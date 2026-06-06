library(tidyverse)



#### Función para generar variables en tareas tipo posner ####

Variables_Posner_tasks <- function(data, grouping_vars){
  
  # La variable de agrupamiento debe ser un string con el nombre de una o más variables (e.g. c("username"))
  # En el caso de que uses más de una variable, devolverá datos en formato long.
  
  
  ## Ensayos sin respuesta e impulsivos
  
  a <- data %>%
    group_by(pick({{grouping_vars}})) %>% 
    summarise(completed_trials = n(),
              omitted_trials_amount = sum(is.na(rt)),                       # Cantidad de ensayos sin respuesta
              omitted_trials_perc = omitted_trials_amount/n(),         # Porcentaje de ensayos sin respuesta
              impulsive_amount = sum(rt <= 250, na.rm = T)) #%>%             # Cantidad de ensayos impulsivos
  # complete({{grouping_vars}}, fill = list(rt = NA))
  
  ## Ensayos correctos
  
  b <- data %>%
    filter(rt > 250) %>%
    group_by(pick({{grouping_vars}})) %>%
    summarise(correct_trials_amount = sum(trial_result),                              # Cantidad de ensayos correctos
              correct_trials_perc = correct_trials_amount/n(),                   # Porcentaje de ensayos correctos
              correct_trials_resp_perc = correct_trials_amount/sum(!is.na(rt))) # Porcentaje de ensayos correctos en base a los ensayos respondidos
  
  
  ## Tiempos de reacción
  
  c <- data %>%
    group_by(pick({{grouping_vars}})) %>%
    filter(rt > 250 & trial_result == 1) %>%
    
    summarise(mean_rt = mean(rt),                                            # Media tiempo de reacción
              median_rt = median(rt))                                        # Mediana tiempo de reacción
  
  
  
  ## Creo un df con todas las combinaciones de grouping_vars posibles.
  ## Esto es para asegurarme de no perder ningún caso cuando junto todo.
  
  unique_values <- data %>%
    select(all_of(grouping_vars), Year, Date) %>%
    distinct() 
  
  
  ## Junto las tres mini bases
  
  
  result_variables = unique_values%>% 
    left_join(a, by = grouping_vars) %>% 
    left_join(b, by = grouping_vars) %>% 
    left_join(c, by = grouping_vars)
  
  return(result_variables)
  
  
}



#### Función para generar variables en específicas de ANT ####

Variables_ANT <- function(data, grouping_vars){
  
  # La variable de agrupamiento debe ser un string con el nombre de una o más variables (e.g. c("username"))
  # En el caso de que uses más de una variable, devolverá datos en formato long.
  
  reaction_time = data %>%
    group_by(pick({{grouping_vars}}), clue_type) %>%
    filter(rt > 250 & trial_result == 1) %>%
    summarise(median_rt = median(rt)) %>% 
    ungroup()
  
  
  ## Red de Alerta
  
  # Se calcula restando el RT de los ensayos sin señal menos RT de los ensayos con señal doble
  
  alertness <- reaction_time%>%
    group_by(pick({{grouping_vars}}))%>%
    filter(clue_type == "no" |
           clue_type == "doble")  %>%
    filter(n() == 2) %>%   # Si tiene 2 trials por sujeto significa que tiene ambas condiciones
    pivot_wider(names_from = clue_type, values_from = median_rt) %>% 
    mutate(rt_alerta = no - doble) %>% 
    select(all_of({{grouping_vars}}),rt_alerta)
  
  
  
  ## Red de Orientación
  
  # Se calcula restando el RT de ensayos con señal central menos RT de los ensayos con señal espacial

  orienting <- reaction_time%>%
    group_by(pick({{grouping_vars}}))%>%
    filter(clue_type == "center" |
           clue_type == "up" |
           clue_type == "down") %>%
    filter(n() == 3) %>%   # Si tiene 3 trials por sujeto significa que tiene ambas condiciones
    pivot_wider(names_from = clue_type, values_from = median_rt) %>% 
    mutate(spatial = (up + down)/2,
           rt_orientacion = center - spatial)%>% 
    select(all_of({{grouping_vars}}), rt_orientacion)
  
  
  
  ## Red ejecutiva
  
  # Se calcula restando el RT de los ensayos incongruentes menos el de los congruentes
  
  executive = data %>%
    group_by(pick({{grouping_vars}}), condition) %>%
    filter(rt > 250 & trial_result == 1) %>%
    summarise(median_rt = median(rt)) %>%
    filter(n() == 2) %>%   # Si tiene 2 trials por sujeto significa que tiene ambas condiciones
    pivot_wider(names_from = condition, values_from = median_rt) %>% 
    mutate(rt_ejecutiva = incongruent - congruent)%>% 
    select(all_of({{grouping_vars}}), rt_ejecutiva)

  
  
  ## Creo un df con todas las combinaciones de grouping_vars posibles.
  ## Esto es para asegurarme de no perder ningún caso cuando junto todo.

  unique_values <- data %>%
    select(all_of(grouping_vars), Year) %>%
    distinct()


  ## Junto las tres mini bases


  result_variables = unique_values%>%
    left_join(alertness, by = grouping_vars) %>%
    left_join(orienting, by = grouping_vars) %>%
    left_join(executive, by = grouping_vars)

  return(result_variables)

  
}



#### Función para generar variables en tareas tipo TOL ####

Variables_TOL_tasks <- function(data, grouping_vars){
  
  # La variable de agrupamiento debe ser un string con el nombre de una o más variables (e.g. c("username"))
  # En el caso de que uses más de una variable, devolverá datos en formato long.
  
  
  data = data %>% 
    mutate(outlier = rt_first_movement > 120)                                ### Si el ensayo tarda más de 2 minutos, lo pongo como outlier
  
  ## Ensayos sin respuesta o con demora en el tiempo
  
  a <- data %>%
    group_by(pick({{grouping_vars}})) %>% 
    summarise(omitted_trials_amount = sum(is.na(rt_first_movement)),                # Cantidad de ensayos sin respuesta
              outlier_amount = sum(outlier == T)) %>%                               # Cantidad de ensayos impulsivos
    mutate(outlier_amount = ifelse(is.na(outlier_amount), 
                                   0,
                                   outlier_amount))
  
  ## Ensayos correctos
  
  b <- data %>%
    filter(outlier != T) %>%
    group_by(pick({{grouping_vars}})) %>%
    summarise(completed_trials = n(),
              correct_trials_amount = sum(trial_result),                         # Cantidad de ensayos correctos
              correct_trials_perc = correct_trials_amount/completed_trials)                   # Porcentaje de ensayos correctos
 
  
  
  ## Tiempos de reacción correctos
  
  c <- data %>%
    group_by(pick({{grouping_vars}})) %>%
    filter(outlier != T & trial_result == 1) %>%
    
    summarise(rt_planning_mean_correct = mean(rt_first_movement),                                            # Media tiempo de reacción
              rt_planning_median_correct = median(rt_first_movement))                                        # Mediana tiempo de reacción
  
  ## Tiempos de reacción incorrectos
  
  d <- data %>%
    group_by(pick({{grouping_vars}})) %>%
    filter(outlier != T & trial_result == 0) %>%
    
    summarise(rt_planning_mean_incorrect = mean(rt_first_movement),                                            # Media tiempo de reacción
              rt_planning_median_incorrect = median(rt_first_movement))                                        # Mediana tiempo de reacción
  
  
  
  ## Creo un df con todas las combinaciones de grouping_vars posibles.
  ## Esto es para asegurarme de no perder ningún caso cuando junto todo.
  
  unique_values <- data %>%
    select(all_of(grouping_vars), Year, Date) %>%
    distinct() 
  
  
  ## Junto las tres mini bases
  
  
  result_variables = unique_values%>% 
    left_join(a, by = grouping_vars) %>% 
    left_join(b, by = grouping_vars) %>% 
    left_join(c, by = grouping_vars) %>% 
    left_join(d, by = grouping_vars)
  
  return(result_variables)
  
  
}


#### Función para generar variables en tareas tipo corsi ####

Variables_CORSI_tasks <- function(data, grouping_vars){
  
  # La variable de agrupamiento debe ser un string con el nombre de una o más variables (e.g. c("username"))
  # En el caso de que uses más de una variable, devolverá datos en formato long.
  
  
  data = data %>% 
    mutate(outlier = rt > 120)                                ### Si el ensayo tarda más de 2 minutos, lo pongo como outlier
  
  ## Ensayos sin respuesta o con demora en el tiempo
  
  a <- data %>%
    group_by(pick({{grouping_vars}})) %>% 
    summarise(outlier_amount = sum(outlier == T))                               # Cantidad de ensayos outliers
  
  
  ## Ensayos correctos
  
  b <- data %>%
    filter(outlier != T) %>%
    group_by(pick({{grouping_vars}})) %>%
    summarise(completed_trials = n(),
              correct_trials_amount = sum(trial_result),                         # Cantidad de ensayos correctos
              correct_trials_perc = correct_trials_amount/completed_trials)                   # Porcentaje de ensayos correctos
  
  
  
  ## Tiempos de reacción correctos
  
  c <- data %>%
    group_by(pick({{grouping_vars}})) %>%
    filter(outlier != T & trial_result == 1) %>%
    
    summarise(rt_mean_correct = mean(rt),                                            # Media tiempo de reacción
              rt_median_correct = median(rt))                                        # Mediana tiempo de reacción
  
  ## Tiempos de reacción incorrectos
  
  d <- data %>%
    group_by(pick({{grouping_vars}})) %>%
    filter(outlier != T & trial_result == 0) %>%
    
    summarise(rt_mean_incorrect = mean(rt),                                            # Media tiempo de reacción
              rt_median_incorrect = median(rt))                                        # Mediana tiempo de reacción
  
  
  
  ## Creo un df con todas las combinaciones de grouping_vars posibles.
  ## Esto es para asegurarme de no perder ningún caso cuando junto todo.
  
  unique_values <- data %>%
    select(all_of(grouping_vars), Year, Date) %>%
    distinct() 
  
  
  ## Junto las tres mini bases
  
  
  result_variables = unique_values%>% 
    left_join(a, by = grouping_vars) %>% 
    left_join(b, by = grouping_vars) %>% 
    left_join(c, by = grouping_vars) %>% 
    left_join(d, by = grouping_vars)
  
  return(result_variables)
  
  
}


