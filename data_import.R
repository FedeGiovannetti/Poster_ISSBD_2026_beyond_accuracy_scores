
#### 1. Cargo datasets y paquetes ####

library(tidyverse)

processed = read.csv("../corsi_error_analysis_2025/Bases PICT 2017 (Villa Dominico)/CORSI/CORSI_PICT2019_processed.csv")
trial_by_trial = read.csv("../corsi_error_analysis_2025/Bases PICT 2017 (Villa Dominico)/CORSI/corsi_clean_data/CORSI_PICT_2019_trials_completos.csv")
movements = read.csv("../corsi_error_analysis_2025/Bases PICT 2017 (Villa Dominico)/CORSI/corsi_clean_data/CORSI_PICT_2019_movements_completos.csv")
protocolo = openxlsx::read.xlsx("../corsi_error_analysis_2025/Protocolo CORSI (versiones 2018 y 2019).xlsx")
entrevistas_xlsx = openxlsx::read.xlsx("../corsi_error_analysis_2025/Bases PICT 2017 (Villa Dominico)/Base de datos cuestionarios PICT 2017 (VD JIM2 2019).xlsx")
entrevistas_sav = haven::read_sav("../corsi_error_analysis_2025/Bases PICT 2017 (Villa Dominico)/Base de datos (Entrevistas, TOL, CORSI y WM PICT 2017).sav") %>% 
  rename(Subject = "caso")

#### 1. Protocolo ####

protocolo =  protocolo %>% 
  # Convierto a numerico
  mutate(trial_number = as.numeric(trial_number)) %>% 
  # Defino trials de practica y de test
  mutate(trial_type = ifelse(is.na(trial_number),
                             "practice",
                             "test"))

protocolo$trial_number[1:5] <- 1:5 # le agrego números a los ensayos de práctica


#### 2. df_trials - Contiene información ensayo por ensayo ####


df_trials = movements%>%  
  rename(trial_number = "Trial.number") %>% 
  # Tiempos de 250 o menos no los contabilizamos
  # y los trial_result pasan a ser incorrectos
  # mutate(Time = ifelse(Time <= 250, NA, Time),
  #        Result = ifelse(is.na(Time), 0, Result)) %>% 
  
  # Cuando la variable "Block" tiene NA es porque el niño no
  # apretó nada. Entonces lo ponemos como "no_response".
  mutate(Block = ifelse(is.na(Block), "no_response", Block)) %>% 
  
  # Hay algunos "Block" que aparecen como "None" es decir que tocaron
  # la pantalla pero no tocaron un bloque. Los elimino
  filter(Block != "None") %>%
  
  summarise(
    response_sequence = paste(Block, collapse = ""),
    trial_result = max(Result),
    total_rt = sum(Time, na.rm = T),
    median_rt = median(Time, na.rm = T),
    first_rt = Time[1],     # Me quedo con el primer movimiento
    .by = c(Subject, trial_number)
  ) %>% 
  
  # Los ensayos 1:5 son práctica
  mutate(trial_type = ifelse(trial_number >= 6,
                             "test",
                             "practice")
  ) %>% 
  
  # Vuelvo a numerar para que el trial_number == 1 sea el primero de test
  mutate(trial_number = ifelse(trial_type == "test",
                               trial_number - 5,
                               trial_number)
  ) %>% 
  left_join(protocolo, by = c("trial_number", "trial_type"))


# Agrego distancia esperada

## Primero cargo la ubicación de los bloques

diccionario_bloques = read.csv("diccionario_bloques.csv")

## Calculo la distancia a partir de la euclideana

distancias_esperadas = protocolo %>% 
  dplyr::select(trial_number, sequence, blocks) %>% 
  # Genero una fila por cada bloque de la secuencia
  mutate(bloque = str_split(sequence, "")) %>% 
  unnest(bloque) %>% 
  # A cada bloque le doy una ubicación
  left_join(diccionario_bloques, by = "bloque") %>% 
  group_by(trial_number, sequence, blocks) %>% 
  # Calculo distancia euclideana 
  # primero movimiento por movimiento
  mutate(distance_by_row = sqrt((lag(ubicacion_x) - ubicacion_x)**2 + (lag(ubicacion_y) - ubicacion_y)**2)
  ) %>% 
  # Calculo distancia total
  summarise(total_distance = sum(distance_by_row, na.rm = T)) %>% 
  # Distancia ajustada por cantidad de bloques
  mutate(adj_distance = total_distance/blocks) %>% 
  ungroup()

## Lo junto con df_agg_corsi

df_trials = df_trials %>% 
  left_join(distancias_esperadas) 



#### 2.1. df_trials_practice es ensayo por ensayo de práctica ####

df_trials_practice = df_trials[df_trials$trial_type == "practice",]

#### 2.2. df_trials_test es ensayo por ensayo del test ####

# Primero armo el dataset de test

df_trials_test = df_trials[df_trials$trial_type == "test",] 

# Calculo el desempeño total en la práctica

practice_result = df_trials_practice %>% 
  filter(trial_number != 1) %>% 
  summarise(practice_acc = mean(trial_result), .by = 'Subject')
  
# Calculo como les fue en los de 2 bloques

block_2_result <- df_trials_test %>%
  filter(blocks == 2) %>%
  summarise(block_2_acc = sum(trial_result), .by = 'Subject')

# Junto todo en df_trials_test
df_trials_test <- df_trials_test %>%
  left_join(practice_result, by = "Subject") %>%
  left_join(block_2_result, by = "Subject")

# Elimino estos df que ya no me interesan
rm(practice_result, block_2_result)

#### 3. df_agg_corsi - Contiene información resumida por sujeto

df_agg_corsi <-  df_trials_test %>% 
  group_by(Subject) %>% 
  summarise(
            # Variables de resultado
            Accuracy = sum(trial_result),
            Prop_correct = mean(trial_result),
            played = n(),
            
            # Variables de tiempo
            total_rt_correct_median = median(total_rt[trial_result == 1], na.rm = TRUE),
            total_rt_incorrect_median = median(total_rt[trial_result == 0], na.rm = TRUE),
            mean_rt_correct_median = median(median_rt[trial_result == 1], na.rm = TRUE),
            mean_rt_incorrect_median = median(median_rt[trial_result == 0], na.rm = TRUE),
            first_rt_correct_median = median(first_rt[trial_result == 1], na.rm = TRUE),
            first_rt_incorrect_median = median(first_rt[trial_result == 0], na.rm = TRUE),
            practice_acc = max(practice_acc),
            block_2_result = max(block_2_acc),
            
            # Máxima longitud total alcanzada
            max_long = max(total_distance[trial_result == 1]),
            # Máxima longitud ajustada alcanzada
            max_long_adj = max(adj_distance[trial_result == 1]),
            
            # Máxima cantidad de cruces alcanzada
            max_crossings = max(Grupo.path[trial_result == 1])
            
            
            
            )
  
  


# Calculo máximo nivel alcanzado

max_level_subj = df_trials_test %>% 
  group_by(Subject, blocks) %>% 
  summarise(suma = sum(trial_result)) %>%
  filter(suma >= 3) %>% 
  # filter(trial_result == 1) %>% 
  group_by(Subject) %>% 
  # slice_max(blocks, n= 1)
  summarise(max_level = max(blocks)) %>% 
  ungroup()

# Agrego máximo nivel alcanzado

df_agg_corsi = df_agg_corsi %>% 
  left_join(max_level_subj) %>% 
  dplyr::select(Accuracy, Prop_correct, max_level, everything()) %>% 
  
  mutate(max_level = ifelse(is.na(max_level), 1, max_level)) %>% 
  filter(played >= 14)
# %>% 
  # filter(Score > 1)
  





#### 4. Subject_info contiene info de las entrevistas

# Selecciono variables de interés 

subject_info = entrevistas_xlsx %>% 
  dplyr::select(Subject, EDAD, SEXO, PJENES) %>% 
  left_join(entrevistas_sav  %>% 
              dplyr::select(Subject,Grnes2, mateWM, lenguaWM, estumad)
  ) %>% 
  filter(PJENES != 0) %>% 
  rename(math_WM = "mateWM")
  
  


#### 5. Agrego subject_info a los datos de corsi y escalo

df_agg_corsi_all_unscaled = df_agg_corsi %>% 
  left_join(subject_info) %>% 
  filter(!is.na(PJENES)) %>% 
  ungroup() 

df_agg_corsi_all = df_agg_corsi_all_unscaled%>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
