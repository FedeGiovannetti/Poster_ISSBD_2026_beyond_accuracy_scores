library(flextable)
library(officer)



custom_flextable = function(data, title){
  
  # Esta función convierte al df en un flextable con algunas características
  
  data %>% 
    mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>%      # Redondea todos los números a 3 decimales
    # mutate(across(starts_with("p.value"),                                # Si hay alguna columna llamada "p.value"        
                  # ~ifelse(.x < .001, "\\textless.001", as.character(.x)))) %>% #  # agarra los valores menores a .001 y les pone   # "<.001"
    mutate(across(starts_with("p.value"),
                  ~ifelse(.x < .001, "<.001", as.character(.x)))) %>%
    flextable() %>%                   
    flextable::fontsize(size = 9) %>% 
    padding(padding = 2, part = "all") %>%
    line_spacing(space = 1) %>% 
    autofit() %>% 

    set_caption(caption= as_paragraph(as_chunk(title,
                                               props = fp_text_default(bold=TRUE, color="black",
                                                                       font.size = 10))), align_with_table = F)
}



# Function from https://www.anthonyschmidt.co/post/2020-06-03-making-apa-tables-with-gt/
apa <- function(x, title) {
  
  x %>% 
    mutate(across(where(is.numeric), ~round(.x, digits = 4))) %>%

  gt() %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = matches("p.value"), # Aplica a columnas que contengan "p.value"
        rows = p.value < 0.05        # Condición lógica
      )
    ) %>%
    
    # 3. TEXTO: Reemplazar el número por "< .001" visualmente
    text_transform(
      locations = cells_body(
        columns = matches("p.value"),
        rows = p.value < 0.001
      ),
      fn = function(x) "< .001" 
    ) %>%
    tab_options(
      table.border.top.color = "white",
      # heading.title.font.size = px(5),
      table.font.size = 12,
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    tab_caption(caption = md(title)) %>% 

    sub_missing(
      columns = where(is.numeric),
      missing_text = ""
    ) %>% 

    opt_align_table_header(align = "left") %>% 
    
    sub_missing(
      columns = where(is.numeric),
      missing_text = ""
    ) 
}


## Setting table propperties

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)





## Saving table

save_table <- function(my_flextable, path){
  
  my_flextable %>%                                                                 
    save_as_docx(path = path, pr_section = sect_properties)
  
  
}





# 1. Definimos una función segura para extraer los datos
obtener_tabla_mediacion <- function(modelo_mediacion) {
  
  # Obtenemos el resumen estadístico oficial
  s <- summary(modelo_mediacion)
  
  # Construimos la tabla manualmente sacando los valores de los "slots" del objeto
  tabla <- tribble(
    ~Term, ~Estimate, ~CI_lower, ~CI_Upper, ~p.value,
    
    # ACME (Average Causal Mediation Effect) -> Efecto Indirecto
    "ACME", s$d0, s$d0.ci[1], s$d0.ci[2], s$d0.p,
    
    # ADE (Average Direct Effect) -> Efecto Directo
    "ADE", s$z0, s$z0.ci[1], s$z0.ci[2], s$z0.p,
    
    # Total Effect
    "Total Effect", s$tau.coef, s$tau.ci[1], s$tau.ci[2], s$tau.p,
    
    # Prop. Mediated (Porcentaje explicado por la mediación)
    "Prop. Mediated", s$n0, s$n0.ci[1], s$n0.ci[2], s$n0.p
  )
  
  return(tabla)
}






