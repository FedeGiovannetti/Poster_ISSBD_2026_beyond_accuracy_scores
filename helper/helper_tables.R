library(flextable)
library(officer)



custom_flextable = function(data, title){
  
  # Esta función convierte al df en un flextable con algunas características
  
  data %>% 
    mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>%      # Redondea todos los números a 3 decimales
    mutate(across(starts_with("p.value"),                              # Si hay alguna columna llamada "p.value" 
                  ~ifelse(.x < .001, "<.001", as.character(.x)))) %>%                # agarra los valores menores a .001 y les pone
    flextable() %>%                                                    # "<.001"
    padding(padding = 2, part = "all") %>%
    line_spacing(space = 1) %>% 
    autofit() %>% 
    
    set_caption(caption= as_paragraph(as_chunk(title,
                                               props = fp_text_default(bold=TRUE, color="black",
                                                                       font.size = 12))), align_with_table = F)
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










