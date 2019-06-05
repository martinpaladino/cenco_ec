library(readr)
electorales <- read_csv("datos/resultados_electorales_2019.csv")


#Anda, cambiar en censo la concordia a Santo domingo y anda!!!
cbind(
  base_cortos %>%
    mutate(p = str_to_lower(provincia), 
           ca = str_to_lower(canton)) %>% 
    filter(p!="zonas no delimitadas") %>% 
    mutate(p = ifelse(ca == "la concordia", "santo domingo de los tsáchilas", p)) %>% 
    arrange(p, ca),
  electorales %>% 
    filter(cargo == "Alcaldía") %>% 
    mutate(p = str_to_lower(provincia), 
           ca = str_to_lower(cantón)) %>% 
    rename(p.elec = p, ca.elec = ca, 
           provincia_elec = provincia, 
           canton_elec = cantón) %>% 
    arrange(p.elec, ca.elec)
) %>% 
  #.[,-1] %>% 
  as_tibble() %>% 
  select(-p.elec, -p, -ca.elec, -ca, -p.elec) -> ics
