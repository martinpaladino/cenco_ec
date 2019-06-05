# Empate de bases de datos de pobreza, educación y demográfica

library(readxl)
library(tidyverse)
demográficos <- read_excel("datos/demográficos.xls", 
                           skip = 1) %>% mutate_at(vars(Total, Urbano, Rural), as.numeric)
educativos <- read_excel("datos/educativos.xls", 
                         skip = 1)
pobreza <- read_excel("datos/pobreza.xls", 
                      skip = 1)
viviendas <- read_excel("datos/viviendas.xls", 
                        skip = 1)

economía <- read_excel("datos/economía.xls", 
                       skip = 1)
# Verificaciones====

# Verificar que en todas las bases hay 222 cantones
lista <- list(demográficos = demográficos, educativos = educativos, pobreza = pobreza, viviendas = viviendas)
map_df(lista, ~select(.x, Cantón) %>% distinct %>% count())
    
# Test de anti-join

# Lista de cruces
combn(names(lista), 2)

# Todos TRUE

nrow(anti_join(demográficos, pobreza, by = c("Provincia", "Cantón"))) == 0
nrow(anti_join(demográficos, educativos, by = c("Provincia", "Cantón"))) == 0
nrow(anti_join(pobreza, educativos, by = c("Provincia", "Cantón"))) == 0

# Variables de interés (exploración)

map_df(lista, ~select(.x, Indicador) %>% distinct(), .id = "datos") -> indicadores

#Limpieza pre join======


left_join(  #Este join es parcial!
  
  #lado izquierdo del join: población por "étnia"

demográficos %>% 
  filter(str_detect(Indicador, 
        "Porcentaje de población (blanca|mestiza|indígena|montubia|mulata|negra-afroecuatoriana|autoidentificada como otra)") |
          str_detect(Indicador, "Población Total")) %>% 
  select(Provincia, Cantón, Indicador, Total) %>% 
  spread(Indicador, Total), 

#join con porcentaje de población rural

demográficos %>% 
  filter(Indicador == "Población Total") %>% 
  mutate("Porcentaje de población rural" = (Rural/Total)*100) %>% 
  select(Provincia, Cantón, "Porcentaje de población rural")) -> demo_limpio

educativos %>% 
  filter(Indicador == "Escolaridad promedio del jefe de hogar" | Indicador == "Tasa de analfabetismo") %>% 
  select(Provincia, Cantón, Indicador, Total) %>% 
  spread(Indicador, Total) -> educativos_limpio

pobreza %>% 
  filter(Indicador == "Pobreza por NBI (Personas)") %>% 
  select(Provincia, Cantón, Indicador, Total) %>% 
  spread(Indicador, Total) -> pobreza_limpio

viviendas %>% 
  filter(Indicador == "Porcentaje de viviendas con servicio de energía eléctrica" | 
                     Indicador == "Porcentaje de viviendas que eliminan la basura por carro recolector" |
                     Indicador == "Porcentaje de viviendas con abastecimiento de agua por red pública en su interior") %>% 
  select(Provincia, Cantón, Indicador, Total) %>% 
  spread(Indicador, Total) -> viviendas_limpio

economía %>% 
  filter(Indicador %in% c("Población económicamente activa", 
                          "Población ocupada", 
                          "Población ocupada en el sector público")) %>% 
  select(Provincia, Cantón, Indicador, Total) %>% 
  spread(Indicador, Total) -> economía_limpio



# Lista de bases de datos para join

lista_limpio <- list(demo_limpio, educativos_limpio, pobreza_limpio, viviendas_limpio, economía_limpio)

#join final

reduce(lista_limpio, left_join) %>% 
  mutate_if(is.numeric, replace_na, 0) -> base_unida

# Nombres cortos y objeto etiquetado

c("provincia", "canton", "pobtot", "otra", "blanca", "indígena", "mestiza", 
  "montubia", "mulata", "afroec", "rural", "escolaridad", "analf", "NBI",
  "agua", "elec", "basura", "pea", "ocupada", "publico") -> cortos


set_names(base_unida, cortos) -> base_cortos

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

nombres_largos <- c("Provincia", "Cantón", "Población Total", "Porcentaje de población autoidentificada como otra", 
  "Porcentaje de población blanca", "Porcentaje de población indígena", 
  "Porcentaje de población mestiza", "Porcentaje de población montubia", 
  "Porcentaje de población mulata", "Porcentaje de población negra-afroecuatoriana", 
  "Porcentaje de población rural", "Escolaridad promedio del jefe de hogar", 
  "Tasa de analfabetismo", "Pobreza por NBI (Personas)", "Porcentaje de viviendas con abastecimiento de agua por red pública en su interior", 
  "Porcentaje de viviendas con servicio de energía eléctrica", 
  "Porcentaje de viviendas que eliminan la basura por carro recolector", 
  "Población económicamente activa", "Población ocupada", "Población ocupada en el sector público", 
  "Provincia en datos electorales", "Cantón en datos electorales", "Cargo", "Candidato o candidata ganarador o ganadora", 
  "Número de boleta", "Nombres del partido")

#write_csv(ics, "./datos/indicadores_censales_seleccionados.csv")
#ics <- read_csv("datos/indicadores_censales_seleccionados.csv")
# diccionario <- data.frame(nombre = names(ics), descripcion = nombres_largos)
