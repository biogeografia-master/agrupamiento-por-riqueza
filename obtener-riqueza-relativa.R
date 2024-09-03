# Scritp para procesar puntos de GBIF para obtener riqueza
# relativa de especies por géneros de bromelias
# Fuente de datos: https://github.com/biogeografia-202202/material-de-apoyo/blob/master/practicas/demos-personalizadas/registros_depurados_de_presencia_bromeliaceae.gpkg

# Paquetes
library(sf)
library(tidyverse)
library(stringr)

# Datos
d <- st_read('data/registros_depurados_de_presencia_bromeliaceae.gpkg')
p <- st_read('data/provincias-4326.gpkg')

# Riqueza relativa por géneros
riqueza_relativa_provincias <- st_join(d, p) %>% 
  st_drop_geometry() %>% 
  select(ESPECIE=acceptedScientificName, PROVINCIA=TOPONIMIA) %>% 
  filter(ESPECIE != 'Bromeliaceae') %>%
  mutate(GENERO = word(ESPECIE,1)) %>%
  distinct() %>% 
  group_by(PROVINCIA, GENERO) %>%
  summarise(RIQUEZA_DE_ESPECIES = n_distinct(ESPECIE)) %>% 
  filter(!is.na(PROVINCIA)) %>% 
  group_by(PROVINCIA) %>% 
  mutate(RIQUEZA_DE_ESPECIES = RIQUEZA_DE_ESPECIES/sum(RIQUEZA_DE_ESPECIES)*100) %>% 
  pivot_wider(names_from = GENERO, values_from = RIQUEZA_DE_ESPECIES, values_fill = 0)

# sf
riqueza_relativa_provincias_sf <- p %>%
  full_join(riqueza_relativa_provincias, by = c('TOPONIMIA' = 'PROVINCIA'))

riqueza_relativa_provincias_sf %>% 
  st_write('data/riqueza_relativa_provincias_sf.gpkg')

