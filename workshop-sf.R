

# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)
library(sf)

#Asignar ubicacion directorios a variables
out_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/05_WorkshopR/Output"
in_dir  <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/05_WorkshopR/Input"
shp_dir <- glue("{in_dir}/shapefiles")

# Leer shapefiles  --------------------------------------------------------------

#RM zonas censales
data_geo_RM <- glue("{shp_dir}/R13_Santiago") %>% 
  st_read()  %>%
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(geocodigo),
    geocode = str_pad(geocode, width = 11, side = "left", pad = "0")) # %>% 
#   inner_join(indicadores_final, by=(c("geocode"))) %>% 


# Estaciones de Metro

metro_stgo <- glue("{shp_dir}/Estaciones_Metro_Santiago") %>% 
  st_read()  %>%
  rename_all(tolower) %>% 
  # Filtrar linea 3 - solo estaciones existentes
  filter(linea != "Linea 3") %>% 
  mutate(
    n_linea = str_sub(linea, -2, -1)
  )


# Plotear mapas  ----------------------------------------------------------

# Plotear mapa base
mapa_base <- data_geo_RM %>% 
  ggplot() + 
  geom_sf(color = "grey50") # +
  #Sacarle coordenadas de ejes X, Y
  #theme(axis.text.x = element_blank(), axis.text.y = element_blank())
mapa_base

# Base + estaciones metro
mapa_metro <-  mapa_base + geom_sf(data = metro_stgo, aes(color = n_linea)) 
# mapa_metro

# Modificar paleta colores por l??nea
metro_colores <- mapa_metro +
  scale_color_brewer(
    name = "Linea",
    palette = "Dark2"
  )
# metro_colores

# Agregar titulos y fuente
metro_colores +
  labs( x = NULL, y = NULL,
        title ="Estaciones de Metro Santiago",
        subtitle = NULL,
        caption = "Fuente: IDE OCUC https://ocuc.maps.arcgis.com/")

# Para asignar colores personalizados
paleta <- c("red", "gold", "navy", "forestgreen", "purple", "blue")
metro_colores2 <- mapa_metro +
  scale_colour_manual(
    name = "Linea",
    values = paleta) 
# metro_colores2

# Operaciones espaciales --------------------------------------------------

# Buffer alrededor estaciones de metro
buf <- st_buffer(metro_stgo, dist = 800)
#Plotear buffer sobre mapa base
mapa_base +
  geom_sf(data = buf, color = "red")

# Dissolve buffer
buf_tot <- st_union(buf)
#Plotear buffer "disuelto"
mapa_base +
  geom_sf(data = buf_tot, color = "red")

#Seleccionar ZC dentro del buffer


# Agregar datos escolaridad
  
# Plotear mapa con color por promedio anios escolaridad
  
# Plotear promedio anios escolaridad dentro del buffer metro
  
# Comparar promedio ponderado dentro/fuera buffer metro
# Tabla comparativa - esto es una operacion que involucra analisis espacial sin necesariamente visualizar un mapa 
  
# Dot density map 
  


  