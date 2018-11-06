

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
  # Sistema coordenadas UTM zone 19S - metros
  st_transform(24879) %>% 
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(geocodigo),
    geocode = str_pad(geocode, width = 11, side = "left", pad = "0")) # %>% 
#   inner_join(indicadores_final, by=(c("geocode"))) %>% 


# Estaciones de Metro

metro_stgo <- glue("{shp_dir}/Estaciones_Metro_Santiago") %>% 
  st_read()  %>%
  # Sistema coordenadas UTM zone 19S
  st_transform(24879) %>% 
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
  geom_sf(color = "grey50") 
  #Sacarle coordenadas de ejes X, Y
  # + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
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
## Plotear buffer "disuelto"
mapa_base +
  geom_sf(data = buf_tot, color = "red", alpha = .5)

# Seleccionar ZC dentro del buffers

#------------- Metodo 1: Intersectar
## Intersectar
buf_inters <- st_intersection(data_geo_RM, buf_tot) # 673 Observaciones
## Plotear
ggplot() +
  geom_sf(data = buf_inters)
# Plotear intersection arroja los atributos (ZC) cortados - como seleccionar el atributo completo?

## Seleccionar solo geocodigo y hacerlo dataframe 
## para que pierda atributos de geometría
zc_buf <- as.data.frame(buf_inters) %>% select(geocode) 
## Unirlo con el shapefile original ZC - inner join
data_RM_buf <- data_geo_RM %>% inner_join(zc_buf, by = "geocode")
## Plotear
ggplot() +
  geom_sf(data = data_RM_buf) +
  ## Sobreponer buffer con transparencia
  geom_sf(data = buf_tot, color = "red" , alpha = .5)
# Funciona e incluye todas las ZC que intersectan el buffer

#------------- Metodo 2: seleccionar solo las ZC cuyto centroide esta dentro del buffer
## Covertir poligonos a puntos - centroide 
zc_centr <- st_centroid(data_geo_RM)
## Plotear sobre mapa base
mapa_base + 
  geom_sf(data = zc_centr, color = "grey40")
## Intersectar con buffer
buf_centr_inter <- st_intersection(zc_centr, buf_tot) # 463 Observaciones
## Plotear interseccion
mapa_base + 
  geom_sf(data = buf_centr_inter, color = "navy") +
  geom_sf(data = buf_tot, color = "red", alpha = .2)


# Agregar datos a nivel espacial------------------------------------

# Agregar datos escolaridad
escolaridad_media <- read.csv2(glue("{out_dir}/escolaridad_media_zc.csv")) %>% 
  mutate(
    geocode = as.character(geocode)
  )

# Plotear mapa con color por promedio anios escolaridad


  
# Plotear promedio anios escolaridad dentro del buffer metro
  
# Comparar promedio ponderado dentro/fuera buffer metro
# Tabla comparativa - esto es una operacion que involucra analisis espacial sin necesariamente visualizar un mapa 
  
# Dot density map - niveles escolaridad

  

# Calcular area y controlar unidades -------------------------------

area <- st_area(data_geo_RM) # Devuelve el valor en m2 siguiendo el sist de coordenadas
area_ha <- area %>% units::set_units(ha)
area_ha
## Agregarlo como variable
data_geo_RM$area_ha <- as.numeric(area_ha)

# Prueba 
  