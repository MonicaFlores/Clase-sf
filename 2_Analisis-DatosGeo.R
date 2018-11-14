# Nombre Programa:  02_Analisis_Espacial.R
# Ubicacion:      	GitHub/Clase-sf
# Autor:        	  Monica Flores
# Fecha Creacion:   05/11/2018
# Proyecto:         Workshop Analisis Datos Espaciales 
# Objetivo:    	    Explorar herramientas de analisis espacial utilizando datos Censo 2017
# Notas:


# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)
library(sf)

# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/05_WorkshopR"
# dir_loc <- "C:/Users/CEDEUS 18/Desktop/Workshop_R" #Ejemplo si la carpeta esta en el escritorio 

# Asignar ubicacion directorios a variables
out_dir <- glue("{dir_loc}/Output")
in_dir  <- glue("{dir_loc}/Input")
shp_dir <- glue("{in_dir}/shapefiles")

# Leer Datos escolaridad limpios - los ocuparemos al final del script
escolaridad_media <- read.csv2(glue("{out_dir}/escolaridad_media_zc.csv")) %>% 
  mutate(
    geocode = as.character(geocode)
  )

# Leer shapefiles  --------------------------------------------------------------

#RM zonas censales
data_geo_RM <- glue("{shp_dir}/R13_Santiago") %>% 
  st_read()  %>%
  # Sistema coordenadas UTM zone 19S - metros
  st_transform(24879) %>% 
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(geocodigo),
    geocode = str_pad(geocode, width = 11, side = "left", pad = "0")) %>% 
  ## Unir datos escolaridad media ZC   
  left_join(escolaridad_media, by = "geocode")

# Estaciones de Metro
metro_stgo <- glue("{shp_dir}/Estaciones_Metro_Santiago") %>% 
  st_read()  %>%
  # Sistema coordenadas UTM zone 19S - metros
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

# Modificar paleta colores por linea
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

# Seleccionar ZC dentro del buffer

#------------- Metodo 1: Intersectar

## Intersectar
buf_inters <- st_intersection(data_geo_RM, buf_tot) # 673 Observaciones
## Plotear
ggplot() +
  geom_sf(data = buf_inters)
# Plotear intersection arroja los atributos (ZC) cortados - como seleccionar el atributo completo?

## Seleccionar solo geocodigo y hacerlo dataframe para que pierda atributos de geometria
zc_buf <- as.data.frame(buf_inters) %>% select(geocode) 
## Unirlo con el shapefile original ZC - inner join
data_RM_buf <- data_geo_RM %>% inner_join(zc_buf, by = "geocode")
## Plotear
ggplot() +
  geom_sf(data = data_RM_buf) +
  ## Sobreponer buffer con transparencia
  geom_sf(data = buf_tot, color = "red" , alpha = .5)
# Funciona e incluye todas las ZC que intersectan el buffer

#------------- Metodo 2: seleccionar solo las ZC cuyo centroide esta dentro del buffer

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

#extraer poligonos ZC
buf_zc_metro <- as.data.frame(buf_centr_inter) %>% select(geocode) %>% inner_join(data_geo_RM, by="geocode")

# Agregar datos Censo a nivel ZC------------------------------------

# Plotear mapa con color por promedio escolaridad
mapa_esc <- ggplot(data = data_geo_RM, aes(fill = a_esc_media)) + 
  geom_sf(lwd = 0.01) +
  scale_fill_continuous(name = "Escolaridad\nPromedio", 
                        trans = "reverse",
                        breaks = c(4, 8, 12, 16)
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) #Sacarle coordenadas de ejes X, Y

mapa_esc +
  labs( x = NULL, y = NULL,
        title ="Escolaridad Promedio por Zona Censal",
        subtitle = "Poblacion Mayor de 25 anos",
        caption = "Fuente: INE Censo 2017 http://www.censo2017.cl/microdatos/") # Agregar titulos y fuente
  
# Plotear promedio escolaridad dentro del buffer metro - usar datos metodo centroide
mapa_esc_buffer <- ggplot(data = buf_zc_metro, aes(fill = a_esc_media)) + 
    geom_sf(lwd = 0.01) +
    scale_fill_continuous(name = "Escolaridad\nPromedio", 
                          trans = "reverse",
                          breaks = c(4, 8, 12, 16)
    ) 
mapa_esc_buffer

# Bonus: Calcular area y controlar unidades -------------------------------

area <- st_area(data_geo_RM) # Devuelve el valor en m2 siguiendo el sist de coordenadas
area_ha <- area %>% units::set_units(ha)
area_ha
## Agregarlo como variable
data_geo_RM$area_ha <- as.numeric(area_ha)

# Comparar promedio ponderado dentro/fuera buffer metro -------------------

## Extraer poligonos que NO estan en el buffer
not_buf_metro <- data_geo_RM  %>% anti_join(buf_zc_metro, by="geocode")

## Juntar bases de datos con un identificador dentro/fuera buffer
total_zc <- buf_zc_metro %>% bind_rows(not_buf_metro, .id = "buffer") %>% 
  mutate(buffer=if_else(buffer==1, "Dentro buffer", "Fuera buffer")) # 1 dentro buffer, 0 fuera buffer

# crear tabla comparativa
comparativa2 <- total_zc %>% 
  group_by(buffer) %>% 
  summarise(
    pop_total = sum(total_pers, na.rm = TRUE),
    total = sum(total_pers * a_esc_media/pop_total, na.rm = TRUE)) # na.rm parametro para descartar NAs

# Graficar histograma con ggplot 2 ------------------------------------------

a <- buf_zc_metro %>% 
  ggplot(aes(a_esc_media)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.05) +
  labs( x = "Escolaridad Media", y = "Frecuencia",
        title ="Escolaridad Según Cercanía al Metro",
        subtitle = "Zonas Censales a menos de 800m de una Estación de Metro",
        caption = NULL) 

b <- not_buf_metro %>% 
  ggplot(aes(a_esc_media)) + 
  geom_histogram(color = "grey", fill= "forestgreen",  lwd=0.1, binwidth = 0.05) +
  labs( x = "Escolaridad Media", y = "Frecuencia",
        title ="Escolaridad Según Cercanía al Metro",
        subtitle = "Zonas Censales a más de 800m de una Estación de Metro",
        caption = NULL) 

#Plotear histogramas en la misma lamina
gridExtra::grid.arrange(a, b, nrow=1) # Considerar que no estan en la misma escala

# Graficar las 2 distribuciones como linea
c <- total_zc %>%  
  ggplot(aes(a_esc_media, color = buffer)) + 
  geom_density() # Grafica la distribucion "suavizada", eje Y es la probabilidad de obtener cierto valor, area bajo la curva = 1 
c

## Mismo grafico version elegante
d <- total_zc %>%  
  ggplot(aes(a_esc_media, color = buffer)) + 
  geom_density() + 
  labs( x = "Escolaridad Media", y = "Densidad de Probabilidad",
        title ="Escolaridad Según Cercanía al Metro",
        subtitle = "Distribución Continua de Escolaridad Promedio\npor Zonas Censales y su Ubicación Respecto al Metro",
        caption = NULL,
        color = "Buffer 800m") +
  scale_y_continuous(labels = scales::percent)
d  

