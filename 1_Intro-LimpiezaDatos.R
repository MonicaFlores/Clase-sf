# Nombre Programa:  1_Intro_LimpiezaDatos.R
# Ubicacion:      	GitHub/Clase-sf
# Autor:        	  Monica Flores
# Fecha Creacion:   05/11/2018
# Proyecto:         Workshop Analisis Datos Espaciales 
# Objetivo:    	    Explorar herramientas de analisis y limpieza de datos
# Notas:

# Instalar paquete y libreria tidyverse - descomentar install.packages quien no lo tenga instalado

# install.packages (c("tidyverse", "glue", "sf"))

library(tidyverse) # Limpieza y visualizacion de datos (dplyr + stringr + ggplot2 + tidyr + readr + purrr)
library(glue)      # Pegar texto desde variables
library(sf)        # Simple Feautures: manipular datos vectoriales espaciales

# Introduccion a R y Limpieza de datos con Dplyr  -------------------------

# Correr comandos: seleccionar + ctrl + enter
2 + 2 

# Asignar informacion a una variable
cuatro <- 2 + 2
cuatro

# Crear un vector
mi_vector <- c(1, 2, 3, 4, 5, 6) # Vector numerico
mi_vector2 <- 1:6
mi_vector3 <- c(1:6, 10, 15, 20)
mi_vector4 <- c("uno", "dos", "tres", "cuatro", "cinco", "seis") # Vector de caracteres
mi_vector5 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE) # Vector logico

#Seleccionar elementos dentro del vector
mi_vector4[6]
mi_vector5[c(1, 3, 5)]

# Operaciones con vectores y variables
mi_vector * 4
mi_vector * cuatro
mi_vector3[8]*2 

# Limpieza de datos con Dyplr 
## Paquete para manipulacion de datos 
## permite filtrar, seleccionar, agrupar y calcular facilmente

# Crear un dataframe
df <- data.frame(mi_vector4, mi_vector, mi_vector5)
# Explorar nuestros datos
head(df)
summary(df)

# Nueva variable a partir de nuestros datos
df$cuadrado <- df$mi_vector^2
# Operaciones con variables
mean(df$mi_vector)
mean(df$cuadrado)

# Los 5 verbos de Dyplr: Select, filter, arrange, mutate, summarise 

select(df, mi_vector, cuadrado)
filter(df, mi_vector5==TRUE)
arrange(df, desc(mi_vector))
mutate(df, cubo = mi_vector^3)
summarise(df,
          media_mv = mean(mi_vector),
          media_cuadrado = mean(cuadrado)
          )

# Operaciones con Pipa %>% ctrl + shft + M  
df %>% summarise(
  media_mv = mean(mi_vector),
  media_cuadrado = mean(cuadrado)
)
# Calculos con datos agrupados: Group by 
df %>% group_by(mi_vector5) %>% 
  summarise(
  media_mv = mean(mi_vector),
  media_cuadrado = mean(cuadrado)
)


# Analisis de datos Censo 2017 a nivel de zona censal ---------------------

# Obtener un promedio de años de escolaridad por Zona Censal

# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/05_WorkshopR"
# dir_loc <- "C:/Users/CEDEUS 18/Desktop/Workshop_R" #Ejemplo si la carpeta esta en el escritorio 

# Asignar ubicacion directorios a variables
out_dir <- "{dir_loc}/Output"
in_dir  <- "{dir_loc}/Input"
shp_dir <- glue("{in_dir}/shapefiles")

# Importar datos como dataframe
escolaridad_csv <- read.csv2(glue("{in_dir}/Escolaridad_edad25mas.csv")) ## Datos previamente filtrados mayores de 25 anos

# Explorar dataset
head(escolaridad_csv)

# Modificar: variables a minuscula, geocodigo a caracteres 
escolaridad <- escolaridad_csv %>% 
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(redcode)
  ) %>% 
  select(-redcode)

## Los datos estan en formato ancho (wide), cada ano de escolaridad es una variable y cada dato el numero de personas
## Necesitamos transformarlo a formato largo (long) para calcular el promedio de anos de escolaridad por ZC

# Transformar a formato largo
escolaridad_long <- escolaridad %>%  
  gather(escolaridad_0:escolaridad_21,
    key="a_escolaridad", value = "n_personas"
  ) %>%
  # Extraer el numero de anos del nombre de la variable
  mutate(
    a_escolaridad = str_replace(a_escolaridad, "escolaridad_", ""),
    a_escolaridad = as.numeric(a_escolaridad)
  ) %>% 
  # Ordenarlo por geocodigo
  arrange(geocode)

# Revisar datos
head(escolaridad_long)
str(escolaridad_long)

#Calcular promedio anos escolaridad por ZC
escolaridad_media <- escolaridad_long %>% 
  group_by(geocode) %>% 
  summarise(
    total_pers = sum(n_personas),
    a_esc_media = sum(n_personas*a_escolaridad)/total_pers
  ) %>% 
  # Excluir NAs
  filter(!is.na(a_esc_media))

# Guardar datos en csv
escolaridad_media %>% write.csv2(glue("{out_dir}/escolaridad_media_zc.csv"), row.names=FALSE)

# Visualizar: Introduccion a ggplot2 --------------------------------------

# Tres partes de ggplot2: data, aesthetics, geometry
## Data - base de datos
## Aesthetics: Lo que quieres graficar (variables)
## Geometry: Como lo quieres graficar (grafico de barras, linea, mapa, etc)

# Plotear histograma
hist1 <- escolaridad_media %>% 
  ggplot(aes(a_esc_media)) + 
  geom_histogram()
hist1

# Agregar eqtiquetas y modificar colores y tamaños - mismo grafico
hist2 <- escolaridad_media %>% 
  ggplot(aes(a_esc_media)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.05)
hist2 + labs(x = "Años de Escolaridad Media", y = "Frecuencia",
              title ="Histograma Escolaridad Promedio",
              subtitle = "por Zona Censal en el Gran Santiago",
              caption = NULL)
  