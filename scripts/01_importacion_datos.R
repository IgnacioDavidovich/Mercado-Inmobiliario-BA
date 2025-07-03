# Script: 01_importacion_datos.R
# Revisión básica
library(tidyverse)
library(janitor)
library(skimr)

# Definir ruta base
base_path <- "C:/Users/patri/OneDrive - Económicas - UBA/Escritorio/facu/Ciencia de datos/TP/Mercado-Inmobiliario-BA/raw/"

# Archivos a importar
archivo_actividad <- paste0(base_path, "actas-notariales-compra-venta-inmuebles-hipotecas.csv")
archivo_prestamos_uva <- paste0(base_path, "montos-creditos-hipotecarios-uva.csv")
archivo_precio_alquiler <- paste0(base_path, "precio-alquiler-deptos.csv")
archivo_precio_venta <- paste0(base_path, "precio-venta-deptos.csv")
archivo_superficie <- paste0(base_path, "superficie-deptos.csv")

# Importar datos con separador punto y coma
actividad <- read_csv2(archivo_actividad) %>% clean_names()
prestamos_uva <- read_csv2(archivo_prestamos_uva) %>% clean_names()
precio_alquiler <- read_csv2(archivo_precio_alquiler) %>% clean_names()
precio_venta <- read_csv2(archivo_precio_venta) %>% clean_names()
superficie <- read_csv2(archivo_superficie) %>% clean_names()


# Mostrar resumen con skimr
skim(actividad)
skim(prestamos_uva)
skim(precio_alquiler)
skim(precio_venta)
skim(superficie)
