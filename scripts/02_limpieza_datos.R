# Script: 02_limpieza_datos.R
# Limpieza y transformación de datos del mercado inmobiliario CABA
library(tidyverse)
library(janitor)
library(lubridate)

# Función auxiliar para convertir texto a numérico (maneja comas y puntos)
convertir_a_numerico <- function(x) {
  x %>%
    str_replace_all("\\.", "") %>%  # Eliminar puntos de miles
    str_replace(",", ".") %>%       # Convertir coma decimal a punto
    str_extract("[0-9,\\.]+") %>%   # Extraer solo números y separadores
    as.numeric()
}

# ==============================================================================
# 1. LIMPIEZA TABLA ACTIVIDAD (Declaraciones Juradas)
# ==============================================================================

actividad_clean <- actividad %>%
  # Convertir columnas monetarias de character a numeric
  mutate(
    across(starts_with("total_"), ~ convertir_a_numerico(.x)),
    cant_hijos = as.numeric(cant_hijos),
    # Limpiar y estandarizar variables categóricas
    jurisdiccion = str_trim(str_to_upper(jurisdiccion)),
    cargo = str_trim(str_to_title(cargo)),
    autoridad_superior = case_when(
      str_detect(str_to_upper(autoridad_superior), "SI|SÍ") ~ "SI",
      str_detect(str_to_upper(autoridad_superior), "NO") ~ "NO",
      TRUE ~ NA_character_
    ),
    # Crear variable de patrimonio total
    patrimonio_total = total_b_inmuebles + total_b_muebles + total_acciones + 
      total_bonos + total_depositos + total_dinero_efectivo + 
      total_dinero_electronico + total_fondos + total_titulos,
    # Crear categorías de patrimonio
    categoria_patrimonio = case_when(
      patrimonio_total <= 1000000 ~ "Bajo",
      patrimonio_total <= 10000000 ~ "Medio",
      patrimonio_total <= 50000000 ~ "Alto",
      patrimonio_total > 50000000 ~ "Muy Alto",
      TRUE ~ "Sin datos"
    ),
    # Extraer año y mes de presentación
    año_presentacion = year(fechapresentacion),
    mes_presentacion = month(fechapresentacion)
  ) %>%
  # Filtrar registros con datos válidos
  filter(!is.na(cuil), !is.na(fechapresentacion))

# Resumen de limpieza
cat("ACTIVIDAD - Resumen de limpieza:\n")
cat("Registros originales:", nrow(actividad), "\n")
cat("Registros limpios:", nrow(actividad_clean), "\n")
cat("Registros con patrimonio calculable:", sum(!is.na(actividad_clean$patrimonio_total)), "\n")

# ==============================================================================
# 2. LIMPIEZA TABLA PRÉSTAMOS UVA
# ==============================================================================

prestamos_uva_clean <- prestamos_uva %>%
  # Seleccionar solo columnas relevantes (eliminar x5, x6)
  select(ano_mes, monto_operado_prestamos_de_uva_en_miles_de_pesos, 
         var_mensual_percent, var_anual_percent) %>%
  # Crear fecha apropiada
  mutate(
    fecha = ym(ano_mes),
    año = year(fecha),
    mes = month(fecha),
    # Limpiar variables de variación
    var_mensual = as.numeric(str_replace_all(var_mensual_percent, "[^0-9.-]", "")),
    var_anual = as.numeric(str_replace_all(var_anual_percent, "[^0-9.-]", "")),
    # Renombrar columna principal
    monto_uva = monto_operado_prestamos_de_uva_en_miles_de_pesos
  ) %>%
  # Seleccionar columnas finales
  select(fecha, año, mes, monto_uva, var_mensual, var_anual) %>%
  # Ordenar por fecha
  arrange(fecha) %>%
  # Filtrar datos válidos
  filter(!is.na(fecha), !is.na(monto_uva))

cat("\nPRÉSTAMOS UVA - Resumen de limpieza:\n")
cat("Registros originales:", nrow(prestamos_uva), "\n")
cat("Registros limpios:", nrow(prestamos_uva_clean), "\n")
cat("Período:", min(prestamos_uva_clean$fecha), "a", max(prestamos_uva_clean$fecha), "\n")

# ==============================================================================
# 3. LIMPIEZA TABLA PRECIOS ALQUILER
# ==============================================================================

precio_alquiler_clean <- precio_alquiler %>%
  # Filtrar solo registros con precio válido
  filter(!is.na(precio_prom)) %>%
  mutate(
    # Estandarizar nombres de barrios
    barrio = str_trim(str_to_title(barrio)),
    # Crear fecha
    fecha = make_date(anio, match(str_to_lower(mes), tolower(month.name))),
    # Limpiar categoría de ambientes
    ambientes_cat = case_when(
      str_detect(str_to_lower(ambientes), "1|2|dos|uno") ~ "1-2 ambientes",
      str_detect(str_to_lower(ambientes), "3|4|tres|cuatro|más|mas") ~ "3+ ambientes",
      TRUE ~ ambientes
    ),
    # Crear variables adicionales
    trimestre = quarter(fecha),
    precio_m2_estimado = precio_prom / case_when(
      ambientes_cat == "1-2 ambientes" ~ 45,  # Estimación superficie promedio
      ambientes_cat == "3+ ambientes" ~ 75,
      TRUE ~ 60
    )
  ) %>%
  # Filtrar outliers extremos (precios irreales)
  filter(precio_prom >= 1000, precio_prom <= 100000) %>%
  select(barrio, comuna, fecha, anio, mes, trimestre, ambientes_cat, 
         precio_prom, precio_m2_estimado)

cat("\nPRECIOS ALQUILER - Resumen de limpieza:\n")
cat("Registros originales:", nrow(precio_alquiler), "\n")
cat("Registros con precio válido:", nrow(precio_alquiler_clean), "\n")
cat("Tasa de completitud:", round(nrow(precio_alquiler_clean)/nrow(precio_alquiler)*100, 1), "%\n")

# ==============================================================================
# 4. LIMPIEZA TABLA PRECIOS VENTA
# ==============================================================================

precio_venta_clean <- precio_venta %>%
  # Filtrar solo registros con precio válido
  filter(!is.na(precio_prom)) %>%
  mutate(
    # Estandarizar nombres
    barrio = str_trim(str_to_title(barrio)),
    # Crear fecha (primer mes del trimestre)
    fecha = make_date(ano, (trimestre - 1) * 3 + 1, 1),
    # Limpiar categorías
    ambientes_cat = case_when(
      str_detect(str_to_lower(ambientes), "1|2|dos|uno") ~ "1-2 ambientes",
      str_detect(str_to_lower(ambientes), "3|4|tres|cuatro|más|mas") ~ "3+ ambientes",
      TRUE ~ ambientes
    ),
    estado_cat = case_when(
      str_detect(str_to_lower(estado), "usado|usada") ~ "Usado",
      str_detect(str_to_lower(estado), "estrenar|nuevo|nueva") ~ "A estrenar",
      TRUE ~ str_to_title(estado)
    ),
    # Precio por m2 estimado
    precio_m2_estimado = precio_prom * 1000 / case_when(
      ambientes_cat == "1-2 ambientes" ~ 45,
      ambientes_cat == "3+ ambientes" ~ 75,
      TRUE ~ 60
    )
  ) %>%
  # Filtrar outliers
  filter(precio_prom >= 500, precio_prom <= 10000) %>%
  select(barrio, comuna, fecha, ano, trimestre, ambientes_cat, estado_cat,
         precio_prom, precio_m2_estimado)

cat("\nPRECIOS VENTA - Resumen de limpieza:\n")
cat("Registros originales:", nrow(precio_venta), "\n")
cat("Registros con precio válido:", nrow(precio_venta_clean), "\n")
cat("Tasa de completitud:", round(nrow(precio_venta_clean)/nrow(precio_venta)*100, 1), "%\n")

# ==============================================================================
# 5. LIMPIEZA TABLA SUPERFICIE
# ==============================================================================

superficie_clean <- superficie %>%
  mutate(
    # Estandarizar nombres
    barrio = str_trim(str_to_title(barrio)),
    # Crear fecha
    fecha = make_date(ano, match(str_to_lower(mes), tolower(month.name))),
    trimestre = quarter(fecha),
    # Filtrar superficies muy pequeñas (probables errores)
    superficie_valida = ifelse(superficie < 10, NA, superficie)
  ) %>%
  # Filtrar registros válidos
  filter(!is.na(fecha), !is.na(superficie_valida)) %>%
  select(barrio, comuna, fecha, ano, mes, trimestre, superficie = superficie_valida)

cat("\nSUPERFICIE - Resumen de limpieza:\n")
cat("Registros originales:", nrow(superficie), "\n")
cat("Registros limpios:", nrow(superficie_clean), "\n")

# ==============================================================================
# 6. CREAR TABLA RESUMEN DE LIMPIEZA
# ==============================================================================

resumen_limpieza <- tibble(
  tabla = c("Actividad", "Préstamos UVA", "Precios Alquiler", "Precios Venta", "Superficie"),
  registros_originales = c(nrow(actividad), nrow(prestamos_uva), nrow(precio_alquiler), 
                           nrow(precio_venta), nrow(superficie)),
  registros_limpios = c(nrow(actividad_clean), nrow(prestamos_uva_clean), 
                        nrow(precio_alquiler_clean), nrow(precio_venta_clean), 
                        nrow(superficie_clean)),
  tasa_retencion = round(registros_limpios / registros_originales * 100, 1),
  periodo_inicio = c(min(actividad_clean$fechapresentacion), 
                     min(prestamos_uva_clean$fecha),
                     min(precio_alquiler_clean$fecha, na.rm = TRUE),
                     min(precio_venta_clean$fecha, na.rm = TRUE),
                     min(superficie_clean$fecha, na.rm = TRUE)),
  periodo_fin = c(max(actividad_clean$fechapresentacion),
                  max(prestamos_uva_clean$fecha),
                  max(precio_alquiler_clean$fecha, na.rm = TRUE),
                  max(precio_venta_clean$fecha, na.rm = TRUE),
                  max(superficie_clean$fecha, na.rm = TRUE))
)

print(resumen_limpieza)

# ==============================================================================
# 7. GUARDAR DATOS LIMPIOS
# ==============================================================================

# Definir ruta de salida
output_path <- "C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario/input/"

# Crear directorio si no existe
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Guardar datos limpios
write_csv(actividad_clean, paste0(output_path, "actividad_clean.csv"))
write_csv(prestamos_uva_clean, paste0(output_path, "prestamos_uva_clean.csv"))
write_csv(precio_alquiler_clean, paste0(output_path, "precio_alquiler_clean.csv"))
write_csv(precio_venta_clean, paste0(output_path, "precio_venta_clean.csv"))
write_csv(superficie_clean, paste0(output_path, "superficie_clean.csv"))

cat("\n=== DATOS GUARDADOS ===\n")
cat("Archivos guardados en:", output_path, "\n")
cat("- actividad_clean.csv\n")
cat("- prestamos_uva_clean.csv\n")
cat("- precio_alquiler_clean.csv\n")
cat("- precio_venta_clean.csv\n")
cat("- superficie_clean.csv\n")

