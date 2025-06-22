# Script: 03_eda_avanzado.R
# Exploración de datos avanzada - Mercado Inmobiliario CABA

library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

# ==============================================================================
# 1. Cargar datos limpios
# ==============================================================================

input_path <- "C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario/input/"
output_path <- "C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario/output/"

actividad_clean <- read_csv(paste0(input_path, "actividad_clean.csv"))
prestamos_uva_clean <- read_csv(paste0(input_path, "prestamos_uva_clean.csv"))
precio_alquiler_clean <- read_csv(paste0(input_path, "precio_alquiler_clean.csv"))
precio_venta_clean <- read_csv(paste0(input_path, "precio_venta_clean.csv"))
superficie_clean <- read_csv(paste0(input_path, "superficie_clean.csv"))

# Crear carpeta output si no existe
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# ==============================================================================
# 2. Evolución del precio promedio por m2 - Venta y Alquiler
# ==============================================================================

venta_trend <- precio_venta_clean %>%
  group_by(fecha) %>%
  summarise(precio_m2 = mean(precio_m2_estimado, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = precio_m2)) +
  geom_line(color = "#1f77b4", size = 1) +
  labs(title = "Evolución Precio m2 - Venta", y = "USD/m2", x = "Fecha") +
  theme_minimal()

alquiler_trend <- precio_alquiler_clean %>%
  group_by(fecha) %>%
  summarise(precio_m2 = mean(precio_m2_estimado, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = precio_m2)) +
  geom_line(color = "#2ca02c", size = 1) +
  labs(title = "Evolución Precio m2 - Alquiler", y = "ARS/m2", x = "Fecha") +
  theme_minimal()

trend_plot <- venta_trend / alquiler_trend

ggsave(paste0(output_path, "evolucion_precios_m2.png"), trend_plot, width = 10, height = 8)

# ==============================================================================
# 3. Precio de venta por ambientes y comuna (Boxplot)
# ==============================================================================

# Solo incluir comunas con más de 200 registros para claridad visual
comunas_frecuentes <- precio_venta_clean %>% count(comuna) %>% filter(n > 200) %>% pull(comuna)

box_venta <- precio_venta_clean %>%
  filter(comuna %in% comunas_frecuentes, !is.na(ambientes_cat)) %>%
  ggplot(aes(x = ambientes_cat, y = precio_prom, fill = ambientes_cat)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  facet_wrap(~ comuna, ncol = 3) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values = c("#ffbb78", "#98df8a")) +
  labs(title = "Precio de Venta por Ambientes en Comunas con Mayor Muestra", x = "Ambientes", y = "Precio (USD)") +
  theme_minimal()

ggsave(paste0(output_path, "boxplot_venta_ambientes_comuna.png"), box_venta, width = 12, height = 8)

# ==============================================================================
# 4. Correlación superficie vs precio venta
# ==============================================================================

scatter_superficie_venta <- precio_venta_clean %>%
  inner_join(superficie_clean, by = c("barrio", "comuna", "fecha")) %>%
  filter(precio_prom < 10000, superficie < 500) %>%
  ggplot(aes(x = superficie, y = precio_prom)) +
  geom_point(alpha = 0.3, color = "#1f77b4") +
  geom_smooth(method = "lm", se = FALSE, color = "#ff7f0e", linewidth = 1) +
  scale_y_log10(labels = comma) +
  labs(title = "Relación entre Superficie y Precio de Venta", x = "Superficie (m2)", y = "Precio (USD)") +
  theme_minimal()

ggsave(paste0(output_path, "correlacion_superficie_precio.png"), scatter_superficie_venta, width = 10, height = 7)

# ==============================================================================
# 5. Patrimonio declarado por funcionarios - Distribución por jurisdicción
# ==============================================================================

barras_patrimonio <- actividad_clean %>%
  filter(!is.na(jurisdiccion)) %>%
  group_by(jurisdiccion) %>%
  summarise(patrimonio_prom = mean(patrimonio_total, na.rm = TRUE)) %>%
  arrange(desc(patrimonio_prom)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(jurisdiccion, patrimonio_prom), y = patrimonio_prom)) +
  geom_col(fill = "#17becf") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Patrimonio Promedio por Jurisdicción (Top 15)", x = "Jurisdicción", y = "Patrimonio Total (USD)") +
  theme_minimal()

ggsave(paste0(output_path, "patrimonio_funcionarios.png"), barras_patrimonio, width = 10, height = 8)

# ==============================================================================
# Fin del script
cat("EDA avanzado completado. Gráficos guardados en la carpeta output.\n")