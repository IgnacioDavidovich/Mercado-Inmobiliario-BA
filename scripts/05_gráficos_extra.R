# =============================================================================
# SCRIPT 04: GRÁFICOS COMPLEMENTARIOS - Análisis Ciclo Créditos UVA
#
# OBJETIVO:
# Generar visualizaciones adicionales para enriquecer la presentación:
# 1. Evolución superficie promedio comprada
# 2. Dispersión precio de venta vs. alquiler por barrio
# 3. Créditos UVA vs. alquileres (boca de cocodrilo)
# 4. Proporción crédito UVA / precio venta
#
# AUTOR: Patricio Cross
# FECHA: 06/07/2025
# =============================================================================

setwd("C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario")

# =============================================================================
# 00. LIBRERÍAS Y CONFIGURACIÓN
# =============================================================================
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)

output_path <- "C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario/output/"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

save_plot <- function(plot, filename, width = 12, height = 8) {
  ggsave(
    filename = file.path(output_path, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "transparent"
  )
}

theme_custom <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1, face = "italic"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# =============================================================================
# 01. CARGA DE DATOS
# =============================================================================
precio_venta_clean     <- read_csv("input/precio_venta_clean.csv")
precio_alquiler_clean  <- read_csv("input/precio_alquiler_clean.csv")
prestamos_uva_clean    <- read_csv("input/prestamos_uva_clean.csv")
superficie_clean       <- read_csv("input/superficie_clean.csv")
tipo_de_cambio         <- read_csv("input/tipo_cambio_historico.csv")

fecha_lanzamiento_uva    <- as.Date("2016-04-01")
fecha_crisis_cambiaria   <- as.Date("2018-04-01")

# Dolarizar alquileres
tipo_de_cambio_clean <- tipo_de_cambio %>%
  mutate(fecha = make_date(ano, mes, 1)) %>%
  select(fecha, tipo_cambio_promedio)

alquiler_dolarizado <- precio_alquiler_clean %>%
  group_by(fecha) %>%
  summarise(precio_m2_promedio = mean(precio_m2_estimado, na.rm = TRUE), .groups = 'drop') %>%
  left_join(tipo_de_cambio_clean, by = "fecha") %>%
  fill(tipo_cambio_promedio, .direction = "downup") %>%
  mutate(precio_m2_usd = precio_m2_promedio / tipo_cambio_promedio) %>%
  filter(!is.na(precio_m2_usd) & is.finite(precio_m2_usd))

# =============================================================================
# 02. GRÁFICO 1: Evolución Superficie Promedio Comprada
# =============================================================================
superficie_prom <- superficie_clean %>%
  group_by(fecha) %>%
  summarise(superficie_promedio = mean(superficie, na.rm = TRUE))

g1 <- ggplot(superficie_prom, aes(x = fecha, y = superficie_promedio)) +
  geom_line(color = "#4682B4", linewidth = 1.2) +
  geom_vline(xintercept = fecha_lanzamiento_uva, linetype = "dashed", color = "black") +
  geom_vline(xintercept = fecha_crisis_cambiaria, linetype = "dashed", color = "red") +
  labs(
    title = "Superficie Promedio Comprada en CABA",
    subtitle = "Evolución mensual (2016–2020)",
    x = "Fecha", y = "Superficie promedio (m²)",
    caption = "Fuente: GCBA"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_custom
save_plot(g1, "Evolucion_Superficie_Promedio.png")

# =============================================================================
# 03. GRÁFICO 2: Dispersión Precio Venta vs. Alquiler por Barrio
# =============================================================================
venta_var <- precio_venta_clean %>%
  group_by(barrio) %>%
  summarise(var_venta = (last(precio_m2_estimado) / first(precio_m2_estimado) - 1) * 100)

alquiler_var <- precio_alquiler_clean %>%
  group_by(barrio) %>%
  summarise(var_alquiler = (last(precio_m2_estimado) / first(precio_m2_estimado) - 1) * 100)

comparacion_var <- left_join(venta_var, alquiler_var, by = "barrio") %>%
  drop_na()

g2 <- ggplot(comparacion_var, aes(x = var_venta, y = var_alquiler)) +
  geom_point(color = "#5B8FB8", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linetype = "dashed") +
  geom_text(aes(label = barrio), size = 3, vjust = -0.5, hjust = 0.5) +
  labs(
    title = "Relación entre Variación de Precios de Venta y Alquiler",
    subtitle = "Variación acumulada por barrio (2016–2020)",
    x = "Variación precio de venta (%)", y = "Variación precio de alquiler (%)",
    caption = "Fuente: GCBA"
  ) +
  theme_custom
save_plot(g2, "Dispersion_Venta_Alquiler.png")

# =============================================================================
# 04. GRÁFICO 3: Créditos UVA vs. Alquileres ("Boca de Cocodrilo")
# =============================================================================
creditos_mensual <- prestamos_uva_clean %>%
  mutate(monto_suavizado = rollmean(monto_uva, 3, fill = NA, align = "right")) %>%
  select(fecha, monto_suavizado) %>%
  rename(valor = monto_suavizado) %>%
  mutate(variable = "Créditos UVA")

alquiler_prom <- alquiler_dolarizado %>%
  select(fecha, precio_m2_usd) %>%
  rename(valor = precio_m2_usd) %>%
  mutate(variable = "Alquiler USD/m²")

boca_data <- bind_rows(creditos_mensual, alquiler_prom) %>%
  filter(!is.na(valor))

g3 <- ggplot(boca_data, aes(x = fecha, y = valor, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Créditos UVA" = "#FF8C00", "Alquiler USD/m²" = "#5B8FB8")) +
  geom_vline(xintercept = fecha_crisis_cambiaria, linetype = "dashed", color = "red") +
  labs(
    title = "Divergencia entre Créditos UVA y Precios de Alquiler",
    subtitle = "La 'boca de cocodrilo' post crisis cambiaria (2018)",
    x = "Fecha", y = "Valor",
    color = "Variable",
    caption = "Fuente: BCRA, GCBA"
  ) +
  theme_custom
save_plot(g3, "Boca_Cocodrilo_Credito_Alquiler.png")

# =============================================================================
# 05. GRÁFICO 4: Proporción Crédito UVA / Precio Promedio de Venta
# =============================================================================
creditos_trimestral_prom <- prestamos_uva_clean %>%
  mutate(trimestre = paste0(year(fecha), "T", quarter(fecha))) %>%
  group_by(trimestre) %>%
  summarise(monto_prom = mean(monto_uva, na.rm = TRUE), .groups = 'drop')

precio_trimestral_prom <- precio_venta_clean %>%
  group_by(anio = ano, trimestre) %>%
  summarise(precio_prom = mean(precio_prom, na.rm = TRUE), .groups = 'drop') %>%
  mutate(trimestre = paste0(anio, "T", trimestre))

acceso_df <- left_join(creditos_trimestral_prom, precio_trimestral_prom, by = "trimestre") %>%
  mutate(ratio = monto_prom / precio_prom) %>%
  filter(!is.na(ratio)) %>%
  mutate(fecha = zoo::as.yearqtr(trimestre, format = "%YT%q") %>% as.Date())

g4 <- ggplot(acceso_df, aes(x = fecha, y = ratio)) +
  geom_line(color = "#B85B5B", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "¿Alcanzaba el Crédito UVA para Comprar una Propiedad?",
    subtitle = "Relación entre monto promedio del crédito y precio promedio de venta (2016–2020)",
    x = "Fecha", y = "Proporción Crédito UVA / Precio de Venta",
    caption = "Fuente: BCRA, GCBA"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_custom
save_plot(g4, "Proporcion_Credito_UVA_vs_Precio.png")

cat("✓ Gráficos complementarios generados y guardados en /output\n")
