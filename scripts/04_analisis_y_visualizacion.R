# =============================================================================
# SCRIPT 03: ANÁLISIS Y VISUALIZACIÓN DEL MERCADO INMOBILIARIO
#
# OBJETIVO:
# Este script realiza un análisis completo del mercado inmobiliario de CABA
# entre 2010-2020, con foco en el impacto de los créditos UVA. Incluye:
# 1. Preparación y enriquecimiento de los datos.
# 2. Cálculo de estadísticas descriptivas clave para el análisis.
# 3. Generación de visualizaciones para el storytelling del informe final.
#
# AUTOR: Patricio Cross
# FECHA: 03/07/2025
# =============================================================================


# =============================================================================
# 00. CONFIGURACIÓN Y CARGA DE LIBRERÍAS
# =============================================================================
library(tidyverse)
library(lubridate)
library(scales)
library(zoo) # Para la función rollmean()

# --- Configuración de Salida ---
output_path <- "C:/Users/patri/OneDrive - Económicas - UBA/Escritorio/facu/Ciencia de datos/TP/Mercado-Inmobiliario-BA/output/"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# --- Función Auxiliar para Guardar Gráficos ---
save_plot <- function(plot, filename, width = 12, height = 8) {
  ggsave(
    filename = file.path(output_path, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# --- Tema Gráfico Personalizado ---
theme_custom <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1, face = "italic"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "white", color = NA)
  )


# =============================================================================
# 01. CARGA Y PREPARACIÓN DE DATOS
# =============================================================================
# --- Carga de Datasets Limpios ---
# Se asume que los scripts 01 y 02 ya fueron ejecutados.
precio_venta_clean <- read_csv("input/precio_venta_clean.csv")
precio_alquiler_clean <- read_csv("input/precio_alquiler_clean.csv")
prestamos_uva_clean <- read_csv("input/prestamos_uva_clean.csv")
tipo_de_cambio <- read_csv("input/tipo_cambio_historico.csv")

# --- Definición de Variables Clave ---
comunas_premium <- c(1, 2, 13, 14)
comunas_emergentes <- c(3, 5, 6, 7, 11, 12, 15)
comunas_perifericas <- c(4, 8, 9, 10)
fecha_lanzamiento_uva <- as.Date("2016-04-01")
fecha_crisis_cambiaria <- as.Date("2018-04-01")

# --- Agrupación y Agregación de Datos ---
venta_agrupada <- precio_venta_clean %>%
  mutate(
    grupo_comuna = case_when(
      comuna %in% comunas_premium    ~ "Premium",
      comuna %in% comunas_emergentes ~ "Emergentes",
      comuna %in% comunas_perifericas  ~ "Periféricas",
      TRUE                           ~ "Otro"
    )
  ) %>%
  filter(grupo_comuna != "Otro") %>%
  group_by(fecha, grupo_comuna) %>%
  summarise(precio_m2_promedio = mean(precio_m2_estimado, na.rm = TRUE), .groups = 'drop')

venta_total_ciudad <- precio_venta_clean %>%
  group_by(fecha) %>%
  summarise(precio_m2_promedio = mean(precio_m2_estimado, na.rm = TRUE), .groups = 'drop')

alquiler_total_ciudad <- precio_alquiler_clean %>%
  group_by(fecha) %>%
  summarise(precio_m2_promedio = mean(precio_m2_estimado, na.rm = TRUE), .groups = 'drop')

# --- Dolarización de Precios de Alquiler ---
tipo_de_cambio_clean <- tipo_de_cambio %>%
  mutate(fecha = make_date(ano, mes, 1)) %>%
  select(fecha, tipo_cambio_promedio)

alquiler_dolarizado <- alquiler_total_ciudad %>%
  left_join(tipo_de_cambio_clean, by = "fecha") %>%
  fill(tipo_cambio_promedio, .direction = "downup") %>%
  mutate(precio_m2_usd = precio_m2_promedio / tipo_cambio_promedio) %>%
  filter(!is.na(precio_m2_usd) & is.finite(precio_m2_usd))


# =============================================================================
# 02. ANÁLISIS ESTADÍSTICO DESCRIPTIVO
# =============================================================================
cat("============================================================\n")
cat("ANÁLISIS ESTADÍSTICO DEL MERCADO INMOBILIARIO\n")
cat("============================================================\n\n")

# --- 2.1. Correlación Crédito vs. Precio Venta (Boom 2016-2018) ---
cor_data <- prestamos_uva_clean %>%
  filter(fecha >= "2016-01-01" & fecha <= "2018-03-01") %>%
  left_join(venta_agrupada %>% filter(grupo_comuna == "Premium"), by = "fecha") %>%
  select(monto_uva, precio_m2_promedio) %>%
  na.omit()

cor_credito_venta <- cor(cor_data$monto_uva, cor_data$precio_m2_promedio)
cat("1. CORRELACIÓN CRÉDITO VS. PRECIOS (2016-2018)\n")
cat("   - Correlación (Crédito UVA vs. Precio Venta Premium):", round(cor_credito_venta, 3), "\n\n")

# --- 2.2. Brecha de Precios: Premium vs. Periféricas ---
brecha_data <- venta_agrupada %>%
  pivot_wider(names_from = grupo_comuna, values_from = precio_m2_promedio) %>%
  na.omit()

brecha_2015 <- brecha_data %>%
  filter(year(fecha) == 2015) %>%
  summarise(brecha_media = mean((Premium / Periféricas) - 1, na.rm = TRUE) * 100) %>%
  pull(brecha_media)

brecha_2018 <- brecha_data %>%
  filter(year(fecha) == 2018) %>%
  summarise(brecha_media = mean((Premium / Periféricas) - 1, na.rm = TRUE) * 100) %>%
  pull(brecha_media)

cat("2. AMPLIACIÓN DE LA BRECHA DE PRECIOS\n")
cat("   - Brecha en 2015 (Pre-UVA):", round(brecha_2015, 1), "%\n")
cat("   - Brecha en 2018 (Pico del Boom):", round(brecha_2018, 1), "%\n")
cat("   - Aumento de la brecha:", round(brecha_2018 - brecha_2015, 1), "puntos porcentuales\n\n")

# --- 2.3. Divergencia Post-Crisis ---
pico_credito <- prestamos_uva_clean %>% filter(year(fecha) == 2018, quarter(fecha) == 1) %>% summarise(pico = mean(monto_uva)) %>% pull(pico)
valle_credito <- prestamos_uva_clean %>% filter(year(fecha) == 2020, quarter(fecha) == 2) %>% summarise(valle = mean(monto_uva)) %>% pull(valle)
caida_credito <- (1 - (valle_credito / pico_credito)) * 100

precio_venta_2018 <- venta_agrupada %>% filter(grupo_comuna == "Premium", year(fecha) == 2018, quarter(fecha) == 1) %>% summarise(p = mean(precio_m2_promedio)) %>% pull(p)
precio_venta_2020 <- venta_agrupada %>% filter(grupo_comuna == "Premium", year(fecha) == 2020, quarter(fecha) == 4) %>% summarise(p = mean(precio_m2_promedio)) %>% pull(p)
caida_venta <- (1 - (precio_venta_2020 / precio_venta_2018)) * 100

precio_alquiler_2019 <- alquiler_dolarizado %>% filter(year(fecha) == 2019, quarter(fecha) == 1) %>% summarise(p = mean(precio_m2_usd)) %>% pull(p)
precio_alquiler_2020 <- alquiler_dolarizado %>% filter(year(fecha) == 2020, quarter(fecha) == 4) %>% summarise(p = mean(precio_m2_usd)) %>% pull(p)
suba_alquiler <- ((precio_alquiler_2020 / precio_alquiler_2019) - 1) * 100

cat("3. DIVERGENCIA POST-CRISIS (BOCA DE COCODRILO)\n")
cat("   - Caída del Crédito (Pico 2018 vs. Valle 2020):", round(caida_credito, 1), "%\n")
cat("   - Variación Precio Venta USD (T1 2018 vs T4 2020):", -round(caida_venta, 1), "%\n")
cat("   - Variación Precio Alquiler USD (T1 2019 vs T4 2020):", round(suba_alquiler, 1), "%\n\n")

# --- 2.4. Volatilidad por Tipo de Propiedad ---
volatilidad_data <- precio_venta_clean %>%
  filter(ambientes_cat %in% c("1-2 ambientes", "3+ ambientes")) %>%
  group_by(ambientes_cat) %>%
  summarise(sd_precio = sd(precio_m2_estimado, na.rm = TRUE))

sd_venta_1_2 <- volatilidad_data %>% filter(ambientes_cat == "1-2 ambientes") %>% pull(sd_precio)
sd_venta_3_mas <- volatilidad_data %>% filter(ambientes_cat == "3+ ambientes") %>% pull(sd_precio)

cat("4. VOLATILIDAD POR TIPO DE PROPIEDAD (2016-2020)\n")
cat("   - Desviación Estándar (1-2 Ambientes):", round(sd_venta_1_2, 2), "\n")
cat("   - Desviación Estándar (3+ Ambientes):", round(sd_venta_3_mas, 2), "\n\n")
cat("============================================================\n\n")


# =============================================================================
# 03. VISUALIZACIONES PARA STORYTELLING
# =============================================================================

# --- Gráfico 1: El crédito como motor de la brecha de precios ---
creditos_trimestral <- prestamos_uva_clean %>%
  mutate(trimestre = quarter(fecha, with_year = TRUE)) %>%
  group_by(trimestre) %>%
  summarise(monto_total_creditos = sum(monto_uva, na.rm = TRUE), fecha = min(fecha), .groups = 'drop')

escala <- max(venta_agrupada$precio_m2_promedio, na.rm = TRUE) / max(creditos_trimestral$monto_total_creditos, na.rm = TRUE)

h1_g1 <- ggplot() +
  geom_col(data = creditos_trimestral, aes(x = fecha, y = monto_total_creditos * escala, fill = "Volumen Crédito UVA"), alpha = 0.5) +
  geom_line(data = venta_agrupada, aes(x = fecha, y = precio_m2_promedio, color = grupo_comuna), linewidth = 1.2) +
  annotate("vline", xintercept = fecha_lanzamiento_uva, linetype = "dashed", color = "black", alpha = 0.8) +
  annotate("text", x = fecha_lanzamiento_uva, y = Inf, label = "Lanzamiento UVA", vjust = 1.5, hjust = -0.1, angle = 90, size = 3) +
  annotate("vline", xintercept = fecha_crisis_cambiaria, linetype = "dashed", color = "red", alpha = 0.8) +
  annotate("text", x = fecha_crisis_cambiaria, y = Inf, label = "Crisis Cambiaria", vjust = 1.5, hjust = -0.1, angle = 90, size = 3, color = "red") +
  scale_y_continuous(name = "Precio de Venta (USD/m²)", labels = dollar_format(prefix = "U$D "), sec.axis = sec_axis(~ . / escala, name = "Créditos UVA (Millones de ARS)", labels = scales::comma_format(big.mark="."))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Premium" = "#D4AF37", "Emergentes" = "#4682B4", "Periféricas" = "#A9A9A9")) +
  scale_fill_manual(values = c("Volumen Crédito UVA" = "#6495ED")) +
  labs(title = "Historia 1: El Crédito UVA Disparó los Precios en Zonas Premium", subtitle = "El boom crediticio (2016-18) se correlacionó con un alza de precios que amplió la brecha", x = "Año", y = "", color = "Grupo de Comunas", fill = "", caption = "Fuentes: GCBA, BCRA.") +
  theme_custom
print(h1_g1)
save_plot(h1_g1, "Historia_Credito_vs_Venta.png")


# --- Gráfico 2: Mapa de Calor de Brechas de Precios ---
brecha_precios <- precio_venta_clean %>%
  group_by(fecha) %>%
  mutate(precio_promedio_ciudad = mean(precio_m2_estimado, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(brecha_pct = (precio_m2_estimado / precio_promedio_ciudad - 1) * 100, comuna_factor = factor(comuna, levels = 1:15)) %>%
  group_by(fecha, comuna_factor) %>%
  summarise(brecha_media = mean(brecha_pct, na.rm = TRUE), .groups = 'drop')

h1_g2 <- ggplot(brecha_precios, aes(x = fecha, y = comuna_factor, fill = brecha_media)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(name = "Brecha vs. Media CABA (%)", low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, labels = function(x) paste0(x, "%")) +
  labs(title = "La Brecha de Precios se Intensificó con el Boom Crediticio", subtitle = "Diferencia porcentual del precio del m² por comuna contra el promedio de la ciudad", x = "Año", y = "Comuna", caption = "Fuente: GCBA.") +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y") +
  theme_custom +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12), legend.key.height = unit(0.8, "cm"), legend.key.width = unit(3, "cm"))
print(h1_g2)
save_plot(h1_g2, "Historia_Heatmap_Brechas.png")


# --- Gráfico 3: Dinámica del Mercado y Crédito (Dolarizado) ---
# Cálculo de bases
venta_base <- venta_total_ciudad %>% filter(year(fecha) == 2018, quarter(fecha) == 1) %>% summarise(b = mean(precio_m2_promedio, na.rm = TRUE)) %>% pull(b)
credito_base <- prestamos_uva_clean %>% filter(year(fecha) == 2018, quarter(fecha) == 1) %>% summarise(b = mean(monto_uva, na.rm = TRUE)) %>% pull(b)
alquiler_base_usd <- alquiler_dolarizado %>% filter(year(fecha) == 2019, quarter(fecha) == 1) %>% summarise(b = mean(precio_m2_usd, na.rm = TRUE)) %>% pull(b)

# Creación de índices
venta_idx <- venta_agrupada %>% filter(grupo_comuna == "Premium") %>% mutate(indice = (precio_m2_promedio / venta_base) * 100, variable = "Precio Venta USD (Premium)")
alquiler_idx <- alquiler_dolarizado %>% mutate(indice = (precio_m2_usd / alquiler_base_usd) * 100, variable = "Precio Alquiler USD (CABA)")
credito_idx <- prestamos_uva_clean %>% mutate(monto_suavizado = rollmean(monto_uva, 3, fill = NA, align = "right")) %>% filter(!is.na(monto_suavizado)) %>% mutate(indice = (monto_suavizado / credito_base) * 100, variable = "Créditos UVA (ARS - media móvil 3m)")

indices_combinados <- bind_rows(
  venta_idx %>% select(fecha, indice, variable),
  alquiler_idx %>% select(fecha, indice, variable),
  credito_idx %>% select(fecha, indice, variable)
) %>%
  filter(!is.na(indice), !is.infinite(indice))

h3_g1 <- ggplot(indices_combinados, aes(x = fecha, y = indice, color = variable)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "black") +
  geom_vline(xintercept = fecha_crisis_cambiaria, linetype = "dashed", color = "red", alpha = 0.8) +
  annotate("text", x = fecha_crisis_cambiaria, y = Inf, label = "Crisis Cambiaria", vjust = 1.8, hjust = 1.05, angle = 0, size = 3.5, color = "red") +
  scale_color_manual(values = c("Precio Venta USD (Premium)" = "#B85B5B", "Precio Alquiler USD (CABA)" = "#5B8FB8", "Créditos UVA (ARS - media móvil 3m)" = "#FF8C00")) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(title = "Historia 3: Dinámica del Mercado Inmobiliario y el Crédito", subtitle = "Evolución relativa de precios (en USD) y crédito (en ARS)", x = "Año", y = "Índice (Base 100)", color = "Variable", caption = "Fuentes: GCBA, BCRA. Nota: Precios de venta y alquiler en USD. El índice de crédito se basa en valores en ARS.") +
  theme_custom
print(h3_g1)
save_plot(h3_g1, "Historia_Indices_Dolarizado.png")


# --- Gráfico 4: Impacto diferencial por tipo de propiedad ---
venta_ambientes <- precio_venta_clean %>%
  group_by(fecha, ambientes_cat) %>%
  summarise(precio_m2_promedio = mean(precio_m2_estimado, na.rm = TRUE), .groups = 'drop')

h4_g1 <- ggplot(venta_ambientes, aes(x = fecha, y = precio_m2_promedio)) +
  geom_line(aes(color = ambientes_cat), linewidth = 1.2) +
  geom_area(data = creditos_trimestral, aes(x = fecha, y = monto_total_creditos * (max(venta_ambientes$precio_m2_promedio, na.rm=T) / max(creditos_trimestral$monto_total_creditos, na.rm=T))), fill = "#6495ED", alpha = 0.3) +
  facet_wrap(~ambientes_cat, scales = "free_y") +
  scale_y_continuous(labels = dollar_format(prefix = "U$D ")) +
  labs(title = "Historia 4: Impacto Diferencial por Tipo de Propiedad", subtitle = "La evolución de precios muestra mayor volatilidad en las unidades más demandadas por el crédito", x = "Año", y = "Precio de Venta (USD/m²)", caption = "Área azul de fondo representa el volumen de crédito UVA (escalado).") +
  theme_custom +
  theme(legend.position = "none")
print(h4_g1)
save_plot(h4_g1, "Historia_Small_Multiples_Ambientes.png")

cat("\n--- Proceso de análisis y visualización completado. Archivos guardados en /output ---\n")

