setwd("C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario")


# 03_eda_graficos.R
# EDA - Mercado inmobiliario CABA
# Autor: Ignacio Davidovich

library(tidyverse)
library(lubridate)
library(scales)
library(janitor)

# Instalar y cargar paquetes adicionales si están disponibles
if (!require(ggthemes, quietly = TRUE)) {
  cat("Paquete ggthemes no disponible - usando tema base mejorado\n")
}
if (!require(viridis, quietly = TRUE)) {
  cat("Paquete viridis no disponible - usando colores base\n")
}

# Cargar datasets
precio_venta_clean <- read_csv("input/precio_venta_clean.csv")
precio_alquiler_clean <- read_csv("input/precio_alquiler_clean.csv")
superficie_clean <- read_csv("input/superficie_clean.csv")
prestamos_uva_clean <- read_csv("input/prestamos_uva_clean.csv")
actividad_clean <- read_csv("input/actividad_clean.csv")

# Configurar carpeta de salida - RUTA CORREGIDA PARA WINDOWS
output_path <- "C:/Users/ThinkPad/Desktop/TP Mercado Inmobiliario/output/"
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Función para guardar gráficos
save_plot <- function(plot, filename, width = 12, height = 8) {
  ggsave(
    filename = paste0(output_path, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

# Tema personalizado mejorado
tema_custom <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 20)),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ============================
# 1. Evolución del precio de venta por m²
# ============================

p1 <- ggplot(precio_venta_clean, aes(x = fecha, y = precio_m2_estimado)) +
  # Banda de confianza (percentiles 25-75)
  stat_summary(
    fun.min = function(x) quantile(x, 0.25, na.rm = TRUE),
    fun.max = function(x) quantile(x, 0.75, na.rm = TRUE),
    geom = "ribbon", 
    alpha = 0.15, 
    fill = "#2C5AA0"
  ) +
  # Líneas individuales más sutiles
  geom_line(aes(group = barrio), alpha = 0.08, color = "gray50", size = 0.2) +
  # Línea promedio más destacada
  stat_summary(fun = mean, geom = "line", color = "#2C5AA0", size = 2) +
  # Puntos en el promedio
  stat_summary(fun = mean, geom = "point", color = "#1B3A5F", size = 1.2, alpha = 0.8) +
  labs(
    title = "Evolución del Precio de Venta por m² en CABA",
    subtitle = "Promedio general con banda de variabilidad (P25-P75) • Líneas grises = barrios individuales",
    x = "Período", 
    y = "Precio por m² (ARS)",
    caption = "Fuente: Gobierno de la Ciudad de Buenos Aires"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  tema_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
save_plot(p1, "01_precio_venta_evolucion.png")

# ============================
# 2. Evolución del precio de alquiler por m²
# ============================

p2 <- ggplot(precio_alquiler_clean, aes(x = fecha, y = precio_m2_estimado)) +
  # Banda de confianza (percentiles 25-75)
  stat_summary(
    fun.min = function(x) quantile(x, 0.25, na.rm = TRUE),
    fun.max = function(x) quantile(x, 0.75, na.rm = TRUE),
    geom = "ribbon", 
    alpha = 0.15, 
    fill = "#228B22"
  ) +
  # Líneas individuales más sutiles
  geom_line(aes(group = barrio), alpha = 0.08, color = "gray50", size = 0.2) +
  # Línea promedio más destacada
  stat_summary(fun = mean, geom = "line", color = "#228B22", size = 2) +
  # Puntos en el promedio
  stat_summary(fun = mean, geom = "point", color = "#1B5F1B", size = 1.2, alpha = 0.8) +
  labs(
    title = "Evolución del Precio de Alquiler por m² en CABA",
    subtitle = "Promedio general con banda de variabilidad (P25-P75) • Líneas grises = barrios individuales",
    x = "Período", 
    y = "Precio por m² (ARS)",
    caption = "Fuente: Gobierno de la Ciudad de Buenos Aires"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  tema_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
save_plot(p2, "02_precio_alquiler_evolucion.png")

# ============================
# 3. Evolución de la superficie promedio
# ============================

p3 <- ggplot(superficie_clean, aes(x = fecha, y = superficie)) +
  # Banda de confianza (percentiles 25-75)
  stat_summary(
    fun.min = function(x) quantile(x, 0.25, na.rm = TRUE),
    fun.max = function(x) quantile(x, 0.75, na.rm = TRUE),
    geom = "ribbon", 
    alpha = 0.15, 
    fill = "#8B4789"
  ) +
  # Líneas individuales más sutiles
  geom_line(aes(group = barrio), alpha = 0.06, color = "gray50", size = 0.2) +
  # Línea promedio más destacada
  stat_summary(fun = mean, geom = "line", color = "#8B4789", size = 2) +
  # Puntos en el promedio
  stat_summary(fun = mean, geom = "point", color = "#5F2F5D", size = 1.2, alpha = 0.8) +
  labs(
    title = "Evolución de la Superficie Promedio de Departamentos",
    subtitle = "Promedio general con banda de variabilidad (P25-P75) • Líneas grises = barrios individuales",
    x = "Período", 
    y = "Superficie promedio (m²)",
    caption = "Fuente: Gobierno de la Ciudad de Buenos Aires"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " m²"),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  tema_custom +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
save_plot(p3, "03_superficie_evolucion.png")

# ============================
# 4. Relación alquiler / venta: años para recuperar inversión
# ============================

alquiler_venta <- inner_join(
  precio_alquiler_clean %>% select(barrio, fecha, precio_m2_estimado) %>% rename(precio_alquiler = precio_m2_estimado),
  precio_venta_clean %>% select(barrio, fecha, precio_m2_estimado) %>% rename(precio_venta = precio_m2_estimado),
  by = c("barrio", "fecha")
) %>%
  # Filtrar valores válidos antes del cálculo
  filter(
    !is.na(precio_alquiler), 
    !is.na(precio_venta),
    precio_alquiler > 0,
    precio_venta > 0
  ) %>%
  mutate(anios_recupero = precio_venta / (precio_alquiler * 12)) %>%
  # Filtrar valores razonables (entre 1 y 100 años)
  filter(
    !is.na(fecha), 
    is.finite(anios_recupero),
    anios_recupero > 0,
    anios_recupero <= 100
  ) %>%
  mutate(anio = year(fecha))

# Verificar si tenemos datos válidos
if(nrow(alquiler_venta) > 0 && any(is.finite(alquiler_venta$anios_recupero))) {
  
  p4 <- ggplot(alquiler_venta, aes(x = factor(anio), y = anios_recupero)) +
    geom_boxplot(
      fill = "#87CEEB", 
      color = "#4682B4", 
      alpha = 0.8,
      outlier.color = "#B22222",
      outlier.alpha = 0.6
    ) +
    stat_summary(
      fun = mean, 
      geom = "point", 
      shape = 18, 
      size = 3, 
      color = "#FF4500"
    ) +
    labs(
      title = "Años Necesarios para Recuperar Inversión Inmobiliaria",
      subtitle = "Relación entre precios de venta y alquiler por m² • Punto naranja = promedio",
      x = "Año", 
      y = "Años estimados de recupero",
      caption = "Fuente: Análisis propio basado en datos del Gobierno CABA"
    ) +
    scale_y_continuous(
      breaks = seq(0, ceiling(max(alquiler_venta$anios_recupero, na.rm = TRUE)/5)*5, by = 5),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    tema_custom
  
  print(p4)
  save_plot(p4, "04_anios_recupero_inversion.png")
  
} else {
  cat("⚠️  No hay datos válidos para el gráfico de años de recupero.\n")
  cat("   Esto puede deberse a:\n")
  cat("   - Precios de alquiler en 0 o muy bajos\n")
  cat("   - Falta de coincidencia entre fechas de alquiler y venta\n")
  cat("   - Datos faltantes en las fechas\n")
}

# ============================
# 5. Créditos hipotecarios UVA - evolución
# ============================

prestamos_uva_clean <- prestamos_uva_clean %>%
  mutate(anio = año, monto_millones = monto_uva / 1000)

prestamos_anual <- prestamos_uva_clean %>%
  group_by(anio) %>%
  summarise(monto_total = sum(monto_millones, na.rm = TRUE), .groups = 'drop')

p5 <- ggplot(prestamos_anual, aes(x = anio, y = monto_total)) +
  geom_col(
    fill = "#FF8C00", 
    color = "#FF7F00", 
    alpha = 0.9,
    width = 0.7
  ) +
  geom_text(
    aes(label = scales::comma(round(monto_total, 0), big.mark = ".", decimal.mark = ",")),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold",
    color = "#8B4513"
  ) +
  labs(
    title = "Evolución del Monto Total de Créditos Hipotecarios UVA",
    subtitle = "Montos anuales acumulados expresados en millones de pesos",
    x = "Año", 
    y = "Millones de pesos (ARS)",
    caption = "Fuente: Banco Central de la República Argentina"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_continuous(breaks = unique(prestamos_anual$anio)) +
  tema_custom

print(p5)
save_plot(p5, "05_creditos_uva_evolucion.png")

# ============================
# 6. Distribución del patrimonio declarado
# ============================

# Preparar datos para mejor visualización
patrimonio_data <- actividad_clean %>%
  count(categoria_patrimonio, name = "cantidad") %>%
  mutate(
    porcentaje = cantidad / sum(cantidad) * 100,
    categoria_patrimonio = fct_reorder(categoria_patrimonio, cantidad)
  )

p6 <- ggplot(patrimonio_data, aes(x = categoria_patrimonio, y = cantidad)) +
  geom_col(
    fill = "#4682B4", 
    color = "#2F4F4F", 
    alpha = 0.9,
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(cantidad, "\n(", round(porcentaje, 1), "%)")),
    hjust = -0.1,
    size = 3.5,
    fontface = "bold",
    color = "#2F4F4F"
  ) +
  labs(
    title = "Distribución del Patrimonio Declarado por Funcionarios",
    subtitle = "Categorías patrimoniales reportadas en declaraciones juradas (2023)",
    x = "Categoría de Patrimonio", 
    y = "Cantidad de Funcionarios",
    caption = "Fuente: Declaraciones Juradas - Gobierno CABA"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  coord_flip() +
  tema_custom +
  theme(
    axis.text.y = element_text(size = 11)
  )

print(p6)
save_plot(p6, "06_patrimonio_distribucion.png")

# ============================
# Resumen de archivos guardados
# ============================

cat("\n", rep("=", 50), "\n", sep = "")
cat("GRÁFICOS GUARDADOS EXITOSAMENTE\n")
cat(rep("=", 50), "\n", sep = "")
cat("Ubicación:", output_path, "\n\n")
cat("Archivos generados:\n")
cat("• 01_precio_venta_evolucion.png\n")
cat("• 02_precio_alquiler_evolucion.png\n") 
cat("• 03_superficie_evolucion.png\n")
cat("• 04_anios_recupero_inversion.png\n")
cat("• 05_creditos_uva_evolucion.png\n")
cat("• 06_patrimonio_distribucion.png\n")
cat(rep("=", 50), "\n", sep = "")