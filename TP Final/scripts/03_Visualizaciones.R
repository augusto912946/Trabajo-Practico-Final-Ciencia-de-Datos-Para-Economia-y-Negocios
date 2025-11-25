# Gráficos finales para el TP Superstore
library(tidyverse)
library(ggplot2)
library(dplyr)

# Función helper para guardar gráficos
save_plot <- function(p, filename, w = 8, h = 5) {
  ggsave(
    filename = filename,
    plot = p,
    path = "output/figures",
    width = w,
    height = h,
    dpi = 300
  )
}

# Cargar dataset final para análisis / modelo
superstore <- read_csv("data/clean/superstore_eda.csv")

# Asegurar tipos adecuados
superstore <- superstore %>%
  mutate(
    segment      = factor(segment),
    region       = factor(region),
    category     = factor(category),
    sub_category = factor(sub_category),
    year         = factor(year),
    month = factor(
      month,
      levels = c("ene", "feb", "mar", "abr", "may", "jun",
                 "jul", "ago", "sep", "oct", "nov", "dic"),
      ordered = TRUE
    )
  )

# ========================
# 1) Evolución de Sales y Profit por año-mes
# ========================

superstore_mes <- superstore %>%
  group_by(year, month) %>%
  summarise(
    sales_total  = sum(sales),
    profit_total = sum(profit),
    .groups = "drop"
  ) %>%
  arrange(year, month)

# Gráfico de líneas: Sales y Profit por mes (faceteados)
p_time <- ggplot(superstore_mes, aes(x = month)) +
  geom_line(aes(y = sales_total, group = year, colour = year)) +
  geom_point(aes(y = sales_total, colour = year)) +
  labs(
    title = "Evolución mensual de Sales por año",
    x = "Mes",
    y = "Sales totales (USD)",
    colour = "Año"
  ) +
  theme_minimal()

save_plot(p_time, "time_sales_por_mes_y_anio.png")

# ========================
# 2) Evolución de Profit por año-mes
# ========================

p_profit_time <- ggplot(superstore_mes, aes(x = month)) +
  geom_line(aes(y = profit_total, group = year, colour = year)) +
  geom_point(aes(y = profit_total, colour = year)) +
  labs(
    title = "Evolución mensual de Profit por año",
    x = "Mes",
    y = "Profit total (USD)",
    colour = "Año"
  ) +
  theme_minimal()

save_plot(p_profit_time, "time_profit_por_mes_y_anio.png")

# ============================
# 3) Distribuciones de Sales y Profit
# ============================
# Histograma de Sales (escala logarítmica para visualizar mejor)
p_sales_hist <- ggplot(superstore, aes(x = sales)) +
  geom_histogram(bins = 40, fill = "#4A90E2", color = "white") +
  scale_x_log10() +
  labs(
    title = "Distribución de Sales (escala logarítmica)",
    x = "Sales (USD, escala log10)",
    y = "Frecuencia"
  ) +
  theme_minimal()

save_plot(p_sales_hist, "dist_sales_histograma.png")


# Histograma de Profit (breaks manuales y límites recortados)
p_profit_hist <- ggplot(superstore, aes(x = profit)) +
  geom_histogram(
    bins = 50,
    fill = "#E24A4A",
    color = "white"
  ) +
  coord_cartesian(xlim = quantile(superstore$profit, c(0.01, 0.99))) +
  labs(
    title = "Distribución de Profit (rango 1%–99%)",
    x = "Profit (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal()

save_plot(p_profit_hist, "dist_profit_histograma.png")

# ============================
# 4) Gráficos clave para validar hallazgos del modelo
# ============================

# 4.1 Profit por Categoria
p_profit_cat <- superstore %>%
  group_by(category) %>%
  summarise(mean_profit = mean(profit)) %>%
  ggplot(aes(x = reorder(category, mean_profit), y = mean_profit)) +
  geom_col(fill = "#4A90E2") +
  coord_flip() +
  labs(
    title = "Profit promedio por Categoría",
    x = "Categoría",
    y = "Profit promedio (USD)"
  ) +
  theme_minimal()

save_plot(p_profit_cat, "profit_por_categoria.png")


# 4.2 Profit por Sub-category (Top 10 o completo ordenado)
p_profit_subcat <- superstore %>%
  group_by(sub_category) %>%
  summarise(mean_profit = mean(profit)) %>%
  ggplot(aes(x = reorder(sub_category, mean_profit), y = mean_profit)) +
  geom_col(fill = "#7A4AE2") +
  coord_flip() +
  labs(
    title = "Profit promedio por Sub-Categoría",
    x = "Sub-Categoría",
    y = "Profit promedio (USD)"
  ) +
  theme_minimal()

save_plot(p_profit_subcat, "profit_por_subcategoria.png")


# 4.3 Relación Sales vs Profit
p_scatter_sales_profit <- ggplot(superstore, aes(x = sales, y = profit)) +
  geom_point(alpha = 0.15, color = "#4A90E2") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(
    title = "Relación entre Sales y Profit",
    x = "Sales (USD)",
    y = "Profit (USD)"
  ) +
  theme_minimal()

save_plot(p_scatter_sales_profit, "scatter_sales_vs_profit.png")

# 4.4 Impacto del Discount en Profit
p_scatter_discount_profit <- ggplot(superstore, aes(x = discount, y = profit)) +
  geom_point(alpha = 0.25, color = "#E24A4A") +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  coord_cartesian(ylim = c(-1500, 1500)) +
  labs(
    title = "Impacto del Discount sobre Profit",
    x = "Descuento (%)",
    y = "Profit (USD)"
  ) +
  theme_minimal()

save_plot(p_scatter_discount_profit, "scatter_discount_vs_profit.png")



# ============================
# 5) Rendimiento por Región y Segmento
# ============================

# ----- A) Profit promedio por región
profit_region <- superstore %>%
  group_by(region) %>%
  summarise(profit_mean = mean(profit), .groups = "drop")

p_profit_region <- ggplot(profit_region, aes(x = region, y = profit_mean)) +
  geom_col(fill = "#4A90E2") +
  labs(
    title = "Profit promedio por Región",
    x = "Región",
    y = "Profit promedio (USD)"
  ) +
  theme_minimal()

save_plot(p_profit_region, "profit_promedio_por_region.png")


# ----- B) Sales totales por región
sales_region <- superstore %>%
  group_by(region) %>%
  summarise(sales_total = sum(sales), .groups = "drop")

p_sales_region <- ggplot(sales_region, aes(x = region, y = sales_total)) +
  geom_col(fill = "#72C472") +
  labs(
    title = "Sales totales por Región",
    x = "Región",
    y = "Sales totales (USD)"
  ) +
  theme_minimal()

save_plot(p_sales_region, "sales_totales_por_region.png")


# ----- C) Profit promedio por segmento
profit_segment <- superstore %>%
  group_by(segment) %>%
  summarise(profit_mean = mean(profit), .groups = "drop")

p_profit_segment <- ggplot(profit_segment, aes(x = segment, y = profit_mean)) +
  geom_col(fill = "#E2A04A") +
  labs(
    title = "Profit promedio por Segmento",
    x = "Segmento",
    y = "Profit promedio (USD)"
  ) +
  theme_minimal()

save_plot(p_profit_segment, "profit_promedio_por_segmento.png")


# ----- D) Sales totales por segmento
sales_segment <- superstore %>%
  group_by(segment) %>%
  summarise(sales_total = sum(sales), .groups = "drop")

p_sales_segment <- ggplot(sales_segment, aes(x = segment, y = sales_total)) +
  geom_col(fill = "#E24A4A") +
  labs(
    title = "Sales totales por Segmento",
    x = "Segmento",
    y = "Sales totales (USD)"
  ) +
  theme_minimal()

save_plot(p_sales_segment, "sales_totales_por_segmento.png")

# ============================
# 6 Heatmap de Sales por Región y Categoría
# ============================

sales_region_cat <- superstore %>%
  group_by(region, category) %>%
  summarise(
    sales_total = sum(sales),
    .groups = "drop"
  )

p_heatmap <- ggplot(sales_region_cat, aes(x = region, y = category, fill = sales_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#E0F3F8", high = "#08589E") +
  labs(
    title = "Heatmap: Sales por Región y Categoría",
    x = "Región",
    y = "Categoría",
    fill = "Sales totales"
  ) +
  theme_minimal()

save_plot(p_heatmap, "heatmap_sales_region_categoria.png")

