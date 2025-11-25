# Cargar librerías principales
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

# Cargar dataset
superstore <- read_csv("data/raw/Sample - Superstore.csv")

# Limpia nombres de columnas (quita espacios y mayúsculas)
superstore <- superstore %>% 
  clean_names()

# Convierte columnas de texto a tipo fecha
superstore <- superstore %>%
  mutate(
    order_date = mdy(order_date),
    ship_date = mdy(ship_date)
  )

# Creamos las variables derivadas
superstore <- superstore %>%
  mutate(
    lead_time = as.numeric(ship_date - order_date),      # días entre pedido y envío
    margin_pct = profit / sales,                         # rentabilidad
    year = year(order_date),
    month = month(order_date, label = TRUE, abbr = TRUE) # mes abreviado
  )

# Chequeos iniciales
# Resumen de estructura
glimpse(superstore)

# Resumen estadístico general
skim(superstore)

# Chequear duplicados
superstore %>%
  summarise(duplicados = sum(duplicated(order_id)))

# Revisar valores faltantes
colSums(is.na(superstore))

# Guardar dataset limpio
write_csv(superstore, "data/clean/superstore_clean.csv")

# Preparar carpeta y ayudantes
# --- CONFIG DE SALIDAS ---
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, filename, w=8, h=5){
  ggsave(filename = file.path("output/figures", filename), plot = p, width = w, height = h, dpi = 300)
}

# 1) Distribuciones univariadas (histogramas)
library(ggplot2)

# Sales
p_sales <- ggplot(superstore, aes(sales)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribución de Sales", x = "Sales (USD)", y = "Frecuencia")
save_plot(p_sales, "hist_sales.png")

p_sales_log <- ggplot(superstore, aes(sales)) +
  geom_histogram(bins = 40) +
  scale_x_log10() +
  labs(title = "Distribución de Sales (escala log10)", x = "Sales (USD, log10)", y = "Frecuencia")
save_plot(p_sales_log, "hist_sales_log.png")

# Profit
p_profit <- ggplot(superstore, aes(profit)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribución de Profit", x = "Profit (USD)", y = "Frecuencia")
save_plot(p_profit, "hist_profit.png")

# Discount (0–0.8 aprox)
p_discount <- ggplot(superstore, aes(discount)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribución de Discount", x = "Discount", y = "Frecuencia")
save_plot(p_discount, "hist_discount.png")

# Quantity
p_quantity <- ggplot(superstore, aes(quantity)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución de Quantity", x = "Quantity", y = "Frecuencia")
save_plot(p_quantity, "hist_quantity.png")

# Lead time (días entre order y ship)
p_lead <- ggplot(superstore, aes(lead_time)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribución de Lead Time", x = "Días", y = "Frecuencia")
save_plot(p_lead, "hist_lead_time.png")

# 2) Boxplots para ver outliers por grupos
# Profit por Segment
p_box_seg <- ggplot(superstore, aes(segment, profit)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_cartesian(ylim = quantile(superstore$profit, c(0.01, 0.99))) +  
  labs(title = "Profit por Segment (boxplot)", x = "Segment", y = "Profit (USD)")
save_plot(p_box_seg, "box_profit_segment.png")

# Profit por Region
p_box_reg <- ggplot(superstore, aes(region, profit)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_cartesian(ylim = quantile(superstore$profit, c(0.01, 0.99))) +
  labs(title = "Profit por Region (boxplot)", x = "Region", y = "Profit (USD)")
save_plot(p_box_reg, "box_profit_region.png")

# Profit por Category
p_box_cat <- ggplot(superstore, aes(category, profit)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_cartesian(ylim = quantile(superstore$profit, c(0.01, 0.99))) +
  labs(title = "Profit por Category (boxplot)", x = "Category", y = "Profit (USD)")
save_plot(p_box_cat, "box_profit_category.png")

# 3) Relaciones clave (scatter + tendencia)
# Profit vs Sales
p_ps <- ggplot(superstore, aes(sales, profit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Profit vs Sales", x = "Sales (USD)", y = "Profit (USD)")
save_plot(p_ps, "scatter_profit_vs_sales.png")

# Profit vs Sales (eje x log para ver mejor ventas grandes)
p_ps_logx <- ggplot(superstore, aes(sales, profit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  labs(title = "Profit vs Sales (x log10)", x = "Sales (USD, log10)", y = "Profit (USD)")
save_plot(p_ps_logx, "scatter_profit_vs_sales_logx.png")

# Profit vs Discount
p_pd <- ggplot(superstore, aes(discount, profit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Profit vs Discount", x = "Discount", y = "Profit (USD)")
save_plot(p_pd, "scatter_profit_vs_discount.png")

# Profit vs Quantity
p_pq <- ggplot(superstore, aes(quantity, profit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Profit vs Quantity", x = "Quantity", y = "Profit (USD)")
save_plot(p_pq, "scatter_profit_vs_quantity.png")

# 4) Correlaciones rápidas (numéricas)
nums <- superstore |>
  dplyr::select(sales, profit, discount, quantity, lead_time, margin_pct)

# Matriz de correlación
cor_mat <- cor(nums)
print(round(cor_mat, 3))

# Correlación de Profit con el resto (nos guía para la regresión)
# Vector de correlaciones con 'profit'
vec_profit <- cor_mat[, "profit"]
sort(vec_profit, decreasing = TRUE)

# 5) Detección y marcado de outliers (IQR)
mark_outliers <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  low <- q1 - 1.5 * iqr
  high <- q3 + 1.5 * iqr
  list(low = low, high = high)
}

thr_sales   <- mark_outliers(superstore$sales)
thr_profit  <- mark_outliers(superstore$profit)
thr_qty     <- mark_outliers(superstore$quantity)

superstore <- superstore |>
  mutate(
    out_sales  = sales  < thr_sales$low  | sales  > thr_sales$high,
    out_profit = profit < thr_profit$low | profit > thr_profit$high,
    out_qty    = quantity < thr_qty$low  | quantity > thr_qty$high
  )

# Conteo de outliers por variable
superstore |>
  summarise(
    out_sales  = sum(out_sales),
    out_profit = sum(out_profit),
    out_qty    = sum(out_qty)
  )

# 6) Guardar versión con flags para trabajar después
write_csv(superstore, "data/clean/superstore_eda.csv")








