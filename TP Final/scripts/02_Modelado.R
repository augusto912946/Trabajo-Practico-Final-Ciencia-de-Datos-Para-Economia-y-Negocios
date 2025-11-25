# Inferencia y Modelado sobre Superstore

# Cargar librerías
library(tidyverse)
library(car)

# Crear carpeta de figuras si no existe
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Función para guardar gráficos
save_plot <- function(p, filename, w=8, h=5){
  ggsave(
    filename = filename,
    plot = p,
    path = "output/figures",
    width = w,
    height = h,
    dpi = 300
  )
}

# -----------------------------------------------------------
# Cargar dataset ya preparado con flags de outliers
superstore <- read_csv("data/clean/superstore_eda.csv")

# Asegurar tipos correctos para variables categóricas
superstore <- superstore %>%
  mutate(
    segment      = factor(segment),
    region       = factor(region),
    category     = factor(category),
    sub_category = factor(sub_category),
    year         = factor(year),
    month        = factor(month)
  )

# -----------------------------------------------------------
# ANOVA: Profit ~ Segment

# Resumen por grupo
resumen_segment <- superstore %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    mean_profit = mean(profit),
    sd_profit = sd(profit)
  )

print(resumen_segment)

# Modelo ANOVA
anova_seg <- aov(profit ~ segment, data = superstore)

# Resumen del ANOVA
summary(anova_seg)

# Test post-hoc (solo si ANOVA NO es significativo igual lo reportamos)
tukey_result <- TukeyHSD(anova_seg)
print(tukey_result)

# -----------------------------------------------------------
# Gráficos para incluir en el informe

# 1) Boxplot de Profit por Segment
p_box_seg <- ggplot(superstore, aes(segment, profit)) +
  geom_boxplot(outlier.alpha = 0.4) +
  coord_cartesian(ylim = quantile(superstore$profit, c(0.01, 0.99))) +
  labs(
    title = "Profit por Segment (ANOVA)",
    x = "Segment",
    y = "Profit (USD)"
  )

save_plot(p_box_seg, "anova_box_profit_segment.png")

# 2) Medias de Profit por Segment
p_means_seg <- ggplot(resumen_segment, aes(segment, mean_profit)) +
  geom_col(fill = "#4A90E2") +
  geom_errorbar(aes(ymin = mean_profit - sd_profit,
                    ymax = mean_profit + sd_profit),
                width = 0.2,
                color = "black") +
  labs(
    title = "Promedio de Profit por Segment",
    x = "Segment",
    y = "Mean Profit (USD)"
  )

save_plot(p_means_seg, "anova_means_profit_segment.png")

# -----------------------------------------------------------
# Gráficos de diagnóstico del ANOVA (base R)

png("output/figures/anova_residuals_vs_fitted.png", width=900, height=700)
plot(anova_seg, 1)   # Residuals vs Fitted
dev.off()

png("output/figures/anova_qqplot_residuals.png", width=900, height=700)
plot(anova_seg, 2)   # QQ plot
dev.off()

# -------------------------
# MODELO DE REGRESIÓN MÚLTIPLE (Modelo Base)
# -------------------------

modelo_base <- lm(
  profit ~ sales + discount + quantity + segment + region,
  data = superstore
)

summary(modelo_base)

# -------------------------
# DIAGNÓSTICOS DEL MODELO
# -------------------------

# 1) Residuos vs ajustados (homocedasticidad)
plot(modelo_base, 1)

# 2) QQ-plot de residuos (normalidad)
plot(modelo_base, 2)

# 3) Scale-location (varianza constante)
plot(modelo_base, 3)

# 4) Residuals vs leverage (detección de outliers influyentes)
plot(modelo_base, 5)

# 5) VIF (multicolinealidad)
library(car)
vif(modelo_base)

# 6) Cook’s distance: puntos influyentes
cooks <- cooks.distance(modelo_base)
summary(cooks)

# Opcional: ver los más influyentes
tail(sort(cooks), 10)

# Función rápida para guardar plots base de R
save_base_plot <- function(expr, filename){
  png(file.path("output/figures", filename), width = 900, height = 700)
  eval(expr)
  dev.off()
}

save_base_plot(quote(plot(modelo_base, 1)), "lm_residuals_vs_fitted.png")
save_base_plot(quote(plot(modelo_base, 2)), "lm_qqplot_residuals.png")
save_base_plot(quote(plot(modelo_base, 3)), "lm_scale_location.png")
save_base_plot(quote(plot(modelo_base, 5)), "lm_residuals_vs_leverage.png")

modelo_mejorado <- lm(
  profit ~ sales + discount + quantity + margin_pct +
    category + region + sub_category + year,
  data = superstore
)

summary(modelo_mejorado)

AIC(modelo_base, modelo_mejorado)
BIC(modelo_base, modelo_mejorado)

# -----------------------------------------------------------
# DIAGNÓSTICOS DEL MODELO MEJORADO
# -----------------------------------------------------------

# Guardar los diagnósticos en output/figures/
save_base_plot(quote(plot(modelo_mejorado, 1)), "lm2_residuals_vs_fitted.png")
save_base_plot(quote(plot(modelo_mejorado, 2)), "lm2_qqplot_residuals.png")
save_base_plot(quote(plot(modelo_mejorado, 3)), "lm2_scale_location.png")
save_base_plot(quote(plot(modelo_mejorado, 5)), "lm2_residuals_vs_leverage.png")

# VIF del modelo mejorado
modelo_vif <- lm(
  profit ~ sales + discount + quantity + margin_pct,
  data = superstore
)

vif(modelo_vif)


# Cook's distance
cooks2 <- cooks.distance(modelo_mejorado)
summary(cooks2)
tail(sort(cooks2), 10)
