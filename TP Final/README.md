# 📊 Trabajo Final – Ciencia de Datos para Economía y Negocios  
### Análisis del dataset *Sample – Superstore* (Retail)

Este proyecto desarrolla un análisis integral del dataset **Sample – Superstore**, siguiendo la metodología vista en la materia:

- **Formulación de hipótesis**
- **Limpieza y EDA**
- **Detección y justificación de outliers y valores faltantes**
- **Evaluación del impacto de la limpieza**
- **Inferencia estadística (ANOVA y regresión múltiple)**
- **Visualizaciones editorializadas**
- **Conclusiones y validación de hipótesis**

El objetivo principal es comprender **cómo distintos factores (ventas, descuentos, segmentos, categorías, regiones)** explican el comportamiento de la variable **`Profit`**.

---

# 📁 Estructura del Proyecto

```
TP_Superstore/
├─ data/
│  ├─ raw/                 # Datos originales
│  └─ clean/               # Datos limpios (superstore_clean y superstore_eda)
├─ scripts/
│  ├─ 01_EDA.R             # Exploración, limpieza, outliers, distribuciones
│  ├─ 02_Modelado.R        # ANOVA, regresiones, diagnósticos
│  └─ 03_Visualizaciones.R # Gráficos finales y storytelling
├─ output/
│  ├─ figures/             # Gráficos generados automáticamente
│  └─ slides/              # Presentación final
└─ README.md               # Este archivo
```

---

# 🛠️ Requisitos

Para ejecutar los scripts se necesitan las siguientes librerías de R:

```r
install.packages(c(
  "tidyverse", 
  "lubridate", 
  "janitor",
  "skimr",
  "car"
))
```

---

# ▶️ **Cómo reproducir el análisis**

## 1. Colocar los datos en la ruta correcta
- El archivo original debe ir en:  
  **`data/raw/Sample - Superstore.csv`**

## 2. Ejecutar los scripts en orden

### **1️⃣ 01_EDA.R**
- Limpia los datos  
- Crea variables derivadas  
- Detecta outliers  
- Realiza histogramas, boxplots y correlaciones  
- Exporta gráficos a `output/figures/`  
- Genera `data/clean/superstore_clean.csv` y `data/clean/superstore_eda.csv`

### **2️⃣ 02_Modelado.R**
- ANOVA + Tukey HSD  
- Modelo de regresión múltiple (dos versiones)  
- Diagnósticos del modelo (QQ-plot, residuos, leverage, Cook’s distance)  
- Exporta gráficos a `output/figures/`

### **3️⃣ 03_Visualizaciones.R**
- Visualizaciones finales  
- Gráficos editorializados (descuento–profit y sales–profit)  
- Heatmap, evoluciones mensuales, rankings por categoría y región  
- Todo guardado en `output/figures/`

---

# 🧪 Hipótesis del proyecto

### **H1 – Confirmada**  
Los descuentos aplicados reducen significativamente el `Profit`.

### **H2 – Refutada**  
No existen diferencias significativas de profit entre segmentos de clientes.

---

# 📌 Salidas principales

Todos los gráficos generados por los scripts se almacenan automáticamente en:

```
output/figures/
```

---

# 👤 Autor
**Augusto Suppa**  

---

# 🎓 Nota sobre reproducibilidad

- Estructura estandarizada  
- Scripts secuenciales  
- Outputs automáticos  
- Modelado y limpieza documentados  
- Visualizaciones editorializadas  