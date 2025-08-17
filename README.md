# 🚀 Análisis Causal Impact - Equipaje JetSmart

## 📝 Descripción

Esta aplicación Shiny permite realizar análisis de impacto causal utilizando la metodología **Causal Impact** para evaluar el efecto de intervenciones en métricas de negocio, específicamente enfocada en el análisis de **conversión de equipaje** de JetSmart.

El Causal Impact es una técnica estadística que utiliza series de tiempo y variables de control para estimar el impacto causal de una intervención (como una campaña, cambio de política, o nueva funcionalidad) comparando lo que realmente ocurrió versus lo que habría ocurrido sin la intervención.

## 🎯 Caso de Uso: Conversión de Equipaje

Esta aplicación está diseñada específicamente para analizar el impacto de intervenciones en la **conversión de equipaje** de JetSmart, tales como:

- 🎨 Cambios en la interfaz de usuario (UI/UX)
- 💰 Modificaciones en precios o estrategias de pricing
- 📧 Campañas de marketing o comunicación
- 🔧 Implementación de nuevas funcionalidades
- 📱 Cambios en el proceso de compra

### Variables Típicas de Análisis

- **Variable Objetivo (KPI)**: Conversión de Equipaje - Tasa de conversión o número de equipajes vendidos
- **Variables de Control**: 
  - Tráfico total de pasajeros
  - Número de vuelos
  - Temporalidad (día de la semana, estacionalidad)
  - Otras métricas relacionadas que puedan influir en la conversión

## 🚀 Características

### 📊 Análisis Completo
- **Carga de datos**: Soporte para archivos Excel (.xlsx) y CSV (.csv)
- **Configuración flexible**: Selección de variable objetivo, variables de control y períodos de análisis
- **Visualizaciones múltiples**: Gráficos nativos de CausalImpact y visualizaciones personalizadas interactivas
- **Resultados estadísticos**: Métricas clave, intervalos de confianza y significancia estadística

### 📈 Visualizaciones Incluidas
- Gráfico principal con serie observada vs contrafactual
- Efectos puntuales por día
- Efectos acumulativos
- Distribución de efectos post-intervención
- Gráficos nativos de la librería CausalImpact

### 📋 Reportes
- Resumen ejecutivo con interpretación automática
- Reporte narrativo oficial de CausalImpact
- Descarga de resultados en CSV
- Descarga de reporte completo en HTML

## 📋 Requisitos del Sistema

### R y Librerías Necesarias

```r
# Librerías principales
install.packages(c(
  "shiny",           # Framework web
  "shinydashboard",  # UI dashboard
  "DT",              # Tablas interactivas
  "plotly",          # Gráficos interactivos
  "readxl",          # Lectura de Excel
  "CausalImpact",    # Análisis causal
  "zoo",             # Series de tiempo
  "dplyr",           # Manipulación de datos
  "lubridate",       # Manejo de fechas
  "ggplot2"          # Visualizaciones
))
```

## 📊 Formato de Datos Requerido

### Estructura del Archivo

El archivo debe contener las siguientes columnas:

| Fecha | Conversion_Baggage | PaxLeg | TM | Trafico_Total |
|-------|-------------------|--------|-----|---------------|
| 2024-01-01 | 0.156 | 1250 | 890 | 2140 |
| 2024-01-02 | 0.162 | 1180 | 920 | 2100 |
| ... | ... | ... | ... | ... |

### Requisitos del Dataset

1. **📅 Columna de fechas**: Formato YYYY-MM-DD (e.g., 2024-01-01)
2. **🔢 Datos diarios**: Observaciones consecutivas día por día
3. **🎯 Variable objetivo**: Métrica numérica a analizar (conversión de equipaje)
4. **⚙️ Variables de control**: Al menos una variable de control numérica
5. **📊 Datos completos**: Sin valores faltantes en el período de análisis

### Ejemplo de Variables

- **Fecha**: 2024-01-01, 2024-01-02, ...
- **Conversion_Baggage**: 0.156, 0.162, ... (tasa de conversión)
- **PaxLeg**: 1250, 1180, ... (número de pasajeros)
- **TM**: 890, 920, ... (métrica de tráfico)
- **Trafico_Total**: 2140, 2100, ... (tráfico total)

## 🚀 Instrucciones de Uso

### 1. 📤 Cargar Datos
1. Ve a la pestaña "Cargar Datos"
2. Selecciona tu archivo Excel (.xlsx) o CSV (.csv)
3. Revisa la vista previa para verificar que los datos se cargaron correctamente

### 2. ⚙️ Configurar Análisis
1. Ve a la pestaña "Configuración"
2. Selecciona la **columna de fechas**
3. Define la **fecha de intervención** (cuando ocurrió el cambio a analizar)
4. Establece el **período pre-intervención** (datos históricos para el modelo)
5. Establece el **período post-intervención** (datos posteriores a la intervención)
6. Selecciona la **variable objetivo** (e.g., Conversion_Baggage)
7. Selecciona las **variables de control** (e.g., PaxLeg, TM, Trafico_Total)
8. Ajusta el **nivel de confianza** (por defecto 95%)

### 3. 📊 Ejecutar Análisis
1. Ve a la pestaña "Análisis"
2. Haz clic en "Ejecutar Análisis Causal Impact"
3. Espera a que se complete el procesamiento
4. Revisa los gráficos generados

### 4. 📈 Revisar Resultados
1. Ve a la pestaña "Resultados" para ver métricas detalladas
2. Ve a la pestaña "Resumen" para el resumen ejecutivo
3. Descarga los resultados en CSV o el reporte completo en HTML

## 📈 Interpretación de Resultados

### Métricas Principales

- **Efecto Absoluto**: Diferencia promedio entre observado y esperado
- **Efecto Relativo**: Porcentaje de cambio causado por la intervención
- **Efecto Acumulativo**: Impacto total acumulado durante el período post-intervención
- **Valor p**: Significancia estadística del efecto detectado

### Criterios de Interpretación

- **p < 0.05**: Efecto estadísticamente significativo
- **p < 0.01**: Efecto altamente significativo
- **p > 0.05**: Sin evidencia de efecto significativo

## 🔧 Ejecutar la Aplicación

### Localmente

```r
# Cargar el archivo principal
source("shiny_causal_impact_baggage.r")

# La aplicación se ejecutará automáticamente
```

### En RStudio

1. Abre el archivo `shiny_causal_impact_baggage.r`
2. Haz clic en "Run App" en la esquina superior derecha
3. La aplicación se abrirá en tu navegador o en el panel de RStudio

## 📞 Soporte

Esta aplicación fue desarrollada para facilitar el análisis de impacto causal en métricas de conversión de equipaje. Para dudas técnicas o mejoras, consulta la documentación de [CausalImpact](https://google.github.io/CausalImpact/CausalImpact.html).

## 🔍 Notas Técnicas

- La metodología se basa en Modelos de Espacio de Estados Bayesianos
- Requiere al menos 3 observaciones en el período pre-intervención
- Las variables de control deben estar correlacionadas con la variable objetivo
- Los efectos se estiman asumiendo que las variables de control no fueron afectadas por la intervención

---

*Desarrollado para JetSmart - Análisis de Conversión de Equipaje*
