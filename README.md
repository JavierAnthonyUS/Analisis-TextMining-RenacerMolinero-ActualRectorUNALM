# 📊 Análisis de Minería de Texto - Plan de Gestión "Renacer Molinero" 2026-2031

<p align="center">
  <img src="https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white" alt="R">
  <img src="https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white" alt="RStudio">
  <img src="https://img.shields.io/badge/Text_Mining-FF6F00?style=for-the-badge" alt="Text Mining">
  <img src="https://img.shields.io/badge/NLP-9C27B0?style=for-the-badge" alt="NLP">
</p>

## 📋 Descripción

Proyecto de **Minería de Texto y Análisis de Sentimientos** aplicado a la propuesta del Plan de Gestión "Renacer Molinero" (Lista 5), candidatura ganadora para el periodo rectoral 2026-2031 de la **Universidad Nacional Agraria La Molina (UNALM)**.

Este análisis fue desarrollado como proyecto académico para el curso **Técnicas de Exploración de Datos**, impartido por el profesor **Jesús Salinas**.

## 📂 Documentos Principales

| Archivo | Descripción |
|---------|-------------|
| 📄 **[analisis_completo.html](analisis_completo.html)** | Reporte completo del análisis con todos los detalles, gráficos y resultados |
| 📑 **[EXPOANALISISTEXTO.pdf](EXPOANALISISTEXTO.pdf)** | Presentación de la exposición realizada del proyecto |

> 💡 **Tip**: Descarga el archivo `analisis_completo.html` y ábrelo en tu navegador para ver el análisis interactivo completo.

## 🎯 Objetivos

- Aplicar técnicas de minería de texto para extraer insights del documento electoral
- Realizar análisis de sentimientos con diccionario contextualizado UNALM
- Identificar términos clave, bigramas y correlaciones entre palabras
- Analizar la distribución temática por secciones del plan de gobierno

## 🛠️ Metodología

### Corpus Dual
- **Corpus General**: Documento completo para análisis global
- **Corpus por Secciones**: 11 secciones temáticas para análisis específico

### Técnicas Aplicadas
1. **Preprocesamiento**: Limpieza, tokenización, eliminación de stopwords
2. **Análisis de Frecuencias**: Top palabras y TF-IDF por sección
3. **Análisis de Sentimientos**: Diccionario personalizado con 80 términos contextualizados
4. **Análisis de N-gramas**: Bigramas más frecuentes
5. **Análisis de Correlaciones**: Redes de co-ocurrencia de palabras

## 📁 Estructura del Proyecto

```
├── 📄 01_lectura_limpieza.R          # Script principal de análisis
├── 📄 analisis_completo.Rmd          # Documento R Markdown completo
├── 📄 analisis_completo.html         # ⭐ Reporte HTML con análisis detallado
├── 📄 EXPOANALISISTEXTO.Rmd          # Presentación Beamer en R Markdown
├── 📄 EXPOANALISISTEXTO.pdf          # ⭐ PDF de la exposición del proyecto
├── 📊 CustomStopWords.xlsx           # Lista de stopwords personalizadas
├── 📊 sentimientos_2.txt             # Diccionario de sentimientos UNALM
├── 📊 corpus_por_seccion.rds         # Corpus procesado por secciones
├── 📊 corpus_procesado.rds           # Corpus procesado completo
├── 📄 resumen_analisis.txt           # Resumen estadístico del análisis
└── 📁 Gráficos/
    ├── lollipop_frecuencias.png      # Top 15 palabras más frecuentes
    ├── grafico_sentimientos.png      # Distribución de sentimientos
    ├── grafico_bigramas.png          # Top 15 bigramas
    ├── red_bigramas.png              # Red de bigramas
    ├── grafico_correlaciones.png     # Red de correlaciones
    ├── evolucion_palabras_propuesta.png  # Evolución por sección
    └── objetivo.png                  # Nube de palabras
```

## 📈 Principales Hallazgos

### 🔤 Palabras Clave
| Palabra | Frecuencia |
|---------|------------|
| UNALM | 29 |
| Fortaleceremos | 17 |
| Estudiante | 17 |
| Docente | 17 |
| Investigación | 15 |

### 💭 Distribución de Sentimientos
- **Positivo**: 54.6% (316 palabras)
- **Confianza**: 26.3% (152 palabras)
- **Premonición**: 8.6% (50 palabras)
- Más del **70%** del contenido transmite positividad y confianza

### 🔗 Bigramas Más Frecuentes
1. Responsabilidad social
2. Actividades culturales
3. Recursos necesarios
4. Proyección social
5. Formación académica

## 🔧 Librerías Utilizadas

```r
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(wordcloud2)
library(pdftools)
library(widyr)
library(ggraph)
library(igraph)
library(scales)
library(stopwords)
library(paletteer)
library(readxl)
```

## 🚀 Cómo Ejecutar

1. **Clonar el repositorio**
   ```bash
   git clone https://github.com/JavierAnthonyUS/PC2TED171125.git
   ```

2. **Abrir el proyecto en RStudio**

3. **Instalar dependencias**
   ```r
   install.packages("pacman")
   pacman::p_load(tidyverse, tidytext, tm, wordcloud, wordcloud2, 
                  pdftools, widyr, ggraph, igraph, scales, 
                  stopwords, paletteer, readxl)
   ```

4. **Ejecutar el análisis**
   ```r
   source("01_lectura_limpieza.R")
   ```

## 👥 Autores
Soto Ortega, Fiorella Belen & **Uraco Silva, Javier Anthony**

## 🏫 Información Académica

- **Universidad**: Universidad Nacional Agraria La Molina (UNALM)
- **Departamento**: Estadística e Informática
- **Curso**: Técnicas de Exploración de Datos
- **Docente**: Jesús Salinas

## 📜 Licencia

Este proyecto es de uso académico. Los datos analizados son de dominio público (propuesta electoral).

---

<p align="center">
  <a href="https://github.com/JavierAnthonyUS">
    <img src="https://img.shields.io/badge/GitHub-JavierAnthonyUS-black?style=flat-square&logo=github" alt="GitHub">
  </a>
  <a href="https://www.linkedin.com/in/javier-anthony-uraco-silva-477334291">
    <img src="https://img.shields.io/badge/LinkedIn-Javier_Anthony-blue?style=flat-square&logo=linkedin" alt="LinkedIn">
  </a>
</p>

<p align="center">
  <em>Universidad Nacional Agraria La Molina - 2025</em>
</p>
