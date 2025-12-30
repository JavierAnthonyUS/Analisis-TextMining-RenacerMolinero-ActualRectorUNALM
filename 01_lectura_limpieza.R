#==============================================================================#
#  ANÁLISIS DE TEXTO: PLAN DE GOBIERNO "RENACER MOLINERO"                    #
#  UNALM - Elecciones Rectorales 2026-2031                                   #
#  VERSIÓN OPTIMIZADA - 11 GRÁFICOS POR SECCIÓN TEMÁTICA                     #
#==============================================================================#

rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
cat("\014")
options(scipen = 999)
options(digits = 3)

#==============================================================================#
# PASO 1: CARGAR PAQUETES                                                     #
#==============================================================================#
library(pacman)
p_load(tidyverse, tidytext, tm, wordcloud, wordcloud2, 
       pdftools, widyr, dplyr, magrittr, readxl, 
       forcats, stringr, ggraph, igraph, scales, stopwords, paletteer)

#==============================================================================#
# PASO 2: PREPARACIÓN DE DATOS - CORPUS DUAL                                  #
#==============================================================================#

#------------------------------------------------------------------------------#
# 2.1: LECTURA Y PREPARACIÓN CORPUS POR SECCIONES TEMÁTICAS (CORREGIDO)     #
#------------------------------------------------------------------------------#
# Leer el archivo PDF
plan_con_titulos <- pdf_text("RENACER MOLINERO.pdf")

# Convertir de páginas a líneas individuales
plan_con_titulos <- unlist(strsplit(plan_con_titulos, "\n"))

#--- LIMPIEZA BÁSICA (sin eliminar subtítulos) ---#

# Eliminar encabezados repetitivos PERO NO LOS SUBTÍTULOS
plan_con_titulos <- plan_con_titulos[!grepl("PROPUESTA DE PLAN DE GESTIÓN 2026-2031", plan_con_titulos)]
plan_con_titulos <- plan_con_titulos[!grepl("\"RENACER MOLINERO\"", plan_con_titulos)]
plan_con_titulos <- plan_con_titulos[!grepl("RENACER MOLINERO", plan_con_titulos)]
plan_con_titulos <- plan_con_titulos[!grepl("^\\s*\\d+\\s*$", plan_con_titulos)]  # Números de página

# Convertir a tibble y eliminar líneas vacías
plan_con_titulos_df <- tibble(texto = plan_con_titulos) %>% 
  filter(texto != "" & !grepl("^\\s*$", texto))

#------------------------------------------------------#
# SECCIONAR EL DOCUMENTO POR SUBTÍTULOS               #
#------------------------------------------------------#

# Identificar las posiciones de cada subtítulo
idx_principios <- which(grepl("^\\s*I\\.2\\s+", plan_con_titulos_df$texto))
idx_gestion <- which(grepl("^\\s*2\\.1\\s+", plan_con_titulos_df$texto))
idx_docente <- which(grepl("^\\s*2\\.2\\s+", plan_con_titulos_df$texto))
idx_estudiantes <- which(grepl("^\\s*2\\.3\\s+", plan_con_titulos_df$texto))
idx_no_docente <- which(grepl("^\\s*2\\.4\\s+", plan_con_titulos_df$texto))
idx_aprendizaje <- which(grepl("^\\s*2\\.5\\s+", plan_con_titulos_df$texto))
idx_investigacion <- which(grepl("^\\s*2\\.6\\s+", plan_con_titulos_df$texto))
idx_social <- which(grepl("^\\s*2\\.7\\s+", plan_con_titulos_df$texto))
idx_innovacion <- which(grepl("^\\s*2\\.8\\s+", plan_con_titulos_df$texto))
idx_financieros <- which(grepl("^\\s*2\\.9\\s+", plan_con_titulos_df$texto))
idx_seguridad <- which(grepl("^\\s*2\\.10\\s+", plan_con_titulos_df$texto))

#--- FUNCIÓN AUXILIAR PARA EXTRAER SECCIONES ---#
extraer_seccion <- function(inicio, fin, nombre_seccion) {
  if (length(inicio) == 0 || length(fin) == 0) {
    cat(sprintf("⚠ %s: NO SE PUDO EXTRAER\n", nombre_seccion))
    return(tibble(texto = character(0), seccion = character(0)))
  }
  
  seccion <- plan_con_titulos_df %>%
    slice((inicio + 1):(fin - 1)) %>%
    filter(texto != "" & !grepl("^\\s*$", texto)) %>%
    mutate(seccion = nombre_seccion)
  
  cat(sprintf("✓ %s: %d líneas extraídas\n", nombre_seccion, nrow(seccion)))
  return(seccion)
}

# Función especial para la última sección (Seguridad)
extraer_ultima_seccion <- function(inicio, nombre_seccion) {
  if (length(inicio) == 0) {
    cat(sprintf("⚠ %s: NO SE PUDO EXTRAER\n", nombre_seccion))
    return(tibble(texto = character(0), seccion = character(0)))
  }
  
  seccion <- plan_con_titulos_df %>%
    slice((inicio + 1):nrow(plan_con_titulos_df)) %>%
    filter(texto != "" & !grepl("^\\s*$", texto)) %>%
    mutate(seccion = nombre_seccion)
  
  cat(sprintf("✓ %s: %d líneas extraídas\n", nombre_seccion, nrow(seccion)))
  return(seccion)
}

#--- EXTRAER TODAS LAS SECCIONES ---#

seccion_1 <- extraer_seccion(idx_principios, idx_gestion, "Principios y Valores")
seccion_2 <- extraer_seccion(idx_gestion, idx_docente, "Gestión Institucional")
seccion_3 <- extraer_seccion(idx_docente, idx_estudiantes, "Personal Docente")
seccion_4 <- extraer_seccion(idx_estudiantes, idx_no_docente, "Estudiantes")
seccion_5 <- extraer_seccion(idx_no_docente, idx_aprendizaje, "Personal No Docente")
seccion_6 <- extraer_seccion(idx_aprendizaje, idx_investigacion, "Proceso Aprendizaje-Enseñanza")
seccion_7 <- extraer_seccion(idx_investigacion, idx_social, "Investigación")
seccion_8 <- extraer_seccion(idx_social, idx_innovacion, "Responsabilidad Social")
seccion_9 <- extraer_seccion(idx_innovacion, idx_financieros, "Innovación y Transferencia")
seccion_10 <- extraer_seccion(idx_financieros, idx_seguridad, "Recursos Financieros")
seccion_11 <- extraer_ultima_seccion(idx_seguridad, "Seguridad")

#--- COMBINAR TODAS LAS SECCIONES EN UN SOLO DATAFRAME ---#
plan_con_secciones <- bind_rows(
  seccion_1, seccion_2, seccion_3, seccion_4, seccion_5, seccion_6,
  seccion_7, seccion_8, seccion_9, seccion_10, seccion_11
)

print(plan_con_secciones %>% count(seccion, sort = TRUE))

# Nombres de secciones (para usar en análisis posteriores)
nombres_secciones <- c(
  "Principios y Valores",
  "Gestión Institucional",
  "Personal Docente",
  "Estudiantes",
  "Personal No Docente",
  "Proceso Aprendizaje-Enseñanza",
  "Investigación",
  "Responsabilidad Social",
  "Innovación y Transferencia",
  "Recursos Financieros",
  "Seguridad"
)

# Verificación de calidad
for(nombre in nombres_secciones) {
  n_lineas <- sum(plan_con_secciones$seccion == nombre, na.rm = TRUE)
  cat(sprintf("  %-35s: %4d líneas\n", nombre, n_lineas))
}

# Ejemplo: Ver el contenido de una sección
muestra_gestion <- plan_con_secciones %>% 
  filter(seccion == "Gestión Institucional") %>% 
  head(5)
if (nrow(muestra_gestion) > 0) {
  print(muestra_gestion)
}

#------------------------------------------------------------------------------#
# 2.2: LECTURA Y PREPARACIÓN CORPUS GENERAL (sin títulos)                    #
#------------------------------------------------------------------------------#

# Leer PDF para análisis general
plan_gobierno <- pdf_text("RENACER MOLINERO.pdf")
plan_gobierno <- unlist(strsplit(plan_gobierno, "\n"))

# Limpieza: eliminar encabezados, títulos, índices
plan_gobierno <- plan_gobierno[!grepl("PROPUESTA\\s+DE\\s+PLAN\\s+DE\\s+GESTIÓN", plan_gobierno)]
plan_gobierno <- plan_gobierno[!grepl("RENACER MOLINERO", plan_gobierno)]
plan_gobierno <- plan_gobierno[!grepl("BASES LEGALES", plan_gobierno)]
plan_gobierno <- plan_gobierno[!grepl("^\\s*I\\.\\d+", plan_gobierno)]
plan_gobierno <- plan_gobierno[!grepl("^\\s*\\d+\\.\\d+", plan_gobierno)]
plan_gobierno <- plan_gobierno[!grepl("^\\s*\\d+\\s*$", plan_gobierno)]

#------------------------------------------------------------------------------#
# 2.3: CARGAR STOPWORDS                                                       #
#------------------------------------------------------------------------------#

# Stopwords NLTK
stopwords_nltk <- tibble(Token = stopwords::stopwords("es", source = "nltk"))

# Stopwords personalizadas (si existe)
if(file.exists("CustomStopWords.xlsx")) {
  stopwords_custom <- read_excel("CustomStopWords.xlsx")
  names(stopwords_custom)[1] <- "Token"
  cat("✓ Stopwords personalizadas cargadas\n")
} else {
  stopwords_custom <- tibble(Token = character())
}

# Stopwords técnicas
stopwords_tecnicas <- tibble(
  Token = c("mm", "cm", "km", "etc", "ej", "pág", "pp", "n", "ª", "°")
)

# Combinar y eliminar duplicados
stopwords_es <- bind_rows(stopwords_nltk, stopwords_custom, stopwords_tecnicas) %>%
  distinct(Token)

#------------------------------------------------------------------------------#
# 2.4: TOKENIZAR CORPUS GENERAL                                               #
#------------------------------------------------------------------------------#

plan_tokens <- tibble(texto = plan_gobierno) %>% 
  slice(-(1:17)) %>%
  unnest_tokens(Token, texto) %>% 
  mutate(Token = removeNumbers(Token)) %>% 
  filter(Token != "")

# Limpiar stopwords
plan_limpio <- plan_tokens %>% 
  anti_join(stopwords_es, by = "Token") %>%
  filter(str_length(Token) > 2)

# Normalizar
plan_limpio <- plan_limpio %>%
  mutate(
    Token = str_replace(Token, "universidad", "unalm"),
    Token = str_replace(Token, "docentes", "docente"),
    Token = str_replace(Token, "estudiantes", "estudiante"),
    Token = str_replace(Token, "internacionales", "internacional"),
    Token = str_replace(Token, "investigadores", "investigador"),
    Token = str_replace(Token, "mejores", "mejor"),
    Token = str_replace(Token, "garanticen", "garantice"),
    Token = str_replace(Token, "instituciones", "institución"),
    Token = str_replace(Token, "institucionales", "institucional"),
    Token = str_replace(Token, "servicios", "servicio"),
    Token = str_replace(Token, "actividades", "actividad"),
    Token = str_replace(Token, "activas", "activo"),
    Token = str_replace(Token, "recursos", "recurso"),
    Token = str_replace(Token, "investigaciones", "investigación")
  )

#------------------------------------------------------------------------------#
# 2.5: TOKENIZAR CORPUS POR SECCIONES                                         #
#------------------------------------------------------------------------------#

# NUEVO (FUNCIONA CON AMBOS ENFOQUES)
plan_tokens_por_seccion <- plan_con_secciones %>%
  unnest_tokens(Token, texto) %>%
  mutate(Token = removeNumbers(Token)) %>%
  filter(Token != "")

# La columna 'seccion' ya viene incluida de plan_con_secciones

# Limpiar stopwords
plan_limpio_por_seccion <- plan_tokens_por_seccion %>%
  anti_join(stopwords_es, by = "Token") %>%
  filter(str_length(Token) > 2)

# Normalizar (mismo proceso)
plan_limpio_por_seccion <- plan_limpio_por_seccion %>%
  mutate(
    Token = str_replace(Token, "universidad", "unalm"),
    Token = str_replace(Token, "docentes", "docente"),
    Token = str_replace(Token, "estudiantes", "estudiante"),
    Token = str_replace(Token, "internacionales", "internacional"),
    Token = str_replace(Token, "investigadores", "investigador"),
    Token = str_replace(Token, "mejores", "mejor"),
    Token = str_replace(Token, "garanticen", "garantice"),
    Token = str_replace(Token, "instituciones", "institución"),
    Token = str_replace(Token, "institucionales", "institucional"),
    Token = str_replace(Token, "servicios", "servicio"),
    Token = str_replace(Token, "actividades", "actividad"),
    Token = str_replace(Token, "activas", "activo"),
    Token = str_replace(Token, "recursos", "recurso"),
    Token = str_replace(Token, "investigaciones", "investigación")
  )

print(plan_limpio_por_seccion %>% count(seccion, sort = TRUE))

#------------------------------------------------------------------------------#
# 2.6: GUARDAR RESULTADOS                                                     #
#------------------------------------------------------------------------------#

write_lines(paste(plan_gobierno, collapse = "\n"), "texto_limpio.txt")
saveRDS(plan_limpio, "corpus_procesado.rds")
saveRDS(plan_limpio_por_seccion, "corpus_por_seccion.rds")

#==============================================================================#
# SECCIÓN 1: ANÁLISIS DE FRECUENCIAS                                          #
#==============================================================================#

#------------------------------------------------------------------------------#
# PASO 1: CALCULAR FRECUENCIAS                                                #
#------------------------------------------------------------------------------#

frecuencias <- plan_limpio %>%
  count(Token, sort = TRUE)



#------------------------------------------------------------------------------#
# GRÁFICO 1: LOLLIPOP CHART (MODIFICADO - estilo RMD)                         #
#------------------------------------------------------------------------------#

grafico_lollipop <- frecuencias %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ggplot(aes(x = fct_reorder(Token, n), y = n)) +
  
  geom_segment(aes(x = Token, xend = Token, y = 0, yend = n),
               linewidth = 4,
               color = "deepskyblue") +
  
  geom_point(size = 8, color = "deepskyblue") +
  
  geom_text(aes(label = n),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  
  labs(
    x = NULL,
    y = "Frecuencia",
    title = "Top 15 palabras más frecuentes - RENACER MOLINERO",
    subtitle = "Plan de Gobierno 2026-2031"
  ) +
  
  coord_flip() +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(linewidth = 1.2, color = "black"),
    axis.text.y = element_text(size = 11),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.ticks.x = element_line(),
    axis.title.x = element_text(size = 11),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

print(grafico_lollipop)

ggsave("lollipop_frecuencias.png", width = 10, height = 6, dpi = 300, bg = "white")

#------------------------------------------------------------------------------#
# GRÁFICO 2: WORDCLOUD                           #
#------------------------------------------------------------------------------#

# Filtrar la palabra "aún" de las frecuencias para el wordcloud
frecuencias_wordcloud <- frecuencias %>%
  filter(Token != "aún")

png("wordcloud.png", width = 1500, height = 1200, res = 150, bg = "white")
par(mar = c(1, 1, 1, 1))

set.seed(2024)
wordcloud(words = frecuencias_wordcloud$Token,
          freq = frecuencias_wordcloud$n,
          min.freq = 2,
          max.words = 150,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale = c(4, 0.5))

dev.off()

#==============================================================================#
# SECCIÓN 2: ANÁLISIS DE SENTIMIENTOS                                         #
#==============================================================================#

#------------------------------------------------------------------------------#
# PASO 1: CARGAR LÉXICO DE SENTIMIENTOS                                       #
#------------------------------------------------------------------------------#

sentimientos <- read.delim("sentimientos_2.txt")
sentimientos <- as_tibble(sentimientos)
sentimientos <- distinct(sentimientos)

#------------------------------------------------------------------------------#
# PASO 2: AGREGAR TÉRMINOS CONTEXTUALIZADOS UNALM                             #
#------------------------------------------------------------------------------#

terminos_unalm <- tibble(
  palabra = c(
    # Positivos - Gestión universitaria (37 palabras)
    "fortalecer", "fortaleceremos", "modernizar", "modernizaremos",
    "implementar", "implementaremos", "garantizar", "garantizaremos",
    "promover", "promoveremos", "mejorar", "gestionar", "gestionaremos",
    "defender", "defenderemos", "proteger", "protegeremos",
    "incrementar", "desarrollar", "potenciar", "repotenciar",
    "alianzas", "estrategicas", "acreditacion", "calidad",
    "competitivos", "innovacion", "investigacion", "produccion",
    "transparente", "autonomia", "participacion", "bienestar",
    "excelencia", "formacion", "integral", "responsabilidad",
    
    # Nuevos términos UNALM-específicos (16 palabras)
    "sostenibilidad", "sostenible", "ambiental", "agrícola", "agro",
    "agroecológica", "agroecológico", "agrario", "agraria",
    "acreditación", "licenciamiento", "sunedu", "sineace",
    "empleabilidad", "competitividad", "internacionalización",
    
    # Negativos (11 palabras)
    "vulnerar", "vulnerados", "deficit", "carencia",
    "insuficiente", "limitado", "deterioro", "obsoleto",
    "burocracia", "trámites", "demora",
    
    # Términos técnicos universitarios (16 palabras)
    "irds", "facultades", "pregrado", "posgrado", "epg",
    "docente", "docentes", "estudiante", "estudiantes",
    "rector", "decano", "universitario", "universitaria",
    "egresados", "alumni", "graduados"
  ),
  
  sentimiento = c(
    rep("positivo", 37),
    rep("positivo", 16),
    rep("negativo", 11),
    rep("confianza", 16)
  )
)

# Combinar con diccionario base
sentimientos_completo <- bind_rows(sentimientos, terminos_unalm) %>%
  distinct(palabra, sentimiento, .keep_all = TRUE)

#------------------------------------------------------------------------------#
# PASO 2.5: MODIFICAR/AGREGAR SENTIMIENTOS PERSONALIZADOS                     #
#------------------------------------------------------------------------------#

# Palabras a modificar/agregar
palabras_modificadas <- tibble(
  palabra = c(
    "extranjero",
    "excelencia",
    "responsabilidad",
    "trabajo",
    "alto",
    "compromiso",
    "música",
    "ejecución",
    "arte",
    "participación",
    "vigilancia",
    "médico",
    "líneas",
    "inteligencia",
    "confianza",
    "aprovechar",
    "rendición",
    "fondo",
    "artificial",
    "aplicación",
    "respeto"
  ),
  sentimiento = c(
    "positivo",       # extranjero
    "positivo",       # excelencia
    "positivo",       # responsabilidad
    "positivo",       # trabajo
    "positivo",       # alto
    "positivo",       # compromiso
    "premonición",    # música
    "confianza",      # ejecución
    "premonición",    # arte
    "positivo",       # participación
    "confianza",      # vigilancia
    "positivo",       # médico
    "neutro",         # líneas
    "neutro",         # inteligencia
    "positivo",       # confianza
    "positivo",       # aprovechar
    "confianza",      # rendición (SOLO confianza)
    "positivo",       # fondo
    "neutro",         # artificial
    "positivo",       # aplicación
    "positivo"        # respeto
  )
)

# IMPORTANTE: Eliminar completamente estas palabras del diccionario (todos sus sentimientos)
sentimientos_completo <- sentimientos_completo %>%
  filter(!palabra %in% palabras_modificadas$palabra)

# Agregar SOLO las versiones modificadas (una única entrada por palabra)
sentimientos_completo <- bind_rows(sentimientos_completo, palabras_modificadas) %>%
  distinct(palabra, sentimiento, .keep_all = TRUE)

#------------------------------------------------------------------------------#
# PASO 3: ANÁLISIS DE SENTIMIENTOS                                            #
#------------------------------------------------------------------------------#

plan_sentimientos <- plan_limpio %>% 
  inner_join(sentimientos_completo, by = c("Token" = "palabra"))

conteo_sentimientos <- plan_sentimientos %>% 
  count(sentimiento, sort = TRUE)

print(conteo_sentimientos)

#------------------------------------------------------------------------------#
# DEFINIR COLORES PERSONALIZADOS PARA SENTIMIENTOS (NUEVO)                    #
#------------------------------------------------------------------------------#

colores_sentimientos <- c(
  "positivo"     = "#4CAF50",
  "negativo"     = "#F44336",
  "confianza"    = "#2196F3",
  "anticipación" = "#FF9800", 
  "premonición"  = "#FF9800",
  "alegría"      = "#FFEB3B", 
  "asombro"      = "#9C27B0", 
  "sorpresa"     = "#9C27B0",
  "enojo"        = "#E91E63", 
  "ira"          = "#E91E63",
  "tristeza"     = "#607D8B",
  "miedo"        = "#795548",
  "disgusto"     = "#8BC34A",
  "neutro"       = "#9E9E9E"
)

#------------------------------------------------------------------------------#
# GRÁFICO 3: DISTRIBUCIÓN DE SENTIMIENTOS (CON ETIQUETAS DE FRECUENCIA)       #
#------------------------------------------------------------------------------#

# Definir colores para las categorías de sentimiento
colores_sentimientos <- c(
  "positivo"     = "#4CAF50",
  "negativo"     = "#F44336",
  "confianza"    = "#2196F3",
  "anticipación" = "#FF9800", 
  "premonición"  = "#FF9800",
  "alegría"      = "#FFEB3B", 
  "tristeza"     = "#607D8B",
  "asombro"      = "#9C27B0", 
  "sorpresa"     = "#9C27B0",
  "enojo"        = "#E91E63", 
  "ira"          = "#E91E63",
  "miedo"        = "#795548",
  "disgusto"     = "#8BC34A",
  "neutro"       = "#9E9E9E"
)

grafico_sentimientos <- plan_sentimientos %>% 
  count(sentimiento) %>% 
  ggplot(aes(x = fct_reorder(sentimiento, n), y = n, fill = sentimiento)) + 
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = n), 
            hjust = -0.2, 
            size = 4, 
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Distribución de sentimientos - RENACER MOLINERO",
       subtitle = "Análisis de emociones en el plan de gobierno",
       x = "Sentimiento",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_sentimientos) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

print(grafico_sentimientos)

ggsave("grafico_sentimientos.png", width = 10, height = 6, dpi = 300, bg = "white")

#------------------------------------------------------------------------------#
# GRÁFICO 4: TOP 5 PALABRAS POR SENTIMIENTO - FACETAS (CON ETIQUETAS)         #
#------------------------------------------------------------------------------#

top_palabras_sentimiento_facetas <- plan_sentimientos %>%
  count(sentimiento, Token, sort = TRUE) %>%
  group_by(sentimiento) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup()

grafico_palabras_sentimiento_facetas <- top_palabras_sentimiento_facetas %>%
  ggplot(aes(x = n, 
             y = reorder_within(Token, n, sentimiento), 
             fill = sentimiento)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), 
            hjust = -0.2, 
            size = 4, 
            fontface = "bold") +
  facet_wrap(~sentimiento, scales = "free_y", ncol = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = colores_sentimientos) +
  labs(title = "Top 5 palabras por sentimiento - RENACER MOLINERO",
       subtitle = "Análisis detallado de emociones por palabra",
       x = "Frecuencia",
       y = NULL) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray85", color = NA),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

print(grafico_palabras_sentimiento_facetas)

ggsave("top_palabras_sentimiento_facetas.png", 
       width = 14, height = 10, dpi = 300, bg = "white")

#==============================================================================#
# SECCIÓN 3: ANÁLISIS DE BIGRAMAS                                             #
#==============================================================================#

#------------------------------------------------------------------------------#
# PASO 1: CREAR BIGRAMAS                                                      #
#------------------------------------------------------------------------------#

# Reconstruir texto completo para bigramas
texto_para_bigramas <- paste(plan_gobierno, collapse = " ")

# Tokenizar en bigramas (pares de palabras)
plan_bigramas <- tibble(texto = texto_para_bigramas) %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2)

# Separar los bigramas en dos columnas
bigramas_separados <- plan_bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ")

# Filtrar bigramas sin stopwords
bigramas_filtrados <- bigramas_separados %>%
  filter(!palabra1 %in% stopwords_es$Token) %>%
  filter(!palabra2 %in% stopwords_es$Token) %>%
  filter(str_length(palabra1) > 2) %>%
  filter(str_length(palabra2) > 2)

# Contar bigramas más frecuentes
bigramas_unidos <- bigramas_filtrados %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

print(head(bigramas_unidos, 10))

#------------------------------------------------------------------------------#
# GRÁFICO 5: TOP 15 BIGRAMAS
#------------------------------------------------------------------------------#

cat("--- Generando gráfico de bigramas ---\n")

# Filtrar bigramas que contengan "resolución"
top_bigramas <- bigramas_filtrados %>%
  filter(palabra1 != "resolución" & palabra2 != "resolución") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  head(15)

grafico_bigramas <- top_bigramas %>%
  ggplot(aes(x = n, y = reorder(bigrama, n), fill = n)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Top 15 bigramas más frecuentes - RENACER MOLINERO",
       subtitle = "Pares de palabras que aparecen juntas",
       x = "Frecuencia",
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

print(grafico_bigramas)

ggsave("grafico_bigramas.png", width = 10, height = 8, dpi = 300, bg = "white")

#------------------------------------------------------------------------------#
# GRÁFICO 6: RED DE BIGRAMAS
#------------------------------------------------------------------------------#

# Filtrar bigramas más frecuentes para la red
# Excluir bigramas que contengan "resolución"
bigramas_red <- bigramas_filtrados %>%
  filter(palabra1 != "resolución" & palabra2 != "resolución") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 2) %>%
  head(50)

# Crear el grafo
grafo_bigramas <- bigramas_red %>%
  graph_from_data_frame()

# Crear la red
set.seed(2024)

grafico_red_bigramas <- ggraph(grafo_bigramas, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = arrow(type = "closed", length = unit(2, "mm")),
                 end_cap = circle(3, "mm"),
                 edge_colour = "steelblue") +
  geom_node_point(color = "darkblue", size = 5) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 3.5,
                 fontface = "bold") +
  labs(title = "Red de bigramas - RENACER MOLINERO",
       subtitle = "Conexiones entre palabras que aparecen juntas") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

print(grafico_red_bigramas)

ggsave("red_bigramas.png", width = 12, height = 10, dpi = 300, bg = "white")

#==============================================================================#
# SECCIÓN 4: ANÁLISIS TF-IDF POR PROPUESTA                                    #
#==============================================================================#

#------------------------------------------------------------------------------#
# PASO 1: CALCULAR TF-IDF POR SECCIÓN TEMÁTICA                                #
#------------------------------------------------------------------------------#

# Calcular TF-IDF usando el corpus por secciones
plan_tfidf <- plan_limpio_por_seccion %>%
  count(seccion, Token) %>%
  bind_tf_idf(Token, seccion, n) %>%
  arrange(desc(tf_idf))

print(head(plan_tfidf, 10))

#------------------------------------------------------------------------------#
# GRÁFICO 7: TF-IDF POR PROPUESTA (FACETAS) - Top 5 por sección               #
#------------------------------------------------------------------------------#

# Top 5 palabras TF-IDF por sección
tfidf_propuestas <- plan_tfidf %>%
  group_by(seccion) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup()

# Paleta de colores para las 11 secciones
colores_secciones <- c(
  "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", 
  "#9B59B6", "#1ABC9C", "#E67E22", "#34495E",
  "#16A085", "#C0392B", "#8E44AD"
)

grafico_tfidf <- tfidf_propuestas %>%
  ggplot(aes(x = tf_idf,
             y = reorder_within(Token, tf_idf, seccion),
             fill = seccion)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~seccion, scales = "free_y", ncol = 3) +
  scale_y_reordered() +
  scale_fill_manual(values = colores_secciones) +
  labs(title = "Palabras características por propuesta (TF-IDF)",
       subtitle = "Términos distintivos de cada sección del plan de gobierno",
       x = "TF-IDF (importancia relativa)",
       y = NULL) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "#2C3E50", color = NA),
    strip.text = element_text(face = "bold", size = 9, color = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  )

print(grafico_tfidf)

ggsave("tfidf_por_propuesta.png", width = 14, height = 12, dpi = 300, bg = "white")

#==============================================================================#
# SECCIÓN 5: ANÁLISIS POR SECCIÓN TEMÁTICA                                    #
#==============================================================================#

#------------------------------------------------------------------------------#
# GRÁFICO 8: EVOLUCIÓN DE PALABRAS CLAVE POR PROPUESTA
#------------------------------------------------------------------------------#

# Crear índice numérico para las secciones (orden lógico)
orden_secciones <- tibble(
  seccion = nombres_secciones,
  orden = 1:length(nombres_secciones)
)

# Palabras clave a trackear
palabras_trackear <- c("unalm", "gestión", "investigación", 
                       "estudiante", "docente", "desarrollo")

# Definir colores institucionales UNALM (NUEVO)
colores_unalm <- c(
  "unalm"         = "#458e5d",
  "gestión"       = "#8f5444",
  "investigación" = "#db241c",
  "estudiante"    = "#3482d4",
  "docente"       = "#fff700",
  "desarrollo"    = "#181414"
)

grafico_evolucion_palabras <- plan_limpio_por_seccion %>%
  filter(Token %in% palabras_trackear) %>%
  count(seccion, Token) %>%
  left_join(orden_secciones, by = "seccion") %>%
  ggplot(aes(x = orden, y = n, color = Token, group = Token)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:length(nombres_secciones),
    labels = function(x) {
      sapply(x, function(i) {
        if(i <= length(nombres_secciones)) {
          # Abreviar nombres largos
          nombre <- nombres_secciones[i]
          if(nchar(nombre) > 15) {
            palabras <- strsplit(nombre, " ")[[1]]
            paste(substr(palabras, 1, 4), collapse = ".")
          } else {
            nombre
          }
        } else ""
      })
    }
  ) +
  scale_color_manual(values = colores_unalm) +
  labs(title = "Evolución de palabras clave por propuesta",
       subtitle = "Tracking de términos importantes en cada sección",
       x = "Sección del plan de gobierno", 
       y = "Frecuencia", 
       color = "Palabra") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico_evolucion_palabras)

ggsave("evolucion_palabras_propuesta.png", 
       width = 12, height = 7, dpi = 300, bg = "white")

#------------------------------------------------------------------------------#
# GRÁFICO 9: SENTIMIENTOS POR PROPUESTA (BARRAS APILADAS)
#------------------------------------------------------------------------------#

# Análisis de sentimientos por sección
sentimientos_por_seccion <- plan_limpio_por_seccion %>%
  inner_join(sentimientos_completo, by = c("Token" = "palabra")) %>%
  count(seccion, sentimiento) %>%
  left_join(orden_secciones, by = "seccion")

grafico_sentimientos_propuesta <- sentimientos_por_seccion %>%
  ggplot(aes(x = reorder(seccion, orden), y = n, fill = sentimiento)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = colores_sentimientos) +
  labs(title = "Distribución de sentimientos por propuesta",
       subtitle = "Proporción de emociones en cada sección del plan",
       x = NULL, 
       y = "Proporción", 
       fill = "Sentimiento") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "right"
  )

print(grafico_sentimientos_propuesta)

ggsave("sentimientos_por_propuesta.png", 
       width = 12, height = 7, dpi = 300, bg = "white")

#==============================================================================#
# SECCIÓN 6: CORRELACIONES ENTRE PALABRAS                                     #
#==============================================================================#

#------------------------------------------------------------------------------#
# PASO 1: CALCULAR CORRELACIONES                                              #
#------------------------------------------------------------------------------#

# Leer el texto limpio y dividir en líneas
plan_lineas <- tibble(texto = scan("texto_limpio.txt", 
                                   what = "character", 
                                   sep = "\n", 
                                   quiet = TRUE)) %>%
  mutate(linea = row_number()) %>%
  unnest_tokens(Token, texto) %>%
  anti_join(stopwords_es, by = "Token") %>%
  filter(str_length(Token) > 2)

# Calcular correlaciones (palabras que aparecen >= 5 veces)
correlaciones_palabras <- plan_lineas %>%
  group_by(Token) %>%
  filter(n() >= 5) %>%
  pairwise_cor(Token, linea, sort = TRUE)

print(head(correlaciones_palabras, 10))

#------------------------------------------------------------------------------#
# GRÁFICO 10: RED DE CORRELACIONES
#------------------------------------------------------------------------------#

set.seed(2024)

grafico_correlaciones <- correlaciones_palabras %>%
  filter(correlation > 0.15) %>%
  head(50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), 
                 edge_colour = "steelblue") +
  geom_node_point(size = 6, color = "#fff700") +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 3.5, 
                 fontface = "bold") +
  theme_void() +
  labs(title = "Red de correlaciones entre palabras - RENACER MOLINERO",
       subtitle = "Palabras que tienden a aparecer juntas en el documento") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

print(grafico_correlaciones)

ggsave("grafico_correlaciones.png", width = 12, height = 10, dpi = 300, bg = "white")

#==============================================================================#
# RESUMEN Y FINALIZACIÓN                                                      #
#==============================================================================#

#------------------------------------------------------------------------------#
# CREAR RESUMEN DEL ANÁLISIS                                                  #
#------------------------------------------------------------------------------#

resumen_analisis <- tibble(
  Metrica = c(
    "Total de palabras (tokens)",
    "Palabras únicas",
    "Palabras después de limpieza",
    "Stopwords eliminadas",
    "Bigramas únicos",
    "Sentimientos detectados",
    "Secciones temáticas analizadas"
  ),
  Valor = c(
    nrow(plan_tokens),
    n_distinct(plan_tokens$Token),
    nrow(plan_limpio),
    nrow(plan_tokens) - nrow(plan_limpio),
    nrow(bigramas_unidos),
    nrow(conteo_sentimientos),
    n_distinct(plan_limpio_por_seccion$seccion)
  )
)

print(resumen_analisis)

# Top 5 palabras más frecuentes

print(head(frecuencias, 5))

# Top 3 sentimientos

print(head(conteo_sentimientos, 3))

# Top 3 bigramas

print(head(bigramas_unidos, 3))

#------------------------------------------------------------------------------#
# GUARDAR RESUMEN EN ARCHIVO                                                  #
#------------------------------------------------------------------------------#

# Crear reporte de texto
reporte <- paste0(
  "=============================================================================\n",
  "  ANÁLISIS DE TEXTO: PLAN DE GOBIERNO RENACER MOLINERO                     \n",
  "  UNALM - Elecciones Rectorales 2026-2031                                  \n",
  "=============================================================================\n\n",
  
  "RESUMEN ESTADÍSTICO:\n",
  "-------------------\n",
  sprintf("Total de palabras (tokens): %d\n", nrow(plan_tokens)),
  sprintf("Palabras únicas: %d\n", n_distinct(plan_tokens$Token)),
  sprintf("Palabras después de limpieza: %d\n", nrow(plan_limpio)),
  sprintf("Stopwords eliminadas: %d\n", nrow(plan_tokens) - nrow(plan_limpio)),
  sprintf("Bigramas únicos: %d\n", nrow(bigramas_unidos)),
  sprintf("Sentimientos detectados: %d\n", nrow(conteo_sentimientos)),
  sprintf("Secciones temáticas analizadas: %d\n\n", n_distinct(plan_limpio_por_seccion$seccion)),
  
  "SECCIONES DEL PLAN DE GOBIERNO:\n",
  "-------------------------------\n"
)

for(i in seq_along(nombres_secciones)) {
  reporte <- paste0(reporte, sprintf("%d. %s\n", i, nombres_secciones[i]))
}

reporte <- paste0(reporte, "\n\nTOP 5 PALABRAS MÁS FRECUENTES:\n",
                  "------------------------------\n")

for(i in 1:5) {
  reporte <- paste0(reporte, 
                    sprintf("%d. %s (%d apariciones)\n", 
                            i, 
                            frecuencias$Token[i], 
                            frecuencias$n[i]))
}

reporte <- paste0(reporte, "\nTOP 3 SENTIMIENTOS:\n",
                  "------------------\n")

for(i in 1:min(3, nrow(conteo_sentimientos))) {
  reporte <- paste0(reporte,
                    sprintf("%d. %s (%d palabras)\n",
                            i,
                            conteo_sentimientos$sentimiento[i],
                            conteo_sentimientos$n[i]))
}

reporte <- paste0(reporte, "\nTOP 3 BIGRAMAS:\n",
                  "---------------\n")

for(i in 1:3) {
  reporte <- paste0(reporte,
                    sprintf("%d. %s (%d apariciones)\n",
                            i,
                            bigramas_unidos$bigrama[i],
                            bigramas_unidos$n[i]))
}

# Guardar reporte
writeLines(reporte, "resumen_analisis.txt")