
# 
# Programa Big Data y Business Intelligence 2019/2020
# Universidad de Deusto
# Modulo 2.2. Tecnología y Desarrollo en Big Data
# Proyecto: Resumen Automático de Textos legales
# 

rm(list = ls());cat("\014")

#library(sparklyr)
#library(sparklyr.nested)
#library(dplyr)
#sc <- spark_connect(master = "local")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Inicializaciones y carga de librerías ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
library(tidyverse)
library(tidytext)
library(textrank)
library(tm)
library(fulltext)
library(flextable)
}

# Carga de stopwords del paquete tm.
# stop_words_sp = copy_to(sc,tibble(word = tm::stopwords("spanish")))
stop_words_sp = tibble(word = tm::stopwords("spanish"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Funciones ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Función TextRank. Algoritmo de resumen extractivo de textos, basado en PageRank de Google.
{
TextRank <- function(data, terminology) {
  preambulo_summary <- textrank_sentences(data = data, 
                                          terminology = terminology)
}

# Función DataSentences. Devuelve un df con frases a las que se va a aplicar el algoritmo.
DataSentences <- function(sub_bloque){
  sentences <- sub_bloque %>%
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  return(sentences)
}

# Función Terminology (words sin stopwords y sin números). Devuelve un df con tokens(palabras
# que forman las frases) que van a ser usados por el algoritmo como conectores entre frases. 
Terminology <- function(sub_bloque) {
  # Lista de palabras con stopwords
  sub_bloque_words <- sub_bloque %>%
    unnest_tokens(word, sentence)
  # Lista de palabras sin stopwords y sin números
  sub_bloque_words <- sub_bloque_words %>%
    anti_join(stop_words_sp, by = "word") %>% 
    filter(!str_detect(word,"[[:digit:]]"))
  return(sub_bloque_words)
}
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Carga de datos ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Extracción del texto con paquete fulltext (obtenemos texto en páginas y metadatos)
{
path <- "data/BOE-A-1994-26003-consolidado_LAU.pdf"
#path <- "data/BOE-A-2019-3814-consolidado.pdf"

pdf_text <- ft_extract(path)
tmp <- c()
for (i in 1:length(pdf_text$data)){
  tmp <- str_c(tmp,pdf_text$data[i])
}
# Data frame 1x1 con todo el texto crudo
textdata <- tibble(text = tmp)
# textdata_tbl <- copy_to(sc, textdata, overwrite = TRUE)
}
# src_tbls(sc)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preprocesado general del texto ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
# Separamos el índice de la ley, quitamos la cabecera y textos repetitivos. 
texto <- str_split(textdata$text, ".+TEXTO CONSOLIDADO", n=2)[[1]][2]
# Eliminamos la cabecera del preámbulo:
texto <- str_split(texto, "PREAMBULO", n=2)[[1]][2]
# Eliminamos cabeceras y pies de página:
texto <- str_replace_all(texto, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
# Eliminamos indicadores alfabéticos de párrafo o línea tipo a), b),...
texto <- str_replace_all(texto, regex("\\s+[[:alpha:]]{1}\\)"), "")
# Eliminamos enumeraciones tipo 1.ª, 2.ª,...
texto <- str_replace_all(texto, regex("\\d+\\.ª(?=\\s[[:upper:]])"), "")
# Eliminamos enumeraciones tipo 10.1, 2.3,...
texto <- str_replace_all(texto, regex("\\d+\\.\\d+(?=\\s[[:upper:]])"), "")
# Eliminamos anomalía del texto tipo "...cuarta. 3.ª del..." que hace división de frase artificial.
texto <- str_replace_all(texto, regex("\\.(?=\\s\\d+\\.ª\\s[[:lower:]])"), "")
}
{
# Trabajo con el índice
indice <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][1]
indice <- stripWhitespace(indice)
indice_cabecera <- str_split(indice, regex("ÍNDICE"), n=2)[[1]][1]
indice <- str_split(indice, regex("ÍNDICE"), n=2)[[1]][2]
indice <- str_replace_all(indice, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
indice <- str_replace_all(indice, regex("\\.\\s\\d+"), "")
indice <- str_replace_all(indice, regex("(?<=\\.)\\s\\."), "")
indice <- stripWhitespace(indice)
indice <- str_replace_all(indice, regex("(?<=[IVXLCM])\\."), ":")
indice <- str_replace_all(indice, regex("(?<=Artículo\\s\\d{1,3})\\."), ":")
indice <- str_replace_all(indice, regex("\\s\\."), "\\.")

# Tabla de líneas de Índice
indice_lineas <- tibble(text = indice) %>% 
  unnest_tokens(linea, text, token = "sentences", to_lower = FALSE)
}
# Limpieza de entorno de trabajo.
rm(list = c("i","path","tmp"));cat("\014")

{
# Generamos la lista titulos que contiene Preámbulo, Títulos y Disposiciones.
titulos <- str_split(texto, "TÍTULO")
# Eliminamos espacios en blanco que pueda haber en exceso.
titulos[[1]] <- stripWhitespace(titulos[[1]])
# Obtenemos nº de documentos en que queda dividido en texto
tit_fin <- length(titulos[[1]])
# Separamos el último título de las disposiciones
titulo_F <- str_split(titulos[[1]][tit_fin],"Disposición")
titulos[[1]][tit_fin]<- titulo_F[[1]][1]
# Obtenemos bloque de Disposiciones para uso posterior
disposiciones <- titulo_F[[1]][2:length(titulo_F[[1]])]
rm(titulo_F)
}
{
# Inicializamos tablon: data frame para guardar frases resumen y otros datos relativos al texto
tablon <- tibble(bloque = character(), 
                 sub_bloque = character(), 
                 n_text_rank = integer(), 
                 frase = character())

frases_cabecera <- tibble(text = indice_cabecera) %>%
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%  
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

tablon <- tibble(bloque = "Cabecera", 
                 sub_bloque = NA, 
                 n_text_rank = NA, 
                 frase = frases_cabecera$sentence)
linea_indice <- 1
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preámbulo: procesado ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{
# Tomamos el preámbulo completo
preambulo <- titulos[[1]][1]
# Eliminamos espacio inicial
preambulo <- str_replace_all(preambulo, regex("^\\s+\\d"), "")
# Número de bloques del preámbulo
bloques_preambulo <- length(str_extract_all(preambulo, regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))[[1]])+1
# Lista de bloques del preámbulo
preambulo_bloques <- str_split(preambulo, regex("(?<=\\s)\\d(?=\\s[[:upper:]])"), n=bloques_preambulo)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preámbulo: extracción de frases con TextRank, por sub-bloques ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cálculo de n (nº de frases del resumen) para TextRank
# Tasa de compresión tau=20% (se considera óptimo 15-30%)
# Contador de frases de cada bloque del preámbulo
for (i in 1:bloques_preambulo) {
  # "Tokenizamos" el bloque en frases
  frases_token <- tibble(text = preambulo_bloques[[1]][i]) %>%
    unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%  
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  # Obtenemos número de frases que salen de la "tokenización"
  numero_frases <- frases_token %>%
    mutate(sentence_id = row_number()) %>%
    summarise(n = n())
  # Tenemos en cuenta que el número de frases sea significativo
  if (numero_frases > 1) {
    n <- as.integer(ceiling(0.2*numero_frases))
    sentences <- DataSentences(frases_token)
    words <- Terminology(frases_token)
    preambulo_summary <- TextRank(sentences, words)
    frases_seleccionadas <- summary(preambulo_summary, n=n, keep.sentence.order = TRUE)
    # Almacenamiento en el tablón
    tablon <- rbind(tablon, tibble(bloque = "Preámbulo", 
                                   sub_bloque = i, 
                                   n_text_rank = n, 
                                   frase = str_c(indice_lineas$linea[linea_indice]," ", i)))
    tabla_parcial <- tibble(bloque ="Preámbulo", 
                            sub_bloque = i, 
                            n_text_rank = n, 
                            frase = frases_seleccionadas )
    tablon <- rbind(tablon,tabla_parcial)
    # Si no es significativo (numero de frases = 1), se emplea la única frase existente
  } else {
    frases_seleccionadas <- frases_token$sentence
    # Almacenamiento en el tablón
    tablon <- rbind(tablon, tibble(bloque = "Preámbulo", 
                                   sub_bloque = i, 
                                   n_text_rank = n, 
                                   frase = str_c(indice_lineas$linea[linea_indice]," ", i)))
    tabla_parcial <- tibble(bloque ="Preámbulo", 
                            sub_bloque = i, 
                            n_text_rank = 1, 
                            frase = frases_seleccionadas )
    tablon <- rbind(tablon,tabla_parcial)
    
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Títulos-Capítulos-Artículos: procesado ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
# Almacenamos los artículos por títulos: cada elemento de la lista se corresponde con un Título
articulos_por_titulo <- list(NULL)
# Lista intermedia para guardar los títulos que se dividen en capítulos (no todos los títulos)
capitulo_bloques <- list(NULL)
# Almacena los bloques de Títulos quitando espacio inicial
for (i in 2:length(titulos[[1]])) {
  articulos_por_titulo[[i-1]] <- str_replace_all(titulos[[1]][i], regex("^\\s+[IVXLCM]+"), "")
}
# Para los Títulos divididos en Capítulos, separamos los capítulos, y los volvemos a guardar 
# en articulos_por_titulo.
for (i in 1:length(articulos_por_titulo)) {
  if (str_detect(articulos_por_titulo[[i]], regex("CAPÍTULO"))) {
    numero_capitulos <- str_count(articulos_por_titulo[[i]], regex("CAPÍTULO"))
    # Separación en bloques de capítulos, eliminando el comienzo
    capitulo_bloques <- str_split(articulos_por_titulo[[i]], 
                                  regex("(?<=\\s)CAPÍTULO(?=\\s+[IVXLCM]+)"), 
                                  n=numero_capitulos+1)[[1]][1:numero_capitulos+1]
    articulos_por_titulo[[i]][1:numero_capitulos] <- capitulo_bloques
  }
}
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Títulos-Capítulos-Artículos: extracción de frases con TextRank del bloque  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tenemos 3 bucles for para recorrer Títulos (i), Capítulos (j) y Artículos (k), la unidad
# de texto para TextRank
{
numero_articulo <- 0
for (i in 1:length(articulos_por_titulo)) {
  linea_indice <- linea_indice + 1
  tablon <- rbind(tablon, tibble(bloque = str_c("Título ", i), 
                                 sub_bloque = NA, 
                                 n_text_rank = NA, 
                                 frase = indice_lineas$linea[linea_indice]))
  for (j in 1:length(articulos_por_titulo[[i]])) {
    # Si hay Capítulos en el Título
    if (length(articulos_por_titulo[[i]]) > 1) {
      linea_indice <- linea_indice + 1
      tablon <- rbind(tablon,tibble(bloque = str_c("Título ", i, " Capítulo ", j), 
                                    sub_bloque = i, 
                                    n_text_rank = NA, 
                                    frase = indice_lineas$linea[linea_indice]))
    }
    # Número de bloques de cada título (para titulos con y sin capítulos)
    bloques_titulo <- length(str_extract_all(articulos_por_titulo[[i]][j], 
                                             regex("(?<=.)Artículo(?=\\s+[[:digit:]]+\\.)"))[[1]])+1
    # Lista de bloques
    titulo_bloques <- str_split(articulos_por_titulo[[i]][j], 
                                regex("(?<=.)Artículo(?=\\s+[[:digit:]]+\\.)"), 
                                n=bloques_titulo)[[1]][2:bloques_titulo]
    # En el caso de que bloques_titulos = 1, aparecen NAs
    titulo_bloques <- titulo_bloques[!is.na(titulo_bloques)]
    titulo_bloques <- list(str_replace_all(titulo_bloques, regex("\\s\\d+\\.(?=\\s[[:upper:]])"), ""))
    for (k in 1:(bloques_titulo-1)) {
      # Para evitar el caso en el que bloques_titulo = 1
      if (k >=1) {
        # "Tokenizamos" el artículo
        frases_token <- tibble(text = titulo_bloques[[1]][k]) %>%
          unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%  
          mutate(sentence_id = row_number()) %>%
          select(sentence_id, sentence) %>% 
          filter(sentence_id != 1) # Eliminamos la primera frase que se corresponde con el título del artículo
        # Cálculo del número de frases
        numero_frases <- frases_token %>%
          mutate(sentence_id = row_number()) %>%
          summarise(n = n())
        # Tenemos en cuenta que el número de frases sea significativo
        if (numero_frases > 1) {
          n <- as.integer(ceiling(0.2*numero_frases))
          sentences <- DataSentences(frases_token)
          words <- Terminology(frases_token)
          articulos_summary <- TextRank(sentences, words)
          frases_seleccionadas <- summary(articulos_summary, n=n, keep.sentence.order = TRUE)
          # Almacenamiento en el tablón
          linea_indice <- linea_indice + 1
          numero_articulo <- numero_articulo + 1
          # Si hay o no hay Capítulos
          if (length(articulos_por_titulo[[i]]) > 1) {
            tablon <- rbind(tablon,tibble(bloque = str_c("Título ", i, " Capítulo ", j, " Artículo ", numero_articulo), 
                                          sub_bloque = k, 
                                          n_text_rank = n, 
                                          frase = str_c(indice_lineas$linea[linea_indice])))
            tabla_parcial <- tibble(bloque =str_c("Título ", i, " Capítulo ", j, " Artículo ", numero_articulo), 
                                    sub_bloque = k, 
                                    n_text_rank = n, 
                                    frase = frases_seleccionadas )
            tablon <- rbind(tablon,tabla_parcial)
          } else {
            tablon <- rbind(tablon,tibble(bloque = str_c("Título ", i, " Artículo ", numero_articulo), 
                                          sub_bloque = k, 
                                          n_text_rank = n, 
                                          frase = str_c(indice_lineas$linea[linea_indice])))
            tabla_parcial <- tibble(bloque =str_c("Título ", i, " Artículo ", numero_articulo), 
                                    sub_bloque = k, 
                                    n_text_rank = n, 
                                    frase = frases_seleccionadas )
            tablon <- rbind(tablon,tabla_parcial)
          }
          # Si no es significativo (numero de frases = 1), se emplea la única frase existente
        } else {
          frases_seleccionadas <- frases_token$sentence
          # Almacenamiento en el tablón
          linea_indice <- linea_indice + 1
          numero_articulo <- numero_articulo + 1
          if (length(articulos_por_titulo[[i]]) > 1) {
            tablon <- rbind(tablon,tibble(bloque = str_c("Título ", i, " Capítulo ", j, " Artículo ", numero_articulo), 
                                          sub_bloque = k, 
                                          n_text_rank = 1, 
                                          frase = str_c(indice_lineas$linea[linea_indice])))
            tabla_parcial <- tibble(bloque =str_c("Título ", i, " Capítulo ", j, " Artículo ", numero_articulo), 
                                    sub_bloque = k, 
                                    n_text_rank = 1, 
                                    frase = frases_seleccionadas )
            tablon <- rbind(tablon,tabla_parcial)
          } else {
            tablon <- rbind(tablon,tibble(bloque = str_c("Título ", i, " Artículo ", numero_articulo), 
                                          sub_bloque = k, 
                                          n_text_rank = 1, 
                                          frase = str_c(indice_lineas$linea[linea_indice])))
            tabla_parcial <- tibble(bloque =str_c("Título ", i, " Artículo ", numero_articulo),
                                    sub_bloque = k, 
                                    n_text_rank = 1, 
                                    frase = frases_seleccionadas )
            tablon <- rbind(tablon,tabla_parcial)
          }
        }
      }
    }
  }
}

}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Disposiciones: procesado ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{
# Eliminamos firma final
disposiciones[length(disposiciones)] <- str_split(disposiciones[length(disposiciones)], 
                                                  regex("Por tanto, Mando"), n=2)[[1]][1]
# Extraemos el texto de las disposiciones sin los títulos
disposiciones_sin_titulo <- disposiciones
for (i in 1:length(disposiciones)) {
disposiciones_sin_titulo[i] <- str_split(disposiciones[i],
                                         regex("\\.\\s(?=[[:upper:]])"), n=3)[[1]][3]
}

# Número de diposiciones en el texto
bloques_disposiciones <- length(disposiciones_sin_titulo)
# Disposiciones en lista
disposiciones_bloques <- list(disposiciones_sin_titulo)  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Disposiciones: extracción de frases con TextRank, por sub-bloques ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cálculo de n (nº de frases del resumen) para TextRank
# Tasa de compresión tau=20% (se considera óptimo 15-30%)
# Contador de frases de cada bloque de las disposiciones
{
for (i in 1:bloques_disposiciones) {
  linea_indice <- linea_indice + 1
  # Cabeceras de los tres tipos de Disposiciones
  if (str_detect(indice_lineas$linea[linea_indice], "Disposiciones adicionales")) {
    tablon <- rbind(tablon,tibble(bloque = "Disposiciones adicionales", 
                                  sub_bloque = NA, 
                                  n_text_rank = NA, 
                                  frase = str_c(indice_lineas$linea[linea_indice])))
    tipo_disposicion <- "Disposiciones adicionales"
    linea_indice <- linea_indice + 1
  } 
  if (str_detect(indice_lineas$linea[linea_indice], "Disposiciones transitorias")) {
    tablon <- rbind(tablon,tibble(bloque = "Disposiciones transitorias", 
                                  sub_bloque = NA, 
                                  n_text_rank = NA, 
                                  frase = str_c(indice_lineas$linea[linea_indice])))
    tipo_disposicion <- "Disposiciones transitorias"
    linea_indice <- linea_indice + 1
  } 
  if (str_detect(indice_lineas$linea[linea_indice], "Disposiciones derogatorias")) {
    tablon <- rbind(tablon,tibble(bloque = "Disposiciones derogatorias", 
                                  sub_bloque = NA, 
                                  n_text_rank = NA, 
                                  frase = str_c(indice_lineas$linea[linea_indice])))
    tipo_disposicion <- "Disposiciones derogatorias"
    linea_indice <- linea_indice + 1
  } 
  if (str_detect(indice_lineas$linea[linea_indice], "Disposiciones finales")) {
    tablon <- rbind(tablon,tibble(bloque = "Disposiciones finales", 
                                  sub_bloque = NA, 
                                  n_text_rank = NA, 
                                  frase = str_c(indice_lineas$linea[linea_indice])))
    tipo_disposicion <- "Disposiciones finales"
    linea_indice <- linea_indice + 1
  }
  # "Tokenizamos" el bloque en frases
  frases_token <- tibble(text = disposiciones_bloques[[1]][i]) %>%
    unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%  
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  # Obtenemos número de frases que salen de la "tokenización"
  numero_frases <- frases_token %>%
    mutate(sentence_id = row_number()) %>%
    summarise(n = n())
  # Tenemos en cuenta que el número de frases sea significativo
  if (numero_frases > 1) {
    n <- as.integer(ceiling(0.2*numero_frases))
    sentences <- DataSentences(frases_token)
    words <- Terminology(frases_token)
    disposiciones_summary <- TextRank(sentences, words)
    frases_seleccionadas <- summary(disposiciones_summary, n=n, keep.sentence.order = TRUE)
    # Almacenamiento en el tablón
    tablon <- rbind(tablon,tibble(bloque = tipo_disposicion,
                                  sub_bloque = i, 
                                  n_text_rank = n, 
                                  frase = str_c(indice_lineas$linea[linea_indice]," ", indice_lineas$linea[linea_indice + 1])))
    linea_indice <- linea_indice + 1
    #tablon <- rbind(tablon,tibble(bloque = tipo_disposicion, sub_bloque = i, n_text_rank = n, frase = str_c(indice_lineas$linea[linea_indice])))
    tabla_parcial <- tibble(bloque = tipo_disposicion, 
                            sub_bloque = i,
                            n_text_rank = n, 
                            frase = frases_seleccionadas )
    tablon <- rbind(tablon,tabla_parcial)
    # Si no es significativo (numero de frases = 1), se emplea la única frase existente
  } else {
    frases_seleccionadas <- frases_token$sentence
    # Almacenamiento en el tablón
    tablon <- rbind(tablon,tibble(bloque = tipo_disposicion, 
                                  sub_bloque = i, 
                                  n_text_rank = 1,
                                  frase = str_c(indice_lineas$linea[linea_indice], " ", indice_lineas$linea[linea_indice + 1])))
    linea_indice <- linea_indice + 1
    # tablon <- rbind(tablon,tibble(bloque = tipo_disposicion, sub_bloque = i, n_text_rank = 1, frase = str_c(indice_lineas$linea[linea_indice])))
    tabla_parcial <- tibble(bloque = tipo_disposicion, 
                            sub_bloque = i, 
                            n_text_rank = 1, 
                            frase = frases_seleccionadas )
    tablon <- rbind(tablon,tabla_parcial)
  }
}

}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Presentación del resumen de la ley ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Usamos paquete flextable
{
t <- tibble(LAU = tablon$frase)
ft <- flextable(t) 
ft <- ft %>% 
  theme_alafoli() %>% 
  set_header_labels(LAU = "Resumen de la Ley de Arrendamientos Urbanos") %>% 
  fontsize(part = "body", size = 16) %>% 
  fontsize(part = "header", size = 20) %>% 
  align(align = "center", part = "all") %>% 
  autofit() %>% 
  font(fontname = "Arial") %>% 
  bold(i = ~ str_detect(ft$body$dataset$LAU,regex("^Preámbulo"))) %>% 
  bold(i = ~ str_detect(ft$body$dataset$LAU,regex("^TÍTULO"))) %>% 
  bold(i = ~ str_detect(ft$body$dataset$LAU,regex("^CAPÍTULO"))) %>% 
  bold(i = ~ str_detect(ft$body$dataset$LAU,regex("^Artículo"))) %>% 
  bold(i = ~ str_detect(ft$body$dataset$LAU,regex("^Disposici")))
ft
# Guardamos el resumen en un .docx reformateando los tamaños de las fuentes
ft <- fontsize(ft, part = "body", size = 12)
ft <- fontsize(ft, part = "header", size = 14)
save_as_docx(ft, path = "data/resumen_output.docx")
}

  
  
  
  