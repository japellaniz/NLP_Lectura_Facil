

# Inicializaciones y carga de librerías
library(tidyverse)
library(tidytext)
library(textrank)
library(readtext)
library(tm)
library(fulltext)

rm(list = ls());cat("\014.")

# Stopwords
stop_words_sp = tibble(word = tm::stopwords("spanish"))

##########################################################################
# Funciones
##########################################################################
# Función TextRank. Algoritmo de resumen extractivo de textos, basado en PageRank de Google.
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


# #########################################################################
# Carga de datos
# #########################################################################

# Extracción del texto con paquete fulltext (obtenemos texto en páginas y metadatos)
path <- "data/BOE-A-1994-26003-consolidado_LAU.pdf"
pdf_text <- ft_extract(path)
tmp <- c()
for (i in 1:length(pdf_text$data)){
  tmp <- str_c(tmp,pdf_text$data[i])
}
# Data frame 1x1 con todo el texto crudo
textdata <- tibble(text = tmp)


# ############################################################################
# Preprocesado general del texto 
# #############################################################################

# Eliminar el índice de la ley, la cabecera y textos repetitivos. 
# Eliminamos índice:
texto <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][2]
# Eliminamos la cabecera del preámbulo:
texto <- str_split(texto, "PREAMBULO", n=2)[[1]][2]
# Eliminamos cabeceras y pies de página:
texto <- str_replace_all(texto, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
# Eliminamos indicadores alfabéticos de párrafo o línea tipo a), b),...
texto <- str_replace_all(texto, regex("\\s+[[:alpha:]]{1}\\)"), "")
# Limpieza de entorno de trabajo.
rm(list = c("i","path","tmp"));cat("\014")


# Generamos la lista titulos que contiene Preambulo, Títulos y Disposiciones.
titulos <- str_split(texto, "TÍTULO")
# Eliminamos espacios en blanco que pueda haber en exceso.
titulos[[1]] <- stripWhitespace(titulos[[1]])

# Obtenemos nº de documentos en que queda dividido en texto
tit_fin <- length(titulos[[1]])
# Separamos el último título de las disposiciones
titulo_F <- str_split(titulos[[1]][tit_fin],"Disposición")
titulos[[1]][tit_fin]<- titulo_F[[1]][1]
disposiciones <- titulo_F[[1]][2:length(titulo_F[[1]])]
rm(titulo_F)

# Inicializamos tablon: data frame para guardar frases resumen y otros datos relativos al texto
tablon <- tibble(bloque = character(), sub_bloque = character(), n_text_rank = integer(), frase = character())


# #############################################################################
# Trabajamos con el Preámbulo
# #############################################################################


# Tomamos el preámbulo completo
preambulo <- titulos[[1]][1]
# Eliminamos espacio inicial
preambulo <- str_replace_all(preambulo, regex("^\\s+\\d"), "")
# Número de bloques del preámbulo
bloques_preambulo <- length(str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))[[1]])+1
# Lista de bloques del preámbulo
preambulo_bloques <- str_split(preambulo, regex("(?<=\\s)\\d(?=\\s[[:upper:]])"), n=bloques_preambulo)


##################################################################
# Extracción de frases con TextRank del Preámbulo, por sub-bloques
##################################################################
# Cálculo de n (nº de frases del resumen) para TextRank
# Tasa de compresión tau=20% (se considera óptimo 15-30%)
# Contador de frases de cada bloque del preámbulo
for (i in 1:bloques_preambulo) {
  # "Tokenizamos" el bloque en frases
  frases_token <- tibble(text = preambulo_bloques[[1]][i]) %>%
    unnest_tokens(sentence, text, token = "sentences") %>%  
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  # Obtenemos número de frases que salen de la "tokenización"
  numero_frases <- frases_token %>%
    mutate(sentence_id = row_number()) %>%
    summarise(n = n())
  # Redondeamos hacia arriba (aseguramos al menos una frase por bloque)
  n <- as.integer(ceiling(0.2*numero_frases))
  # Obtenemos "sentences" y "words" para TextRank
  sentences <- DataSentences(frases_token)
  words <- Terminology(frases_token)
  # Ejecutamos TexRank
  preambulo_summary <- TextRank(sentences, words)
  frases_seleccionadas <- summary(preambulo_summary, n=n, keep.sentence.order = TRUE)
  # Almacenamiento en el tablón
  tabla_parcial <- tibble(bloque ="Preambulo", sub_bloque = i, n_text_rank = n, frase = frases_seleccionadas )
  tablon <- rbind(tablon,tabla_parcial)
}
# Resumen del preambulo 
tablon %>% select(bloque,frase) %>% 
  filter(bloque == "Preambulo") %>% 
  select(frase)

# #############################################################################
# Trabajamos con bloque Títulos-Capítulos-Artículos
# #############################################################################

# Almacenamos los artículos por títulos: cada elemento de la lista se corresponde con un Título
articulos_por_titulo <- list(NULL)
# Lista intermedia para guardar los títulos que se dividen en capítulos (no todos los títulos)
capitulo_bloques <- list(NULL)
# Almacena los bloques de Títulos quitando espacio inicial
for (i in 2:length(titulos[[1]])) {
  articulos_por_titulo[[i-1]] <- str_replace_all(titulos[[1]][i],regex("^\\s+[IVXLCM]+"), "")
}
# Para los Títulos divididos en Capítulos, separamos los capítulos, y los volvemos a guardar 
# en articulos_por_titulo.
for (i in 1:length(articulos_por_titulo)) {
  if (str_detect(articulos_por_titulo[[i]], regex("CAPÍTULO"))) {
    numero_capitulos <- str_count(articulos_por_titulo[[i]], regex("CAPÍTULO"))
    # Separación en bloques de capítulos, eliminando el comienzo
    capitulo_bloques <- str_split(articulos_por_titulo[[i]], regex("(?<=\\s)CAPÍTULO(?=\\s+[IVXLCM]+)"), n=numero_capitulos+1)[[1]][1:numero_capitulos+1]
    articulos_por_titulo[[i]][1:numero_capitulos] <- capitulo_bloques
  }
}

##############################################################################
# Extracción de frases con TextRank del bloque Tiítulos-Capitulos-Artículos
##############################################################################

# Tenemos 3 bucles for para recorrer Títulos (i), Capítulos (j) y Artículos (k), la unidad
# de texto para TextRank

for (i in 1:length(articulos_por_titulo)) {
  for (j in 1:length(articulos_por_titulo[[i]])) {
    # Número de bloques de cada título (para titulos con y sin capítulos)
    bloques_titulo <- length(str_extract_all(articulos_por_titulo[[i]][j],regex("(?<=.)Artículo(?=\\s+[[:digit:]]+\\.)"))[[1]])+1
    # Lista de bloques
    titulo_bloques <- str_split(articulos_por_titulo[[i]][j], regex("(?<=.)Artículo(?=\\s+[[:digit:]]+\\.)"), n=bloques_titulo)[[1]][2:bloques_titulo]
    titulo_bloques <- list(str_replace_all(titulo_bloques, regex("\\s\\d+\\.(?=\\s[[:upper:]])"), ""))
    for (k in 1:(bloques_titulo-1)) {
      # "Tokenizamos" el artículo
      frases_token <- tibble(text = titulo_bloques[[1]][k]) %>%
        unnest_tokens(sentence, text, token = "sentences") %>%  
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
        tabla_parcial <- tibble(bloque ="Titulo", sub_bloque = k, n_text_rank = n, frase = frases_seleccionadas )
        tablon <- rbind(tablon,tabla_parcial)
      # Si no es significativo (numero de frases = 1), se emplea la única frase existente
      } else {
        frases_seleccionadas <- frases_token$sentence
        # Almacenamiento en el tablón
        tabla_parcial <- tibble(bloque ="Titulo", sub_bloque = k, n_text_rank = 1, frase = frases_seleccionadas )
        tablon <- rbind(tablon,tabla_parcial)
    
      }
    }
  }
}


# #############################################################################
# Trabajamos con bloque de Disposiciones
# #############################################################################

disposiciones_bloques <-   






##################################################################
# Trabajo con words como "terminology" para TextRank
##################################################################
# Lista de frases con stopwords
preambulo_sentences <- tibble(text = preambulo) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)
# Lista de palabras con stopwords
preambulo_words <- preambulo_sentences %>%
  unnest_tokens(word, sentence)
# Lista de palabras sin stopwords
stop_words_sp = tibble(word = tm::stopwords("spanish"))
preambulo_words <- preambulo_words %>%
  anti_join(stop_words_sp, by = "word")

# Eliminando números de las words
# Lista de frases sin números
preambulo_sin_numeros <- str_replace_all(preambulo, regex("\\d+"), "")
preambulo_sentences_sin_numeros <- tibble(text = preambulo_sin_numeros) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

# Lista de palabras sin números y con stopwords
preambulo_words_sin_numeros <- preambulo_sentences_sin_numeros %>%
  unnest_tokens(word, sentence)
# Lista de palabras sin números y sin stopwords
stop_words_sp = tibble(word = tm::stopwords("spanish"))
preambulo_words_sin_numeros <- preambulo_words_sin_numeros %>%
  anti_join(stop_words_sp, by = "word")
# Lista de palabras sin números, sin stopwords filtradas para n>=2
preambulo_words_filtrado <- preambulo_words_sin_numeros %>%
  select(sentence_id,word) %>% 
  count(word, sort = TRUE) %>% 
  filter(n>=2)
preambulo_words_sin_numeros <- preambulo_words_sin_numeros %>% filter(word %in% preambulo_words_filtrado$word)









########################################################################
# TextRank
########################################################################
preambulo_summary <- textrank_sentences(data = preambulo_sentences, 
                                        terminology = preambulo_words)
########################################################################




# Escogemos frases ordenadas según texto original
preambulo_summary[["sentences"]] %>%
  arrange(desc(textrank)) %>% 
  slice(1:5) %>%
  arrange(textrank_id) %>% 
  pull(sentence)
# Orden de las frases en el texto original
preambulo_summary[["sentences"]] %>%
  arrange(desc(textrank)) %>% 
  slice(1:5) %>%
  arrange(textrank_id) %>% 
  pull(textrank_id)
# Puntuación obtenida en el ranking de las frases ya ordenadas
preambulo_summary[["sentences"]] %>%
  arrange(desc(textrank)) %>% 
  slice(1:5) %>%
  arrange(textrank_id) %>% 
  pull(textrank)


preambulo_summary[["sentences"]] %>%
  ggplot(aes(textrank_id, textrank, fill = textrank_id)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_c() +
  guides(fill = "none") +
  labs(x = "Sentence",
       y = "TextRank score",
       title = "Nivel informativo de las frases",
       subtitle = 'Preambulo de la Ley',
       caption = "Source: BOE-A-1994-26003-consolidado_LAU.pdf")







