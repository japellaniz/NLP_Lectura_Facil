

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
# Función TextRank
TextRank <- function(data, terminology) {
  preambulo_summary <- textrank_sentences(data = data, 
                                          terminology = terminology)
}

# Función DataSentences
DataSentences <- function(sub_bloque){
  sentences <- sub_bloque %>%
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  return(sentences)
}

# Función Terminology (words sin stopwords y sin números)
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

# Extracción del texto con fulltext (obtenemos texto en páginas y metadatos)
path <- "data/BOE-A-1994-26003-consolidado_LAU.pdf"
pdf_text <- ft_extract(path)
tmp <- c()
for (i in 1:length(pdf_text$data)){
  tmp <- str_c(tmp,pdf_text$data[i])
}
textdata <- tibble(text = tmp)


# ############################################################################
# Preprocesado del texto 
# #############################################################################

# Eliminar el índice de la ley, la cabecera y textos repetitivos. 
# Eliminamos índice:
texto <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][2]
# Eliminamos la cabecera del preámbulo:
texto <- str_split(texto, "PREAMBULO", n=2)[[1]][2]
# Eliminamos cabeceras y pies de página:
texto <- str_replace_all(texto, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
# Limpieza de entorno de trabajo.
rm(list = c("i","path","tmp"));cat("\014")


# Generamos la lista titulos que contiene Preambulo, Títulos y Disposiciones.
titulos <- str_split(texto, "TÍTULO")
# Eliminamos espacios en blanco en exceso.
titulos[[1]] <- stripWhitespace(titulos[[1]])
# Inicializamos tablon
tablon <- tibble(bloque = character(), sub_bloque = character(), n_text_rank = integer(), frase = character())


# #############################################################################
# Trabajamos con el preámbulo
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
# Tasa de compresión tau=20%
# Contador de frases de cada bloque
for (i in 1:bloques_preambulo) {
  frases_token <- tibble(text = preambulo_bloques[[1]][i]) %>%
    unnest_tokens(sentence, text, token = "sentences") %>%  
    mutate(sentence_id = row_number()) %>%
    select(sentence_id, sentence)
  numero_frases <- frases_token %>%
    mutate(sentence_id = row_number()) %>%
    summarise(n = n())
n <- as.integer(ceiling(0.2*numero_frases))
sentences <- DataSentences(frases_token)
words <- Terminology(frases_token)
preambulo_summary <- TextRank(sentences, words)
frases_seleccionadas <- summary(preambulo_summary, n=n, keep.sentence.order = TRUE)
# Almacenamiento en el tablón
tabla_parcial <- tibble(bloque ="Preambulo", sub_bloque = i, n_text_rank = n, frase = frases_seleccionadas )
tablon <- rbind(tablon,tabla_parcial)
}


##################################################################
# Forma de extraer los apartados del preambulo
str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))
str_extract_all(preambulo,regex("\\d+"))
temp <- temp %>% filter(!str_detect(temp$word,"[[:digit:]]"))

##################################################################

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







