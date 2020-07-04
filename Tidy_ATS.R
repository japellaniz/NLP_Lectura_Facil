

# Inicializaciones y carga de librerías
rm(list = ls());cat("\014.")
library(tidyverse)
library(tidytext)
library(textrank)
library(readtext)
library(tm)
library(fulltext)

# #########################################################################
# Carga de datos: 3 formas diferentes (escoger una).
# #########################################################################

# Extracción del texto con fulltext (obtenemos texto en páginas y metadatos)
path <- "data/BOE-A-1994-26003-consolidado_LAU.pdf"
pdf_text <- ft_extract(path)
tmp <- c()
for (i in 1:length(pdf_text$data)){
  tmp <- str_c(tmp,pdf_text$data[i])
}
textdata <- tibble(text = tmp)

# Extracción del texto del pdf con tm (obtenemos texto en páginas y metadatos)
path <- "data/BOE-A-1994-26003-consolidado_LAU.pdf"
reader <- readPDF(engine = "pdftools")
pdf_text <- reader(elem = list(uri = path), language = "sp", id = "id1")
tmp <- c()
for (i in 1:length(pdf_text[[1]])){
   tmp <- str_c(tmp,pdf_text[[1]][i])
}
textdata <- tibble(text = tmp)

# Extracción del texto directamente sin paginar, con readtext.
data_file <- list.files(path="data", full.names = T, recursive = T)
textdata <- readtext(data_file[1],docvarsfrom = "filepaths", dvsep="/", encoding = "UTF-8")

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
rm(list = setdiff(ls(),c("textdata","texto","pdf_text")));cat("\014")


# Generamos la lista titulos que contiene Preambulo, Títulos y Disposiciones.
titulos <- str_split(texto, "TÍTULO")
# Eliminamos espacios en blanco en exceso.
titulos[[1]] <- stripWhitespace(titulos[[1]])


# #############################################################################
# Trabajamos con el preámbulo
# #############################################################################

# Tomamos el preámbulo completo
preambulo <- titulos[[1]][1]
# Limpiar preambulo y obtener tokens

rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
preambulo_sin_stopwords <- rm_words(preambulo, tm::stopwords("sp"))
str_to_title(preambulo_sin_stopwords)

#################################
# Forma de extraer los apartados del preambulo
str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))
##################################

preambulo_sentences <- tibble(text = preambulo) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

# nuevo
preambulo_sentences_sin_stopwords <- tibble(text = str_to_title(preambulo_sin_stopwords)) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)
#############################################

preambulo_words <- preambulo_sentences %>%
  unnest_tokens(word, sentence)


stop_words_sp = c(word = tm::stopwords("spanish"))
preambulo_words <- preambulo_words %>%
  anti_join(stop_words_sp, by = "word")

# nuevo #################
preambulo_sentences_sin_stopword <- preambulo_sentences
for (i in 1:length(tm::stopwords("spanish"))){
preambulo_sentences_sin_stopword$sentence2 <- str_replace_all(preambulo_sentences_sin_stopword$sentence, pattern = tm::stopwords("spanish")[1], "")
}
# nuevo ###############################################
preambulo_bigrams <- preambulo_sentences %>%
  unnest_tokens(word, sentence, token = "ngrams", n=2)
############################################################
###################





# TextRank
preambulo_summary <- textrank_sentences(data = preambulo_sentences, 
                                      terminology = preambulo_words)

preambulo_summary <- textrank_sentences(data = preambulo_sentences, 
                                        terminology = preambulo_bigrams)


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
