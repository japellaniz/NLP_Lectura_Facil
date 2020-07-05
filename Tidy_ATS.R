

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
# Inicializamos tablon
tablon <- tibble(bloque = character(), sub_bloque = character(), n_text_rank = integer())


# #############################################################################
# Trabajamos con el preámbulo
# #############################################################################


# Tomamos el preámbulo completo
preambulo <- titulos[[1]][1]
# Eliminamos espacio inicial
preambulo <- str_replace_all(preambulo, regex("^\\s+\\d"), "")
# Número de bloques del preámbulo
bloques_preambulo <- length(str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))[[1]])+1
preambulo_bloques <- str_split(preambulo, regex("(?<=\\s)\\d(?=\\s[[:upper:]])"), n=bloques_preambulo)
preambulo_bloques[[1]]

# Cálculo de n (nº de frases del resumen) para TextRank
# Tasa de compresión tau=20%
# Contador de frases de cada bloque
for (i in 1:bloques_preambulo) {
  numero_frases <- tibble(text = preambulo_bloques[[1]][i]) %>%
    unnest_tokens(sentence, text, token = "sentences") %>%
    mutate(sentence_id = row_number()) %>%
    summarise(n = n())
n <- as.integer(ceiling(0.2*numero_frases))
tmp <- tibble(bloque ="Preambulo", sub_bloque = i, n_text_rank = n)
tablon <- rbind(tablon,tmp)
}





##################################################################
# Forma de extraer los apartados del preambulo
str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))
str_extract_all(preambulo,regex("\\d+"))

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
# Función
TextRank <- function(data, terminology) {
preambulo_summary <- textrank_sentences(data = data, 
                                      terminology = terminology)
}
##########################################################################

preambulo_summary <- textrank_sentences(data = preambulo_sentences, 
                                        terminology = preambulo_words)


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







