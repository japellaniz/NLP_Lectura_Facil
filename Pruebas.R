
# #########################################################################
# Carga de datos
# #########################################################################

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
library(readtext)
data_file <- list.files(path="data", full.names = T, recursive = T)
textdata <- readtext(data_file[1],docvarsfrom = "filepaths", dvsep="/", encoding = "UTF-8")



###############################################################################
# Trabajo con bigramas
###############################################################################

# Eliminar stopwords del preambulo (stackoverflow)
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
preambulo_sin_stopwords <- rm_words(preambulo, tm::stopwords("sp"))
# Paso a mayúsculas 
str_to_title(preambulo_sin_stopwords)

# Sacamos la lista de frases sin stopwords
preambulo_sentences_sin_stopwords <- tibble(text = str_to_title(preambulo_sin_stopwords)) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

# Bigramas sin stopwords
preambulo_bigrams_sin_stopwords <- preambulo_sentences_sin_stopwords %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n=2)
# 
bigram_separated <-preambulo_bigrams_sin_stopwords %>% 
  separate(bigram, c("word1","word2"), sep = " ")

bigram_filtered <- bigram_separated %>%
  filter(!str_detect(word1, "\\d+")) %>%
  filter(!str_detect(word2, "\\d+")) 

bigram_united <- bigram_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigram_united %>% count(bigram, sort = TRUE)

preambulo_bigrams_sin_stopwords <- bigram_united %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n>=2)

# Lista de frases con stopwords
preambulo_sentences <- tibble(text = preambulo) %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentence_id = row_number()) %>%
  select(sentence_id, sentence)

preambulo_summary <- TextRank(preambulo_sentences, preambulo_bigrams_sin_stopwords)
##################################################################################
##################################################################################




#######################################################################
# Número de bloques del preámbulo
bloques_preambulo <- length(str_extract_all(preambulo,regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))[[1]])+1
# Lista de bloques del preámbulo
preambulo_bloques <- str_split(preambulo, regex("(?<=\\s)\\d(?=\\s[[:upper:]])"), n=bloques_preambulo)
############################################################################


##################################################################
# Pruebas con expresiones reg.
str_extract_all(articulos_por_titulo[[2]],regex("(?<=.)Artículo(?=\\s+[[:digit:]]+\\.)"))
str_extract_all(articulos_por_titulo[[1]][1],regex("(?<=\\s)\\d(?=\\s[[:upper:]])"))
str_extract_all(titulo_bloques, regex("(?<=\\s)\\d(?=\\.\\s[[:upper:]])"))
str_extract_all(texto,regex("[[:alpha:]]{1}\\)"))
str_extract(disposiciones[1],regex("(?<=.[[:upper:]].)[[:upper:]]"))
str_extract_all(disposiciones[1],regex("\\s(?=[[:upper:]])"))[[1]][2]
str_extract_all(disposiciones[1],regex("\\s(?=[[:upper:]])"))
str_split(disposiciones[21],regex("\\.\\s(?=[[:upper:]])"), n=3)[[1]][3]
str_extract(disposiciones[21], regex("Por tanto, Mando"))
temp <- temp %>% filter(!str_detect(temp$word,"[[:digit:]]"))
str_extract_all(disposiciones,regex("\\.(?=\\s\\d+\\.ª\\s[[:lower:]])"))

##################################################################


#######################################################################
# Trabajo con el índice
########################################################################
indice <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][1]
indice <- stripWhitespace(indice)
indice_cabecera <- str_split(indice, regex("ÍNDICE"), n=2)[[1]][1]
indice_cabecera
indice <- str_split(indice, regex("ÍNDICE"), n=2)[[1]][2]
indice <- str_replace_all(indice, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
indice <- str_replace_all(indice, regex("\\.\\s\\d+"), "")
indice <- str_replace_all(indice, regex("(?<=\\.)\\s\\."), "")
indice <- stripWhitespace(indice)
indice <- str_replace_all(indice, regex("(?<=[IVXLCM])\\."), ":")
indice <- str_replace_all(indice, regex("(?<=Artículo\\s\\d{1,3})\\."), ":")

indice_lineas <- tibble(text = indice) %>% 
  unnest_tokens(linea, text, token = "sentences", to_lower = FALSE)

tablon <- tibble(bloque = character(), sub_bloque = character(), n_text_rank = integer(), frase = character())





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







