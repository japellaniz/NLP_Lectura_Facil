
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
