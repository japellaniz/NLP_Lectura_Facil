

# Inicializaciones y carga de librerías.
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
options(stringsAsFactors = FALSE)
library(readtext)
# library(quanteda)
library(dplyr)
library(tidytext)
library(stringr)
# library(readr)
# library(ggplot2)
library(textdata)
library(tidyr)
# library(wordcloud)
library(lexRankr)
library(LSAfun)
library(tm)


##########################################################################################
# Función para ejecutar algoritmo lexRank
LexRank <- function(doc, k = 5) {
  top_k_lexRankr <- lexRankr::lexRank(text = doc,
                                      #threshold = 0.1,
                                      #only 1 article; repeat same docid for all of input vector
                                      docId = rep(1, length(doc)),
                                      #sentencesAsDocs = TRUE,
                                      n = k,
                                      continuous = TRUE)
  
  #reorder the top 5 sentences to be in order of appearance in text
  order_of_appearance = order(as.integer(gsub("_","",top_k_lexRankr$sentenceId)))
  #extract sentences in order of appearance
  ordered_top_k_lexRankr = top_k_lexRankr[order_of_appearance, "sentence"]
  return(ordered_top_k_lexRankr)
}
##############################################################################################
# Función para ejecutar algoritmo LSAfun
LsaFun <- function(doc, k = 5, lang = "spanish") {
  top_k_LSAfun <- genericSummary(doc, language = lang, breakdown = TRUE, k=k)
  return(top_k_LSAfun)
}
##############################################################################################



# Extracción del texto del pdf utilizando librería readtext.
data_file <- list.files(path="data", full.names = T, recursive = T)
textdata <- readtext(data_file[1],docvarsfrom = "filepaths", dvsep="/", encoding = "UTF-8")


# Eliminar el índice de la ley, la cabecera y textos repetitivos. 
# Eliminamos índice:
texto <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][2]
# Eliminamos la cabecera del preámbulo:
texto <- str_split(texto, "PREAMBULO", n=2)[[1]][2]
# texto <- str_replace(texto,regex(".+TEXTO CONSOLIDADO",dotall = TRUE),"")
# Eliminamos cabeceras y pie de páginas:
texto <- str_replace_all(texto, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")


rm(list = setdiff(ls(),c("textdata","texto","LexRank", "LsaFun")));cat("\014")
# Partir por Títulos de la ley
titulos <- str_split(texto, "TÍTULO")
titulos <- as.data.frame(titulos,optional=TRUE,col.names=c("texto"))

# Extraer Preámbulo y Disposiciones de Títulos
preambulo <- titulos %>% 
  filter(row_number() == "1") 
titulos <- titulos %>% 
  filter(row_number()!= "1")
tit_fin <- nrow(titulos)
titulo_F <- str_split(titulos$texto[tit_fin],"Disposición")
titulos[tit_fin,]<- titulo_F[[1]][1]
disposiciones <- as.data.frame(titulo_F[[1]][2:length(titulo_F[[1]])])
colnames(disposiciones) <- c("texto")
rm(titulo_F)

# Limpiar preambulo y obtener tokens
preambulo$texto <- str_replace_all(preambulo$texto,regex("\\s+\\d\\r\\n"), "")
preambulo_token <- preambulo %>% 
  unnest_tokens(frase,preambulo, token = "sentences", to_lower = FALSE,drop = TRUE) %>% 
  select("frase")
ordered_top_k_lexRankr <- LexRank(preambulo_token$frase,5)
ordered_top_k_lexRankr

ordered_top_k_LSAfun <- LsaFun(preambulo_token$frase, 5)
ordered_top_k_LSAfun

# Separar Títulos en Artículos y limpiar
titulos$texto <- stripWhitespace(titulos$texto)

# Hacemos limpieza del Títulos.
ordered_all_senteces_lexRank <- character()
nombre_columna <- c()
for (i in 1:tit_fin) {
titulo <- titulos %>% 
  filter(row_number() == i)
  # Eliminar números de párrafo y palabras como Artículo y CAPÍTULO
  titulo$texto <- str_replace_all(titulo$texto,regex("CAPÍTULO\\s[IVXLCM]+\\s"), "")
  titulo$texto <- str_replace_all(titulo$texto,regex("Artículo\\s[[:digit:]]+\\."), "")
  titulo$texto <- str_replace_all(titulo$texto,regex("\\s[[:digit:]]+\\."), "")

  titulo_token <- titulo %>% 
    unnest_tokens(frase,titulo, token = "sentences", to_lower = FALSE,drop = TRUE) %>% 
    select("frase")
  if (nrow(titulo_token) >= 3) {
  # Ejecución del algoritmo LexRank para k=3. Habrá que ver cómo se ajusta este parámetro.
  ordered_top_k_lexRankr <- LexRank(titulo_token$frase,3)
  ordered_all_senteces_lexRank <- cbind(ordered_all_senteces_lexRank,ordered_top_k_lexRankr)
  nombre_columna <- c(nombre_columna, paste0("titulo ",i))
  }
  colnames(ordered_all_senteces_lexRank) <- nombre_columna
  
}

tabla_ordenada <- as.data.frame(ordered_all_senteces_lexRank) %>% 
  gather(key = "titulo")





# for (i in 1:tit_fin) {
# tmp <- titulos %>% 
#  filter(row_number() == i)
#print(i)
# }

#num_titulos <- length(titulos[[1]])-1
#for (i in 2:num_titulos) {
#  titulo <- titulos[[1]][i]
#}












###########################################################################################
###########################################################################################
# Pruebas expresiones regulares y otras cosas.
str_locate(textdata$text,regex("TÍTULO.+(?=Artículo)",dotall = TRUE))
str_extract_all(textdata$text,regex("Artículo(s\\s+|\\s+)(.+\\.|.+\\n.+\\.)",dotall = FALSE))
str_extract_all(textdata$text,regex("Artículo(s\\s+|\\s+)\\d+\\.\\s+",dotall = FALSE))
str_extract_all(texto, regex("Página\\s[[:digit:]]+"))
str_extract(texto,regex("TEXTO CONSOLIDADO"))
tmp<-stripWhitespace(titulos$texto[1])
str_split(titulos$texto[1],regex("Artículo"))
str_extract_all(titulo$texto, regex("CAPÍTULO\\s[IVXLCM]+\\s"))
str_extract_all(titulo$texto, regex("Artículo\\s[[:digit:]]+\\."))
str_extract_all(titulo_token$frase,regex("^[[:digit:]]+\\."))
##############################################################           
tidy_ley <- textdata %>% 
  mutate(text = str_replace(text,regex(".+TEXTO CONSOLIDADO",dotall = TRUE),"")) %>% 
  mutate(text = str_replace_all(text, "(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s\\d)", "")) %>%
  mutate(text = str_split(text, "PREAMBULO", n=2)[[1]][2]) %>% 
  #mutate(text = str_replace_all(text,"TÍTULOArtículo \\d"))
  unnest_tokens(frase, text, token = "sentences", to_lower = FALSE) %>% 
  mutate(num_frase = row_number(),
         titulo = cumsum(str_detect(frase, regex("TÍTULO"))))
tidy_ley$frase[1]
###################################################################################################
###################################################################################################


