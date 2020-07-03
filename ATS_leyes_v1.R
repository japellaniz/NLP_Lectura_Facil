

# Inicializaciones y carga de librerías.
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
options(stringsAsFactors = FALSE)
library(readtext)
library(dplyr)
library(tidytext)
library(stringr)
library(textdata)
library(tidyr)
library(lexRankr)
library(LSAfun)
library(tm)

# Definición de funciones ################################################################
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
  top_k_LSAfun <- genericSummary(doc, language = lang, breakdown = FALSE, k=k)
  return(top_k_LSAfun)
}
##############################################################################################


# Carga de datos ###########################################################################
# Extracción del texto del pdf utilizando librería readtext.
data_file <- list.files(path="data", full.names = T, recursive = T)
textdata <- readtext(data_file[1],docvarsfrom = "filepaths", dvsep="/", encoding = "UTF-8")
# #############################################################################################


# Preprocesado del texto ######################################################################
# Eliminar el índice de la ley, la cabecera y textos repetitivos. 
# Eliminamos índice:
texto <- str_split(textdata$text,".+TEXTO CONSOLIDADO", n=2)[[1]][2]
# Eliminamos la cabecera del preámbulo:
texto <- str_split(texto, "PREAMBULO", n=2)[[1]][2]
# Eliminamos cabeceras y pies de página:
texto <- str_replace_all(texto, regex("(BOLETÍN OFICIAL DEL ESTADO|LEGISLACIÓN CONSOLIDADA|Página\\s[[:digit:]]+)"), "")
# Limpieza de entorno de trabajo.
rm(list = setdiff(ls(),c("textdata","texto","LexRank","LsaFun")));cat("\014")


# Generamos la lista titulos que contiene Preambulo, Títulos y Disposiciones.
titulos <- str_split(texto, "TÍTULO")
titulos[[1]] <- stripWhitespace(titulos[[1]])
# Nº de documentos en que queda dividido en texto
tit_fin <- length(titulos[[1]])

# Extraer Disposiciones de Títulos
titulo_F <- str_split(titulos[[1]][tit_fin],"Disposición")
titulos[[1]][tit_fin]<- titulo_F[[1]][1]
disposiciones <- titulo_F[[1]][2:length(titulo_F[[1]])]
rm(titulo_F)
# ########################################################################################


# Trabajo con el preámbulo ###############################################################
# Obtenemos texto del preámbulo
preambulo <- titulos[[1]][1]
# Limpiar preambulo y obtener tokens
preambulo<- str_replace_all(preambulo,regex("\\s+\\d\\r\\n"), "")
preambulo_token <- tibble(preambulo) %>% 
  unnest_tokens(frase,preambulo, token = "sentences", to_lower = FALSE,drop = TRUE) %>% 
  select("frase")
# Ejecutamos algoritmos de resumen automático
ordered_top_k_lexRankr <- LexRank(preambulo_token$frase,5)
ordered_top_k_lexRankr

# Pendiente revisar parámetros de este algoritmo
ordered_top_k_LSAfun <- LsaFun(preambulo_token$frase[1:5], 5)
ordered_top_k_LSAfun
top_k_LSAfun <- sort(ordered_top_k_LSAfun, decreasing = FALSE)
top_k_LSAfun
# ########################################################################################

# Trabajo con Títulos ####################################################################
# Hacemos limpieza del Títulos y ejecutamos algoritmo LexRank
ordered_all_senteces_lexRank <- character()
nombre_columna <- c()
for (i in 2:tit_fin) {
  titulo <- titulos[[1]][i] 
  # Eliminar números de párrafo y palabras como Artículo y CAPÍTULO
  titulo <- str_replace_all(titulo,regex("CAPÍTULO\\s[IVXLCM]+\\s"), "")
  titulo <- str_replace_all(titulo,regex("Artículo\\s[[:digit:]]+\\."), "")
  titulo <- str_replace_all(titulo,regex("\\s[[:digit:]]+\\."), "")
  
  titulo_token <- tibble(titulo) %>% 
    unnest_tokens(frase,titulo, token = "sentences", to_lower = FALSE,drop = TRUE) %>% 
    select("frase")
  if (nrow(titulo_token) >= 3) {
    # Ejecución del algoritmo LexRank para k=3. Habrá que ver cómo se ajusta este parámetro.
    ordered_top_k_lexRankr <- LexRank(titulo_token$frase,5)
    ordered_all_senteces_lexRank <- cbind(ordered_all_senteces_lexRank,ordered_top_k_lexRankr)
    nombre_columna <- c(nombre_columna, paste0("titulo ",i-1))
  }
  colnames(ordered_all_senteces_lexRank) <- nombre_columna
  
}

tabla_ordenada <- as.data.frame(ordered_all_senteces_lexRank) %>% 
  gather(key = "titulo")
# ###########################################################################################

# Trabajo con las Disposiciones #############################################################
disposiciones_token <- tibble(disposiciones) 
# Ejecutamos algoritmos de resumen automático
ordered_top_k_lexRankr <- LexRank(disposiciones_token$disposiciones,5)
ordered_top_k_lexRankr
#############################################################################################

