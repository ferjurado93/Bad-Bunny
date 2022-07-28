library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(readxl)
library(purrr)
library(tidyverse)
library(lubridate)
library(scales)
library(igraph)
library(ggraph)
library(stringi)
library(tm)
library(stringr)
library(stringi)

#leer archivo de excel
Bad_bunny_canciones <- read_excel("C:/Users/ferna/Desktop/Bad bunny canciones.xlsx")
View(Bad_bunny_canciones)


#vista previa de los datos 
glimpse(Bad_bunny_canciones)

#dimensión de los datos
dim(Bad_bunny_canciones)

#limpieza
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  #eliminar tilde 
  nuevo_texto <- stri_trans_general(nuevo_texto,"Latin-ASCII")
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 2})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
Bad_bunny_canciones <- Bad_bunny_canciones %>% mutate(texto_tokenizado = map(.x = letra,
                                                                 .f = limpiar_tokenizar))

Bad_bunny_canciones %>% select(texto_tokenizado) %>% head() %>% view()

BB_canciones = Bad_bunny_canciones %>% select(-letra) %>% unnest()
BB_canciones <- BB_canciones %>% rename(token = texto_tokenizado) 


BB_canciones$token = removeWords(BB_canciones$token, words = stopwords("spanish"))
BB_canciones2 = BB_canciones %>% filter(token != "")

colnames(Bad_bunny_canciones)



### wordcloud conejo


contar = BB_canciones2 %>% 
  dplyr::group_by(token) %>% count() %>% arrange(desc(n))

cc = contar %>% arrange(desc(n)) %>%  head(250) %>% top_n(250) 

wordcloud2(cc, figPath = "Imagen3.png", size = 1.2)


###### palabras más largas

wc2 = contar %>% mutate(len = nchar(token)) %>% arrange(desc(len)) %>% head(100) 

wordcloud2(wc2)



##### barras conteo de palabras - geom_col()

contar %>% head(20) %>%
  ggplot() +
  geom_col(aes(reorder(token,n), n, fill = "steelblue")) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") + coord_flip()

######## grafico palabras vs el largo geom_histogram()

contar %>% mutate(len = nchar(token)) %>% 
  ggplot(aes(len), 
         binwidth = 20) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,20, by = 1), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) 


###### correlaciones

relacion = BB_canciones2 %>% group_by(album, token) %>% count(token) %>%
  spread(key = album, value = n, fill = NA, drop = TRUE)

cor.test(~ x100pre + oasis, method = "pearson", data = relacion)

cor.test(~ `el último tour` + `un verano sin ti`, data = relacion)

ggplot(relacion, aes(x100pre, `un verano sin ti`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())



###### diversidad de lexico - geom_point, stat_smooth y geom_smooth


lexico = BB_canciones2 %>% group_by(cancion, año) %>% 
  summarise(lexico = n_distinct(token)) %>% 
  arrange(desc(lexico))

lexico %>%
  ggplot(aes(año, lexico)) +
  geom_point(color = "red",
             alpha = .4, #transparencia
             size = 4, #tamaño
             position = "jitter") + #posicion aleatoria de los puntos 
  stat_smooth(color = "black", se = FALSE, method = "lm") + #traza una regresión lineal - "se" = intervalos de confianza
  geom_smooth(aes(x = año, y = lexico), se = FALSE,         #traza una linea suavizada
              color = "blue", lwd = 2) +                   #"lw": grueso de la linea
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic()

###### densidad de lexico

lexico2 = BB_canciones %>% group_by(cancion, año) %>% 
  summarise(lexico = n_distinct(token)/n()) %>% 
  arrange(desc(lexico))

lexico2 %>%
  ggplot(aes(año, lexico)) +
  geom_point(color = "red",
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = año, y = lexico), se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  #scale_color_manual(values = my_colors) +
  theme_classic()



### TF-IDF

top_popular = BB_canciones2 %>% group_by(año, token) %>% 
  count(sort = T) %>% bind_tf_idf(token, año, n) 

top_popular_tfidf_words <- top_popular %>%
  arrange(desc(tf_idf)) %>%
  mutate(token = factor(token, levels = rev(unique(token)))) %>%
  group_by(año) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(año, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = año)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Chart Level") +
  #theme_lyrics() +  
  facet_wrap(~año, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$token) +
  coord_flip()


##### bigrama

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

bigramas <- Bad_bunny_canciones %>% mutate(texto = limpiar(letra)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)

# Separación de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
  filter(!palabra2 %in% tm::stopwords(kind="es"))

# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas  %>% count(bigrama, sort = TRUE) %>% print(n = 20) 

#gráfico de barras para bigramas
bigramas %>% group_by(bigrama) %>% summarise(total=n()) %>% 
  arrange(desc(total)) %>%  top_n(20) %>% 
  ggplot(aes(x = reorder(bigrama,total), y = total, fill = "stillblue")) + 
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Conteo de Bigramas", x = "Bigramas",
       y = "Cantidad")


graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 15) %>% graph_from_data_frame(directed = FALSE)

set.seed(123)

###gráfico de bigramas
plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 1, edge.color = "black")

##########



