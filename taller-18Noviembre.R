
### Carga de Librer韆s ####
library(rtweet)
library(tidyverse)
library(lubridate)
library(wordcloud2)
library(RColorBrewer)
library(ggwordcloud)
library(gridExtra)
library(scales)
library(tidytext)
library(igraph)
library(ggraph)
library(quanteda)
library(e1071)
library(ggjoy)
library(dplyr)

## store api keys (these are fake example values; replace with your own keys)
consumer_key = 'xxxx'
consumer_secret = 'xxxx'
access_token = 'xxxx'
access_secret = 'xxx'

## authenticate via web browser
token <- create_token(
  app = "xxxx",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

n = 3200 #n鷐ero maximo de tweets

guillermo <- get_timeline(user = "@lassoguillermo", n = n, parse = TRUE,
                          check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
guillermo <- guillermo %>% mutate(created_at = with_tz(created_at,
                                                       tz = "America/Bogota") )


arauz <- get_timeline(user = "@ecuarauz", n = n, parse = TRUE,
                       check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
arauz <- arauz %>% mutate(created_at = with_tz(created_at,
                                                 tz = "America/Bogota") )


yaku <- get_timeline(user = "@yakuperezg", n = n, parse = TRUE,
                       check = TRUE, include_rts = FALSE)
### Ajustamos la fecha para que sea la de Ecuador ###
yaku <- yaku %>% mutate(created_at = with_tz(created_at,
                                                 tz = "America/Bogota") )

# Se unen todos los tweets en un 煤nico dataframe
tweets <- bind_rows(guillermo, arauz,yaku )

saveRDS(guillermo,"guillermo2.rds")
saveRDS(arauz,"arauz1.rds")
saveRDS(yaku,"yaku1.rds")

#agrupo por nombre de usuario y totalizo
tweets %>% dplyr::group_by(screen_name) %>% summarise(numero_tweets = n()) 

#Columnas disponibles
colnames(tweets) %>% view()

#Selecci贸n de variables
tweets <- tweets %>% select(screen_name, created_at, status_id, text)

#Se renombran las variables con nombres m谩s pr谩cticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
head(tweets)
### FIN DE PARTE 1 ####

### INICIO PARTE 2 ####

#### Limpieza de texto y tokenizaci贸n ####
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a min煤sculas
  nuevo_texto <- tolower(texto)
  # Eliminaci贸n de p谩ginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminaci贸n de signos de puntuaci贸n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminaci贸n de n煤meros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminaci贸n de espacios en blanco m煤ltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenizaci贸n por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminaci贸n de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}
# Se aplica la funci贸n de limpieza y tokenizaci贸n a cada tweet
tweets <- tweets %>% dplyr::mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))

tweets %>% select(texto_tokenizado) %>% head() %>% view()

tweets %>% slice(1) %>% select(texto_tokenizado) %>% pull()
### FIN PARTE 2 ####

#### INICIO PARTE 3 ####
#### An谩lisis exploratorio #####
tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

## La funci贸n unnest_tokens() del paquete tidytext permite, entre otras cosas, automatizar el
## proceso tokenizaci贸n y almacenamiento en formato tidy en un 煤nico paso.
## https://www.tidytextmining.com/

### Distribuci贸n temporal de los tweets ####

### Distribuci贸n por separado ###
tweets <- tweets %>% filter(fecha >= '2020-08-01')

tweets %>% ggplot(aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month") +
  labs(x = "Fecha de publicaci髇", y = "N鷐ero de tweets") +
  facet_wrap(~ autor, ncol = 1,scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#En el mismo gr谩fico l铆neas
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "N鷐ero de tweets publicados", x = "Fecha de publicaci髇",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

#### Frecuencia de palabras ####

## Total de palabras escritas por cada usuario ##
tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
tweets_tidy %>% ggplot(aes(x = autor)) +
  geom_bar() + coord_flip() + theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Total de palabras escritas",
    subtitle = "Meses de Agosto - Noviembre",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Palabras distintas utilizadas por cada usuario ###
tweets_tidy %>% select(autor, token) %>% 
  distinct() %>%  group_by(autor) %>% summarise(palabras_distintas = n()) 

tweets_tidy %>% dplyr::select(autor, token) %>%
  distinct() %>%   ggplot(aes(x = autor)) + geom_bar() + coord_flip() +
  theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Total de palabras diferentes escritas",
    subtitle = "Meses de Agosto - Noviembre",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet" )                    

### Longitud media de los tweets por usuario ###
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>% summarise(media_longitud = mean(longitud),sd_longitud = sd(longitud))

tweets_tidy %>%  group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw() +
  labs(
    x = NULL, y = NULL,
    title = "Longitud media de los tweets por usuario",
    subtitle = "Y su desviaci髇 est醤dar",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

### Palabras m谩s utilizadas por usuario ###
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

### Filtramos StopWords ###
tweets_tidy <- tweets_tidy %>% filter(!(token %in% tm::stopwords(kind="es")))

### Representaci贸n gr谩fica de las frecuencias ###
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(token,-n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

### Word Clouds ### Nube de palabras ### 

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia))

df_frame <- as.data.frame(df_grouped)
df_frame %>% select(token,n) %>%
  wordcloud2(minSize = 5,shape = "circle",size = .7)

#### EJERCICIO 1 - SACAR NUBES DE PALABRAS POR persona ####
### Tip Crear dataframes usando filter



### Correlaci贸n entre usuarios por palabras utilizadas ###
library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ LassoGuillermo + yakuperezg, method = "pearson", data = tweets_spread)

cor.test(~ LassoGuillermo + ecuarauz, data = tweets_spread)

p1 <- ggplot(tweets_spread, aes(LassoGuillermo, yakuperezg)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(LassoGuillermo, ecuarauz)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)

## Para poder valorar adecuadamente el nivel de correlaci贸n es interesante 
## conocer el n煤mero de palabras comunes entre cada par de autores.

palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="LassoGuillermo") %>%
                                       select(token), tweets_tidy %>% filter(autor=="yakuperezg") %>%
                                       select(token)) %>% nrow()
paste("N煤mero de palabras comunes entre Guillermo Lasso y Yaku Perez", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="LassoGuillermo") %>%
                                       select(token), tweets_tidy %>% filter(autor=="ecuarauz") %>%
                                       select(token)) %>% nrow()
paste("N煤mero de palabras comunes entre Guillermo Lasso y Andr閟 Arauz", palabras_comunes)

### Sentimiento ####
library(tidytext)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

#### Sentimiento promedio de cada tweet ####

tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos,
                          by = c("token" = "word"))

### Se suman los sentimientos de las palabras que forman cada tweet. ###
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  head()

#### Porcentaje de tweets positivos, negativos y neutros por autor ####

porcentaje_sentimiento <- tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

porcentaje_sentimiento %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw() +
  labs(title="Analizando sentimiento de personajes pol韙icos",
       subtitle="",
       caption="Gr醘ico hecho por RED",
       x="",
       y="")

textostop <- tm::stopwords(kind="es") #stopwords de la librer?a TM para tener en espa?ol
textostop <- as_tibble(textostop) # lo convierto a tibble

## cambio de nombre a words la columna ##
colnames(textostop)
colnames(textostop)[1] <- "token"

dic_nrc <- get_sentiments("nrc")
colnames(dic_nrc)
colnames(dic_nrc)[1] <- "token"

sentiment_nrc_tbl <- tweets_tidy %>%
  inner_join(dic_nrc, by = "token")

library("ggjoy")
## analizando todo el tiempo disponible ##
sentiment_nrc_tbl %>% 
  ggplot() +
  geom_joy(aes(
    x = fecha,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentimiento en los posteos en el tiempo",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=FALSE)

## filtramos Julio - Agosto

sentiment_nrc_tbl %>%  filter(fecha >= '2020-08-01') %>% 
  ggplot() +
  geom_joy(aes(
    x = fecha,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentimiento en los posteos de Agosto - Noviembre",
       x = "Fecha",
       y = "Sentimento") + 
  scale_fill_discrete(guide=FALSE)

#Crear la funci贸n coord_radar()
#C贸digo cortes铆a de Erwan Le Pennec
#http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

sentiment_nrc_tbl %>%
  group_by(autor, sentiment) %>%
  tally %>%
  ggplot(aes(x=sentiment, y=n, group=autor)) +
  geom_polygon(aes(color = autor),        #geom_polygon para que cierre las l铆neas.
               fill = NA, size = 1.1) +  #fill=NA para relleno del pol铆gono sea transparente.
  theme(axis.ticks.y = element_blank(),  #Elimino marcas de ejes
        axis.text.y = element_blank()) + #Elimino nombres de ejes.
  labs(title="Analizando Personalidad de candidatos a la presidencia",
       subtitle="Utilizando NRC",
       caption="SEE Taller de #PLN",
       x="",
       y="") +                           #Elimino etiquetas de ejes
  coord_radar()    
