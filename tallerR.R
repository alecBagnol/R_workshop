install.packages("extrafont")
install.packages("modeest")

library(readxl)
library(dplyr)
library(ggplot2)
library(extrafont)
library(modeest)

loadfonts(device = "win")


# Cargando la base de datos de peliculas
peliculas <-read_xlsx("./PeliculasTallerR.xlsx")

# Número de variables que se midieron en cada película
ncol(peliculas)

# Número de películas que se tuvieron en cuenta para el estudio.
nrow(peliculas)

colnames(peliculas)


# Películas publicadas para todo público
peliculas_todo_publico <- peliculas %>%
  filter(Age == "all") %>%
  nrow()


# Peliculas con calificación sobre 8 IMDb
peliculas_imbd_sobre_ocho <- peliculas %>%
  filter(IMDb > 8) %>%
  nrow()

# Duración promedio de las películas de género Documental
duracion_promedio_documentales <- peliculas %>%
  group_by(Genres) %>%
  summarize(Promedio = mean(Runtime, na.rm = TRUE)) %>%
  filter(Genres == "Documentary")

duracion_promedio_documentales$Promedio


#país con mayor número de películas publicadas
pais_mas_peliculas <- peliculas %>%
  group_by(Country) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
  # slice(1)

#### 3.
# Para la variable calificación en Rotten Tomatoes
rating_rotten_tomatoes <- peliculas %>%
  group_by(`Rotten Tomatoes`) %>%
  summarize(Total = n()) %>%
  na.omit %>%
  arrange(desc(Total)) %>%
  select('Rotten Tomatoes', Total) %>%
  mutate('Rating by Freq' = `Rotten Tomatoes` * Total)

# Calcule su media
media_rotten_tomatoes <- peliculas %>%
  summarize(Promedio = mean(`Rotten Tomatoes`, na.rm = TRUE))

# Calcule su mediana
mediana_rotten_tomatoes <- peliculas %>%
  summarize(Mediana = median(`Rotten Tomatoes`, na.rm = TRUE))


rating_rt_freq_total <- sum(rating_rotten_tomatoes$Total)
rating_rt_ratingxfreq_total <- sum(rating_rotten_tomatoes$`Rating by Freq`)
rating_rt_mean <- mean(peliculas$`Rotten Tomatoes`, na.rm = TRUE)
rating_rt_median <- median(peliculas$`Rotten Tomatoes`, na.rm = TRUE)
rating_rt_var <- var(peliculas$`Rotten Tomatoes`, na.rm = TRUE)
rating_rt_sd <- sd(peliculas$`Rotten Tomatoes`, na.rm = TRUE)
rating_rt_cv <- rating_rt_sd / rating_rt_mean * 100

rating_rt_quantile20 <- quantile(peliculas$`Rotten Tomatoes`, 0.20, na.rm = TRUE)
rating_rt_quantile80 <- quantile(peliculas$`Rotten Tomatoes`, 0.80, na.rm = TRUE)

histograma <- ggplot(peliculas, aes(x=`Rotten Tomatoes`)) +
  geom_histogram(fill="#4f942a", binwidth = 5)

nBins <- nclass.Sturges(peliculas$`Rotten Tomatoes`)
colors <- c(rep("#7400b8",2), rep("#6930c3",2), rep("#5e60ce",2), rep("#5390d9",2), rep("#4ea8de",2), rep("#48bfe3",2), rep("#56cfe1",2), rep("#64dfdf",2))

histograma2 <- ggplot(peliculas, aes(x=`Rotten Tomatoes`)) +
  geom_histogram(fill = colors, bins = nBins) +
  theme_light(base_family = "serif") +
  xlab("Rating Rotten Tomatoes") +
  ylab ("Cantidad")


#### 4.
colombia <- peliculas %>%
  filter(Country == "Colombia")

genero <- colombia %>%
  group_by(Genres) %>%
  summarise(Frec=n()) %>%
  select(Frec, Genres) %>%
  mutate(totalfreq = Frec /  sum(Frec)) %>%
  mutate(totalfreq = scales::percent(totalfreq))

colors_pie <- c(rep("#ffadad",1), rep("#ffd6a5",1), rep("#fdffb6",1), rep("#caffbf",1), rep("#9bf6ff",1), rep("#a0c4ff",1), rep("#bdb2ff",1))

pie <- ggplot(genero, aes(x = "", y = Frec, fill = Genres)) +
  geom_col() +
  geom_text(aes(label = totalfreq),
             position = position_stack(vjust = 0.5), show.legend = FALSE) +
  guides(fill = guide_legend(title = "Genres",
                             title.theme = element_text(size = 16, face = "bold", family = "serif"),
                             label.theme = element_text(size = 14, family = "serif"))) +
  scale_fill_manual(values = colors_pie) +
  # scale_fill_viridis_d() +
  coord_polar(theta = "y") +
  theme_void(base_family = "serif", base_size = 14)



#### 5.
clasificacion_edad <- peliculas %>%
  filter(!is.na(Age)) %>%
  group_by(Age) %>%
  summarise(frecuencia = n(), runtime = mean(Runtime, na.rm = TRUE))

clasificacion_edad_plot <- peliculas %>%
  filter(!is.na(Age))

boxplot_clasificacion_edad <- ggplot(clasificacion_edad_plot, aes(x= reorder(Age, Runtime), y = Runtime, fill=Age)) +
  geom_boxplot(show.legend = FALSE) +
  theme_light(base_family = "serif") +
  xlab("Clasificación de edad") +
  ylab ("Duración")



#### 6.
# a. Diagrama de barras según genero de las películas producidas por estados unidos


freq_pais <-as.data.frame(table(peliculas$Country))        # Frecuencia absoluta de películas por pais

Genero_sin_NA <- peliculas[!is.na(peliculas$Genres),]

Genero_peli <- Genero_sin_NA %>%
  group_by(Genres) %>%
  summarise(Frec=n())

Barras <- ggplot(Genero_peli, aes(x= reorder(Genres,-Frec) , y=Frec, fill= Genres)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Frecuencia absoluta") +
  ggtitle("Frecuecia de películas producidas por Estados unidos según el género") +
  theme_light(base_family = "serif") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=12),
        axis.text.x = element_text(angle=90))


# b. Relación entre el género y la duración de las películas

pelisin_Atip <- peliculas %>%
  # filter( Title!= "Colorado") %>%   ## Se elimina el dato porque posee una duración de 1440 min lo cual es un error
  na.omit(Genres)

Caja <- ggplot(pelisin_Atip, aes(x= reorder(Genres, Runtime), y= Runtime, fill= Genres)) +
  geom_boxplot(show.legend = FALSE) +
  theme_light(base_family = "serif") +
  xlab("Géneros") +
  ylab ("Duración de la película") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5, size=13)) +
  ggtitle("Duración de las películas según el género")

# Relación entre IMDb y Rotten Tomatoes

covariaza <- cov(peliculas$IMDb, peliculas$`Rotten Tomatoes`, use = "complete.obs")
correlacion2 <- cor(peliculas$Runtime, peliculas$`Rotten Tomatoes`, use = "complete.obs")
correlacion3 <- cor(peliculas$Runtime, peliculas$IMDb, use = "complete.obs")


ggplot(peliculas, aes(x= IMDb, y= Runtime)) +
  geom_point()