setwd("PONER DIRECCIÓN AQUI!")
movies <- read.csv("tmdb-movies.csv")

# Se importa librearía para hacer tests de nomralizacion
library(nortest)

# Se importa librería para que el gráfico de correlación muestre el coeficiente
install.packages("ggpubr")
library("ggpubr")

# Variables cuantitativas:

# 1. Popularidad
qq <- qqnorm(movies$popularity)
qqline(movies$popularity)
ad.test(movies$popularity)

#2. Presupuesto
qq <- qqnorm(movies$budget)
qqline(movies$budget)
ad.test(movies$budget)

#3. Ingreso
qq <- qqnorm(movies$revenue)
qqline(movies$revenue)
ad.test(movies$revenue)

#4. Duracion
qq <- qqnorm(movies$runtime)
qqline(movies$runtime)
ad.test(movies$runtime)

#5. Votos
qq <- qqnorm(movies$vote_count)
qqline(movies$vote_count)
ad.test(movies$vote_count)

#6. Votos promedio
qq <- qqnorm(movies$vote_average)
qqline(movies$vote_average)
hist(movies$vote_average)
ad.test(movies$vote_average)


# Variables cualitativas

# Año de lanzamiento
table(movies$release_year)
hist(movies$release_year)

# Actores (elenco)
castVector <- movies$cast
individualCast <- unlist(strsplit(as.character(movies$cast), "\\|"))
individualCast <- table(individualCast)
individualCast <- individualCast[order(individualCast, decreasing = TRUE)]
plot(head(individualCast,10))

# Director
directors <- movies$director
individualDirectors <- table(unlist(strsplit(as.character(directors), "\\|")))
individualDirectors <- individualDirectors[order(individualDirectors, decreasing = TRUE)]
plot(head(individualDirectors,10))

# Keywords
kwords <- table(unlist(strsplit(as.character(movies$keywords), "\\|")))
kwords <- kwords[order(kwords, decreasing = TRUE)]
plot(head(kwords,10))

# Production companies
pc <- table(unlist(strsplit(as.character(movies$production_companies), "\\|")))
pc <- pc[order(pc, decreasing = TRUE)]
plot(head(pc,8))


# 4.1 10 películas con más presupuesto
topBudgetMovies <- movies[order(movies$budget, decreasing = TRUE),]
top10BudgetMovies <- topBudgetMovies[1:10,]

# 4.2 10 películas con más ingresos
topRevenueMovies <- movies[order(movies$revenue, decreasing = TRUE),]
top10RevenueMovies <- topRevenueMovies[1:10,]

# 4.3 Película con más votos
mostVoted <- movies[order(movies$vote_count, decreasing = TRUE),]
mostVoted <- mostVoted[1,]

# 4.4 Peor película acorde a todos los usuarios
worstRated <- movies[order(movies$vote_average),]
worstRated <- worstRated[1,]

# 4.5 Peliculas en cada año.
moviesPerYear <- table(movies$release_year)
barplot(moviesPerYear)

# 4.6 Genero principal de las películas más populares
popularMovies <- movies[order(movies$popularity,decreasing = TRUE),]
popular20Movies <- popularMovies[1:20,c("genres")]
genres20 <- unlist(strsplit(as.character(popular20Movies), "\\|"))
# # # Funcion para obtener la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
topGenre20 <- getmode(genres20)

# Resultados en orden descendiente:
genres20Decreasing <- table(genres20)
genres20Decreasing <- genres20Decreasing[order(genres20Decreasing, decreasing = TRUE)]

# Visualizción de la tabla
plot(genres20Decreasing)


# 4.7 Genero que predomina
totalGenres <- unlist(strsplit(as.character(movies$genres), "\\|"))
barplot(table(totalGenres))

# 4.8 En las peliculas de mayor ganancia, cual es el genero principal
movies$profits <- movies$revenue - movies$budget
topProfits <- movies[order(movies$profits, decreasing = TRUE),]
genresTopProfits <- topProfits[1:10,c("genres")]
genresTopProfitsNew <- unlist(strsplit(as.character(genresTopProfits), "\\|"))
barplot(table(genresTopProfitsNew))

# 4.9 En las peliculas de mayor presupuesto, cual es el genero principal
genresTopBudget <- topBudgetMovies[1:10,c("genres")]
genresTopBudgetNew <- unlist(strsplit(as.character(genresTopBudget), "\\|"))
barplot(table(genresTopBudgetNew))

# 4.10 20 mejores directores con peliculas altamente calificadas
highlyVoted <- movies[order(movies$vote_average, decreasing = TRUE),]
newHighlyVoted <- highlyVoted[highlyVoted$director != "",]
top20Directors <- newHighlyVoted[1:20,c("director", "original_title", "vote_average")]

# 4.11 Correlacion entre presupuesto e ingreso
ggscatter(movies, x = "budget", y = "revenue", 
          add = "reg.line", add.params = list(color="red"),
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Presupuesto", ylab = "Ingresos",
          color = "dark grey", shape = 19,
          title = "Correlacion entre presupuesto e ingreso")

# 4.12 ¿Se asocian ciertos meses de lanzamiento con mejores ingresos?


# 4.13 ¿En qué meses se han visto los lanzamientos máximos?


# 4.14 ¿Cómo se correlacionan las calificaciones con el éxito comercial?
ggscatter(movies, x = "popularity", y = "revenue",
          add = "reg.line", add.params =  list(color="red"),
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "calificación", ylab = "éxito comercial", 
          color = "dark grey", shape = 19,
          title = "Correlacion entre calificaciones y el éxito comercial")

# 4.15 ¿A qué género principal pertenecen las películas más largas?



# ESTABA HACIENDO UNOS EXPERIMENTOS.... A LO MEJOR ALGO DE AQUÍ SIRVE
popularMovies <- movies[order(movies$vote_average),]
popularMoviesTable <- table(popularMovies$genres)
popularMoviesGenre <- popularMoviesTable[order(popularMoviesTable, decreasing = TRUE)]
head(popularMoviesGenre)
