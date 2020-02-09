setwd("PONER DIRECCION AQUI!")
movies <- read.csv("tmdb-movies.csv")

# Se importa librearía para hacer tests de nomralizacion
library(nortest)

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
table(movies$vote_average)
hist(movies$vote_average)
table(movies$release_year)
hist(movies$release_year)


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

# ESTABA HACIENDO UNOS EXPERIMENTOS.... A LO MEJOR ALGO DE AQUÍ SIRVE
popularMovies <- movies[order(movies$vote_average),]
popularMoviesTable <- table(popularMovies$genres)
popularMoviesGenre <- popularMoviesTable[order(popularMoviesTable, decreasing = TRUE)]
head(popularMoviesGenre)
