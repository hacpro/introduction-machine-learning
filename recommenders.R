
# Frage: Was soll ich als naechstes tun?
# Methode: Recommenders


#install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)


setwd("c:/source/introduction-machine-learning")


ratings <- read.csv("movielens-datasets/ratings.csv")
movies_and_genres <- read.csv("movielens-datasets/movies_and_genres.csv",
                              stringsAsFactors=FALSE) 


# Aus ratings eine Matrix machen 
# (Rows = userId, Columns = movieId)
ratingmat <- dcast(ratings, userId~movieId, 
                   value.var = "rating", 
                   na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) # UserId entfernen

# Die Matrix in eine 'sparse' recommanderlab Matrix verwandeln
ratingmat <- as(ratingmat, "realRatingMatrix")

# Normalisieren der Daten
ratingmat_norm <- normalize(ratingmat)

# UBFC Recommender Model erzeugen 
# (UBCF stands for User-Based Collaborative Filtering)
# Similarity Calculation Method: Cosine Similarity
recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",
                                            nn=30))

# Die top 10 Empfehlungen fuer Benutzer 1 in ratingset bestimmen
recommendations <- predict(recommender_model, 
                 ratingmat[1], n=10) 

# Recommenderelab objekt in lesbare Liste verwandeln
recommendations_list <- unlist(as(recommendations, "list"))

# Funktion zum bestimmen von Titeln von Movie-Id
getMovieTitle <- function(id)
{
  movies_and_genres[movies_and_genres$movieId == id, ]$title
}

# Was das System dem Benutzer 1 vorschlagen wuerde
lapply(recommendations_list, getMovieTitle) %>% unlist

# Welche Filme der User bereits mit einer Bewertung >= 3.5 bewertet hat
lapply(ratings[ratings$userId == 1 & ratings$rating >= 3.5, ]$movieId,
       getMovieTitle) %>% unlist

