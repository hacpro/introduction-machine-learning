
# Frage: Was soll ich als naechstes tun?
# Methode: Recommenders


library(recommenderlab)
library(ggplot2)
library(data.table)


setwd("c:/source/introduction-machine-learning")

ratings <- read.csv("movielens-datasets/ratings.csv")
movies_and_genres <- read.csv("movielens-datasets/movies_and_genres.csv",stringsAsFactors=FALSE) 

## Create a user profile
binaryratings <- ratings


# ratings of 4 and 5 are mapped to 1, 
# representing likes, and ratings of 3 
# and below are mapped to -1, representing 
# dislikes:

for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

# convert binaryratings matrix to the correct format:
binaryratings2 <- dcast(binaryratings, movieId~userId, 
                        value.var = "rating", 
                        na.rm=FALSE)

# Eine Zeile pro Film, fuer jeden Benutzer eine Column mit Rating

# Kleine Bereinigung
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds



#Remove rows that are not rated from movies dataset
movieIds <- nrow(movies_and_genres) #9125
ratingmovieIds <- length(unique(ratings$movieId)) #9066
movies2 <- movies_and_genres[-which((movies_and_genres$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL


# ---------------------------------------------------------------------------------

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- movies_and_genres[
              -which((movies_and_genres$movieId %in% ratings$movieId) == FALSE),
              -c(1,2,3,4,5,6)]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles

# Fuer jeden User das Rating pro Genre bestimmen
result = matrix(0,18,671) # here, 668=no of users/raters, 18=no of genres
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c])) #ratings per genre
  }
}

#Convert to Binary scale
# Benutzer mag Genre oder nicht
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles

# The User-Based Collaborative Filtering Approach

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, 
                   value.var = "rating", 
                   na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

# fuer jeden Benutzer eine Zeile und fuer jeden Film eine Column
nrow(ratingmat)

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30

#install.packages("recommenderlab")
library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")


#Normalize the data
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")

#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom


recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies_and_genres, 
                                         movies_and_genres$movieId == as.integer(recom_list[[1]][i]))$title)
}

# Was das System dem Benutzer 1 vorschlagen wuerde
recom_result

# Welche Filme der User bereits mit einer Bewertung >= 3.5 bewertet hat
movies_and_genres[movies_and_genres$movieId %in% 
                    (ratings[ratings$userId == 1 
                             & ratings$rating >= 3.5, ])$movieId, 
                  ]$title


