

# Packages von oeffentlichem Repo installieren 
# install.packages("ggplot2", dependencies = T)
# install.packages("GGally", dependencies = T)
# install.packages("plotly", dependencies = T)
# install.packages("scatterplot3d", dependencies = T)
# install.packages("reshape2", dependencies = T)
# install.packages("e1071", dependencies = T)
# install.packages("caret", dependencies = T)
# install.packages("rpart", dependencies = T)
# install.packages("rpart.plot", dependencies = T)

# Packages referenzieren
library(ggplot2)
library(GGally)
library(plotly)
library(e1071)
library(scatterplot3d)
library(reshape2)
library(caret)
library(rpart)
library(rpart.plot)
library(datasets)


# Arbeitsverzeichnis setzen
setwd("c:/source/introduction-machine-learning")

# File zur Untersuchung laden
buildings <- read.csv2("buildings.csv", sep = ",")

# Eine Uebersicht der Daten  anzeigen
View(buildings)

#

# Wir wollen uns als ersten die Preise anschauen
summary(buildings$price)

# Histogramm der Preise ausgeben
hist(buildings$price, breaks = 200, 
     prob = T)

# Mittelwert und Median darstellen
abline(v = mean(buildings$price),
       col = "darkblue",
       lwd = 1)
abline(v = median(buildings$price),
     col = "darkblue",
     lwd = 1, 
     lty = 2)

#

# Kerndichteschätzung einzeichnen
lines(density(buildings$price),  
 lwd = 1, # thickness of line
 col = "red"
 )

# Kurtosis und Schiefe ausgeben
skewness(buildings$price)
kurtosis(buildings$price)

#

# --------------------------------------------------------------------------
# Frage: Wieviel kostet ein Gebaeude?
# Methode: Regression
# (Einschaetzung von neuem Haus ohne Preisinformation)
# --------------------------------------------------------------------------



# Lineares Regressionsmodell erstellen
lm_price <- lm(price ~ sqft, buildings)

# Modell in Streudiagramm visualisieren
ggplot(buildings, aes(sqft, price)) +
  geom_point() +
  geom_smooth(method = "lm")

# Kennzahlen ausgeben
summary(lm_price)


#


# Regressionsmodell verfeinern
lm_price2 <- lm(price ~ sqft + bath, buildings)
summary(lm_price2)


# 3D-Plot anzeigen
plot_ly(buildings, x = ~sqft, y = ~as.integer(bath), 
        z = ~price,
        color = ~price,
        type = "scatter3d", 
        mode = "markers")

#


# Schaetzung machen fuer Gebaeude mit 2000 sqft und 2 Baeder
predict(lm_price2, data.frame(sqft=2000, bath=as.factor(2)))


# ------------------------------------------------------------------------
# Vertiefung - Darstellung Regressionsebene
# ------------------------------------------------------------------------

graph_reso <- 10
axis_x <- seq(min(buildings$sqft), max(buildings$sqft), 
              by = graph_reso)
axis_y <- seq(min(buildings$price_per_sqft), max(buildings$price_per_sqft), 
              by = graph_reso)
price_surface <- expand.grid(sqft = axis_x,
                             price_per_sqft = axis_y, KEEP.OUT.ATTRS = F)
price_surface$price <- predict.lm(model, newdata = price_surface)
price_surface <- acast(price_surface, 
                      sqft ~ price_per_sqft, value.var = "price")

advanced_plot %>% add_trace(z = price_surface,
                                        x = axis_x,
                                        y = axis_y,
                                        type = "surface")
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Frage: Ist ein Haus aus San Francisco oder New York?
# ------------------------------------------------------------------------



# Anzeigen von Erhoehung, gruppiert nach Stadt
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(in_sf, elevation, colour = in_sf)) +
  geom_point(alpha = .2, size = 5)


# 


# Datensatz mit gewonnener Erkenntnis klassifizieren
in_sf <- as.integer(as.logical(buildings$elevation > 73))


# Richtige Kategorisierungen bestimmen
classifications <- df$in_sf == in_sf
all_classifications <- length(classifications)
correct_classifications <- sum(classifications == T)

# Prozent richtige bestimmen
correct_classifications / all_classifications


# 


# Konfusionsmatrix erstellen
confusionMatrix(buildings$in_sf,
                in_sf,
                dnn = c("Prediction", "Reference"))



# -> Kennzahlen



# m2-Preis und Hohe darstellen nach Stadt
ggplot(df, aes(price_per_sqft, elevation, colour = in_sf)) +
  geom_point(alpha = .5) 


# 



# Einzeichnen was wir inzwischen wissen
ggplot(df, aes(price_per_sqft, elevation, colour = in_sf)) +
  geom_point(alpha = .5) + 
  annotate("rect", xmin=0,xmax=Inf, ymin=73, ymax=Inf, alpha=0.2, fill="#00BFC4") + 
  annotate("rect", xmin=2250,xmax=Inf, ymin=0, ymax=73, alpha=0.2, fill="#F8766D") 


#


# Korrelationsmatrix darstellen
# (Wir entfernen kategoriale Variablen mit zu vielen Varianten)
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf))


#


# Schauen wir uns noch einmal unsere erste Regel (> 73ft) an
fourfoldplot(table(Predicted=as.integer(buildings$elevation > 73), 
                    Actual=buildings$in_sf), 
             color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, 
             main = "Confusion Matrix")


#


# Verschieben wir die Hoehengrenze nach unten
fourfoldplot(table(Predicted=as.integer(buildings$elevation > 40), 
                   Actual=buildings$in_sf), 
             color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, 
             main = "Confusion Matrix")


#



# Entscheidungsbaum (Decision Tree)



# Reproduzierbarkeit sicherstellen
set.seed(1234)

# Klassischen Descision Tree erstellen
dtree <- rpart(in_sf ~ ., buildings, 
               method = "class")

# Anhand von Modell Klassifizierung machen
in_sf_tree <- predict(dtree, buildings, type = "class")


# Schauen wir uns diesen Baum an (Vorteil von Descision Trees)
rpart.plot(dtree)

# Unser Resultat begutachten
confusionMatrix(in_sf_tree, 
                buildings$in_sf,
                dnn = c("Prediction", "Reference"))



#



# Wir teilen unsere Daten in Training und Testset auf (70%/30%)
train_rows <- sample(nrow(buildings), .7*nrow(buildings))
train_set <- buildings[train_rows,]
validation_set <- buildings[-train_rows,]

# Erstellen das Modell noch einmal neu mit dem Training set
dtree <- rpart(in_sf ~ ., train_set, method = "class")

# Jetzt klassifizieren wir das Test set
in_sf_tree <- predict(dtree, validation_set, type = "class")

# und schauen uns diese Konfusionsmatrix an
confusionMatrix(in_sf_tree, 
                validation_set$in_sf,
                dnn = c("Prediction", "Reference"))


# 

# --------------------------------------------------------------------------
# Vertiefung - Beschneidung des Baumes mit 'cross validation error'
# --------------------------------------------------------------------------

# Tiefe des Baumes bestimmen
dtree$cptable
plotcp(dtree) # Baum zeigt Komplexitaet (cp) vs cross-validated error

## Cross validation 
## cross-validated error: Je hoeher dieser Wert, desto genauer ist der Baum
## und desto mehr Fehler gibt es bei neuen Daten (cross validation)

## Empfehlung ist Knoten am weitesten Links unterhalb Linie
## Haengt mit standard error zusammen (se < min(x-val e))

dtree_pruned <- prune(dtree, cp = .02679)
# --------------------------------------------------------------------------



# ------------------------------------------------------------------------
# Frage: wie ist das organisiert?
# Methode: Clustering
# ------------------------------------------------------------------------



# Wir schauen uns den Iris-Datensatz an. 
iris_set <- iris[, -5]
head(iris_set)


#


# Korrelationsmatrix anzeigen
ggpairs(data = iris_set, title = "Korrelationsmatrix")


#



# Wir lassen R einmal zwei Clusters erstellen
clusters <- kmeans(iris_set, 
                   2, # Anzahl erwarteter Cluster 
                   nstart = 20)
# Und schauen wir uns diese Clusters einmal an
clusters

# Visualisieren wir diese einmal in unserer Korrelationsmatrix
iris_set$cluster <- as.factor(clusters$cluster)
ggplot(iris_set, aes(Petal.Length, Petal.Width, 
                     color = cluster)) + 
  geom_point()


#


# Und wir probieren nun mal 3 Clusters
clusters <- kmeans(iris_set, 
                   3, # Anzahl erwarteter Cluster 
                   nstart = 20)

# Statistik ausgeben
clusters

# Schauen wir uns unsere neuen Centers an
iris_set$cluster <- as.factor(clusters$cluster)
ggplot(iris_set, aes(Petal.Length, Petal.Width, 
                     color = cluster)) + 
  geom_point()


#


# Korrekte Spezien der Blumen anzeigen
ggplot(iris, aes(Petal.Length, Petal.Width, 
                     color = Species)) + 
                        geom_point()


#



# --------------------------------------------------------------------------
# Frage: Ist das sonderbar?
# Methode: Anomaly detection algorithm
# --------------------------------------------------------------------------

# Library fuer Paketinstallation ab Github installieren
# install.packages("devtools")
 
# AnomalyDetection Paket von Twitter installieren und laden
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# Schauen wir uns Beispiel-Twitterdaten an
raw_data %>% head()

# Daten laden
data(raw_data)


#



# Anomaly Detection ausfuehren mit Twitter package
result = AnomalyDetectionTs(raw_data, 
                            max_anoms=0.02, 
                            direction='both', 
                            plot=TRUE)

# Plot anzeigen
result$plot



#



# --------------------------------------------------------------------------
# Frage: Was soll ich als nächstes tun?
# Methode: Reinforcment Learning
# --------------------------------------------------------------------------


# Paket fuer modellfreies Reinforcement Learning installieren
# devtools::install_github("nproellochs/ReinforcementLearning")
library(ReinforcementLearning)

# Beispiel Daten laden (100k zufaellig ausgewaelte Tic-Tac-Toe Spiele)
data(tictactoe)

# Jede Zeile zeigt eine Aktion und die Belohnung auf diese Aktion
# (0 = Spiel laeuft weiter, -1 = X verliert, 1 = X gewinnt)
head(tictactoe, 50)


# --!

# Parameter fuer Reinforcement Learning definieren
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Reinforcement learning ausfuehren
# (Achtung, das kann eine Weile dauern)
# model <- ReinforcementLearning(tictactoe, s = "State", 
#                                a = "Action", 
#                               r = "Reward", 
#                                s_new = "NextState", 
#                               control = control)
# saveRDS(model, "tic-tac-toe.rds")

model <- readRDS("tic-tac-toe.rds")

# --!


# Modell ausgeben mit bestem Zug fuer jeden Status
print(model$Q)


#


# Beispiel fuer gelerntes Handel:

# Momentaner Status
#     B . .
#     B X .
#     . . X

# Algorithmus nach naechstem Zug fragen
policy(model)["B..BX...X"]



#


# --------------------------------------------------------------------------
# Vertiefung: Was soll ich als naechstes tun?
# Methode: Recommenders
# --------------------------------------------------------------------------

#install.packages("recommenderlab")
#install.packages("data.table")
#install.packages("dplyr")

library(recommenderlab)
library(data.table)
library(dplyr)

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



