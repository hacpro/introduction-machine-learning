

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

## Es reicht wenn ihr konzeptionell wisst, was passiert.
## Der Code koennt ihr dann alle runterladen und ausfuehren/analysieren
## Deshalb: Ich gebe so viel Gas wie möglich und ihr bremst wo voellig 
## unlar

## Frage vorab: Wer hat keine Ahnung von Statistik 
## (Histogramm, Regression, Bestimmtheitsmass (R^2))


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

## TODO:
## Was ist elevation im Datensatz?
## Was kann Visual Studio alles?
## Korrelationsmatrix einsetzen irgendwo, resp. entweder bei fourfoldplot oder
##    bei erster Modellevaluation
## Precision and Recall -> Folien
## Df's loeschen

# Arbeitsverzeichnis setzen
setwd("c:/source/introduction-machine-learning")

# File zur Untersuchung laden
buildings <- read.csv2("buildings.csv", sep = ",")

# Eine Uebersicht der Daten  anzeigen
head(buildings)
summary(buildings)

## Kurz erklaren was fuer Daten wir hier vor uns haben

# Wir wollen uns als ersten die Preise anschauen
summary(buildings$price)

## Mean und Median erklaeren

# Histogramm der Preise ausgeben
hist(buildings$price, breaks = 200, prob = T)

# Mittelwert und Median darstellen
abline(v = mean(buildings$price),
       col = "blue",
       lwd = 1)
abline(v = median(buildings$price),
     col = "blue",
     lwd = 1, 
     lty = 2)


# Dichteverteilung einzeichnen
lines(density(buildings$price),  
 lwd = 2 # thickness of line
 )

## Die kleinen Statistiker haben es bereits herausgefunden
## wir haben eine positive Schiefe / rechtsschiefe Verteilung

# Kurtosis und Schiefe ausgeben
skewness(buildings$price)
kurtosis(buildings$price)

## Das ganze koennen wir natuerlich auch etwas schoener machen
## aber das werden wir spaeter sehen


# Frage 1: Wieviel kostet ein Gebaeude?

## Gesetzt wir haetten keinen Preis, wie koennten wir diesen annähnern. 
## Entweder neue Daten oder fehlender Preis
# --------------------------------------------------------------------------

## Hat jemand eine Idee? 
## Welche Eigenschaften (Features) in unserem Datensatz könnten
## dafuer relevant sein?

## Wenn jemand eine Idee hat nehmen wir diese Eigenschaft und machen eine Regression
## (e.g. price or sqft)

# Regression machen
summary(lm(price ~ sqft, buildings))
## Adjusted R-squared: 0.5107

## Kurz Lineare Regression erklaeren (p-value, R-Squared)
## Klammerbemkerung: Andere Formen von Regression wie Logistische Regression

# Gleiches Regressionsmodell visualisieren
ggplot(buildings, aes(sqft, price, col = buildings$in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")

## Erklaeren:
## Je naeher die Punkte an der Linie, desto besser unser Modell
## Nicht schlecht, aber das muss noch besser gehen


# Neues Modell mit mehreren Variablen
## Vorschlaege? Wenn nicht bereits gekommen..

summary(lm(price ~ sqft + price_per_sqft, buildings))
## Adjusted R-squared:  0.873
## Ziemlich gut, aber wie visualisien wir das?

# Erster Ansatz: Einfacher 3D-Plot
plot <- scatterplot3d(buildings$sqft, buildings$price_per_sqft, buildings$price)
model  <- lm(price ~ sqft + price_per_sqft, buildings)
plot$plane3d(model)

# Etwas ausgefeiltere Variante
advanced_plot <- plot_ly(buildings, x = ~sqft, y = ~price_per_sqft, 
        z = ~price,
        color = ~price,
        type = "scatter3d", 
        mode = "markers")


# ------------------------------------------------------------------------
# Und noch die Regressionsebene fuer die welche es wissen wollen
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



# Frage: Ist ein Haus aus San Francisco oder New York
# ------------------------------------------------------------------------

## Kategorisieren (NYC oder SF) in Machine Learning Jargon ist
## Classification

## Zuerst Intuition
## Hat jemand eine gute Idee? Was sagt die Intuition (SF ist huegelig)
## elevation: Schauen wir uns die Hoehe in den verschiedenen Staedten an

# Anzeigen von Erhoehung, gruppiert nach Stadt
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(in_sf, elevation, colour = in_sf)) +
  geom_point(alpha = .2, size = 5)

## Wer hat also einen Vorschlag?
## Haeuser ueber 73 Meter sollten Klassifiziert werden als in_sf

# Datensatz mit gewonnener Erkenntnis klassifizieren
in_sf <- as.integer(as.logical(buildings$elevation > 73))


# Richtige Kategorisierungen bestimmen
classifications <- df$in_sf == in_sf
all_classifications <- length(classifications)
correct_classifications <- sum(classifications == T)

# Prozent richtige bestimmen
correct_classifications / all_classifications

## [1] 0.6443089 -> 65% Korrekt -> nicht schlecht

## Ausflug nach Precision and Recall

# Konfusionsmatrix erstellen
confusionMatrix(in_sf, 
                buildings$in_sf,
                dnn = c("Prediction", "Reference"))

## Matrix erklaren und unseren Wert und Sensitivity und Specifity referenzieren


## Wir wollen das ganze noch etwas verfeinern. Vorschlaege?
## Preis pro Quadratmeter?

## Schauen wir uns unsere Daten anhand unserem neuen Wissen an
## und visualisieren m2-Preis und Hoehe

# m2-Preis und Hohe darstellen nach Stadt
ggplot(df, aes(price_per_sqft, elevation, colour = in_sf)) +
  geom_point(alpha = .5) 

## Im Scatterplot koennen wir erkennen, dass man bei den tieferen
## Haeuser eine weitere Praezisierung machen kann (ca. bei 2300$ pro sqft)
## Dimensions in a data set are called features, predictors, or variables.



# Einzeichnen was wir inzwischen wissen
ggplot(df, aes(price_per_sqft, elevation, colour = in_sf)) +
  geom_point(alpha = .5) + 
  annotate("rect", xmin=0,xmax=Inf, ymin=73, ymax=Inf, alpha=0.2, fill="#00BFC4") + 
  annotate("rect", xmin=2250,xmax=Inf, ymin=0, ymax=73, alpha=0.2, fill="#F8766D") 


## Identifying boundaries in data using math is the essence of statistical learning.


## Nun haben wir die sicheren, aber was mit denen im nicht markierten Bereich?
## Wir brauchen mehr Informationen

# Korrelationsmatrix darstellen
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(2,3, 4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf),
        cardinality_threshold = 16)

## Unsere Daten haben 7 Dimensionen
## Kardinale Daten anders angezeigt

## Wir entfernen kategoriale Variablen mit zu vielen Varianten
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf))


## Es sind definitiv Muster erkennbar, aber sie sind nichth umbedingt
## direkt ersichtlich, resp. die Trennungen koennen nicht klar gezogen werden


## Jetzt starten wir mit Machine Learning
## Muster in Daten zu finden ist eine Aufgabe für ML
## ML-Algorithmen benutzen statistisches Lernen um Grenzen
## zu bestimmen


# Starten wir mit einem Entscheidungsbaum (Decision Tree)

# Schauen wir uns noch einmal unsere erste Regel (> 73m) an
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(in_sf, elevation, colour = in_sf)) +
  geom_point(alpha = .2, size = 5)

# Als Histogramm, damit die Dichten klarer ersichtlich sind
df %>% 
  ggplot(aes(elevation, fill=in_sf)) +
  geom_histogram(aes(y = ..density..), position = "identity",
                 alpha = .4)

## Obwohl ein Gebäude in SF auf 73m liegt, die meisten liegen
## einiges darunter

## Ein Entscheidungsbaum verwendet Entscheidungen um Muster
## in Daten zu erkennen, lets have a look

## Unser erster Versuch mit einer Entscheidung erbrachte 63%
## Wir hatten viele false negatives

# Confusion Matrix darstellen (viele false negatives)
fourfoldplot(table(in_sf, buildings$in_sf), 
             color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, 
             main = "Confusion Matrix")

# Verschieben wir die Hoehengrenze (e.g. 50m), dann gibt es mehr (false positives)
fourfoldplot(table(as.integer(as.logical(buildings$elevation > 50)), 
                   buildings$in_sf), 
             color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, 
             main = "Confusion Matrix")

## Es gibt verschiedene mathematische Verfahren den besten
## Kompromiss herauszufinden (gini index, cross entropy)

# Der beste Wert liegt bei ungefaehr 28m
fourfoldplot(table(as.integer(as.logical(buildings$elevation > 28)), 
                   buildings$in_sf), 
             color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, 
             main = "Confusion Matrix")


## Wir sehen, auch der beste Wert ist ein Kompromiss
## Es braucht also weitere Forks (Unterteilungen des Teile)
## Gluecklicherweise haben wir R

# Reproduzierbarkeit sicherstellen
set.seed(1234)

# Klassischen Descision Tree erstellen
dtree <- rpart(in_sf ~ ., buildings, method = "class")

# Anhand von Modell Klassifizierung machen
in_sf_tree <- predict(dtree, buildings, type = "class")

# Unser Resultat begutachten
confusionMatrix(in_sf_tree, 
                buildings$in_sf,
                dnn = c("Prediction", "Reference"))

# Accuracy : 0.9146
# Sensitivity : 0.9464
# Specificity : 0.8881

# Schauen wir uns diesen Baum an (Vorteil von Descision Trees)
rpart.plot(dtree)

## Das ist verdammt gut, was denkst ihr?
## Overfitting!
## Wie begegnen wir Overfitting? Yes, cross-validation!

# Wir teilen unsere Daten in training und test set (70%/30%)
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

## Accuracy : 0.8649
## Sensitivity : 0.8387         
## Specificity : 0.8837

## Falls Zeit vorhanden

# ------------------------------------------------------------------------
# Beschneidung des Baumes mit cross validation error

# Tiefe des Baumes bestimmen
dtree$cptable
plotcp(dtree) # Baum zeigt Komplexitaet (cp) vs cross-validated error

## Cross validation 
## cross-validated error: Je hoeher dieser Wert, desto genauer ist der Baum
## und desto mehr Fehler gibt es bei neuen Daten (cross validation)

## Empfehlung ist Knoten am weitesten Links unterhalb Linie
## Haengt mit standard error zusammen (se < min(x-val e))

dtree_pruned <- prune(dtree, cp = .02679)
# ------------------------------------------------------------------------

# Wrap up
# 1. Machine Learning identifiziert Grenzen und Muster 
#   mit statistischem Lernen
# 2. Wir kennen ersten ML-Methode: Descision Trees
# 3. Wissen was Overfitting ist und wie man sie umgeht
# 4. Kennen Cross-Validation


## Welcher Art von ML hatten wir also hier?
## Schauen wir uns als naechstes unsupervised learning an
## Die wichtigsten Konzepte kenn wir nun, wir koenne jetzt also
## ein bisschen schneller voran gehn

## Nehmen wir einen Datensatz bei dem wir
# -https://www.r-bloggers.com/k-means-clustering-in-r/

## Schauen wir uns einen unverbindlichen Datensatz an: Blumen

# Und jetzt etwas unsupervised learning

# Wir schauen uns den Iris-Datensatz an. 
iris_set <- iris[, -5]
head(iris_set)

## Sepal = Kelch, Petal = Blütenblatt

# Schauen wir uns auch hier die Daten an
ggpairs(data = iris_set, title = "Korrelationsmatrix")

## Sieht nach zwei Gruppen aus oder? (zwei Taeler, zwei Gruppen)

# Wir lassen R einmal zwei Clusters erstellen
clusters <- kmeans(iris_set, 
                   2, # Anzahl erwarteter Cluster 
                   nstart = 20)

# Und schauen wir uns diese Clusters einmal an
clusters

## Within cluster sum of squares by cluster:
##   [1]  28.55208 123.79588
## (between_SS / total_SS =  77.6 %)
## Kurz: Rund 78 der Variation im Dataset wird
## durch das Clustering erklaert

# Visualisieren wir diese einmal in unserer Korrelationsmatrix
iris_set$cluster <- as.factor(clusters$cluster)
ggpairs(data = iris_set,
        columns = c(1,2,3,4),
        mapping = ggplot2::aes(colour = cluster),
        title = "Korrelationsmatrix")

## Was wenn wir drei Clusters wollen wuerden?
## (Weil wir wissen, dass es drei gibt, oder wir drei haben wollen
## e.g. high-performer und low-performer)

# Und wir probieren nun mal 3 Clusters
clusters <- kmeans(iris_set, 
                   3, # Anzahl erwarteter Cluster 
                   nstart = 20)

# Statistik ausgeben
clusters$betweenss / clusters$totss
# [1] 0.8858283

## Sind wir jetzt besser? Klar, dass es immer besser wird.

# Schauen wir uns unsere neuen Centers an
iris_set$cluster <- as.factor(clusters$cluster)
ggplot(iris_set, aes(Petal.Length, Petal.Width, 
                     color = cluster)) + 
  geom_point()

# Und jetzt schauen wir uns die Spezien dieser Blumen 
# im urspruenglichen set an

## Jaaa, die habe ich unterschlagen

ggplot(iris, aes(Petal.Length, Petal.Width, 
                     color = Species)) + 
                        geom_point()

# Konfusionsmatrix anzeigen
table(iris_set$cluster, 
                iris$Species,
                dnn = c("Prediction", "Reference"))


# Wrap-up
# 1. Unsupervised Learning muss ohne Training auskommen
# 2. k-means ist ein bekannter Clustering-Algorithmus


# Frage: Ist das seltsam
# Methode: Anonaly detection algorithm

# Library fuer Paketinstallation ab Github installieren
# install.packages("devtools")
 
# AnomalyDetection Paket von Twitter installieren und laden
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# Schauen wir uns Beispiel-Twitterdaten an
raw_data %>% head()

# Daten laden
data(raw_data)

## (Trend und Saisonalitaet werden bei diesem Algorithmus beruecksichtigt)
## TODO:
## Unterschied zwischen lokalen und globalen Anomalien herausfinden
## (https://blog.twitter.com/engineering/en_us/a/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series.html)

# Anomaly Detection ausfuehren mit Twitter package
result = AnomalyDetectionTs(raw_data, 
                            max_anoms=0.02, 
                            direction='both', 
                            plot=TRUE)

# Plot anzeigen
result$plot


# Frage: Was soll ich als nächstes tun
# Methode: Recommenders, Reinforcment Learning




#   https://cran.r-project.org/web/packages/ReinforcementLearning/vignettes/ReinforcementLearning.html
#   http://www.is.uni-freiburg.de/ressourcen/business-analytics/13_reinforcementlearning.pdf
#   

# Weiterführende Themen
# Ensemble Learning: Randon Forest
# Deep Learning: Neural Networks mnist
