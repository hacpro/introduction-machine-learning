
# --------------------------------------------------------------------------------
# Methode: Neuronales Netz
# Paket: h2o (https://www.h2o.ai/download/)
# Input: Gelabeltes Dataset mit handgeschriebenen Zeichen 
# (https://en.wikipedia.org/wiki/MNIST_database)
# --------------------------------------------------------------------------------

#H2o mit Abhaengigkeiten installieren falls noch nicht vorhanden
#install.packages("RCurl")
#install.packages("jsonlite")
#install.packages("h2o", type="source", 
#                  repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/6/R")

# Arbeitsverzeichnis setzen
setwd("c:/source/introduction-machine-learning/advanced-concepts")

# Pakete referenzieren
library(h2o)
library(ggplot2)

# CSV-File in Umgebung laden
# Achtung: Erst muess Dataset von \\p-win-file1\TRANSFER\ch\mnist.csv
# in entsprechendes Verzeichnis kopiert werden
mnist = read.csv( "data/mnist.csv" )

# 10x10 Layout fuer Ausgabe erzeugen
par( mfrow = c(10,10), mai = c(0,0,0,0))

# Ersten 100 Zeichen des Datasets visualisieren
for(i in 1:100){
  y = as.matrix(mnist[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, mnist[i,1], cex = 3, col = 2, pos = c(3,4))
}

# Da wir am Ende ein neuronales Netz (NN) wollen, welches uns
# Voraussagen mit ganzen Zahlen macht, verwandeln wir erst noch 
# in Faktoren
mnist[,1] <- as.factor(mnist[,1])

# Daten in ein Trainings- und ein Testset aufteilen
# (Wir haben hier ein Beispiel von supervised learning und
# wollen demzufolge mit einem separaten Datensatz validieren)
train_rows <- sample(nrow(mnist), .7 * (nrow(mnist)))
train <- mnist[train_rows,]
test <- mnist[-train_rows,]


# Nun starten wir einen lokalen h2o-Server, alternativ koennte
# hier ein Cluster auf Amazon ec2 oder Microsoft Azure gestartet
# werden (Achtung für 4gb braucht es Java 64bit)
h2o.init(nthreads=-1, max_mem_size="4G")


# 1. Ein neuronales Netz trainieren und Performance-Masse bestimmen
# -------------------------------------------------------------------

# Nun laden wir unsere beiden Datensets in unsere h2o-Umgebung
train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)

# Wir trainieren nun ein NN mit 4 'hidden' Layern a 200 Neuronen
# ACHTUNG: Das Trainieren dieses NN kann je nach Rechner
# einige Stunden in Anspruch nehmen
# (Fuer schneller - weniger praezisere - Ergebnisse, kann
# die Anzahl der Layers, Neuronen oder Iterationen limitiert werden)
nn_model = h2o.deeplearning(
  x = 2:785,                          # die Predictor-Variablen
  y = 1,                              # die Response-Variable (label)
  training_frame = train_h2o,         # das Training-Set
  hidden = rep(200,4),                # Anzahl Neuronen pro Layer
  epochs = 600,                       # Anzahl Iterationen 
  activation = "Tanh"                 # die Aktivierungsfunktion der Neuronen
)

# Nun versuchen wir unser Testset an unserem neu erstellten Modell
pred <- h2o.predict(nn_model, 
                          newdata = test_h2o[,-1])

# (Hinweis: die vorausgesagten Labels sind im Attribut predict
# auf unserem Rueckgabeobjekt von der Methode h2o.predict)

# Schauen wir uns die ersten 25 Ergebnisse noch visuell an
# (Welche Zahlen sind haeufiger falsch und welche nicht?)
par( mfrow = c(5,5), mai = c(0,0,0,0))
for(i in 1:25){
  y = as.matrix(test[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text(0.2, 0, pred$predict[i, 1], cex = 3, col = 2, pos = c(3,4))
}

# Als einfaches Performance-Mass fuer unser neuronales Netz
# bestimmen wir die Anzahl richtig erkannter Zahlen
# (Wir erinnern uns, die Diagonale in einer Confusion-Matrix
# sind die korrekten Labels. Wir summieren diese Werte auf
# der Diagonalen der Matrix)
correct_labels <- sum(diag(table(test[,1], as.data.frame(pred$predict)[,1])))
all_labels <- nrow(test)

# Prozent von korrekten Labels bestimmen
correct_labels / all_labels

# (Zufrieden? Spiele mit den Paramatern des NN fuer bessere Predictions)

# h2o gibt uns auch eine Moeglichkeit die Performance von unserem NN 
# auszugeben (Schau dir die Confusion-Matrix an mit den Hit Ratios und den Errors)
h2o.performance(nn_model, test_h2o)



# 2. Einen Auto-Encoder trainieren (unsupervised learning)
# (siehe https://de.wikipedia.org/wiki/Autoencoder)
# -------------------------------------------------------------------

# Ein Autoencoder wird dazu genutzt um eine komprimierte Repraesentation
# der Daten zu erlernen. Er extrahiert also relevante Merkmale. 

# Wir trainieren nun einen Auto-Encoder (Option autoencoder)
# und reduzieren die Anzahl Neuronen im mittleren Layer auf zwei
nn_model = h2o.deeplearning(
  x = 2:785,
  training_frame = train_h2o,
  hidden = c(400, 200, 2, 200, 400),    # Mittlerer Layer hat nur zwei Neuronen!
  epochs = 600,
  activation = "Tanh",
  autoencoder = TRUE
)

# Nun koennen wir den mittleren Layer (Layer 3) mittels folgender
# Methode aus unserem Datenset extrahieren
layer3_features = h2o.deepfeatures(nn_model, 
                                   train_h2o, 
                                   layer=3)

# Diesen mittleren Layer der nun aus zwei Dimensionen besteht, koennen
# wir nun visualieren. Zu diesem Zweck faerben wir die einzelnen Zahlen
# in einer anderen Farbe ein
plotdata = as.data.frame(layer3_features)
plotdata$label = as.character(as.vector(train_h2o[,1]))
qplot(DF.L3.C1, 
      DF.L3.C2, data = plotdata, 
      color = label, 
      main = "Neuronales Netz (400 - 200 - 2 - 200 - 400)")

# (Durch die Reduktion auf zwei Dimensionen koennen wir nun die Pixels
# visualisieren, welche die Komprimierung (Encoding) 'ueberlebt' haben und so
# fuer den Algorithmus von hoeherer Wichtigkeit sind. Diese sind fuer jede
# Zahl natuerlich anders und wir koennen eine Separierung erkennen. Aus
# diesem reduzierten Repraesentation koennte das neuronale Netz nun wieder
# einen Output generieren (je nach Netz-Topologie mit mehr oder weniger Verlust).

# Plot-Layout wieder zurueck setzen
par(mfrow=c(1,1))

# Und wir beenden unser virtuelles H2O cluster wieder
h2o.shutdown(prompt = FALSE)


