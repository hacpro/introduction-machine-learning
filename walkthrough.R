

# Packages von oeffentlichem Repo installieren 
# install.packages("ggplot2", dependencies = T)
# install.packages("GGally", dependencies = T)
# install.packages("plotly", dependencies = T)
# install.packages("scatterplot3d", dependencies = T)
# install.packages("reshape2", dependencies = T)
# install.packages("e1071", dependencies = T)
# install.packages("caret", dependencies = T)


## Es reicht wenn ihr konzeptionell wisst, was passiert.
## Der Code koennt ihr dann alle runterladen und ausfuehren/analysieren
## Deshalb: Ich gebe so viel Gas wie möglich und ihr bremst wo voellig 
## unlar

## Frage vorab: Wer hat keine Ahnung von Statistik 
## (Regression, Bestimmtheitsmass etc.)


# Packages referenzieren
library(ggplot2)
library(GGally)
library(plotly)
library(e1071)
library(scatterplot3d)
library(reshape2)
library(caret)

## TODO:
## Was ist elevation im Datensatz?
## Was kann Visual Studio alles?
## Korrelationsmatrix einsetzen irgendwo
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
## direkt ersichtlich



# Praezisierte Konfusionsmatrix erstellen
confusionMatrix(as.integer(as.logical(
                    (buildings$elevation > 73 & buildings$price_per_sqft > 2250)
                    )), 
                buildings$in_sf,
                dnn = c("Prediction", "Reference"))







# Question 3: Is this weird?
# Different dimensional Visualizations (why not the Kernel Trick, 3.3. Romeo)

# Quesion 4: How is it organized?
# Clustering

# Quesion 5: What should I do next?
# Recommenders
