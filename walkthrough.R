

# Packages von oeffentlichem Repo installieren 
# install.packages("ggplot2", dependencies = T)
# install.packages("GGally", dependencies = T)
# install.packages("plotly", dependencies = T)
# install.packages("scatterplot3d", dependencies = T)
# install.packages("reshape2", dependencies = T)
# install.packages("e1071", dependencies = T)


# Packages referenzieren
library(ggplot2)
library(GGally)
library(plotly)
library(e1071)
library(scatterplot3d)
library(reshape2)


## TODO:
## Was ist elevation im Datensatz?
## Was kann Visual Studio alles?

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



# Question 2: Is this A or B
# -----------------------------------------------------------------------------------------------

# How can we determine if a house is from sf or ny?

# Lets look at the correlation matrix
df2 <- buildings
df2$in_sf <- as.factor(buildings[, 1])
ggpairs(data = df2, columns = c(4, 5, 6, 7, 8), title = "Korrelationsmatrix",
        mapping = ggplot2::aes(colour = in_sf))

# Any suggestions what may be a good indicator ?

# If suggestions: We learned regression, so this must be different for both
df <- buildings
df$in_sf <- as.factor(buildings[, 1])
ggplot(df, aes(sqft, price, colour = in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")



# Question 3: Is this weird?
# Different dimensional Visualizations (why not the Kernel Trick, 3.3. Romeo)

# Quesion 4: How is it organized?
# Clustering

# Quesion 5: What should I do next?
# Recommenders
