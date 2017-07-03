library(ggplot2)

df <- read.csv2("C:\\Users\\ch0125\\Dropbox\\Machine Learning\\part_1_data.csv", 
                sep = ",")

summary(df)

# Datensatz ohne Differenzierung (Können wir einen Preis vorhersagen)

# Regression (Welche Variablen könnten wir einander gegenüberstellen?)
ggplot(df, aes(price, sqft, col = df$in_sf)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(price ~ sqft, df))

# Mehrere Variablen kombinieren
summary(lm(price ~ sqft+elevation+in_sf, df))

# Datensatz mit Differenzierung (Können wir Muster zwischen SF und NY erkennen)

# Korrelationsmatrix (Kann jemand ein Muster erkennen mit dem wir weiterarbeiten?)
df2 <- df
df2$in_sf <- as.factor(df[, 1])
ggpairs(data=df2, columns = c(4,5,6,7,8), title="Korrelationsmatrix",
        mapping=ggplot2::aes(colour = in_sf))







