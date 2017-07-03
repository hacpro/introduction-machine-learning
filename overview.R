library(ggplot2)

df <- read.csv2("C:\\Users\\ch0125\\Dropbox\\Machine Learning\\part_1_data.csv", 
                sep = ",")

# Regression
summary(df)
ggplot(df, aes(price, sqft, col = in_sf)) +
  geom_point()

ggplot(df, aes(price, sqft, col = in_sf)) +
  geom_point()

# Korrelationsmatrix
df2 <- df
df2$in_sf <- as.factor(df[, 1])
ggpairs(data=df2[,c(1,4:8)], title="Korrelationsmatrix",
        mapping=ggplot2::aes(colour = in_sf))
        
        
pairs(df[,1:7], col = df$in_sf)
