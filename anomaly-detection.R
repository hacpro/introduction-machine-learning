# https://github.com/twitter/AnomalyDetection

install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# Schauen wir uns Twitterdaten an
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
