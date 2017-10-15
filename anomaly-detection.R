# https://github.com/twitter/AnomalyDetection

install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# Schauen wir uns Twitterdaten an
raw_data %>% head()

# Daten laden
data(raw_data)

## (Trend und Saisonalitaet werden bei diesem Algorithmus beruecksichtigt)

# Anomaly Detection ausfuehren mit Twitter package
result = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)

# Plot anzeigen
result$plot
