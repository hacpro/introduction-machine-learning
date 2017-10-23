
# --------------------------------------------------------------------------------
# Methode: Neuronales Netz
# Paket: h2o (https://www.h2o.ai/download/)
# Input: Gelabeltes Dataset mit handgeschriebenen Zeichen
# --------------------------------------------------------------------------------

#install.packages("RCurl")
#install.packages("jsonlite")
#install.packages("h2o", type="source", 
#                  repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/6/R")



setwd("c:/source/introduction-machine-learning/advanced-concepts")

library(h2o)
library(ggplot2)


# Pfad
mnist = read.csv( "data/mnist.csv" )
par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:100){
  y = as.matrix(mnist[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, mnist[i,1], cex = 3, col = 2, pos = c(3,4))
}




# faktoren
mnist[,1] <- as.factor(mnist[,1])

train_rows <- sample(nrow(mnist), .7 * (nrow(mnist)))
train <- mnist[train_rows,]
test <- mnist[-train_rows,]


# starts a local h2o server, alternativ waere amazon ec2 oder microsoft azure
h2o.init(nthreads=-1, max_mem_size="4G")

train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)


# auto encoder, kind of automatic feature engineering
# erste column ist immer das laben wenn kein header angegeben
nn_model = h2o.deeplearning(
  x = 2:785,
  training_frame = train_h2o,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 1, #epochs = 600,
  activation = "Tanh",
  autoencoder = TRUE
)

layer3_features = h2o.deepfeatures(nn_model, 
                                              train_h2o, 
                                              layer=3)

plotdata = as.data.frame(layer3_features)
plotdata$label = as.character(as.vector(train_h2o[,1]))

qplot(DF.L3.C1, 
      DF.L3.C2, data = plotdata, 
      color = label, 
      main = "Neural network: 400 - 200 - 2 - 200 - 400 ")



nn_model = h2o.deeplearning(
  x = 2:785,
  y = 1,
  training_frame = train_h2o,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 1, #epochs = 600,
  activation = "Tanh"
)


# Prediction for test set
pred <- h2o.predict(nn_model, 
                          newdata = test_h2o[,-1])

# Performance von unserem NN ausgeben
# (Schau dir die Confusion-Matrix an)
h2o.performance(nn_model, test_h2o)


# Schauen wir uns die ersten 25 Ergebnisse noch visuell an
par( mfrow = c(5,5), mai = c(0,0,0,0))
for(i in 1:25){
  y = as.matrix(test[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text(0.2, 0, pred$predict[i, 1], cex = 3, col = 2, pos = c(3,4))
}

# Und wir beenden unser virtuelles H2O cluster wieder
h2o.shutdown(prompt = FALSE)


