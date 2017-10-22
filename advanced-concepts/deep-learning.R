
# https://github.com/h2oai/h2o-tutorials/tree/master/tutorials/deeplearning

# https://www.r-bloggers.com/a-little-h2o-deeplearning-experiment-on-the-mnist-data-set/


# Split datasets
# http://h2o-release.s3.amazonaws.com/h2o/master/3552/docs-website/h2o-docs/datamunge/splitdatasets.html

# https://kuanhoong.wordpress.com/2016/02/01/r-and-deep-learning-cnn-for-handwritten-digits-recognition/

# as factors!
# convert digit labels to factor for classification
#training[,1]<-as.factor(training[,1])


install.packages("RCurl")
install.packages("jsonlite")
install.packages("h2o", type="source", 
                  repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/6/R")


setwd("c:/source/introduction-machine-learning/advanced-concepts")

library(h2o)
library(ggplot2)

# starts a local h2o server, alternativ waere amazon ec2 oder microsoft azure
h2o.init(nthreads=-1, max_mem_size="4G")

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
mnist[,1]<-as.factor(mnist[,1])

train_rows <- sample(nrow(mnist), .7 * (nrow(mnist)))
train <- mnist[train_rows,]
test <- mnist[-train_rows,]


train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)


# auto encoder, kind of automatic feature engineering
# erste column ist immer das laben wenn kein header angegeben
nn_model = h2o.deeplearning(
  x = 2:785,
  training_frame = train_h2o,
  hidden = c(400, 200, 2, 200, 400 ),
  epochs = 10, #epochs = 600,
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


# Prediction for test set
pred <- h2o.predict(nn_model, 
                          newdata = test_h2o[,-1])

as.data.frame(pred)



summary(prediction$p1, exact_quantiles=TRUE)


summary(prediction)
perf <- h2o.performance(nn_model, test_h2o)


# shut down virtual H2O cluster
h2o.shutdown(prompt = FALSE)


