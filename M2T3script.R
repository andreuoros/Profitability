#### Installing packages ####

install.packages("pacman")
library(pacman)

p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, C50, party, reshape, dplyr)

#### Loading data ####

ExistProd<- read_csv("existingproductattributes2017.csv")

NewProd <- read_csv("newproductattributes2017.csv")

summary(ExistProd)

# dummify the data

ExistProd$ProductType <- dummyVars(" ~ .", data = ExistProd$ProductType)

readyData <- data.frame(predict(newDataFrame, newdata = ExistProd))

#### Preprocessing data ####

ExistProd <- mutate(ExistProd, id = rownames(ExistProd))

duplicated(ExistProd)

is.na(NewProd)

sum(is.na(ExistProd))

is.na(ExistProd)

ExistProd$BestSellersRank <- NULL

str(ExistProd$ProductType)

ExistProd$ProductType <- as.factor(ExistProd$ProductType)

ExistProd2 <- createDummyFeatures(obj = ExistProd, cols = "ProductType")

str(ExistProd2)

ExistProd2$id <- NULL


##### Correlation Matrix ####


CorrelationMatrixEP2 <- cor(ExistProd2, y = NULL, use = "everything",
                            method = "pearson")

findCorrelation(CorrelationMatrixEP2, cutoff = 0.9, verbose = TRUE, names = TRUE)

corrplot(CorrelationMatrixEP2, order="hclust",
         tl.col = "black", tl.srt = 90 , tl.cex = 0.5, tl.pos = "t")

#### Creating train and test set ####

set.seed(107)
inTrain <- createDataPartition(ExistProd2$Volume,p = 0.75, list = F)
training <- ExistProd2[inTrain,]
testing <- ExistProd2[-inTrain,]
ctrl <- trainControl(method = "cv", number = 10)
nrow(training)
nrow(testing)

str(inTrain)

#### TRAIN MODELS ####

a <- c("lm", "rf", "knn", "svmLinear", "gbm")

compare.model <- c()

for (i in a) {
  
  model <- caret::train(Volume ~., data = training, method = i, )
  
  pred <- predict(model, newdata = testing)
  
  pred.metric <- postResample(testing$Volume, obs = pred)
  
  compare.model <- cbind(pred.metric, compare.model)
  
}


