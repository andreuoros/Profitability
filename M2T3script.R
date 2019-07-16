#### Installing packages ####

install.packages("pacman")
library(pacman)

p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, C50, party, reshape, dplyr)

#### Loading data ####

ExistProd<- read_csv("existingproductattributes2017.csv")

NewProd <- read_csv("newproductattributes2017.csv")

summary(ExistProd)

#### Preprocessing data ####

sum(is.na(ExistProd))

sum(is.na(NewProd))

ExistProd$ProductNum <- NULL 

ExistProd$BestSellersRank <- NULL

NewProd$ProductNum <- NULL

str(ExistProd$ProductType)

ExistProd$ProductType <- as.factor(ExistProd$ProductType)

ExistProd <- createDummyFeatures(obj = ExistProd, cols = "ProductType")

NewProd$ProductType <- as.factor(NewProd$ProductType)

NewProd <- createDummyFeatures(obj = NewProd, cols = "ProductType")

str(ExistProd)

#### Normalization ####

for (i in c(2:15)){
  ExistProd[,i] <- scale(ExistProd[,i])
}

for (i in c(2:15)){
  NewProd[,i] <- scale(NewProd[,i])
}

#### Outliers ####

##### Correlation Matrix ####

CM <- cor(ExistProd)

x <- findCorrelation(CM, cutoff = 0.9, names = TRUE)

ExistProd <- ExistProd[, - which(names(ExistProd) %in% x)]


ExistProd$x3StarReviews <- NULL

ExistProd$x5StarReviews <- NULL

ExistProd$x2StarReviews <- NULL

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





GBMmodel <- caret::train(Volume ~ ., data=training, method = "gbm", tuneLength = 2, trControl = ctrl)

summary(GBMmodel)

GBMmodel

#### Predictions ####

predictions <- predict(object = model, newdata =  NewProd2)

summary(predictions)

predictions

#### ANOVA Test ####

res.aov <- aov(Volume ~ ., data = ExistProd)

summary(res.aov)
 
TukeyHSD(res.aov)
