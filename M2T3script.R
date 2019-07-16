#### Installing packages ####

install.packages("pacman")
library(pacman)

p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, C50, party, reshape, dplyr, 
       markdown, ggpubr, tidyr, hydroGOF, BBmisc, tidyverse, textclean, 
       inum, doParallel, Hmisc, caretEnsemble, mboost, cluster, ade4, factoextra, asbio, SatMatch, FactoMineR, fpc,
       e1071, randomForest, rstudioapi, MASS, ParamHelpers, mlr 
)

#### Enable parallel computing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#### Loading data ####

ExistProd<- read_csv("existingproductattributes2017.csv")

NewProd <- read_csv("newproductattributes2017.csv")

summary(ExistProd)

#### 1. Exploration ####

#### 2. Preprocessing: Missing values & Duplicates ####

ExistProd <- mutate(ExistProd, id = rownames(ExistProd))

duplicated(ExistProd)

is.na(NewProd)

sum(is.na(ExistProd))

is.na(ExistProd)

#Function to exclude certain columns defined in the null-vector
null <- c("BestSellersRank", "ProductNum")

for (i in null) {
  ExistProd[which((names(ExistProd) == i))] <- NULL
}


#### 2. Preprocessing: Dummify the data ####

# ExistProd$ProductType <- dummyVars(" ~ .", data = ExistProd$ProductType)

# readyData <- data.frame(predict(newDataFrame, newdata = ExistProd))

str(ExistProd$ProductType)

ExistProd$ProductType <- as.factor(ExistProd$ProductType)

ExistProd2 <- createDummyFeatures(obj = ExistProd, cols = "ProductType")

str(ExistProd2)


#### 2. Preprocessing: Outliers ####

features <- colnames(ExistProd2[,c(1:15)])
function_outliers <- function(features, data) {
  
  outliers <- data.frame(matrix(ncol = length(features), nrow = 0))
  colnames(outliers) <- features
  #b <- vector()
  
  outliers_list <- NULL
  outliers_rowsAll <- NULL
  
  for (i in features) {
    bp <- boxplot(data[, c(i)], main = paste("Boxplot - ", i))
    #c <- c(c, row.names(data[which(data$i %in% bp$out),]))
    #b <- c(b, length(bp$out))                                                   #number of outliers for every feature
    outliers[1, i] <- length(bp$out)                                             #number of outliers of each feature
    outliers_list[[i]] <- data.frame(as.numeric(row.names(data[which(data[,i] %in% bp$out),])), bp$out)  #list of outliers 
  }
  
  outliers <<- outliers
  outliers_list <<- outliers_list
  
  for (i in features) {
    outliers_rowsAll <- c(outliers_rowsAll, outliers_list[[i]][,1])
  }
  
  total_outliers <<- sort(outliers_rowsAll[!duplicated(outliers_rowsAll)])
  message("The dataset ", deparse(substitute(data))," has been analysed in terms of outliers using boxplots. Check boxplot(s) in the window 'plot'!")
  message("The dataset ", deparse(substitute(data))," contains a total of ", length(total_outliers), " outliers.")
  
  if (length(total_outliers != 0)){
    if (readline("Do you want to exclude outliers for modelling? (Type 'yes' or 'no'!) ") == "yes") {
      ouliers_output <<- data[-total_outliers,]  #removes outlier rows
      message(length(total_outliers), " outliers have been excluded.")
    } else { 
      message("No outliers have been excluded")
    }
  }
}

function_outliers(features, ExistProd2)

#### 3. Selection/Engineering - Correlation Matrix ####

CorrelationMatrixEP2 <- cor(ExistProd2, y = NULL, use = "everything",
                            method = "pearson")

findCorrelation(CorrelationMatrixEP2, cutoff = 0.9, verbose = TRUE, names = TRUE)

corrplot(CorrelationMatrixEP2, order="hclust",
         tl.col = "black", tl.srt = 90 , tl.cex = 0.5, tl.pos = "t")

#### 4. Modelling - Creating train and test set ####
#Using Subsetting function with input lab = label, p = partition, data = dataset

subsetting <- function(lab, p, data){     #p: train size in .xx %; lab: label to be selected
  set.seed(123)
  inTrain <<- createDataPartition(y = lab,               ## the outcome data are needed
                                  p = p,            ## the percentage of data in the training set
                                  list = FALSE)             ## the format of the results. 
  ## The output is a set of integers for the rows that belong in the training set.
  
  training <<- data[inTrain,]
  testing  <<- data[-inTrain,]
  
  str(inTrain)
  prop.table(table(training$Volume)) * 100
  prop.table(table(testing$Volume)) * 100
}

subsetting(ExistProd2$Volume, 0.80, ExistProd2)

ctrl <- trainControl(method = "cv", number = 10)
nrow(training)
nrow(testing)

str(inTrain)

#### 4. Modelling - TRAIN MODELS ####
#rf to check iomportance of features
mRF <- randomForest(Volume ~., ExistProd2, ntree = 200)
plot(mRF)
varImpPlot(mRF)


a <- c("lm", "rf", "knn", "svmLinear", "gbm")

compare.model <- c()




