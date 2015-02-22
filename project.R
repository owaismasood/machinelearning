library(plyr)
library(caret)
library(rpart)
library(MASS)
library(randomForest)

## LOADING TRAINING AND TESTING DATASETS
df <- read.csv('pml-training.csv',stringsAsFactors = FALSE)
dftesting <- read.csv('pml-testing.csv',stringsAsFactors = FALSE)
df$classe <- as.factor(df$classe)

## CREATING TRAINING AND VALIDATION DATASETS
index <- createDataPartition(y=df$classe, p = 0.8, list = FALSE)
trainingData <- df[index,]
validationData <- df[-index,]

#PREPROCESSING PREDICTORS
predictors <- trainingData[,-c(1,160)]
naColCount <-function(x){sum(is.na(x) | x == '')}
naCount <- colwise(naColCount)(predictors)
boxplot(naCount)
temp <- naCount[,naCount > 0]

##CREATING A CLEAN TRAINING DATASET
cleanPredictors <- predictors[,!names(predictors) %in%  names(temp)]
cleanTrainingDateset <-  cleanPredictors[,-(1:6)]
cleanTrainingDateset$classe <- as.factor(trainingData$classe)

#MODEL SELECTION (1.RANDOM FOREST 2. CLASSICATION TREE 3. LDA )

rfModel <- randomForest(classe ~ . , data = cleanTrainingDateset , ntree = 10 ,  maxnoddes = 8)
rpartModel <-  rpart(classe ~ . , data = cleanTrainingDateset)
ldaModel <- lda(classe ~ . , data = cleanTrainingDateset)

#rpartTrain <- train(classe ~ . , data = cleanTrainingDateset , method ='rpart')
#treeModel <-  tree(classe ~ . ,  data = cleanTrainingDateset , control = tree.control(nobs = nrow(cleanTrainingDateset), mindev = 0 , minsize = 2))

##CROSS VALIDATION

predictRfTraining <-  predict(rfModel , cleanTrainingDateset)
predictRfValidation <- predict(rfModel,validationData)

cmRf <- confusionMatrix(predictRfValidation,validationData$classe)


predictTrainingRpart <- predict(rpartModel,cleanTrainingDateset , type = 'class')
predictValidationRpart <- predict(rpartModel , validationData , type = 'class')

cmRpart <- confusionMatrix(predictValidationRpart,validationData$classe)

predictLdaTraining <- predict(ldaModel,cleanTrainingDateset)$class
predictLdaValidation<- predict(ldaModel,validationData)$class

cmLda <- confusionMatrix(predictLdaValidation,validationData$classe)

cmRf$overall[1]
cmRpart$overall[1]
cmLda$overall[1]
