library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)
inTrain<-createDataPartition(y=segmentationOriginal$Case, p=0.5, list=FALSE)
trainF<-segmentationOriginal[inTrain,]
testF<-segmentationOriginal[-inTrain,]
head(trainF)
str(trainF)
# 2. Set the seed to 125 and fit a CART model with the rpart method using all 
#predictor variables and default caret settings. (The outcome class is contained
# in a factor variable called Class with levels "PS" for poorly segmented 
# and "WS" for well segmented.)
modFit <- train(Class ~ ., method = "rpart", data = trainF)
library(rattle)
modFit$finalModel
fancyRparPlot(modFit$finalModel)
fancyRpartPlot(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)




3.
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
modolive <- train(Area ~ ., method = "rpart", data = olive)
modolive
predict(modolive, newdata = newdata)


4.
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)



set.seed(13234)
modelSA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                 data = trainSA, method = "glm", family = "binomial")
missClass = function(values, prediction){sum(((prediction > 0.5) * 1) != values) / length(values)}

missClass(testSA$chd, predict(modelSA, newdata = testSA))
missClass(trainSA$chd, predict(modelSA, newdata = trainSA))


5.
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(randomForest)
modvowel <- randomForest(y ~ ., data = vowel.train)
order(varImp(modvowel), decreasing = T)
