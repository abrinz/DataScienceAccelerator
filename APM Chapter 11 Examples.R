library(AppliedPredictiveModeling)
set.seed(975)
simulatedTrain <- quadBoundaryFunc(500)
simulatedTest <- quadBoundaryFunc(1000)
head(simulatedTrain)

library(randomForest)
rfModel <- randomForest(class ~ X1 + X2,data = simulatedTrain,ntree = 2000)
library(MASS) ## for the qda() function
qdaModel <- qda(class ~ X1 + X2, data = simulatedTrain)

qdaTrainPred <- predict(qdaModel, simulatedTrain)
names(qdaTrainPred)
head(qdaTrainPred$class)
head(qdaTrainPred$posterior)

qdaTestPred <- predict(qdaModel, simulatedTest)
simulatedTrain$QDAprob <- qdaTrainPred$posterior[,"Class1"]
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]

rfTestPred <- predict(rfModel, simulatedTest, type = "prob")
head(rfTestPred)

simulatedTest$RFprob <- rfTestPred[,"Class1"]
simulatedTest$RFclass <- predict(rfModel, simulatedTest)
library(caret)

sensitivity(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class1")
specificity(data = simulatedTest$RFclass,reference = simulatedTest$class,negative = "Class2")

posPredValue(data = simulatedTest$RFclass, reference = simulatedTest$class, positive = "Class1")
negPredValue(data = simulatedTest$RFclass, reference = simulatedTest$class, positive = "Class2")
posPredValue(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class1",prevalence = .9)
confusionMatrix(data = simulatedTest$RFclass, reference = simulatedTest$class, positive = "Class1")


#ROC
library(pROC)
rocCurve <- roc(response = simulatedTest$class,predictor = simulatedTest$RFprob,levels = rev(levels(simulatedTest$class)))
auc(rocCurve)
ci.roc(rocCurve) #DNW
plot(rocCurve, legacy.axes = TRUE)

#Lift charts
labs <- c(RFprob = "Random Forest",QDAprob = "Quadratic Discriminant Analysis")
liftCurve <- lift(class ~ RFprob + QDAprob, data = simulatedTest,labels = labs)
liftCurve
xyplot(liftCurve, auto.key = list(columns = 2, lines = TRUE, points = FALSE))

#Calibrating probabilities
calCurve <- calibration(class ~ RFprob + QDAprob, data = simulatedTest)
xyplot(calCurve, auto.key = list(columns = 2))

sigmoidalCal <- glm(relevel(class, ref = "Class2") ~ QDAprob,data = simulatedTrain,family = binomial)
coef(summary(sigmoidalCal))

sigmoidProbs <- predict(sigmoidalCal, newdata = simulatedTest[,"QDAprob", drop = FALSE],type = "response")
simulatedTest$QDAsigmoid <- sigmoidProbs

library(klaR)
BayesCal <- NaiveBayes(class ~ QDAprob, data = simulatedTrain,usekernel = TRUE)
BayesProbs <- predict(BayesCal,newdata = simulatedTest[, "QDAprob", drop = FALSE])
simulatedTest$QDABayes <- BayesProbs$posterior[, "Class1"]
head(simulatedTest[, c(5:6, 8, 9)])

calCurve2 <- calibration(class ~ QDAprob + QDABayes + QDAsigmoid,data = simulatedTest)
 xyplot(calCurve2)
 