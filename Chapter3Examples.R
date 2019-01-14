# apropos("confusion")
# RSiteSearch("confusion", restrict = "functions")
library(AppliedPredictiveModeling)
library(e1071)
library(caret)
library(corrplot)
library(tidyverse)
data(segmentationOriginal)
segData <- subset(segmentationOriginal, Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]

statusColNum <- grep("Status", names(segData))
segData <- segData[, -statusColNum]

skewness(segData$AngleCh1)
# Since all the predictors are numeric columns, the apply function can
# be used to compute the skewness across columns.
skewValues <- apply(segData, 2, skewness)
head(skewValues)


Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
head(segData$AreaCh1)
predict(Ch1AreaTrans, head(segData$AreaCh1))
(819^(-.9) - 1)/(-.9) #Math checks out

pcaObject <- prcomp(segData,center = TRUE, scale. = TRUE)
# Calculate the cumulative percentage of variance which each component
# accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]
head(pcaObject$x[, 1:5])

trans <- preProcess(segData,method = c("BoxCox", "center", "scale", "pca"))
# Apply the transformations:
transformed <- predict(trans, segData)
# These values are different than the previous PCA components since
# they were transformed prior to PCA
head(transformed[, 1:5])

#Filtering
nearZeroVar(segData)
# When predictors should be removed, a vector of integers is
# returned that indicates which columns should be removed.

correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

corrplot(correlations, order = "hclust")
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

#Creating Dummy Variables
carSubset <-cars %>%  select(Price,Mileage,Type)
levels(carSubset$Type)
simpleMod <- dummyVars(~Mileage + Type,data = carSubset, levelsOnly = TRUE)
simpleMod
predict(simpleMod, head(carSubset))
withInteraction <- dummyVars(~Mileage + Type + Mileage:Type,data = carSubset,levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(carSubset))