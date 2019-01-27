library(caret)
library(RANN)

#Step 0: Import the data
heart <- read.csv('./HDdata.csv')
head(heart) #Making sure the data looks good
dim(heart)
#Step 1: Choosing an appropriate model type
#I'm going to choose a logistic regression model because the target variable
#is a binary categorical variable: it's either 0 or 1. Logistic regression would be best
#for this because it constrains probability to be between 0 and 1.

#Step 2: Dividing data into training and testing
set.seed(975) #specifying a seed for a random number generator. I chose 975 to emulate the book
#I'm going to split the data into 75%/25% portions since there are only 303 observations
training_size <- floor(.75*nrow(heart))
forTraining <-  sample(seq_len(nrow(heart)), size = training_size)
train <- heart[forTraining,]
test <- heart[-forTraining,]

#Step 3: In your data are there:
#a. NAs?
naCount <- is.na(heart)
sum(naCount)
#Yes, there are NAs - 10 of them and only in age

#b. categorical values?
str(heart)
#Yes: sex, fbs, cp, restecg, exang, num

#c.
#Yes: sex, fbs, exang, num
#I'm working under the assumption that binary variables are categorical, based on my reading

#d.
#Yes: age, trestbps, chol, thalac, oldpeak

#For preprocessing, first step is to impute the nulls
#The missing data looks to be MCAR
#From chapter 3 in the textbook, I'm going to use the preprocess function
heartP <- preProcess(heart,method = c("medianImpute"))
heartClean <- predict(heartP,heart)
head(heartClean)

#Step 4: fitting a logistic model to the training set
#Need to make the training set off of the pre-processed data, so I'm going to re-run the lines above
train <- heartClean[forTraining,]
test <- heartClean[-forTraining,]
modelFit <- glm(num~.,data = train,family = binomial)


#Step 5: assessing the accuracy of the model
# a. Choose a prediction threshold.
#Since we're dealing with heart disease, it's best to err on the side of caution.
#Therefore, I chose a prediction threshold of 0.5
results <- predict(modelFit, newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10)),type = "response")
results <- ifelse(results > 0.5,1,0)
misClasificError <- mean(results != test$num)
results
test$num
names(results) <- NULL
resultsT <- t(results)
testNum <- test$num

# b. Construct a confusion matrix.
confMatrix <- confusionMatrix(table(results,testNum))
#I had to do it this way because I kept getting a ton of errors of `data` and `reference` should be factors with the same levels.

# c. Calculate the accuracy, PPV, NPV, Sensitivity and Specificity.
accuracy <- 1-misClasificError #Or could look at the accuracy in the confusion matrix

#PPV = .8 (from confusion matrix)
#NPV = .667 (from confusion matrix)
#Sensitivity = 0.7273 (from confusion matrix)
#Specificity = 0.75 (from confusion matrix)