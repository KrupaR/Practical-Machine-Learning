# Exercise Analysis - Course Project
# Last updated 2-Apr-2016

# Load libraries
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

# Download datafiles and read into dataframes
	setwd("D:/Data specialist course/Practical Machine Learning/Course Project")
	if (!file.exists("pml-training.csv"))
	{
  		download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
	}
	if (!file.exists("pml-testing.csv"))
	{
  		download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
	}

	training <- read.csv("pml-training.csv", sep = ",", na.strings = c("", "NA"))
	testing <- read.csv("pml-testing.csv", sep = ",", na.strings = c("", "NA"))

#Explore Data
	dim(training)
	dim(testing)
	str(training)

# Preprocess Data

# Step 1: Eliminate column 1 as it is not relevant
	training <- training[,-1]

# Step 2: Eliminate Zero and Near Zero variance olumns
	myDataNZV <- nearZeroVar(training, saveMetrics=TRUE)
	NZVList <- c()
	for (i in 1:159)
	{
    		if((myDataNZV[i,3] == TRUE) | (myDataNZV[i,4] == TRUE)) NZVList <- c(NZVList,i)
	}
	training <- training[,-NZVList]

# Step 3: Eliminate columns having more than 80% values as NA
	NaList <- c()
	MaxEntries <- nrow(training)
	for (i in 1:ncol(training))
	{
		if(sum(is.na(training[ ,i])) > MaxEntries*0.8)NaList <- c(NaList,i)
	}
	cleanedTraining <- training[,-NaList]
	dim(cleanedTraining)

# Data Partition
# Partition 70% of Data into Training Data and remaining 30% as Testing Data; These wil be used for building the model

	inTrain <- createDataPartition(y=cleanedTraining$classe, p=0.7, list=FALSE)
	myTraining <- cleanedTraining[inTrain, ]
	myTesting <- cleanedTraining[-inTrain, ]

# Final Testing data will have to have the same columns to test the model
	cleanCols <- colnames(myTraining[, -58])
	testing <- testing[cleanCols]

# Display dimensions
	dim(myTraining)
	dim(myTesting)
	dim(testing)

#Ensure that predictors in myTraining and Testing dataframes have the same class
	for (i in 1:length(testing) ) 
	{
    		for(j in 1:length(myTraining)) 
    		{
        		if( names(myTraining[j]) == names(testing[i])) 
       		{
            		class(testing[i]) <- class(myTraining[j])
        		}      
    		}      
	}


#Ensure coertion by adding and deleting a row from myTraining dataframe to testing dataframe
	testing <- rbind(myTraining[2, -58] , testing) 
	testing <- testing[-1,]

# Build Model with myTraining data using rpart(Recursive partitioning)function
# classe is the outcome variable and all other columns are predictors

	modelFit1 <- rpart(classe ~ ., data=myTraining, method="class")
	fancyRpartPlot(modelFit1)

#use the model on myTesting data and predict classe

	predictions1 <- predict(modelFit1, myTesting, type = "class")
	confusionMatrix(predictions1, myTesting$classe)


#Build Model with myTraining data using randomForest function to improve predictive accuracy
#classe is the outcome variable and all other columns are predictors

	modelFit2 <- randomForest(classe ~. , data=myTraining)
	predictions2 <- predict(modelFit2, myTesting, type = "class")
	confusionMatrix(predictions2, myTesting$classe)

# Apply it on testing data

	predictionsFinal <- predict(modelFit2, testing, type = "class")

# print output
	predictionsFinal
