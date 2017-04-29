

source('C:/work/workspaces/autocoding/data-science/latentDirichletAlocation/lda_ims_utils.R')
library("topicmodels")
library("tm")
library(randomForest)
library(dplyr)
library(tidytext)

tree_number=50
k <- 5
SEED <- 2100
ngramLength=1

trainFilePath = 'C:/work/r/data/train_20.csv'
testFilePath = 'C:/work/r/data/test_20.csv'

#trainFilePath = 'C:/work/r/data/training_data.csv'
#testFilePath = 'C:/work/r/data/testing_data.csv'

imsData <- prepareImsData(trainFile=trainFilePath, testFile=testFilePath)

cleanedData <- prepareCleanedData(imsData, ngramLength=10)

tidy(cleanedData$cleanedTrainMatrix)

cleanedData$cleanedTrainMatrix[2,]

dim(cleanedData$cleanedTrainMatrix)
length(cleanedData$cleanedTrainLabels)
dim(cleanedData$cleanedTestMatrix)
length(cleanedData$cleanedTestLabels)



tree_numbers <- c(50, 500, 2000)
topic_numbers <- c(2, 5, 10)

dev.off()
windows()
par(mfrow=c(3,3))
for(topic_number in topic_numbers){
  
  lda <- calculateLDA(cleanedData, 10)
  
  for(tree_number in tree_numbers) {
    r <- trainAndPredict(tree_number, lda$ldaTrainData, cleanedData$cleanedTrainLabels, 
                         lda$ldaTestData, cleanedData$cleanedTestLabels)
    plotResults(r$testResult$threshold, r$testResult$bridgeRatio, r$testResult$errorRatio)
  }
}










plotResults <- function(threshold, bridgeRatio, errorRatio) {
  
  plot(threshold, errorRatio, type = "l", main = "Error Ratio (%)", xlab = "Threshold", ylab = "Error Ratio (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  plot(threshold, bridgeRatio, type = "l", main = "Bridge Ratio (%)", xlab = "Threshold", ylab = "Bridge Ratio (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  plot(bridgeRatio, errorRatio, type = "l", main = "Error Ratio vs. Bridge Ratio", xlab = "Bridge Ratio (%)", ylab = "Error Ratio (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  
}