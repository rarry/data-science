

source('C:/work/workspaces/autocoding/data-science/latentDirichletAlocation/lda_ims_utils.R')
library("topicmodels")
library("tm")
library(randomForest)
library(RWeka)

tree_number=1000
k <- 30
SEED <- 2100

trainFilePath = 'C:/work/r/data/train_20.csv'
trainFile <- trainFilePath
testFilePath = 'C:/work/r/data/test_20.csv'

#trainFilePath = 'C:/work/r/data/training_data.csv'
#testFilePath = 'C:/work/r/data/testing_data.csv'

imsData <- prepareImsData(trainFile=trainFilePath, testFile=testFilePath)

#imsData$train

cleanedData1 <- prepareCleanedData(imsData, ngramCount = 1)
dim(cleanedData1$cleanedTrainMatrix)

cleanedData2 <- prepareCleanedData(imsData, ngramCount = 2)
dim(cleanedData2$cleanedTrainMatrix)

lda1 <- calculateLDA(cleanedData1, k)

posterior(lda1$topicmodel)[2]

r1 <- trainAndPredict(tree_number, lda1$ldaTrainData, cleanedData1$cleanedTrainLabels, 
                     lda1$ldaTestData, cleanedData1$cleanedTestLabels)

plotResults(r1$testResult$threshold, r1$testResult$bridgeRatio, r1$testResult$errorRatio)


lda2 <- calculateLDA(cleanedData2, k)

r2 <- trainAndPredict(tree_number, lda2$ldaTrainData, cleanedData2$cleanedTrainLabels, 
                      lda2$ldaTestData, cleanedData2$cleanedTestLabels)

plotResults(r2$testResult$threshold, r2$testResult$bridgeRatio, r2$testResult$errorRatio)















plot(r$testResult$bridgeRatio, r$testResult$errorRatio, type = "l", main = "Error Ratio vs. Bridge Ratio", 
     xlab = "Bridge Ratio (%)", ylab = "Error Ratio (%)", col="red", 
     col.axis = "dimgray", col.lab = "blueviolet")




