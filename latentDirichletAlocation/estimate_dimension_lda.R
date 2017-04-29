

source('C:/work/workspaces/autocoding/data-science/latentDirichletAlocation/lda_ims_utils.R')
library("topicmodels")
library("tm")
library(randomForest)
library(RWeka)
library("ldatuning")

tree_number=10
k <- 2
SEED <- 2100

trainFilePath = 'C:/work/r/data/train_20.csv'
trainFile <- trainFilePath
testFilePath = 'C:/work/r/data/test_20.csv'

#trainFilePath = 'C:/work/r/data/training_data.csv'
#testFilePath = 'C:/work/r/data/testing_data.csv'

#imsData <- prepareImsData(trainFile=trainFilePath, testFile=testFilePath)
imsData <- prepareImsData2classes(trainFile=trainFilePath, testFile=testFilePath)
dim(imsData$train)
dim(imsData$test)


cleanedData1 <- prepareCleanedData(imsData, ngramCount = 1)
dim(cleanedData1$cleanedTrainMatrix)
dim(cleanedData1$cleanedTestMatrix)

class(cleanedData1$cleanedTrainMatrix)

result <- FindTopicsNumber(
  cleanedData1$cleanedTrainMatrix,
  topics = seq(from = 2, to = 100, by = 1),
  #metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  metrics = c("Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)



for(k in 2:3){
  lda <- calculateLDA(cleanedData1, k)
  error <- test(lda, lda$ldaTestData, cleanedData$cleanedTestLabels, threshold = )
  error
}

lda1 <- calculateLDA(cleanedData1, 30)

posterior(lda1$topicmodel)[2]

r <- trainAndPredict(tree_number, lda1$ldaTrainData, cleanedData1$cleanedTrainLabels, 
                      lda1$ldaTestData, cleanedData1$cleanedTestLabels)

plotResults(r$testResult$threshold, r$testResult$bridgeRatio, r$testResult$errorRatio)
