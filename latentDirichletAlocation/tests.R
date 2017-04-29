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