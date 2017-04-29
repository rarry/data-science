

source('C:/doc/workspace/data-science/latentDirichletAlocation/lda_ims_utils.R')
library("topicmodels")
library("tm")
library(randomForest)
library(RWeka)
library("ldatuning")

tree_number=100


trainFilePath = 'C:/work/r/data/train_20.csv'
trainFile <- trainFilePath
testFilePath = 'C:/work/r/data/test_20.csv'
testFile <- testFilePath

#trainFilePath = 'C:/work/r/data/training_data.csv'
#testFilePath = 'C:/work/r/data/testing_data.csv'

remove(imsData)
imsData <- prepareImsData(trainFile=trainFilePath, testFile=testFilePath)
#imsData <- prepareImsData2classes(trainFile=trainFilePath, testFile=testFilePath)
dim(imsData$train)
dim(imsData$test)

cleanedData <- prepareCleanedData(imsData, ngramCount = 1)

res <- estimateTopicsCount(2,100,1, cleanedData)


























topicCounts <- seq(2,100,10)

testLDAWithCleanedData = function(k){
  testLda(k, cleanedData)
}

results <- lapply(topicCounts, testLda)
errors <- lapply(results, function(x){x$error})
durations <- lapply(results, function(x){
    units(x$duration) <- "secs"
    x$duration
  })

dev.new()
plot(topicCounts, errors, type = "l", main = "Estymacja liczby tematów/czasu dzia³ania", xlab = "Liczba tematów", 
     ylab = "Poziom b³êdu (%)/czas dzia³ania", col="red", col.axis = "dimgray", col.lab = "blueviolet")

par(new=TRUE)
plot(topicCounts, durations,type = "l", col="blue", xlab = "", ylab="")




result <- FindTopicsNumber(
  cleanedData$cleanedTrainMatrix,
  topics = seq(from = 2, to = 100, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  #metrics = c("Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)


result <- res$ldatuningResults
dev.new()
plot(result$topics, result$Griffiths2004,type = "l", col="blue", xlab = "", ylab="")
par(new=TRUE)
plot(result$topics, result$CaoJuan2009,type = "l", col="red", xlab = "", ylab="")
par(new=TRUE)
plot(result$topics, result$Arun2010,type = "l", col="green", xlab = "", ylab="")
par(new=TRUE)
plot(result$topics, result$Deveaud2014,type = "l", col="violet", xlab = "", ylab="")


errors <- res$rfResult
par(new=TRUE)
plot(result$topics, errors, type = "l", main = "Estymacja liczby tematów/czasu dzia³ania", xlab = "Liczba tematów", 
     ylab = "Poziom b³êdu (%)/czas dzia³ania", col="black", col.axis = "dimgray", col.lab = "blueviolet")

help(plot)
library(RColorBrewer)
#all palette available from RColorBrewer

dev.new()
display.brewer.all()


res <- estimateTopicsCount(2,3,1, cleanedData)
