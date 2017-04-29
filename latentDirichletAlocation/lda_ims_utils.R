
#install.packages("stargazer")
#library(stargazer)
#library(RTextTools)
#library(sqldf)
##library(maxent)
#library(randomForest)
#library(reshape2)
#library(ipred)
#library(wordcloud)
#library(lsa)
#library(cluster)

library("tm")
library("maxent")
library("ldatuning")

prepareImsData <- function(trainFile, testFile) {
  ims_train_data <- read.csv(trainFile, sep="\t", quote = "")
  ims_test_data <- read.csv(testFile, sep="\t", quote = "")
  
  dim(ims_train_data)
  dim(ims_test_data)
  
  ims_train_data$concat <- do.call(paste, c(ims_train_data[c("Field1", "Field2", "Field3", "Field4", "Field5", "Identifier1", "Identifier2")], sep = " "))
  ims_test_data$concat <- do.call(paste, c(ims_test_data[c("Field1", "Field2", "Field3", "Field4", "Field5", "Identifier1", "Identifier2")], sep = " ")) 
  
  train.data <- ims_train_data[,c("ReferenceID", "concat")]
  test.data <- ims_test_data[,c("ReferenceID", "concat")]
  
  dim(train.data)
  dim(test.data)
  
  labels_not_na <- !is.na(train.data$ReferenceID)
  indices <- which(labels_not_na == TRUE)
  
  test_labels_not_na <- !is.na(test.data$ReferenceID)
  test_indices <- which(test_labels_not_na == TRUE)
  
  train.data <- train.data[indices,]
  test.data <- test.data[test_indices,]
  dim(train.data)
  dim(test.data)
  
  list(train=train.data, test=test.data)
}

prepareImsData2classes <- function(trainFile, testFile) {
  ims_train_data <- read.csv(trainFile, sep="\t", quote = "")
  class(ims_train_data)
  
  ims_train_data <- ims_train_data[ims_train_data$ReferenceID == c(3195400046, 3168650095, 3066109172),]
  #ims_train_data <- ims_train_data[sample(nrow(ims_train_data), 100),] 
  
  ims_test_data <- read.csv(testFile, sep="\t", quote = "")
  ims_test_data <- ims_test_data[ims_test_data$ReferenceID == c(3195400046, 3168650095, 3066109172),]
  #ims_test_data <- ims_test_data[sample(nrow(ims_test_data), 20),] 
  
  dim(ims_train_data)
  dim(ims_test_data)
  
  ims_train_data$concat <- do.call(paste, c(ims_train_data[c("Field1", "Field2", "Field3", "Field4", "Field5", "Identifier1", "Identifier2")], sep = " "))
  ims_test_data$concat <- do.call(paste, c(ims_test_data[c("Field1", "Field2", "Field3", "Field4", "Field5", "Identifier1", "Identifier2")], sep = " ")) 
  
  train.data <- ims_train_data[,c("ReferenceID", "concat")]
  test.data <- ims_test_data[,c("ReferenceID", "concat")]
  
  dim(train.data)
  dim(test.data)
  
  labels_not_na <- !is.na(train.data$ReferenceID)
  indices <- which(labels_not_na == TRUE)
  
  test_labels_not_na <- !is.na(test.data$ReferenceID)
  test_indices <- which(test_labels_not_na == TRUE)
  
  train.data <- train.data[indices,]
  test.data <- test.data[test_indices,]
  dim(train.data)
  dim(test.data)
  
  list(train=train.data, test=test.data)
}

createRawMatrices <- function(imsData, sparseLevel=0,  ngramLength = 1) {
  start.time <- Sys.time()
  #data <- prepareImsData(trainFile, testFile)
  train.data = imsData$train
  test.data = imsData$test
  
  train_doc_matrix <- create_matrix(train.data$concat, language="english", removeNumbers=FALSE, stemWords=FALSE, 
                                    removeSparseTerms=sparseLevel, ngramLength=ngramLength)
  
  #Run this:
  #   trace("create_matrix",edit=T)
  #In the source code box that pops up, line 42 will have a misspelling of the word "acronym". Change the "A" to an "a" and hit "Save" - it should work fine after that.
  
  test_doc_matrix <- create_matrix(test.data$concat, language="english", removeNumbers=FALSE, stemWords=FALSE, 
                                   removeSparseTerms=sparseLevel, originalMatrix = train_doc_matrix, ngramLength=ngramLength)
  
  dim(train_doc_matrix)
  dim(test_doc_matrix)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  list(rawTrainMatrix=train_doc_matrix, rawTestMatrix=test_doc_matrix, duration=duration)
}







trainRandomForest <- function(trainMatrix, trainLabels, ntree){
  
  start.time <- Sys.time()
  model <- randomForest(x=as.matrix(trainMatrix), y=trainLabels, ntree=ntree, keep.forest=TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  list(trainingDuration=duration, model=model)
}

test <- function(model, testMatrix, test_code) {
  
  bridge_for_threshold <- function(threshold){
    
    confidence_value <- apply(out,1, max)
    confidence_value_for_threshold <- confidence_value > threshold
    indices_for_threshold <- which(confidence_value_for_threshold)
    
    if(length(indices_for_threshold) > 0){
      out_for_threshold <- out[indices_for_threshold,]
      test_code_for_threshold <-   test_code[indices_for_threshold]
      
      best.class.index <- apply(out_for_threshold,1, which.max)
      best.class <- column.names[best.class.index]
      test.result <- best.class[best.class == test_code_for_threshold]
      error <- 1-length(test.result)/length(test_code_for_threshold)
      
      selected_count <- length(indices_for_threshold)
      selected_count_ratio <- selected_count/length(test_code)
      
      list(error_ratio=error*100, bridge_ratio=selected_count_ratio*100, bridge_count=selected_count)
    }else{
      list(error_ratio=0, bridge_ratio=0, bridge_count=0)
    }
  }
  
  out <- predict(model, as.matrix(testMatrix), type="prob")
  column.names <- colnames(out)
  
  threshold = seq(0,1,by=0.01)
  
  bridge <- lapply(threshold, bridge_for_threshold)
  bridge_ratio <- lapply(bridge, function(x){x$bridge_ratio})
  error_ratio <- lapply(bridge, function(x){x$error_ratio})
  
  list(threshold=threshold, bridgeRatio=bridge_ratio, errorRatio=error_ratio)
}

testWithoutThreshold <- function(model, testMatrix, test_code){
  
  out <- predict(model, as.matrix(testMatrix), type="prob")
  threshold <- seq(0)
  
  test.result <- out[out == test_code]
  error <- 1-length(test.result)/length(test_code)
  
}

plotResults <- function(threshold, bridgeRatio, errorRatio) {
  par(mfrow=c(1,3))
  
  plot(threshold, errorRatio, type = "l", main = "Poziom b³êdu (%)", xlab = "Próg pewnoœci", ylab = "Poziom b³êdu (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  plot(threshold, bridgeRatio, type = "l", main = "Poziom klasyfikacji (%)", xlab = "Próg pewnoœci", ylab = "Poziom klasyfikacji (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  plot(bridgeRatio, errorRatio, type = "l", main = "Poziom klasyfikacji vs. Poziom b³êdu", xlab = "Poziom klasyfikacji (%)", ylab = "Poziom b³êdu (%)", col="red", col.axis = "dimgray", col.lab = "blueviolet")
  
}

plotWordCloud <- function(imsData){
  
  imsCorpus <- Corpus(VectorSource(imsData$train))
  imsCorpus <- tm_map(imsCorpus, PlainTextDocument)
  wordcloud(imsCorpus, max.words = 300, random.order = FALSE)
}

prepareCleanedData <- function(imsData, sparseLevel=.998, ngramCount = 1){

  start.time <- Sys.time()
  train_code <- as.factor(imsData$train$ReferenceID)
  test_code <- as.factor(imsData$test$ReferenceID)
  
  rawMatrices <- createRawMatrices(imsData, sparseLevel=sparseLevel, ngramLength=ngramCount)
  
  testRowTotals <- apply(rawMatrices$rawTestMatrix, 1, sum) #Find the sum of words in each Document
  cleanedTestMatrix   <- rawMatrices$rawTestMatrix[testRowTotals> 0, ]
  cleanedTestLabels <- test_code[testRowTotals> 0]
  
  rowTotals <- apply(rawMatrices$rawTrainMatrix, 1, sum) #Find the sum of words in each Document
  cleanedTrainMatrix   <- rawMatrices$rawTrainMatrix[rowTotals> 0, ]
  cleanedTrainLabels <- train_code[rowTotals> 0]
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  list(cleanedTrainMatrix=cleanedTrainMatrix, cleanedTrainLabels=cleanedTrainLabels,
       cleanedTestMatrix=cleanedTestMatrix, cleanedTestLabels=cleanedTestLabels, duration=duration)
}

calculateLDA <- function(cleanedData, topic_number){
  
  start.time <- Sys.time()
  topicmodel <- LDA(cleanedData$cleanedTrainMatrix, k=topic_number, method="Gibbs", control=list(seed=SEED))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  trainData <- posterior(topicmodel)[2]$topics
  testData <- posterior(topicmodel, cleanedData$cleanedTestMatrix)[2]$topics
  
  list(topicmodel=topicmodel, ldaTrainData=trainData, ldaTestData=testData, duration=duration)
}

testLda <- function(k, cleandData){
  printf("Calculating lda fot %d topics\n",k)
  lda <- calculateLDA(cleanedData, k)
  printf("Training lda took  %f seconds\n",lda$duration)
  error <- trainAndPredictSimple(tree_number, lda$ldaTrainData, cleanedData$cleanedTrainLabels, 
                                 lda$ldaTestData, cleanedData$cleanedTestLabels)
  list(error=error, topicCount=k, duration=lda$duration)
}

trainAndPredict <- function(tree_number, trainData, trainLabels, testData, testLabels){
  start.time <- Sys.time()
  model <- randomForest(x=as.matrix(trainData), y=trainLabels, ntree=tree_number, keep.forest=TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  testResult <- test(model, testData, testLabels)
  list(model=model, testResult=testResult, duration=duration)
}

trainAndPredictSimple <- function(tree_number, trainData, trainLabels, testData, testLabels){
  start.time <- Sys.time()
  model <- randomForest(x=as.matrix(trainData), y=trainLabels, ntree=tree_number, keep.forest=TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  out <- predict(model, as.matrix(testData))
  test.result <- out[out != testLabels]
  errorRate <- length(test.result)/length(testLabels)
  errorRate
}

estimateTopicsCount <- function(from,to,step, cleanedData){
  
  start.time <- Sys.time()
  topicCounts <- seq(from,to,step)
  
  results <- lapply(topicCounts, testLda)
  errors <- lapply(results, function(x){x$error})
  
  result <- FindTopicsNumber(
    cleanedData$cleanedTrainMatrix,
    topics = seq(from = from, to = to, by = step),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 4L,
    verbose = TRUE
  )
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  duration <- time.taken
  
  list(ldatuningResults=result, rfResult=errors, duration=duration)
}

printf <- function(...) cat(sprintf(...), sep='\n', file=stderr())

create_matrix <- function (textColumns, language = "english", minDocFreq = 1, 
                             maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
                             ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, 
                             removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, 
                             stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
                             weighting = weightTf) 
  {
    stem_words <- function(x) {
      split <- strsplit(x, " ")
      return(wordStem(unlist(split), language = language))
    }
    tokenize_ngrams <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = ngramLength))
    #tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, 
    #                                                                                              method = "string", n = n)))))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = ngramLength))
    

    control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)), 
                    language = language, tolower = toLower, removeNumbers = removeNumbers, 
                    removePunctuation = removePunctuation, stopwords = removeStopwords, 
                    stripWhitespace = stripWhitespace, wordLengths = c(minWordLength, 
                                                                       maxWordLength), weighting = weighting)
    if (ngramLength > 1) {
      control <- append(control, list(tokenize = BigramTokenizer), 
                        after = 7)
    }
    else {
      control <- append(control, list(tokenize = scan_tokenizer), 
                        after = 4)
    }
    if (stemWords == TRUE && ngramLength == 1) 
      control <- append(control, list(stemming = stem_words), 
                        after = 7)
    trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
                            collapse = " ")
    trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
                             iconv, to = "UTF8", sub = "byte")
    corpus <- VCorpus(VectorSource(trainingColumn), readerControl = list(language = language))
    matrix <- DocumentTermMatrix(corpus, control = control)
    if (removeSparseTerms > 0) 
      matrix <- removeSparseTerms(matrix, removeSparseTerms)
    if (!is.null(originalMatrix)) {
      terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in% 
                                                 colnames(matrix))])
      weight <- 0
      if (attr(weighting, "acronym") == "tf-idf") 
        weight <- 1e-09
      amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
      colnames(amat) <- terms
      rownames(amat) <- rownames(matrix)
      fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% 
                                                            colnames(originalMatrix))], amat), weighting = weighting)
      matrix <- fixed
    }
    matrix <- matrix[, sort(colnames(matrix))]
    gc()
    return(matrix)
}

printDocumentTermMatrix <- function(dtm){
  mat=as.data.frame(as.matrix(dtm)) # you get the dataframe from DocumentTerm Matrix 
  rowCount <- nrow(dtm)
  colCount <- ncol(dtm)
  similMatrix = matrix(nrow = rowCount, ncol = rowCount)
  similMatrix[ row(similMatrix) >= col(similMatrix) ] <- 0
  for(i in 1:(rowCount)){ #set all columns NA you can change to zeros if you need later
    similMatrix[i,i]=NA
  } # then we will do the actual job
  for(i in 1:rowCount ){  # rows
    for (j in 1:rowCount ){      # cols
      if(is.na(similMatrix[i,j])==F){
        a=mat[i,]
        b=mat[j,]
        for(k in 1:colCount){   #n number of Cols in Document term matrix
          
          if(a[k]==1 && a[k]==b[k]){
            similMatrix[i,j]=similMatrix[i,j]+1
          }
        }
      }
    }
  }
}