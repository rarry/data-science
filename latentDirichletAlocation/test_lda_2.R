
source('C:/work/workspaces/autocoding/data-science/latentDirichletAlocation/lda_ims_utils.R')
library("topicmodels")
library("tm")
library(randomForest)
library(RWeka)
library(ngram)

trainFilePath = 'C:/work/r/data/train_20.csv'
testFilePath = 'C:/work/r/data/test_20.csv'

imsData <- prepareImsData2classes(trainFile=trainFilePath, testFile=testFilePath)

df <- imsData$train
u <- unique(df[c("ReferenceID")])
l <- as.list(u)
class(l)

length(imsData$train$concat)

trainingColumn <- apply(as.matrix(imsData$train$concat), 1, paste, 
                        collapse = " ")
trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
                         iconv, to = "UTF8", sub = "byte")
corpus <- VCorpus(VectorSource(trainingColumn), readerControl = list(language = "en"))
#matrix <- DocumentTermMatrix(corpus, control = control)

data("crude")

meta(crude[[1]])
lapply(crude[1], as.character)
lapply(corpus[1], as.character)

inspect(crude[1:2])
inspect(corpus[1:2])

class(crude)
class(corpus)
inspect(crude)
dim()
crude$`reut-00023.xml`

newBigramTokenizer = function(x) {
  tokenizer1 = NGramTokenizer(x, Weka_control(min = 2, max = 2))
  if (length(tokenizer1) != 0L) { return(tokenizer1)
  } else return(WordTokenizer(x))
}

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm[1:100,1:10])



















newBigramTokenizer = function(x) {
  return(NGramTokenizer(x, Weka_control(min = 2, max = 2)))
}

dim(DocumentTermMatrix(corpus))
inspect((DocumentTermMatrix(corpus)))























dim(DocumentTermMatrix(corpus, control=list(tokenize=newBigramTokenizer)))

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)


newBigramTokenizer = function(x) {
  return(NGramTokenizer(x, Weka_control(min = 1, max = 3)))
}

wordTokenizer = function(x) {
  return(WordTokenizer(x))
}

newBigramTokenizer = function(x) {
  tokenizer1 = NGramTokenizer(x, Weka_control(min = 2, max = 2))
  if (length(tokenizer1) != 0L) { return(tokenizer1)
  } else return(WordTokenizer(x))
} # WordTokenizer is an another tokenizer in the RWeka package.

tdm <- DocumentTermMatrix(crude, control = list(tokenize = newBigramTokenizer))
dim(tdm)
inspect(tdm)
tdm <- DocumentTermMatrix(crude, control = list(tokenize = BigramTokenizer))
dim(tdm)
inspect(tdm)
#printDocumentTermMatrix(tdm)
tdm <- DocumentTermMatrix(crude, control = list(tokenize = scan_tokenizer))
dim(tdm)
inspect(tdm)


tdm <- DocumentTermMatrix(corpus, control = list(tokenize = newBigramTokenizer))
dim(tdm)
inspect(tdm)
tdm <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
dim(tdm)
inspect(tdm)
tdm <- DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer))
dim(tdm)
inspect(tdm)

corpus[[1]]$content
a <- ngram(corpus[[1]]$content)

crude[[1]]$content
strsplit(crude[[1]]$content, split=" ")


tdm_sparse <- removeSparseTerms(tdm, 0.998)

inspect(tdm_sparse)

remove(tdm)


library("tm")
data("crude")

crude[1,]


tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
tdm <- TermDocumentMatrix(crude)
dim(tdm)
inspect(removeSparseTerms(tdm[, 1:10], 0.7))
crude
