library(caret)
library(tm)

set.seed(825)
runML = TRUE


readData <- function(fileName) {
  f = paste(getwd(), fileName, sep = "/")
  data =  read.csv(f, header = TRUE, sep = ",")
  return (data)
}

spam <- (readData("dataset.csv"))

#performing NPL on each text 
document = tolower(as.character(spam$text))
corpus =  Corpus(VectorSource(document))
#removing white space, stopwords, punctuation, numbers
corpus <- tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, PlainTextDocument)

dtm <- t(TermDocumentMatrix(corpus));
matrix = as.matrix(dtm) 

#transforming the document term matrix into a data frame
wordMatrix = data.frame(matrix[, colnames(matrix)], row.names=NULL )
wordMatrix[, "type"] = spam$type 

#list with the frequency of each word
countFreq = c()
for(n in colnames(wordMatrix)) {
  if("type" != n) {
    s = sum(wordMatrix[, n])
    countFreq = c(s, countFreq)
  }
}

featureSelected= c()
for(n in colnames(wordMatrix)) {
  if("type" != n) {
    s = sum(wordMatrix[, n])
    if(s > (mean(countFreq) + 30)) {
      featureSelected = c(n, featureSelected)
    }
  }
}

wordMatrixFiltered <- wordMatrix[, c(featureSelected, "type")]



if(runML) {
  
  trainIndex <- createDataPartition(wordMatrixFiltered$type, p = .6, 
                                    list = FALSE, 
                                    times = 1)
  dataTrain <- wordMatrixFiltered[ trainIndex,]
  dataTest  <- wordMatrixFiltered[-trainIndex,]
  
  #cross-validation 
  fitControl <- trainControl(## 5-fold CV
    method = "repeatedcv",
    number = 5, 
    classProbs = TRUE,
    repeats = 5,
    summaryFunction = twoClassSummary)  
  
  #grid used for parameters tunning
  grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = (1:30)*50, 
                       shrinkage = 0.1,
                       n.minobsinnode = 20)
  
  modelFit <- train(type ~ . , data = dataTrain, 
                    method = "gbm",  
                    trControl = fitControl,
                    tuneGrid = grid,
                    verbose = FALSE,
                    metric = "ROC" 
  )
  
  saveRDS(modelFit, "gbm-model-spam-ham.rds")
  
  #my_model <- readRDS("model.rds")

  #printing the results  
  print(modelFit)
  
  print("Prediction")
  predictions <- predict(modelFit, newdata = dataTest)
  
  cm = confusionMatrix(predictions, dataTest$type)
  
  print(cm)
  
  precision <- posPredValue(predictions,  dataTest$type)
  recall <- sensitivity(predictions,  dataTest$type)     
  F1 <- (2 * precision * recall) / (precision + recall)
  print("----------------------")
  print(precision)
  print(recall)
  print(F1)
  
  trellis.par.set(caretTheme())
  print(plot(modelFit, metric = "ROC"))
  
  print(plot(modelFit, metric = "ROC", plotType = "level",
             scales = list(x = list(rot = 90))))
  
}


