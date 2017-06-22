library(caret)
library(tm)


set.seed(825)


printWelcome <- function(modelName, method, cv) {
  print(paste("Creating", modelName, sep = " : "))
  print(paste("Using", method, sep = " : "))
  print(paste("CV", cv, sep = " : "))
}

readData <- function() {
  fileName = "../resources/spam.csv"
  f = paste(getwd(), fileName, sep = "/")
  data =  read.csv(f, header = TRUE, sep = ",")
  #giving better name to v1
  colnames(data)[colnames(data)== "v1"] = "type"
  #removing strange chars
  data[, "text"] = gsub("[^[:alnum:]]", " ", data$v2)
  return (data)
}



createWordMatrix <- function(data) {
  #performing natural language processing on each text 
  document = tolower(as.character(spam$text))
  corpus =  Corpus(VectorSource(document))
  #removing white space, stopwords, punctuation, numbers
  corpus <- tm_map(corpus,stripWhitespace)
  corpus = tm_map(corpus, removeWords, stopwords('english'))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  
  #transforming the document term matrix into a data frame
  #the document term matrix counts the frequency of each word considering each text
  #For instance:
  # apple, hello, doing, type
  #   1  ,  0 ,     0  , spam   ---  equivalent text = Hi apple!
  #   0  ,  1 ,     1 , ham     --- equivalent text = Hello! How are you doing?
  
  dtm <- t(TermDocumentMatrix(corpus));
  matrix = as.matrix(dtm) 
  wordMatrix = data.frame(matrix[, colnames(matrix)], row.names=NULL )
  wordMatrix[, "type"] = spam$type 
  
  #Now, each word is a feature of our model. 
  #Based on it, we will predict the type (spam or ham)
  
  # However, we have many features.
  # We need to select the most important ones. 
  
  #So, we create a list with the frequency of all words, in all texts.
  countFreq = c()
  for(n in colnames(wordMatrix)) {
    if("type" != n) {
      s = sum(wordMatrix[, n])
      countFreq = c(s, countFreq)
    }
  }
  # Then, we create a threshold. It is: the mean of the counts + 30
  threshold = (mean(countFreq) + 30)
  print(paste("threshold", threshold, sep = " : "))
  
  # After that, we select the feature (word) which its frequency is higher than the threshold  
  featureSelected= c()
  for(n in colnames(wordMatrix)) {
    if("type" != n) {
      s = sum(wordMatrix[, n])
      if(s > threshold) {
        featureSelected = c(n, featureSelected)
      }
    }
  }
  wordMatrixFiltered <- wordMatrix[, c(featureSelected, "type")]
  
  #Summarizing, we selected the most important features (words) to make our predictions
  print(colnames(wordMatrixFiltered))
  return (wordMatrixFiltered)
}

createModelFit <- function(dataTrain, method, fitControl, grid, cv) {
	if(cv == TRUE) {
		print("Creating model with cross-validation")
	    modelFit <- train(type ~ . , data = dataTrain, 
	                      method = method,  
	                      trControl = fitControl,
	                      tuneGrid = grid,
	                      verbose = FALSE,
	                      metric = "ROC" 
	      )  
	    return (modelFit)
	} else {
		  print("Creating model with no cross-validation")
	      modelFit <- train(type ~ . , data = dataTrain, method = method, verbose = FALSE)  
	      return (modelFit)
	}
}


saveModel <- function(cv, modelName) {
	dir = "../resources/"
	name = ""
	if(cv) {
		name = paste(modelName, "cv", sep = "-")
	} else {
		name = modelName
	}
	name = paste(name, ".rds", sep = "")
	name = paste(dir, name, sep = "")	
	print(paste("Saving model at", name, sep = " "))
	saveRDS(modelFit, name)
}


printModelFitAndPredictions <- function(modelFit, dataTest, data) {
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
  return (predictions)
  #data[, "pred"] = predictions
  #trellis.par.set(caretTheme())
  #print(plot(modelFit, metric = "ROC"))
  
  #print(plot(modelFit, metric = "ROC", plotType = "level",
  #           scales = list(x = list(rot = 90))))
}
