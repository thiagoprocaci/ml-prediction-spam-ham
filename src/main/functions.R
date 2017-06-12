library(caret)
library(tm)


set.seed(825)

readData <- function() {
  fileName = "../resources/SMSSpamCollection.csv"
  f = paste(getwd(), fileName, sep = "/")
  data =  read.csv(f, header = TRUE, sep = "	", encoding="UTF-8")
  return (data)
}



createWordMatrix <- function(data) {
	print(head(data))
	#performing NPL on each text 
	document = tolower(as.character(data$text))	
	corpus =  Corpus(VectorSource(document))		
	#removing white space, stopwords, punctuation, numbers
	corpus = tm_map(corpus,stripWhitespace)	
	corpus = tm_map(corpus, removeWords, stopwords('english'))	
	corpus = tm_map(corpus, removePunctuation)	
	corpus = tm_map(corpus, removeNumbers)	
	corpus = tm_map(corpus, PlainTextDocument)	



	dtm <- t(TermDocumentMatrix(corpus));	
	matrix = as.matrix(dtm) 
	#transforming the document term matrix into a data frame
	wordMatrix = data.frame(matrix[, colnames(matrix)], row.names=NULL )
	wordMatrix[, "type"] = data$type 

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
	return (wordMatrixFiltered)

}

createModelFit <- function(dataTrain, method, fitControl, grid, cv) {

	if(cv) {
		print("Creating model with no cross-validation")
	    modelFit <- train(type ~ . , data = dataTrain, 
	                      method = method,  
	                      trControl = fitControl,
	                      tuneGrid = grid,
	                      verbose = FALSE,
	                      metric = "ROC" 
	      )  
	    return (modelFit)
	} else {
		  print("Creating model with cross-validation")
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
	name = paste(dir, name, sep = "")	
	saveRDS(modelFit, name)
}


printModelFitAndPredictions <- function(modelFit, dataTest) {
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
