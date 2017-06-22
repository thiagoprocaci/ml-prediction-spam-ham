library(caret)
source("functions.R")
#my_model <- readRDS("model.rds")


runML = TRUE
cv =  FALSE
modelName = "gbm-model-spam-ham"
method = "gbm"
spam = readData()
wordMatrixFiltered = createWordMatrix(spam)


if(runML) {  
  printWelcome(modelName, method, cv) 
  trainIndex <- createDataPartition(wordMatrixFiltered$type, p = .6,  list = FALSE, times = 1)
  dataTrain <- wordMatrixFiltered[ trainIndex,]
  dataTest  <- wordMatrixFiltered[-trainIndex,]
  
  #cross-validation 
  # 5-fold CV
  fitControl <- trainControl( method = "repeatedcv", number = 5,  classProbs = TRUE, repeats = 5, summaryFunction = twoClassSummary)  
  
  #grid used for parameters tunning
  grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = (1:30)*50, 
                       shrinkage = 0.1,
                       n.minobsinnode = 20)
  

  
  modelFit = createModelFit(dataTrain, method, fitControl, grid, cv) 
  
  saveModel(cv, modelName)  
  
  predictions = printModelFitAndPredictions(modelFit, dataTest, wordMatrixFiltered)
  #spam[, "pred"] = as.numeric(predictions)
  
  
}


