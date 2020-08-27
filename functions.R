# Funkcija za racunanje evalucionih metrika
compute.eval.metrics <- function(cmatrix){
  TP <- cmatrix[2,2]
  TN <- cmatrix[1,1]
  FP <- cmatrix[1,2]
  FN <- cmatrix[2,1]
  accuracy <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  #KAPPA
  n = sum(cmatrix) # number of instances
  nc = nrow(cmatrix) # number of classes
  rowsums = apply(cmatrix, 1, sum) # number of instances per class
  colsums = apply(cmatrix, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  expAccuracy = sum(p*q)
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  c(F1 = F1, KAPPA = kappa)
}

#funkcija koja kreira roc krivu i vraca interval poverenja za ROC krivu
test_roc <- function(model, data) {
  roc_obj <- roc(data$Revenue, 
                 predict(model, data, type = "prob")[, "Yes"],
                 levels = c("No", "Yes")) #pravimo ROC krivu
  ci(roc_obj) # interval poverenja za ROC krivu
}