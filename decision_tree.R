shoppers_intentions <- readRDS("cleaned_data.RData")
source("functions.R")

#install.packages("rpart")
library(rpart)
#install.packages("caret")
library(caret)
#install.packages("lattice")
library(lattice)
#install.packages("e1071")
library(e1071)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("rattle")
library(rattle)
#install.packages("ROSE")
library(ROSE)
#install.packages("pROC")
library(pROC)



########## Kreiranje modela ##########

# Podela seta podataka na train i test
set.seed(1010)
train.indices <- createDataPartition(shoppers_intentions$Revenue, p = 0.8, list = FALSE)
shoppers_intentions.training <- shoppers_intentions[train.indices,]
shoppers_intentions.test <- shoppers_intentions[-train.indices,]
# Proveravamo da li je raspodela otprilike ista u trening i test setu podataka, vidimo da jeste
prop.table(table(shoppers_intentions.training$Revenue))
prop.table(table(shoppers_intentions.test$Revenue))

# Predikcija - sa default cp 0.001
#?rpart
#?rpart.control
tree1 <- rpart(Revenue~., data = shoppers_intentions.training, method = "class")
print(tree1)
prp(tree1, type = 3, extra = 1)
fancyRpartPlot(tree1)
#?predict
tree1.pred <- predict (object = tree1, newdata = shoppers_intentions.test, type = "class")
tree1.cm <- table(true = shoppers_intentions.test$Revenue, predicted = tree1.pred)
tree1.cm

tree1.pred.prob <- predict (object = tree1, newdata = shoppers_intentions.test)
tree1.auc <- roc.curve(shoppers_intentions.test$Revenue, tree1.pred.prob[,2])$auc 
tree1.auc # 0.876

tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

# Cross-validation - racunamo optminalnu vrednost za parametar kompleksnosti (cp)
numFolds <- trainControl(method = "cv", 
                         number = 10,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)
cpGrid <- expand.grid(.cp = seq(from = 0.001, to = 0.05, by = 0.001))

tree2.cv <- train (x = shoppers_intentions.training[,-17],
                   y = shoppers_intentions.training$Revenue,
                   method = "rpart", 
                   trControl = numFolds, 
                   tuneGrid = cpGrid,
                   metric = "ROC")
tree2.cv
plot(tree2.cv)
# Najbolja vrednost za cp je 0.001, sto je i predefinisana vrednost ovog parametra, nema potrebe orezivati stablo
# i kreirati novi model, ali cemo to uraditi da bismo pokazali da se dobijaju isti rezultati
# i kako bi se radilo u slucaju da smo dobili vrednost razlicitu od default vrednosti za cp

########## Kreiranje poboljsanog modela ##########

tree2 <- prune(tree1, cp = 0.001)
prp(tree2, type = 3, extra = 1)
fancyRpartPlot(tree2)
tree2.pred <- predict(tree2, newdata = shoppers_intentions.test, type = "class")
tree2.cm <- table(true = shoppers_intentions.test$Revenue, predicted = tree2.pred)
tree2.cm
tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval

tree2.pred.prob <- predict (object = tree2, newdata = shoppers_intentions.test)
tree2.auc <- roc.curve(shoppers_intentions.test$Revenue, tree2.pred.prob[,2])$auc 
tree2.auc # 0.876

########## Balansiranje podataka pri kros validaciji ##########

#install.packages("gplots")
library(gplots)
#install.packages("DMwR")
library(DMwR)

ctrl <- trainControl(method = "cv", number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

cpGrid <- expand.grid(.cp = seq(from = 0.001, to = 0.05, by = 0.001))

set.seed(5627)
down_inside <- train(x = shoppers_intentions.training[,-17],
                     y = shoppers_intentions.training$Revenue,
                     method = "rpart",
                     metric = "ROC",
                     tuneGrid = cpGrid,
                     trControl = ctrl)

ctrl$sampling <- "up"
set.seed(5627)
up_inside <- train(x = shoppers_intentions.training[,-17],
                   y = shoppers_intentions.training$Revenue,
                   method = "rpart",
                   metric = "ROC",
                   tuneGrid = cpGrid,
                   trControl = ctrl)

ctrl$sampling <- "rose"
set.seed(5627)
rose_inside <- train(x = shoppers_intentions.training[,-17],
                     y = shoppers_intentions.training$Revenue,
                     method = "rpart",
                     metric = "ROC",
                     tuneGrid = cpGrid,
                     trControl = ctrl)

ctrl$sampling <- "smote"
set.seed(5627)
smote_inside <- train(x = shoppers_intentions.training[,-17],
                      y = shoppers_intentions.training$Revenue,
                      method = "rpart",
                      metric = "ROC",
                      tuneGrid = cpGrid,
                      trControl = ctrl)

inside_models <- list(down = down_inside,
                      up = up_inside,
                      SMOTE = smote_inside,
                      ROSE = rose_inside)

#?resamples
inside_resampling <- resamples(inside_models) # funkcija koja rezimira i uporedjuje balansiranе setovе podataka

inside_test <- lapply(inside_models, test_roc, data = shoppers_intentions.test) # funkcija test_roc se primenjuje na svaki element inside_models
inside_test <- lapply(inside_test, as.vector)
inside_test <- do.call("rbind", inside_test)
colnames(inside_test) <- c("lower", "ROC", "upper")
inside_test <- as.data.frame(inside_test)

summary(inside_resampling, metric = "ROC")
inside_test
# Najbolju vrednost dobili smo za down-sampling



########## Kreiranje modela sa balansiranim setom podataka ##########

tree3.pred <- predict(object = down_inside, newdata = shoppers_intentions.test, method = "class")
tree3.cm <- table(true = shoppers_intentions.test$Revenue, predicted = tree3.pred)
tree3.cm
tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval

tree3.auc <- test_roc(model = down_inside, data = shoppers_intentions.test) 
tree3.auc # 95% CI: 0.9024-0.9328

tree3.auc <- roc(shoppers_intentions.test$Revenue,
                 predict(down_inside, shoppers_intentions.test, type = "prob")[, "Yes"],
                 levels = c("No", "Yes"))$auc
tree3.auc # 0.9169

?varImp
tree3.Imp <- varImp(down_inside, scale = TRUE)
tree3.Imp

########## Poredjenje metrika ##########

AUC <- c(tree1.auc, tree2.auc, tree3.auc)
df_trees_metrics <- data.frame(rbind(tree1.eval, tree2.eval, tree3.eval), row.names = c("tree 1 ","tree 2 ","tree 3 "))

df_trees_metrics <- cbind(df_trees_metrics, AUC)

df_trees_metrics
