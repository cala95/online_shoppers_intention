shoppers_intentions <- readRDS("cleaned_data.RData")
source("functions.R")

#install.packages("bnlearn")
library(bnlearn)
library(ggplot2)
library(e1071)
library(caret)
library(pROC)

########## Sredjivanje podataka ##########

summary(shoppers_intentions)
str(shoppers_intentions)
shoppers_intentions_nb <- shoppers_intentions

# Proveravamo da li numericke varijable imaju normalnu raspodelu
numeric_vars <- c(1:10)
apply (X = shoppers_intentions_nb[c(1:5000),numeric_vars], MARGIN = 2, FUN = shapiro.test)
#?shapiro.test

# Nijedna numericka varijabla nema normalnu raspodelu, sve ih moramo diskretizovati

to_discretize <- c("Administrative","Administrative_Duration","Informational","Informational_Duration","ProductRelated",
                   "ProductRelated_Duration","BounceRates","ExitRates","PageValues","SpecialDay")

# int varijable moramo pretvoriti u num
int_vars <- c("Administrative","Informational","ProductRelated")
shoppers_intentions_nb[int_vars] <- lapply(shoppers_intentions_nb[int_vars], as.numeric)
str(shoppers_intentions_nb)

summary(shoppers_intentions_nb$Administrative)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = Administrative)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$Administrative_Duration)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = Administrative_Duration)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$Informational)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = Informational)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$Informational_Duration)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = Informational_Duration)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$ProductRelated)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = ProductRelated)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$ProductRelated_Duration)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = ProductRelated_Duration)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$BounceRates)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = BounceRates)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$ExitRates)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = ExitRates)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$PageValues)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = PageValues)) + geom_histogram(bins = 20)
summary(shoppers_intentions_nb$SpecialDay)
ggplot(data = shoppers_intentions_nb, mapping = aes(x = SpecialDay)) + geom_histogram(bins = 20)

#discretized <- discretize(data = shoppers_intentions_nb[,to_discretize],
#                          method = 'quantile',
#                          breaks = c(2,2,1,1,5,5,2,5,1,1)) 

# Informational, Informational_Duration, PageValues, SpecialDay ne mogu na vise od 1 intervala
# Pretvoricemo ih u binarne factor varijable sa nivoima 0 i 1 (sve vrednosti vece od 0 ce postati 1) 

shoppers_intentions_nb$Informational <- ifelse(test = shoppers_intentions_nb$Informational > 0, yes = 1, no = 0)
shoppers_intentions_nb$Informational_Duration <- ifelse(test = shoppers_intentions_nb$Informational_Duration > 0, yes = 1, no = 0)
shoppers_intentions_nb$PageValues <- ifelse(test = shoppers_intentions_nb$PageValues > 0, yes = 1, no = 0)
shoppers_intentions_nb$SpecialDay <- ifelse(test = shoppers_intentions_nb$SpecialDay > 0, yes = 1, no = 0)

to_factor <- c("Informational","Informational_Duration", "PageValues", "SpecialDay")

shoppers_intentions_nb[to_factor] <- lapply(shoppers_intentions_nb[to_factor], factor)

str(shoppers_intentions_nb)

to_discretize <- c("Administrative","Administrative_Duration","ProductRelated",
                   "ProductRelated_Duration","BounceRates","ExitRates")
  
discretized <- discretize(data = shoppers_intentions_nb[,to_discretize],
                          method = 'quantile',
                          breaks = c(2,2,5,5,2,5))

summary(discretized)

cols_to_add <- setdiff(names(shoppers_intentions_nb), names(discretized))
shoppers_intentions_nb_new <- data.frame(cbind(shoppers_intentions_nb[,cols_to_add], discretized))
str(shoppers_intentions_nb_new)

########## Kreiranje modela ##########

set.seed(1010)
train.indices <- createDataPartition(shoppers_intentions_nb_new$Revenue, p = 0.8, list = FALSE)
shoppers_intentions.training.nb <- shoppers_intentions_nb_new[train.indices,]
shoppers_intentions.test.nb <- shoppers_intentions_nb_new[-train.indices,]

#?naiveBayes
nb1 <- naiveBayes(Revenue ~ ., data = shoppers_intentions.training.nb)
print(nb1)

nb1.pred <- predict(nb1, newdata = shoppers_intentions.test.nb, type = "class")
nb1.cm <- table(true = shoppers_intentions.test.nb$Revenue, predicted = nb1.pred)
nb1.cm

nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval

nb1.pred.prob <- predict(nb1, newdata = shoppers_intentions.test.nb, type = "raw")

nb1.roc <- roc(response = as.numeric(shoppers_intentions.test.nb$Revenue), 
               predictor = nb1.pred.prob[,2],
               levels = c(1,2)) # levels(controls, cases) cases je pozitivna klasa

nb1.auc <- nb1.roc$auc 
nb1.auc #0.8632

########## Balansiranje podataka pri kros validaciji ##########
library(gplots)
library(DMwR)
library(naivebayes)

ctrl <- trainControl(method = "cv", number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

grid <- expand.grid(usekernel = TRUE, laplace = 1, adjust = c(0,0.5,1))
#laplace - Defaults to 0 (no Laplace smoothing)
#specifies a value for the Laplace smoothing factor, which sets the conditional probability of a predictor. 
#If the Laplace smoothing parameter is disabled (laplace = 0), then Naive Bayes will predict a probability of 0 
#for any row in the test set that contains a previously unseen categorical level. 
#if Laplace smoothing parameter is used (e.g. laplace = 1), then the model can make predictions for rows that include 
#previously unseen categorical level.
#usekernel - if TRUE, density is used to estimate the class conditional densities of metric predictors. This applies to vectors with class "numeric"
#adjust - Bandwidth Adjustment

set.seed(5627)
down_inside <- train(x = shoppers_intentions.training.nb[,-11], 
                     y = shoppers_intentions.training.nb$Revenue,
                     method = "naive_bayes",
                     metric = "ROC",
                     tuneGrid =  grid,
                     trControl = ctrl)

ctrl$sampling <- "up"
set.seed(5627)
up_inside <- train(x = shoppers_intentions.training.nb[,-11], 
                   y = shoppers_intentions.training.nb$Revenue,
                   method = "naive_bayes",
                   metric = "ROC",
                   tuneGrid =  grid,
                   trControl = ctrl)

ctrl$sampling <- "smote"
set.seed(5627)
smote_inside <- train(x = shoppers_intentions.training.nb[,-11], 
                      y = shoppers_intentions.training.nb$Revenue,
                      method = "naive_bayes",
                      metric = "ROC",
                      tuneGrid =  grid,
                      trControl = ctrl)

ctrl$sampling <- "rose"
set.seed(5627)
rose_inside <- train(x = shoppers_intentions.training.nb[,-11], 
                     y = shoppers_intentions.training.nb$Revenue,
                     method = "naive_bayes",
                     metric = "ROC",
                     tuneGrid =  grid,
                     trControl = ctrl)

inside_models <- list(down = down_inside,
                      up = up_inside,
                      SMOTE = smote_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)

inside_test <- lapply(inside_models, test_roc, data = shoppers_intentions.test.nb) # funkcija test_roc se primenjuje na svaki element inside_models
inside_test <- lapply(inside_test, as.vector)
inside_test <- do.call("rbind", inside_test)
colnames(inside_test) <- c("lower", "ROC", "upper")
inside_test <- as.data.frame(inside_test)

summary(inside_resampling, metric = "ROC")
inside_test

# Najbolju vrednost dobili smo za ROSE

########## Kreiranje modela sa balansiranim setom podataka ##########
nb2.pred <- predict(object = rose_inside, newdata = shoppers_intentions.test.nb, method = "class")
nb2.cm <- table(true = shoppers_intentions.test.nb$Revenue, predicted = nb2.pred)
nb2.cm
nb2.eval <- compute.eval.metrics(nb2.cm)
nb2.eval

nb2.auc <- test_roc(model = rose_inside, data = shoppers_intentions.test.nb) 
nb2.auc # 95% CI: 0.8526-0.8867

nb2.roc <- roc(shoppers_intentions.test.nb$Revenue,
                predict(rose_inside, shoppers_intentions.test.nb, type = "prob")[, "Yes"],
                levels = c("No", "Yes"))

nb2.auc <- nb2.roc$auc 
nb2.auc #0.8696

########## Odredjivanje optimalne vrednosti praga ##########

plot.roc(nb2.roc, print.thres = TRUE, print.thres.best.method = "youden") # 0.678(0.810, 0.798)
plot.roc(nb2.roc, print.thres = TRUE, print.thres.best.method = "closest.topleft") # 0.678(0.810, 0.798)

threshold <- 0.678

nb3.pred.prob <- predict(rose_inside, newdata = shoppers_intentions.test.nb, type = "prob")

nb3.pred <- ifelse(test = nb3.pred.prob[,2] >= threshold, yes = "Yes", no = "No")
nb3.pred <- as.factor(nb3.pred)

nb3.cm <- table(true = shoppers_intentions.test.nb$Revenue, predicted = nb3.pred)
nb3.cm

nb3.eval <- compute.eval.metrics(nb3.cm)
nb3.eval

nb3.auc <- nb2.auc # 0.8696


# Pokusacemo da forsiramo tacnost predvidjanja pozitivne klase,
# odnosno da maksimiziramo sensitivity metriku na racun smanjenja specificity metrike

nb.coords <- coords(nb2.roc, ret = c("spec","sens","thr"),x = "local maximas", transpose = FALSE)
head(t(nb.coords), 60)
#specificity 0.7077735,sensitivity 0.8661417,threshold 0.4085622

threshold <- 0.4085622
nb4.pred <- ifelse(test = nb3.pred.prob[,2] >= threshold, yes = "Yes", no = "No")
nb4.pred <- as.factor(nb4.pred)

nb4.cm <- table(true = shoppers_intentions.test.nb$Revenue, predicted = nb4.pred)
nb4.cm

nb4.eval <- compute.eval.metrics(nb4.cm)
nb4.eval

nb4.auc <- nb2.auc

# Poredjenje rezultata
AUC <- c(nb1.auc, nb2.auc, nb3.auc, nb4.auc)
df_nb_metrics <- data.frame(rbind(nb1.eval, nb2.eval, nb3.eval, nb4.eval), row.names = c("nb 1 ","nb 2 ", "nb 3 ", "nb 4 "))
df_nb_metrics <- cbind(df_nb_metrics, AUC)
df_nb_metrics



