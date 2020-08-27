shoppers_intentions <- readRDS("cleaned_data.RData")
source("functions.R")

#install.packages("car")
library(car)

shoppers_intentions_lr <- shoppers_intentions
summary(shoppers_intentions_lr)
str(shoppers_intentions_lr)

########## Sredjivanje podataka ##########

# Izbacicemo faktorske varijable "OperatingSystems", "Browser", "VisitorType", "TrafficType"
to_remove <- c(12:15)
shoppers_intentions_lr <- shoppers_intentions_lr[-to_remove]

factor_vars <- c(11,12)
shoppers_intentions_lr[factor_vars] <- lapply(shoppers_intentions_lr[factor_vars], as.integer)

summary(shoppers_intentions_lr$Weekend)

shoppers_intentions_lr$Weekend <- ifelse(shoppers_intentions_lr$Weekend == 2, 1, 0)

num_vars <- c(1:10)

boxplot(shoppers_intentions_lr$Administrative, main = "Administrative") # ima outliere
boxplot(shoppers_intentions_lr$Administrative_Duration, main = "Administrative_Duration") # ima outliere
boxplot(shoppers_intentions_lr$Informational, main = "Informational") # ima outliere
boxplot(shoppers_intentions_lr$Informational_Duration, main = "Informational_Duration") # ima outliere
boxplot(shoppers_intentions_lr$ProductRelated, main = "ProductRelated") # ima outliere
boxplot(shoppers_intentions_lr$ProductRelated_Duration, main = "ProductRelated_Duration") # ima outliere
boxplot(shoppers_intentions_lr$BounceRates, main = "BounceRates") # ima outliere
boxplot(shoppers_intentions_lr$ExitRates, main = "ExitRates") # ima outliere
boxplot(shoppers_intentions_lr$PageValues, main = "PageValues") # ima outliere
boxplot(shoppers_intentions_lr$SpecialDay, main = "SpecialDay") # ima 

apply (X = shoppers_intentions_lr[,num_vars], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))
# Sve numericke varijable imaju veliki broj outliera, zato cemo koristiti standardizaciju da reskaliramo vrednosti

# Proveravamo da li varijable imaju normalnu raspodelu
apply (X = shoppers_intentions_lr[c(1:5000),num_vars], MARGIN = 2, FUN = shapiro.test)
# Nijedna od numerickih varijabli nema normalnu raspodelu, pa cemo za sve koristiti
# formulu u kojoj se koristi medijana i IQR
shoppers_intentions_lr_st = as.data.frame(apply(X = shoppers_intentions_lr[,num_vars], MARGIN = 2, FUN = 
                                      function(x) scale(x, center = median(x), scale = IQR(x))))
summary(shoppers_intentions_lr_st)

# Informational, Informational_Duration, PageValues, SpecialDay nismo uspeli da standardizujemo na ovaj nacin
# posto imaju veliki broj vrednosti koje su 0, one koje su vece od postavicemo na 1
shoppers_intentions_lr$Informational[shoppers_intentions_lr$Informational > 0] = 1
shoppers_intentions_lr$Informational_Duration[shoppers_intentions_lr$Informational_Duration > 0] = 1
shoppers_intentions_lr$PageValues[shoppers_intentions_lr$PageValues > 0] = 1
shoppers_intentions_lr$SpecialDay[shoppers_intentions_lr$SpecialDay > 0] = 1

# ukljucujemo standardizovane varijable u data set
shoppers_intentions_lr$Administrative <- shoppers_intentions_lr_st$Administrative
shoppers_intentions_lr$Administrative_Duration <- shoppers_intentions_lr_st$Administrative_Duration
shoppers_intentions_lr$ProductRelated <- shoppers_intentions_lr_st$ProductRelated
shoppers_intentions_lr$ProductRelated_Duration <- shoppers_intentions_lr_st$ProductRelated_Duration
shoppers_intentions_lr$BounceRates <- shoppers_intentions_lr_st$BounceRates
shoppers_intentions_lr$ExitRates <- shoppers_intentions_lr_st$ExitRates

summary(shoppers_intentions_lr)

########## Resavanje multikolineatnosti i kreiranje modela ##########

library(caret)
set.seed(1010)
train.indices <- createDataPartition(shoppers_intentions_lr$Revenue, p = 0.8, list = FALSE)
shoppers_intentions.training.lr <- shoppers_intentions_lr[train.indices,]
shoppers_intentions.test.lr <- shoppers_intentions_lr[-train.indices,]

# prvo formiramo model koristeci sve nezavisne promenljive
glm1.fit = glm(Revenue ~ ., data = shoppers_intentions.training.lr, family = binomial)

# Proveravamo multikolinearnost
sqrt(vif(glm1.fit))

# izbacivacemo atribute cija je korena vrednost linearne medjuzavisnosti sa drugim atributima veca od 2
# prvo cemo izbaciti varijablu sa najvecom vif vrednoscu, a to je Informational_Duration
glm2.fit = glm(Revenue ~.-Informational_Duration
               , data = shoppers_intentions.training.lr, family = binomial)

# zatim cemo ponovo izracunati vif
sqrt(vif(glm2.fit))
# i dalje imamo atribute cija je vif vrednost veca od 2, najveca je za ProductRelated, pa cemo izbaciti ovaj atribut

glm2.fit = glm(Revenue ~.-Informational_Duration-ProductRelated
               , data = shoppers_intentions.training.lr, family = binomial)

# ponovo racunamo vif
sqrt(vif(glm2.fit))
# vidimo da je multikolinearnost resena izbacivanjem ove varijable 
glm2.fit
summary(glm2.fit)
AIC2 <- glm2.fit$aic
AIC2 #5209.086

glm2.probs = predict(glm2.fit, newdata = shoppers_intentions.test.lr, type = "response")
glm2.probs

glm2.pred = ifelse(glm2.probs < 0.5, "No", "Yes")

glm2.cm = table (true = shoppers_intentions.test.lr$Revenue, predicted = glm2.pred)
glm2.cm

glm2.eval = compute.eval.metrics(glm2.cm)
glm2.eval

library(pROC)
lr2.auc <- roc(Revenue ~ glm2.probs, data = shoppers_intentions.test.lr)$auc
lr2.auc #0.9137


# Pokusacemo da kreiramo poboljsani model tako sto cemo izbaciti
# atribute ciji je nivo znacaja izuzetno nizak (p-vrednost veca od 0.05) 
summary(glm2.fit)
# to su Administrative_Duration, Informational, ProductRelated_Duration, BounceRates, Weekend
glm3.fit = glm(Revenue ~ Administrative + ExitRates + PageValues + SpecialDay + Month
               , data = shoppers_intentions.training.lr, family = binomial)
glm3.fit
summary(glm3.fit)
AIC3 <- glm3.fit$aic
AIC3 #5200.86

glm3.probs = predict(glm3.fit, newdata = shoppers_intentions.test.lr, type = "response")
glm3.probs

glm3.pred = ifelse(glm3.probs < 0.5, "No", "Yes")

glm3.cm = table (true = shoppers_intentions.test.lr$Revenue, predicted = glm3.pred)
glm3.cm

glm3.eval = compute.eval.metrics(glm3.cm)
glm3.eval

lr3.auc <- roc(Revenue ~ glm3.probs, data = shoppers_intentions.test.lr)$auc
lr3.auc #0.9128

########## Balansiranje podataka pri kros validaciji ##########

#install.packages("gplots")
library(gplots)
#install.packages("DMwR")
library(DMwR)

ctrl <- trainControl(method = "cv", number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

set.seed(5627)

exclude <- c(which( colnames(shoppers_intentions.training.lr)=="Informational_Duration"), which( colnames(shoppers_intentions.training.lr)=="ProductRelated"), which( colnames(shoppers_intentions.training.lr)=="Revenue"))

down_inside <- train(x = shoppers_intentions.training.lr[,-exclude],
                     y = shoppers_intentions.training.lr$Revenue,
                     method = "glm",
                     family=binomial(),
                     metric = "ROC",
                     trControl = ctrl)

ctrl$sampling <- "up"
set.seed(5627)
up_inside <- train(x = shoppers_intentions.training.lr[,-exclude],
                   y = shoppers_intentions.training.lr$Revenue,
                   method = "glm",
                   family=binomial(),
                   metric = "ROC",
                   trControl = ctrl)

ctrl$sampling <- "rose"
set.seed(5627)
rose_inside <- train(x = shoppers_intentions.training.lr[,-exclude],
                     y = shoppers_intentions.training.lr$Revenue,
                     method = "glm",
                     family=binomial(),
                     metric = "ROC",
                     trControl = ctrl)

ctrl$sampling <- "smote"
set.seed(5627)
smote_inside <- train(x = shoppers_intentions.training.lr[,-exclude],
                      y = shoppers_intentions.training.lr$Revenue,
                      method = "glm",
                      family=binomial(),
                      metric = "ROC",
                      trControl = ctrl)

inside_models <- list(down = down_inside,
                      up = up_inside,
                      SMOTE = smote_inside,
                      ROSE = rose_inside)

#?resamples
inside_resampling <- resamples(inside_models) # funkcija koja rezimira i uporedjuje balansiranе setovе podataka

inside_test <- lapply(inside_models, test_roc, data = shoppers_intentions.test.lr) # funkcija test_roc se primenjuje na svaki element inside_models
inside_test <- lapply(inside_test, as.vector)
inside_test <- do.call("rbind", inside_test)
colnames(inside_test) <- c("lower", "ROC", "upper")
inside_test <- as.data.frame(inside_test)

summary(inside_resampling, metric = "ROC")
inside_test
# Najbolju vrednost dobili smo za smote 


########## Kreiranje modela sa balansiranim setom podataka ##########

glm4.pred = predict(object = smote_inside, newdata = shoppers_intentions.test.lr, type = "raw")
glm4.pred

glm4.cm = table (true = shoppers_intentions.test.lr$Revenue, predicted = glm4.pred)
glm4.cm

glm4.eval = compute.eval.metrics(glm4.cm)
glm4.eval

AIC4 <- smote_inside$finalModel$aic
AIC4

lr4.auc <- roc(shoppers_intentions.test.lr$Revenue,
               predict(smote_inside, shoppers_intentions.test.lr, type = "prob")[, "Yes"],
               levels = c("No", "Yes"))$auc
lr4.auc #0.9125

summary(glm3.fit)
summary(smote_inside)

# Poredjenje rezultata
AUC <- c(lr2.auc, lr3.auc, lr4.auc)
AIC <- c(AIC2, AIC3, AIC4)
df_lr_metrics <- data.frame(rbind(glm2.eval, glm3.eval, glm4.eval), row.names = c("lr 2 ", "lr 3 ", "lr 4"))
df_lr_metrics <- cbind(df_lr_metrics, AUC)
df_lr_metrics <- cbind(df_lr_metrics, AIC)
df_lr_metrics