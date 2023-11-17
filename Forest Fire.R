################### Introduction ##########################
##the dataset included data from two region in Algeria, so the first step is to devied the dataset to two different csv file

# 1. Date : (DD/MM/YYYY) Day, month ('june' to 'september'), year (2012)
# Weather data observations 
# 2. Temp : temperature noon (temperature max)  in Celsius degrees: 22 to 42
# 3. RH : Relative Humidity in %: 21 to 90 
# 4. Ws :Wind speed in km/h: 6 to 29 
# 5. Rain: total day in mm: 0 to 16.8
# FWI Components  
# 6. Fine Fuel Moisture Code (FFMC) index from the FWI system: 28.6 to 92.5 
# 7. Duff Moisture Code (DMC) index from the FWI system: 1.1 to 65.9 
# 8. Drought Code (DC) index from the FWI system:  7 to 220.4
# 9. Initial Spread Index (ISI) index from the FWI system: 0 to 18.5 
# 10. Buildup Index (BUI) index from the FWI system: 1.1 to 68
# 11. Fire Weather Index (FWI) Index: 0 to 31.1
# 12. Classes: two classes, namely are fire and not fire
# 13. Region: two classes, Sidi_Bel_Abbes & Bejaia

## claer
cat("\014")  # ctrl+L

## get working directory
getwd()

## packages for this script
if (!require("caret")) install.packages('caret', dependencies = TRUE) #just in case caret has not been installed previously to this machine
## Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

## load packages (including pacman) with pacman
pacman::p_load(pacman, tidyverse, gmodels,ROCR, rpart, rpart.plot,caret,ggplot2,dplyr,psych,rattle,pROC,GGally,leaps)
library(gridExtra)
library(randomForest)
## data

ForestfireData <- read.csv("Data/Algerian_Forest_Fire_Data.csv")

glimpse(ForestfireData)
str(ForestfireData)

# declare factors
ForestFire_factors <- c("Classes","month","Region")
ForestfireData[ForestFire_factors] <- lapply(ForestfireData[ForestFire_factors],as.factor)

# declare numeric
ForestFire_numeric <- c("FFMC", "DMC", "DC", "ISI", "Temperature", "Rain", "RH","Ws")
ForestfireData[ForestFire_numeric] <- lapply(ForestfireData[ForestFire_numeric],as.numeric)

str(ForestfireData)

############### plot ####################

############### produce correlation matrix
ggpairs(ForestfireData,columns = ForestFire_numeric,ggplot2::aes(color = ForestfireData$Classes,alpha = 0.3))
## DMC have strong linear relation with DC, FFMC have strong linear relation with ISI

############### plotting number of fires and the probability of fires by month
ForestfireData$month <- factor(ForestfireData$month,levels = c("6","7","8","9"))
layer1 <- ggplot(ForestfireData, aes(x = month, fill = Classes))
geom1 <- geom_bar(position = "dodge")
geom2 <- geom_bar(position = "fill")
plot1 <- layer1 + geom1 + ylab("Times of forest fire")
plot2 <- layer1 + geom2 + ylab("percentage of forest fire")

############### plotting number of fires and the probability of fires by Region
ForestfireData$Region <- factor(ForestfireData$Region)
layerR1 <- ggplot(ForestfireData, aes(x = Region, fill = Classes))
geomR1 <- geom_bar(position = "dodge")
geomR2 <- geom_bar(position = "fill")
plotR1 <- layerR1 + geomR1 + ylab("Times of forest fire")
plotR2 <- layerR1 + geomR2 + ylab("percentage of forest fire")

## combine plots
combined_plots <- gridExtra::grid.arrange(plot1,plot2,plotR1,plotR2,ncol = 2)
plot(combined_plots)

############### plot bar plot, density plot and scater plot#######

## bar plot
plot_list <- list()
for (var_name in ForestFire_factors){
  plot <- ggplot(data = ForestfireData, aes(x = .data[[var_name]],fill = .data[[var_name]])) +
    geom_bar() +
    labs(title = var_name) + 
    theme_minimal()
  plot_list[[var_name]] <- plot
}

# Arrange and display the plots in a single panel
gridExtra::grid.arrange(grobs = plot_list, ncol = 3)

## density plot
plot_list1 <- list()

for (var_name in ForestFire_numeric){
  plot <- ggplot(data = ForestfireData, aes(x = .data[[var_name]],fill = factor(Classes))) +
    geom_density(alpha = 0.3) +
    labs(title = var_name) + 
    theme_minimal()
  plot_list1[[var_name]] <- plot
}

## cross table examine
table(ForestfireData$Classes,ForestfireData$Temperature)
prop.table(table(ForestfireData$Classes,ForestfireData$Temperature))*100
chisq.test(ForestfireData$Classes,ForestfireData$Temperature)
TemperatureTable <- xtabs(~Classes + Temperature, data = ForestfireData)
print(TemperatureTable)
summary(TemperatureTable)

# Arrange and display the plots in a single panel
gridExtra::grid.arrange(grobs = plot_list1, ncol = 4)

## scatter point plot
plot_list2 <- list()

for(var_name in ForestFire_numeric){
  plot <- ggplot(data = ForestfireData, aes(x = .data[[var_name]], y = Classes)) +
    geom_point() + 
    geom_smooth() + 
    labs(title = var_name)
  plot_list2[[var_name]] <- plot
}

gridExtra::grid.arrange(grobs = plot_list2, ncol = 4)


###################### Machine Learning #############
## The dependent variable is Classes: fire/ not fire
## we take Temperature,RH,Ws,Rain as weather variables
## and 8 integer or numeric variables:  
## day(1-31) year(2012) FFMC, DMC, DC, ISI, BUI, FWI
# FWI Components  
# 6. Fine Fuel Moisture Code (FFMC) index from the FWI system: 28.6 to 92.5, 
## FFMC is about ignition and spread of fire and relate with four factors
# 7. Duff Moisture Code (DMC) index from the FWI system: 1.1 to 65.9 
## DMC is obtained with rain, relative humidity and temperature
# 8. Drought Code (DC) index from the FWI system:  7 to 220.4
## DC is related with rain and temperature
# 9. Initial Spread Index (ISI) index from the FWI system: 0 to 18.5 
## ISI denotes the velocity spread of the fire and is calculated using the FFMC and the wind
## the above four are directly affected by weather variables
# 10. Buildup Index (BUI) index from the FWI system: 1.1 to 68
## BUI is calculated using DMC and DC
# 11. Fire Weather Index (FWI) Index: 0 to 31.1
## FWI is calculated using ISI and BUI

################################# SPLIT DATA################################
set.seed(12222)
Algerian_ind <- ForestfireData$Classes %>%
  createDataPartition(p = 0.90, list = FALSE)

Algerian_train <- ForestfireData[Algerian_ind, ]
Algerian_test <- ForestfireData[-Algerian_ind, ]

summary(Algerian_train$Classes)  ## fire 56.6%
summary(Algerian_test$Classes)   ## fire 56.5%

rm(Algerian_ind)
############################ LR Model examine########################
## LR model for all
model_all <- glm(Classes ~ ., data = Algerian_train, binomial(link = 'logit'), control = list(maxit = 100))
summary(model_all)
p <- predict(model_all,type = "response")
plot(sort(p),col = "blue")

## using stepwise regression to check for best variables
model0 <- regsubsets(Classes ~ ., data = Algerian_train, nvmax = ncol(data) - 1, method = "exhaustive" )
summary(model0)
plot(model0)

## turns out to be WS DMC DC
model1 <- glm(Classes ~ Ws + DMC + Region, data = Algerian_train, family = "binomial")
summary(model1)

## further LR model with only DMC left 
model2 <- glm(Classes ~ DMC + Region, data = Algerian_train, family = "binomial")
summary(model2)

prediction0 <-predict(model2, Algerian_test, type="response")
predROCR0 <- prediction(prediction0, Algerian_test$Classes)
perfROCR0 <- performance(predROCR0, "tpr", "fpr")
plot(perfROCR0, colorize = TRUE) + abline(0,1)
performance(predROCR0, "auc")@y.values

## Auc = 0.946
## confusion matrix
prediction2 <- predict(model2,
                         newdata = Algerian_test,
                         type = "response")
prediction2 <-  confusionMatrix(data = prediction2,       
                                  reference = Algerian_test$Classes,
                                positive = "1")
prediction2

## using same formula to build decision tree model as comparison test
modelDT1 <- rpart(formula = Classes ~ Ws + DMC + Region,
                  method = "class",
                  data = Algerian_train)
fancyRpartPlot(modelDT1, main = "Decision Tree comparing with model1")

plot(roc(Algerian_test$Classes,
         predict(modelDT1, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")

# AUC = 0.938

## confusion matrix
predictionDT1 <- predict(modelDT1,
                         newdata = Algerian_test,
                         type = "class")
predictionDT1 <-  confusionMatrix(data = predictionDT1,       
                                  reference = Algerian_test$Classes)
predictionDT1

## modelDT2
modelDT2<- rpart(formula = Classes ~ DMC + Region,
                  method = "class",
                  data = Algerian_train)
fancyRpartPlot(modelDT2, main = "Decision Tree comparing with model2")

plot(roc(Algerian_test$Classes,
         predict(modelDT2, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")

## AUC = 0.900

## confusion matrix
predictionDT2 <- predict(modelDT2,
                           newdata = Algerian_test,
                           type = "class")
predictionDT2 <-  confusionMatrix(data = predictionDT2,       
                                    reference = Algerian_test$Classes)
predictionDT2

## Decision Tree all

modelDT_all<- rpart(formula = Classes ~ .,
                 method = "class",
                 data = Algerian_train)
fancyRpartPlot(modelDT_all, main = "Decision Tree all")

plot(roc(Algerian_test$Classes,
         predict(modelDT_all, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")
## AUC = 0.950
## FMC >= 80 posibility of forest fire will rise to 98%
## confusion matrix
predictionDTall <- predict(modelDT_all,
                         newdata = Algerian_test,
                         type = "class")
predictionDTall <-  confusionMatrix(data = predictionDTall,       
                                    reference = Algerian_test$Classes)
predictionDTall

## try to use random forest model
##define cross-validation
# make.names(c("fire"," ", "not fire"),unique = TRUE,allow_ = FALSE)
# Fcontrol <- trainControl(method = "cv", number = 10,
#                          classProbs = T,
#                          savePredictions = T)
# ## Random forest model
# Algerian_rf <- train(Classes ~ .,
#                      data = Algerian_train,
#                      method = "rf",
#                      prox = TRUE,
#                      trControl = Fcontrol,
#                      metric = "Accuracy")
# print(Algerian_rf)
Algerian_rf1 <- randomForest(Classes ~ .,
                             data = Algerian_train,
                             mtry = 10,
                             importance = TRUE,
                             proximity = TRUE)
print(Algerian_rf1)
plot(Algerian_rf1)

## ROC curve of general rf model
plot(roc(Algerian_test$Classes,
         predict(Algerian_rf1, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")

## importance of variables
importance(Algerian_rf1,type = 1)
## line chart
matplot(importance(Algerian_rf1)[,1:2], type = "o",
        pch = 1:2,lty = 1:2,col = 1:2,
        xlab = "vN",ylab = "Importance",axes = F)
axis(1,1:14,names(ForestfireData)[-1]);axis(2)
title("variable importance")
legend("topleft",
       legend = paste("Type=",
                      levels(ForestfireData[,14]),
                      sep = ""),
       pch = 1:2,lty = 1:2,
       col = 1:2)

## bar charts
barplot(importance(Algerian_rf1)[,3], cex.names = .6)
title("variable importance according to mean decrease accuracy")
barplot(importance(Algerian_rf1)[,4], cex.names = .6)
title("variable importance according to mean decrease gini")

## prediction model and ROC curve, confusion metrix

plot(roc(Algerian_test$Classes,
         predict(Algerian_rf1, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")

## confusion matrix
predictionRF1 <- predict(Algerian_rf1,
                         newdata = Algerian_test,
                         type = "class")
predictionRF1_cf <- caret::confusionMatrix(as.factor(predictionRF1),
                                           as.factor(Algerian_test$Classes))
predictionRF1_cf

## use top5 importance variables to generate final rf model
Algerian_rf2 <- randomForest(Classes ~ month + RH + DC + Ws +  ISI,
                             data = Algerian_train,
                             mtry = 10,
                             importance = TRUE,
                             proximity = TRUE)
print(Algerian_rf2)
plot(Algerian_rf2)


## importance of variables
## line chart
matplot(importance(Algerian_rf2)[,1:2], type = "o",
        pch = 1:2,lty = 1:2,col = 1:2,
        xlab = "vN",ylab = "Importance",axes = F)
axis(1,1:14,names(ForestfireData)[-1]);axis(2)
title("variable importance")
legend("topleft",
       legend = paste("Type=",
                      levels(ForestfireData[,14]),
                      sep = ""),
       pch = 1:2,lty = 1:2,
       col = 1:2)

## bar chart - by accuracy & gini
barplot(importance(Algerian_rf2)[,3], cex.names = .6)
title("variable importance according to mean decrease accuracy")
barplot(importance(Algerian_rf2)[,4], cex.names = .6)
title("variable importance according to mean decrease gini")

## examine the final predictioni model
## ROC curve
plot(roc(Algerian_test$Classes,
         predict(Algerian_rf2, Algerian_test, type = 'prob')[,2],),
     print.auc = TRUE, print.thres = TRUE,
     xlab = "Specificity",
     ylab = "Sensitinity")
## confusion matrix
predictionRF2 <- predict(Algerian_rf2,
                         newdata = Algerian_test,
                         type = "class")
predictionRF2_cf <- caret::confusionMatrix(as.factor(predictionRF2),
                                           as.factor(Algerian_test$Classes))
predictionRF2_cf
## with the AUC-value = 0.992, accuracy = 95.7%, the best fit model is random forest model using 
## month + RH + DC + Ws +  ISI to predict fire.