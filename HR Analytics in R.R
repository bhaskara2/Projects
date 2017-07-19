library(needs) #automatiaclly loads packages that are not in computer;
needs(dplyr,tidyr,ggplot2,ggvis,corrplot,DT)


hr = read.table('HR_comma_sep.csv', header = T,sep=',')
head(hr)
summary(hr)

##
HR_correlation <- hr %>% select(satisfaction_level:promotion_last_5years) # "%>%" is a pipe like operator. selection is done after takingn hr dataset and the result is stored in hr_correlation.
M <- cor(HR_correlation)
corrplot(M, method="circle")
cor(HR_correlation)

##visualizations
hr_hist <- hr %>% filter(left==1) # first take HR dataset and filter the data then store it in hr_hist
par(mfrow=c(1,3))
hist(hr_hist$satisfaction_level,col="blue", main = "Satisfaction level") 
hist(hr_hist$last_evaluation,col="violet", main = "Last evaluation")
hist(hr_hist$average_montly_hours,col="orange", main = "Average montly hours")
hist(hr_hist$Work_accident,col="purple", main = "Work accident")
plot(hr_hist$salary,col="#3090C7", main = "Salary")

##counting number of people left
hr_leaving_people <- hr %>% filter(left==1)
nrow(hr_leaving_people) #3571 people left 

##counting the numbere of "good" people left.
hr_good_leaving_people <- hr_leaving_people %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
nrow(hr_good_leaving_people)

## Why good people are leaving.
hr_good_people_select <- hr_good_leaving_people2 %>% select(satisfaction_level, number_project: promotion_last_5years)
M <- cor(hr_good_people_select)
corrplot(M, method="circle")

#Sampling munst be done to estimate the performance of the model.
#cross validtion/leave out one cross validation/K fold cross validatio
#we set k fold cross validation here 
#
# Set the target variable as a factor i.e categorical
hr_model <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
summary(hr_model)
hr_model$left <- as.factor(hr_model$left)# set the variable as categorical
library("caret")

# cross-validation
train_control<- trainControl(method="cv", number=5, repeats=3) #creates 3 seperate k=5 folds, k fold cv data sets.
head(train_control)
gmlmodel <- train(left~., data=hr_model, trControl=train_control, method="LogitBoost")

# make predictions
predictions<- predict(gmlmodel,hr_model)
gmlmodelbinded <- cbind(hr_model,predictions)

# summarize results
confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$left)
confusionMatrix

# library("ROCR")
# gmlmodelbinded$predictions <- as.numeric(paste(gmlmodelbinded$predictions))
# 
# perf.obj <- prediction(predictions=gmlmodelbinded$predictions, labels=gmlmodelbinded$left)
# # Get data for ROC curve
# roc.obj <- performance(perf.obj, measure="tpr", x.measure="fpr")
# plot(roc.obj,
#      main="Cross-Sell - ROC Curves",
#      xlab="1 - Specificity: False Positive Rate",
#      ylab="Sensitivity: True Positive Rate",
#      col="blue")
# abline(0,1,col="grey")