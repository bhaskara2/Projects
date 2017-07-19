library(ISLR,needs) #automatiaclly loads packages that are not in computer;
needs(dplyr,tidyr,ggplot2,DMwR)

diabetes = read.table('C:/Users/Bhaskara/Downloads/Summer/Work/Diabetes/diabetes.csv', header = T,sep=',')
summary(diabetes)
str(diabetes)

#Data cleaning and formatting
diabetes$Outcome <- factor(diabetes$Outcome)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}

summary(diabetes)
# modify the data column names slightly for easier typing
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

##We will find how the different factors are distributed among the pregnant women with/without diabetes.
library(gridExtra)
univar_graph <- function(univar_name, univar, data, output_var) {
  g_1 <- ggplot(data, aes(x=univar)) + geom_density() + xlab(univar_name)
  g_2 <- ggplot(data, aes(x=univar, fill=output_var)) + geom_density(alpha=0.4) + xlab(univar_name)
  grid.arrange(g_1, g_2, ncol=2, top=paste(univar_name,"variable", "/ [ Skew:",skewness(univar),"]"))
}

for (x in 1:(ncol(diabetes)-1)) {
  univar_graph(names(diabetes)[x], diabetes[,x], diabetes, diabetes[,'outcome'])
}

##The variables pregnacies,Insulin,Diabetes pedigree function,age have right skew.
##The person with diabetes have high age, more pregnancies and high insulin.
library(caret)
set.seed(1234)
dindex <- createDataPartition(diabetes$outcome, p=0.7, list=FALSE)
train_data <- diabetes[dindex,]
test_data <- diabetes[-dindex,]

#-----------------------------------------------------------------------------------------------
##Error in createDataPartition(diabetes$Outcome, p = 0.7, list = FALSE) : 
## y must have at least 2 data points
##The above error comes whenever the input data is a dataframe. CDP accepts data in vector form.
#Modelling and estimating parameters
#------------------------------------------------------------------------------------------------
#using normal cross validation to get the performance of the model.
library(boot)
library(caret)
library(mice)

# Fitting
model <- glm(outcome~.,family=binomial,data=train_data)
summary(model)
glm.probs=predict(model,newdata=test_data,type="response") 
glm.pred=ifelse(glm.probs >0.5,"1","0")

# Accuracy
answers <- test_data$outcome
table(glm.pred,answers)
mean(glm.pred==answers)

##Even after using k-fold cross validation, the model seems to perform well, which means the model is not overfitting on training data.
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
ctrl

kcv_fit <- train(outcome ~.,data=train_data, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
kcv_fit
pred = predict(kcv_fit, newdata=test_data)
pred
confusionMatrix(data=pred, test_data$outcome)

##Roc curve
library(pROC)
roc_glm <- roc(test_data$outcome,glm.probs)
colAUC(glm.probs,test_data$outcome, plotROC = TRUE)

##Random Forest
##Random Boosting

















#--------------------------------------------------------------------------
##principle components
dimnames(diabetes)
str(diabetes)
#pca.out=prcomp(diabetes, scale=TRUE)
# log transform 
log.db <- log(diabetes[, 1:8])
log.db
#ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
db.pca <- prcomp(log.db,
                 center = TRUE,
                 scale. = TRUE) 






