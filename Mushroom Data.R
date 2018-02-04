#*******************************************************************************************************
# Name    : Bhaskara Pothula                                                                            *
# Project : Predicting if the mushroom is poisonous or not                                              *
#*******************************************************************************************************



#*******************************************************************************************************
#                                *AGENDA*                                                               *
#1.OBJECTIVE                                                                                            *
#2.DATA QUALITY REPORT                                                                                  *
#3.EDA(EXPLANATORY DATA ANALYSIS)                                                                       *
#4.FEATURE ENGINEERING                                                                                  *
#5.DATA MODELLING                                                                                       *
#6.MODEL EVALUATION AND SELECTION                                                                       *
#7.INFERENCES;RECOMMENDATIONS                                                                           *
#******************************************************************************************************



#*******************************************************************************************************
#                             *1.OBJECTIVE*                                                             *
#                                                                                                       *
#FROM GIVEN SET OF CHARECTERSTICS OF MUSHROOM LIKE ODOR,SHAPE,COLOR,BRUISE                              *
#DETERMINE IF A MUSHROOM IS POSINOUS OR EATABLE.                                                        *
#                                                                                                       *
#*******************************************************************************************************



#automatiaclly loads packages that are not in computer;
library(needs)


#For basic data manipulation operations like filtering,mutating dataframes
needs(plyr)
needs(dplyr)


#Create Univariate and Multivariate plots for both continous and categorical data
needs(ggplot2)


#Pipeline operator to sequence a series of operations on dataset
needs(magrittr)


#Package to implement Random Forest
needs(randomForest)


#Feature seection using recursive feature importance
needs(mlbench)


#Package with tools to implement classification, regression models / sampling data
needs(caret)

needs(logistf)
needs(base)
#Package to select important features
needs(Boruta)


#Package for Model performance metrics like AUC,ROC 
needs(caTools)

#Package to aid grid graphics/ arrange plots gridwise
needs(gridExtra)

#Package fro implementing NAIVE BAYES ALGORITHM
needs(e1071)

#Load data file
mush = read.table('C:/Users/Bhaskara/Desktop/Mushroom Data/mushroom.csv', header = T,sep=',')
mush_fin = subset(mush, select = c(PE,odor,spore.print.color,gill.size,gill.color,stalk.root,population,habitat))


summary(mush)
#*******************************************************************************************************
#There are no missing Values in the data                                                                *
#'stalk.root' has ? as one of the lables. This can be replaced with respective class lable              *
#'as the proprotion of ? is second largest                                                              *
#******************************************************************************************************

str(mush)
attach(mush_fin)

#"%>%" is a pipe like operator and selection is done using it
mush_d1 <- mush %>% select(cap.shape:habitat) 


#equal proportions of output class is important to obtain a non biased model.
prop.table(table(mush$PE)) #output classes are decently balanced.


#*******************************************************************************************************
#                             *2.DATA QUALITY REPORT*                                                   *
#                                                                                                       *
#*******************************************************************************************************
#The dataset has 1) 22 categorical input variables 
#                2) 1 output variable with 2 levels
#                3) 8124 input records
#
#After checking the data, no missing values are found in the dataset and the 
#dataset is already preproceesed.
#
#Since the dats is completely categorical Outliers doesn't affect the models
#
#
#
#*******************************************************************************************************
#                                     *3.EDA*                                                           *
#                                                                                                       *
#*******************************************************************************************************

#OUPUT CLASSES IN THE DATA ARE BALANCED IN EQUAL PROPORTIONS WHICH RESULTS IN AN UNBIASED MODEL.

histogram(mush$PE,col="LIGHTGREEN",xlab = "PE",ylab = "Frequency",main = "Mushroom : Histogram of PE")

#HOW DIFFERENT ODORS ARE VARYING ACROSS OVER POISONOUS AND EDIBLE MUSHROOMS

ggplot(mush_fin, aes(mush$odor, fill = mush$PE)) + geom_bar()+
  labs(title ="Stacked Bar Chart", x = "odor" , y = "count"  )

#ODOR 'f' IS TYPICAL ODOR OVER POISONOUS MUSHROOMS.
#ODOR C,M,P,S,Y ALSO CHARECTARIZE POISONOUS MUSHROOMS.
#WHEREAS ODORS A,I AND N CHARECTARIZE EDIBLE MUSHROOMS.

#HOW DIFFERENT GILL SIZES ARE VARYING ACROSS OVER POISONOUS AND EDIBLE MUSHROOMS

ggplot(mush_fin, aes(mush_fin$gill.size, fill = mush$PE)) + geom_bar()+
  labs(title ="Stacked Bar Chart", x = "Gill Size" , y = "Count"  )

#GILL SIZE 'n' INDICATES POISONOUS MUSHROOMS.

#EXPLORE THE INTERSECTION OF DIFFERENT CHARECTERSTICS OVER POSINOUS AND EDIBLE MUSHROOMS

qp1 = qplot(habitat, gill.color, color = PE, data = mush_fin, geom = "jitter", main = "Mushroom data set - Cap color vs Habitat")
#MUSHROOMS WOTH BLUE GILS IN HABITATS OF P,L AND D ARE MOSTLY POISONOUS.
#EATABLE MUSHROOMS ARE MOSTLY FROM HABITAT 'W'.

qp2 = qplot(odor, spore.print.color, color = PE, data = mush_fin, geom = "jitter", main = "Mushroom data set - Odor vs Spore Print Color")
#MUSHROOMS WITH WHITE SPORS ARE MOSTY POSIONOUS.
#MUSHROOMS OF COLOR N,K WITH ODOR C AND P ARE ALSO MOSTLY POISONOUS.

qp3 = qplot(population , habitat , color = PE, data = mush_fin, geom = "jitter", main = "Mushroom data set - Stalk Surface below Ring vs Stalk Color above Ring")
#MAJOR PROPORTION OF POISONOUS MUSHROOMS ARE FROM POPULATION S,V AND E.
#HABITATAS FROM U,P,G AND D FROM POPULATION ARE MOSTLY POISONOUS.

qp4 = qplot(spore.print.color,population , color = PE, data = mush_fin, geom = "jitter", main = "Mushroom data set - Stalk Surface below Ring vs Stalk Color above Ring")
#MUSHROOMS FROM POPULATION Y,V,S AND WITH SPORS COLOR OF H ARE MOSTLY POISONOUS.
#MUSHROOMS WITH SPORE COLORS K,N AND W ARE MOSTLY EDIBLE.

grid.arrange(qp1, qp2, qp3, qp4, ncol = 2, nrow = 2)


#*******************************************************************************************************
#                             *4.FEATURE ENGINEERING*                                                   *
#*******************************************************************************************************
#                                                                                                       *
#*****************************CURSE OF DIMENSIONALITY***************************************************
#
#
# THIS DATASET HAS 22 PREDICTORS, HOWEVER ADDING ALL OF THEM DOESN'T NECESSARILY DECREASE THE TEST MSE
#RATHER INCREASE THE OVERFITTING OF TRAINING MODEL.


# IN GENERAL ADDING PREDICTORS THAT ARE TRULY ASSOCIATED WITH THE MODEL WILL DECREASE THE TEST MSE AND 
#AND DOESN'T ALLOW MODEL TO CAPTURE NOISE. HOWEVER ADDING VARIABLES THAT ARE NOT ASSOCIATED WITH THE MODEL
#WILL INCREASE THE VARIANCE OF MODEL WHICH MAKES MODEL CAPTURE NOISE COMPONENTS.


# SO I TRY TO LIMIT THE PREDICTORS USED IN THE MODEL BY USING FEATURE ENGINEERING.


#*******************************************************************************************************
#EXPLORE VARIOUS FEATURE ENGINEERING METHODS TO REDUCE DIMENSIONALITY OF DATA                           *
#*******************************************************************************************************


#***************************METHOD 1 TO SELECT IMPORTANT FEATURES***************************************
#                                                                                                       *
#Using RANDOM FOREST to select feture importance                                                        *
#veil type has importance of 0, so it can be removed while building model.Also explore other            *
#feature importance techniques to chooose the best.                                                     *
#                                                                                                       *
#*******************************************************************************************************
library (randomForest)                                                        

bag.mush =randomForest(PE~.,data=mush ,mportance =TRUE)                       
varImp(bag.mush)                                                        



#**************************METHOD 2 TO SELECT IMPORTANT FEATURES****************************************
#                                                                                                       *
#FEATURE SELECTION USING BORUTA PACAKGE IN R.BORUTA IS AN INBUILT PACKAGES THAT RUNS RANDOM FOREST ON   *
#THE PREDICTOR VARIABLES AND SELECTS IMPORTANT VARIABLES BASED ON MEAN DECREASE ACCURACY                *
#                                                                                                       *
#ALL THE VARIABLES ARE DEEMED AS IMPORTANT BY BORUTA PACKAGE, WHICH IS NOT DESIRED FOR US.              *
#*******************************************************************************************************

mush1 = subset(mush, select = -c(veil.type ))

boruta.train <- Boruta(PE~.-PE, data = mush1, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
#*******************************************************************************************************
#**************************METHOD 3 TO SELECT IMPORTANT FEATURES****************************************
#                                                                                                       *
#Feature selection using RECURSIVE FEATURE ELIMINATION.                                                 *
#RFE FUNCTION ALSO RUNS RANDOM FOREST ON THE DATA TO SELECT IMPORTANT FEATURES                          *
#RFE FUNCTION ALSO BEAUTIFULLY DEPICTS THE CHANGE IN CORSS VALIDATION ACCURACY WITH                     *
#ADDITION OF EACH VARIABLE TO THE MODEL.SO THIS HELPS TO SELECT THE LESS NUMBER OF                      *
#FEATURES THAT PROVIDE BEST ACCURACY.                                                                   *                                                          
#                                                                                                       *
#                                                                                                       *                                                                            
#VARIABLES "ODOR, SPORE.PRINT.COLOR, GILL.SIZE, GILL.COLOR, STALK.ROOT, POPULATION                      *
#AND HABITAT" SEEMS TO GIVE 100% ACCURACY.                                                              *
#                                                                                                       *
#OUT OF 22 VARIABLES ONLY 7 ARE USED IN THE MODEL NOW. COMPUTATIONS INVOLVED IN THE                     *
#MODEL ARE REDUCED BY 70% ALMOST WITH OUT DECREASE IN MODEL'S PERFORMANCE                               *
#                                                                                                       *
#*******************************************************************************************************
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

results <- rfe(mush1[,2:22], mush1[,1], sizes=c(1:22), rfeControl=control)

print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
#6 features explain 100% accuracy of the model
results

varImp(results)


#*******************************************************************************************************
#USE METHOD 3 TO SELECT SUBSET OF IMPORTANT FEATURES AS IT HELPS TO DETERMINE THE OPTIMAL NUMBER OF     *
#PREDICTORS FOR AN OPTIMUM PREDICTIVE MODEL.                                                            *
#                                                                                                       *
#SELECT THE IMPORTANR FEATURES ONLY                                                                     *
#*******************************************************************************************************
mush_fin = subset(mush, select = c(PE,odor,spore.print.color,gill.size,gill.color,stalk.root,population,habitat))


#*******************************************************************************************************
#                             *5.DATA MODELLING*                                                        *
#                                                                                                       *
#*******************************************************************************************************
#                             *CROSS VALIDATION*                                                        *
#                                                                                                       *
#CROSS VALIDATION DESIGNATES A TEST SUBSET TO ESTIMATE THE TEST ERROR FROM TRAINING ERROR WHEN THERE IS *
#NO PROPER TEST DATASET.                                                                                *
#                                                                                                       *
#10 FOLD CV(DIVIDE DATA IN TO 10 PARTS/FOLDS);TRAINS THE MACHINE LEARNING MODEL ON 9 PARTS OF DATA AND  * 
#ESTIMATES TEST ERROR ON THE REMAINING PART.                                                            *
#                                                                                                       *
#THIS PROCESS IS REPEATED 10 TIMES ON DIFFERENT TEST DATA FOLDS                                         *
#                                                                                                       *
#THE AVERAGE OF TEST ERROR OVER 10 FOLDS IS THE FINAL ESTIMATED TEST ERROR                              *
#                                                                                                       *
#*******************************************************************************************************
#
#Fit Models on usingK 10 FOLD CV
#Sample data in to 70/30 train and validation parts respectively

sample = sample.split(mush_fin$PE, SplitRatio = .7)
x_train = subset(mush_fin, sample == TRUE)
x_test = subset(mush_fin, sample == FALSE)

#*******************************************************************************************************
#                             *RANDOM FOREST*                                                           *
#*******************************************************************************************************
#                     *RANDOM FOREST VS DECISION TREES*                                                 *
#DECISION TREES DO NOT HAVE SAME PREDEICTIVE CAPABILITY AS OTHER REGRESSION/CLASSIFICATION              *
#PREDICTIVE MODELS AS DECISION TREES SUFFER FROM HIGH VARIANCE.                                         *
#                                                                                                       *
#RANDOMFOREST ON THE OTEHR HAND BUILD N INDEPENDENT TREES ON DATA AND AVERAGES PREDICTIONS OVER 'N'     *
#TREES WHICH REDUCES VARIANCE AND AVOIDS OVERFITITNG.                                                   * 
#                                                                                                       *
#*******************************************************************************************************
y_train<-x_train$PE
y_test <- x_test$PE

x_train$PE<-NULL
x_test$PE<-NULL

#CREATE TRAIN CONTROL OBJECT FOR 10 FOLD CV
ctrl.1<-trainControl(method="repeatedcv",number=10)

#Train RANDOM FOREST 
rf.cv<-train(x=x_train,y=y_train,method="rf",trControl=ctrl.1,tuneLength=3)

#Validate the RANDOM FOREST model
y_predicted<-predict(rf.cv,x_test)

df1<-data.frame(Orig=y_test,Pred=y_predicted)

confusionMatrix(table(df1$Orig,df1$Pred))

#*******************************************************************************************************
#                           *LOGISTIC REGRESSION*                                                       *
#*******************************************************************************************************
#SIMPLE LOGISTIC REGRESSION HAS 2 ISSUES HERE A                                                         *
#                                                                                                       *
#1.FITTED PROBABILITIES NUMERICALLY 0 OR 1 OCCURRED                                                     *
#2.ALGORITHM DID NOT CONVERGE                                                                           *
#                                                                                                       *
#THIS ISSUE OVERINFLATES THE COEFFICIENT ESTIMATORS.                                                    *
#                                                                                                       *
#PERFECT SEPARATION, MEANING THE PREDICTOR IS 'TOO GOOD', THE LOGITS GO TO +/- INFINITY AND EVERYTHING  * 
#FALLS OVER.                                                                                            *
#                                                                                                       *
#THERE ARE 2 POSSIBLE SOLUTIONS FOR THIS :                                                              *
#            1)IMPLEMENT A PENALIZED LOGISTIC REGRESSION(L1/L2 NORMALIZATION)                           *
#            2)IMPLEMENT A BAYESIAN LOGISTIC REGRESSION                                                 *
#                                                                                                       *
#L1/L2 LOGISTIC REGRESSION PENALIZES THE MODEL FOR INCREASING THE NUMBER OF VARIABLES USED IN THE MODEL *
#THERE BY REDUCING THE VARIANCE AND PROVIDING OPTIMAL COEFFICIENT ESTIMATES. SINCE WE ALREADY CHOOSE    *
#6 BEST VARIABLES OUT OF ALL AVAILABLE VARIABLES, I INTEND TO GO WITH BAYESIAN LOGISTIC REGRESSION.     *
#                                                                                                       *
#*******************************************************************************************************


ctrl.2<-trainControl(method="repeatedcv",number=3)

Log_CV = train(x=x_train,y=y_train, method="glm", family=binomial, trControl=ctrl.1,control = list(maxit = 25))

y_predicted_log<-predict(Log_CV,x_test)

df2<-data.frame(Orig=y_test,Pred=y_predicted_log)

confusionMatrix(table(df2$Orig,df2$Pred))

summary(Log_CV)

#*******************************************************************************************************
#                           *BAYESIAN LOGISTIC REGRESSION*                                                       *
#*******************************************************************************************************

Log_B = train(x=x_train,y=y_train, method="bayesglm", family=binomial, trControl=ctrl.1,control = list(maxit = 10000))

y_predicted_B<-predict(Log_B,x_test)

df3<-data.frame(Orig=y_test,Pred=y_predicted_B)

confusionMatrix(table(df3$Orig,df3$Pred))

summary(Log_B)
#Model non convergence issues reduced significantly after using bayesioan logistic regression.
#*******************************************************************************************************
#                                   *KNN CLASSIFIER*                                                    *
#*******************************************************************************************************
model_nv1<-naiveBayes(PE~., data=mush_fin, laplace=1)

y_predicted_nb<-predict(model_nv1,x_test)

df4<-data.frame(Orig=y_test,Pred=y_predicted_nb)

confusionMatrix(table(df3$Orig,df3$Pred))

#*******************************************************************************************************
#                             *6.MODEL EVALUATION AND SELECTION*                                        *
#                                                                                                       *
#*******************************************************************************************************
# # Comparison of different Models based on accuracy                                      
#                                                                                       
# Logistic Regression       - 100.00%                                                    
#                                                                                       
# Bayesian Logistic         - 100.00%  
#
# Random Forest             - 100.00%
#                                                                                       
# NAIVE BAYES CLASSIFIER    - 098.07%                   
#
#*******************************************************************************************************
#                   *IN THIS CASE,WHY DO I CHOOSE LOGISTIC OVER RANDOM FOREST*                          *
#EVEN THOUGH RANDOM FOREST AND LOGISTIC REGRESSION PRODUCE SAME ACCURACY, I CHOOSE LOGISTIC REGRESSION  *
#FOR BELOW REASONS                                                                                      *
#*******************************************************************************************************
#                               *ADVANTAGES OF LOGISTIC REGRESSION*                                     *
#*******************************************************************************************************
#                                                                                                       *
#LOGISTIC REGRESSION CAN ALSO ASSIGN PROBABILITIES TO EACH OBSERVATION, BY CHANGING THE CUTOFFS         *
#ACCORDING TO BUSINESS NEEDS WE CAN SEE DIFFERENT RESULTS FOR SAME CLASSIFICATION MODEL                 *
#                                                                                                       *
#LOGISTIC REGRESSION WITH L2 REGULARIZATION DEALS WELL WITH MULTICOLLINEARITY ISSUE                     *
#                                                                                                       *                                                                                                      *  
#EASY INTERPRETABILITY OF VARIABLES ON OUTPUT                                                           *
#                                                                                                       *
#LESS COMPUTATION COMPARED TO RANDOM FOREST                                                             *
#                                                                                                       *
#SINCE MACHINE LEARNING MODELS WORK ON NEW DATA EVERY TIME, IT IS BETTER TO HAVE A INTERPRETABLE SIMPLE *
#MODEL INORDER TO DRAW SOME INFERENCES FROM DATA AND THEN COMPLEX MODELS CAN BE APPLIED ON THE DATA     * 
#WITH THE HELP OF BUSINESS EXPERTISE.                                                                   *
#                                                                                                       *
#                                                                                                       *
#*******************************************************************************************************
#                             *7.INFERENCES;RECOMMENDATIONS*                                            *
#                                                                                                       *
#*******************************************************************************************************
#           *THE FOLLOWING FEATURES DETERMINE IF A MUSHROOM IS POISONOUS OR EDIBLE*                     *                                            
#                                                                                                       *
#1.ODOR : ODOR P,C,N ARE SUPPOSED TO INCREASE THE LOG ODDS OF MUSHROOM BEING POISONOUS WHEN COMPARED TO *
#DIFFERENT ODORS.                                                                                       *
#MUSHROMS WITH ODORS L AND M LIKE TO BE LESS POISONOUS THAN MNUSHROMS WITH ANY OF REMAINING ODORS.      *
#                                                                                                       *
#                                                                                                       *
#2.SPORE PRINT COLOR : OTHER THAN SPORE PRINT COLOR H AND W EVERY OTHER COLOR IS INCREASING THE ODDS OF *
#MUSHROM BEONG POISONOUS.                                                                               *
#SPORES WITH COLORS RED AND WHITE ARE HIGHLY SIGNIFICANT IN DETERMINING POISONOUS MUSHROMS              *
#                                                                                                       *
#                                                                                                       *
#3.GILL SIZE: COEFFICIENTS OF SIZE N ARE MORE SIGNIFICANT THAN B, WHICH MEANS MUSHROMS WITH GILL SIZE N *
# ARE MORE LIKEY TO BE POISONOUS THAN MUSHROMS WITH GILL SIZE B.                                        *
#                                                                                                       *
#                                                                                                       *
#4.STALK ROOT : ESTIMATES OF STALK ROOT B AND C ARE AGAINIST OTHER CATEFORIES OF STALK ROOT, WHICH MEANS*
#MUSHROMS WITH STALKROOT B AND C ARE MORE POISONOUS COMPARED TO OTHER CATEGORIES OF STALK ROOT          *
#                                                                                                       *
#                                                                                                       *
#GILL COLOR : GILL COLOR INCREASES THE ODDS OF MUSHROM BEING POISONOUS. IRRESPECTIVE OF SPECIFIC COLOR  * 
#OF GILL THE USHROMS ARE POISNOUS ONCE THEIR GILLS CATCH ANY COLOR.                                     *
#                                                                                                       *
#                                                                                                       *
#POPULATIONS : MUSHROMS FROM POPULATION C ARE MOSTLY POSINOUS IRRESPECTIVE OF OTHER ATTIRBUTES.         *
#MUSHROMS FROM OTEHER POPULATIONS ARE INDENDENLTY IND=SIGNIFICANT BUT WHHEN COMBINED WITH OTHER         *
#VARIABLES  POPULATION V AND Y ARE SIGNIFICANT                                                          *
#                                                                                                       *
#                                                                                                       *
#HABITAT : HABITAT IS NOT SIGNIFICANT INDEPENDENLTY BUT WHEN IT INTERACTS WITH OTHER VAIRBALES LIKE     *  
#GILL COLOR IT SEEMS TO HAVE DECENT IMPACT IN DETERMINING IF A MUSHROM IS POISONOUS.                    *
#                                                                                                       *
#*******************************************************************************************************

