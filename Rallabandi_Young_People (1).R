# Reading the data 
young <- read.csv(file.choose(),na.strings = c('',"NA"))

#Understanding the Data
summary(young)
sum(is.na(young))
na.omit(young)

#True for no na , False for yes NA
complete.cases(young)
#To remove null values
young1 <- young[complete.cases(young), ]
summary(young1)

par(mfrow = c(4,4))
for (i in 1:16){
  hist(young[ , i], xlab = names(young)[i], main = names(young)[i], col = "blue")}

#PRE_PROCESSING
#knn to remove null values
library(VIM)
young2 <- kNN(young)
young2 <- young2[,c(1:150)];
sum(is.na(young2))
View(young2)
# All int except smoking and alcohol
str(young2$Internet)
young2 <- subset(young2,select = -c(Punctuality,Lying))
young2$Internet.usage <- as.factor(young2$Internet.usage) 
young2$Gender <- as.factor(young2$Gender) 
young2$Left...right.handed <- as.factor(young2$Left...right.handed) 
young2$Education <- as.factor(young2$Education) 
young2$Only.child <- as.factor(young2$Only.child) 
young2$Village...town <- as.factor(young2$Village...town) 
young2$House...block.of.flats <- as.factor(young2$House...block.of.flats)
str(young2)

row <- nrow(young2)
set.seed(12345)
trainindex <- sample(row,0.6*row,replace = FALSE)
training <- young2[trainindex,]
validation <- young2[-trainindex,]

model <- lm(Age~.,data=training)
summary(model)
plot(model)

model1 <- lm(Age~Pop+Hiphop..Rap+Swing..Jazz+Mathematics+Chemistry+Foreign.languages+Gardening+Alcohol+Elections+Judgment.calls+Compassion.to.animals+Changing.the.past+Parents..advice+Questionnaires.or.polls+Weight+Education,data=training)
summary(model1)
plot(model1)

back <- step(model, direction = 'backward')
coefficients(back)
summary(back)

#Accuracy of the model
library(caret)
ctrl <- trainControl(method = "cv", number = 10)
ac_model1 <- train(x = training[,c('Pop','Hiphop..Rap','Swing..Jazz','Mathematics','Chemistry','Foreign.languages','Gardening','Alcohol','Elections','Judgment.calls','Compassion.to.animals','Changing.the.past','Parents..advice','Questionnaires.or.polls','Weight','Education')], y = training[,'Age'], method = "lm", trControl = ctrl)
ac_model1

#for model1 with selected variables
mean((young2$Age-predict(back,young2))[-trainindex]^2)
#for model with all variables
mean((young2$Age-predict(model,young2))[-trainindex]^2)
AIC(back)
BIC(back)
AIC(model)
BIC(model)
#Applying PCA on all variables
#Finding the factor variables in data frame
young3 <- young2
str(young3)
rm_nam <- names(Filter(is.factor,young3))
str(young3$Internet.usage)
str(young3$Gender)
str(young3$Left...right.handed)
str(young3$Education)
str(young3$Only.child)
str(young3$Village...town)
str(young3$House...block.of.flats)
young3$Internet.usage <- as.integer(as.factor(young3$Internet.usage))
young3$Gender <- as.integer(young3$Gender)
young3$Left...right.handed <- as.integer(young3$Left...right.handed)
young3$Education  <-  as.integer(young3$Education)
young3$Only.child <- as.integer(young3$Only.child)
young3$Village...town <- as.integer(young3$Village...town)
young3$House...block.of.flats <- as.integer(young3$House...block.of.flats)
young3$Smoking <- as.integer(young3$Smoking)
young3$Alcohol <- as.integer(young3$Alcohol)
str(young3)
#pca_train <- subset(training,select=-c(Internet.usage,Gender,Left...right.handed,Education,Only.child,Village...town,House...block.of.flats,Smoking,Alcohol,Age))
#pca_test <- subset(validation,select=-c(Internet.usage,Gender,Left...right.handed,Education,Only.child,Village...town,House...block.of.flats,Smoking,Alcohol,Age))
pca_train <- young3[trainindex,]
pca_test <- young3[-trainindex,]
str(pca_train)
names(Filter(is.factor,pca_train))
names(Filter(is.numeric,pca_train))
young4 <- prcomp(pca_train,center=TRUE,scale. = TRUE)
sd <- young4$sdev
var <- sd^2
var[1:10]
#higher the varience more is the information contained in PCA
propvar <- var/sum(var)
propvar[1:10]
#Deciding number of PCA's for modelling by finding % of variance explained by the model
plot(cumsum(propvar),xlab='Principal Component',ylab='Cummulative Sum of Percent of varience explained')
# therefore ffrom the plot we see that 100 components can explain about 90% variemce in the data
#Now Number of components are reduced to 100 from 139
pr_train <- data.frame(Age=pca_train$Age,young4$x)
#First 100 PC's are selected 
pr_train <- pr_train[,1:101] 

#Run the model using random forest
library(randomForest)
rf_model <- randomForest(Age~.,data = pr_train)
rf_model

#library(caret)
#ctrl <- trainControl(method = "cv", number = 10)
#rf2 <- train(x = pr_train[,-1], y = pr_train[,1], method = "rf", trControl = ctrl)
#rf2
#transform test into PCA
pr_test <- predict(young4,newdata = pca_test)
pr_test <- as.data.frame(pr_test)
names(pr_test)
#select first 100 Pc's from test
pr_test <- pr_test[,1:100]
result <- predict(rf_model,pr_test)
result <- as.integer(result)
result_value <- data.frame(Actual = validation$Age,Predicted = result) 
View(result_value)
MSE <- sum((result_value$Actual-result_value$Predicted)^2)/nrow(result_value)
MSE

#MODELLING
#Use Linear regression for model
pca_lm <- lm(Age~.,data=pr_train)
summary(pca_lm)
lm_res <- predict(pca_lm,pr_test)
lm_res <- as.integer(lm_res)
lm_resvalue <- data.frame(Actual = validation$Age,Predicted = lm_res)
lm_MSE <- sum((lm_resvalue$Actual-lm_resvalue$Predicted)^2)/nrow(lm_resvalue)
lm_MSE
summary(pca_lm)
AIC(pca_lm)
BIC(pca_lm)
plot(pca_lm)
plot(back)
plot(model)

Random Forest:
  library(randomForest)
set.seed(12345)
iris.rf <- randomForest(Age ~ Pop+Hiphop..Rap+Swing..Jazz+Mathematics+Chemistry+Foreign.languages+Gardening+Alcohol+Elections+Judgment.calls+Compassion.to.animals+Changing.the.past+Parents..advice+Questionnaires.or.polls+Weight+Education, data=young2, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)

plot(iris.rf)
iris.rf.variableimportance
varImpPlot(iris.rf)

##explaining rand
min_depth_frame <- min_depth_distribution(iris.rf)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame)

importance_frame <- measure_importance(iris.rf)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame
plot_importance_ggpairs(importance_frame)
explain_forest(irf, interactions = TRUE, data = young2)


#randomforest regression for age 
set.seed(12345)
irf <- randomForest(Age ~ Education+Weight+Elections, data=young2, importance=TRUE,
                    proximity=TRUE)
print(irf)
plot(irf)
#treee regression for age
library(tree)
set.seed(12345)
tr<-tree(Age ~ Education+Weight+Elections, data=young2, method = "class")
print(tr)
#plot for tree
plot(tr, uniform=TRUE)
text(tr, use.n=TRUE, all=TRUE, cex=.8)

#a
library(SDMTools)
mylogit.probs<-predict(tr,validation)
AccuMeasures = accuracy(validation$Age,mylogit.probs)
AccuMeasures

#multi
fit <- lm(Age ~ Education+Weight+Elections, data=young2)
summary(fit) # show results

plot(fit)
#correlation for age
cor(young)

#comparing the model
fit1 <- lm(Age ~ Education+Weight+Elections+Gardening, data=young2)
fit2 <- lm(Age ~ Education+Weight+Election ,data=young2)
anova(fit1, fit2)

#
install.packages("mclust")
library(mclust)
fit <- Mclust(young2)
plot(fit) # plot results 
summary(fit) # display the best model

#converting eduction as factor
df$young2.Education=as.integer(as.factor(df$young2.Education))
table(young2$Education)
#cluster
df=data.frame(young2$Age,young2$Education,young2$Weight)
install.packages("cluster")
install.packages("fpc")
library(cluster)
library(fpc)
install.packages("factoextra")
install.packages("tidyverse")
library(factoextra)
library(tidyverse)
dat <- df[1:100, ] # without known classification 
# Kmeans clustre analysis
clus <- kmeans(dat, centers=6)
# Fig 01
fviz_cluster(clus, data = dat)

###scatterplot for data df
library(ggplot2)
ggplot(df,aes(x=young2.Age,y=young2.Weight,col=young2.Education) )+geom_point()
##
df2=data.frame(young2$Questionnaires.or.polls,young2$Mood.swings,young2$Elections,young2$Foreign.languages)
dat2 <- df2[1:100, ]
clus2<- kmeans(dat2, centers=5)
fviz_cluster(clus2, data = dat)

#SVM
#Load Library
library(e1071)
pr_test1 <- as.data.frame(pr_test, Age = young3$Age)
#Regression with SVM
modelsvm <- svm(Age~.,pr_train)
summary(modelsvm)
#Predict using SVM regression
predYsvm <- predict( modelsvm, pr_test)

lsvm_resvalue <- data.frame(Actual = validation$Age,Predicted = predYsvm)
plot(lsvm_resvalue, main = "SVM model- Predicted Vs Actual values of Age")
lsvm_MSE <- sum((lsvm_resvalue$Actual-lsvm_resvalue$Predicted)^2)/nrow(lsvm_resvalue)
lsvm_MSE
plot(modelsvm)

tuneResult1 <- tune(svm, Age~.,  data = pr_train,
                    ranges = list(epsilon = seq(0,1 ,0.5), cost = 2^(seq(0.5,8,.5)))
)
plot(tuneResult1)
## Select the best model out of 1100 trained models and compute RMSE

#Find out the best model
BstModel=tuneResult1$best.model
summary()
library(rcompanion)
nagelkerke(BstModel)
rsq(BstModel)
#Predict Y using best model
PredYBst=predict(BstModel,pr_test)

library(hydroGOF)
#Calculate RMSE of the best model 

RMSEBst=rmse(PredYBst, validation$Age)
##Calculate parameters of the Best SVR model

#Find value of W
W = t(BstModel$coefs) %*% BstModel$SV
W
#Find value of b
b = BstModel$rho
b

lsvm_resvalue <- data.frame(Actual = validation$Age,Predicted = PredYBst)
plot(lsvm_resvalue, main = "SVM model- Predicted Vs Actual values of Age")


ggplot(lsvm_resvalue, aes(x=`Actual`, y= Predicted, label=Predicted)) + 
  geom_bar(stat='identity', width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip() 
#Ensembling
library(mlbench)
library(caret)
library(caretEnsemble)

predDF <- data.frame(lm_res, result, PredYBst, Age = validation$Age)

# Train the ensemble
modelStack <- train(Age ~ ., data = predDF, method = "rf")
plot(modelStack)

row <- nrow(pr_train)
set.seed(12345)
trainindex <- sample(row,0.2*row,replace = FALSE)
test <- pr_train[trainindex,]

testPredrf <- predict(rf_model, test)
testPredlm <- predict(pca_lm, test)
testPredsvm <- predict(BstModel, test)

# Using the base learner test set predictions, 
# create the level-one dataset to feed to the ensemble
testPredLevelOne <- data.frame(testPredrf, testPredlm, testPredsvm, Age = test$Age)
combPred <- predict( modelStack, validation$Age)

ensem_resvalue <- data.frame(Actual = validation$Age,Predicted = combPred)

ensem_MSE <- sum((ensem_resvalue$Actual-ensem_resvalue$Predicted)^2)/nrow(ensem_resvalue)
ensem_MSE

#Tuning parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'lm', 'knn', 'svmRadial')
set.seed(12345)
models1 <- caretList(Age~., data=pr_train, trControl=control, methodList=algorithmList)
results1 <- resamples(models1)
summary(results1)
dotplot(results1)

# correlation between results
modelCor(results1)
splom(results1)


#Cluster Analysis
df2=data.frame(young2$Questionnaires.or.polls,young2$Mood.swings,young2$Elections,young2$Foreign.languages,young2$Age)
dat2 <- df2[1:100, ]
clus2<- kmeans(dat2, centers=2)

#finding k value
kn <- NbClust(dat2, min.nc=2, max.nc=15, method="kmeans")
fviz_cluster(clus2, data = dat2)
clus2

#Logistic Regression
pr_train$Age5 <- cut(pr_train$Age, breaks = c(14, 19, 30), labels = c(0, 1))
pr_train$Age <- NULL
pr_test <- data.frame(Age=pca_test$Age,young5$x)
pr_test$Age5 <- cut(pr_test$Age, breaks = c(14, 19, 30), labels = c(0, 1))
pr_test$Age <- NULL
pca_lr <- glm(Age5~., data = pr_train, family = "binomial")
summary(pca_lr)
pca_lr.probs<-predict(pca_lr,pr_test,type="response")
matrix = confusion.matrix(pr_test$Age5, pca_lr.probs,threshold=0.2)    
matrix

AccuMeasures = accuracy(pr_test$Age5,pca_lr.probs,threshold=0.2)
# Extracting specific values from accuracy table
AccuMeasures

# print and plot the accuracy vs cutoff threshold values
print(c(accuracy= acc, cutoff = thresh))
plot(thresh,acc,type="l",xlab="Threshold",ylab="Accuracy", main="Validation Accuracy for Different Threshold Values")

# attach mylogit.probs into the validation set, and set response as 1 if mylogit.probs >.5 and 0 otherwise.
mydf <-cbind(validation,young8.probs)
mydf$response <- as.factor(ifelse(mydf$young8.probs>0.35, 1, 0))
mydf$Age5
library(ROCR)
young8_scores <- prediction(predictions=mydf$young8.probs, labels=mydf$Age5)

#PLOT ROC CURVE
young8_perf <- performance(young8_scores, "tpr", "fpr")

plot(young8_perf,
     main="ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")

# AREA UNDER THE CURVE
young8_auc <- performance(young8_scores, "auc")
as.numeric(young8_auc@y.values)  ##AUC Value

#Gain Chart
gain <- performance(young8_scores, "tpr", "rpp")
plot(gain, main = "Gain Chart")

