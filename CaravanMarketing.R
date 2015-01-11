#Datamining Exercise Insurance Customer
library(dplyr)

columns <- read.table("./Data/columns.csv" , sep=",", header=FALSE, strip.white=TRUE, na.strings="")
columns <- as.character(na.omit(columns[,2]))


train_data            <- read.table("./Data/ticdata2000-training.txt" , sep="\t", header=FALSE)
colnames(train_data)  <- columns

#Creating data-frame 
train_data_df <- tbl_df(train_data)

#Creating factor variables
train_data_df$MOSTYPE  <- as.factor(train_data_df$MOSTYPE)
train_data_df$MGEMLEEF <- as.factor(train_data_df$MGEMLEEF)
train_data_df$PWAPART  <- as.factor(train_data_df$PWAPART)
train_data_df$MOSHOOFD <- as.factor(train_data_df$MOSHOOFD)


#Same for test_data
test_data             <- read.table("./Data/ticeval2000-validation.txt" , sep="\t", header=FALSE)
colnames(test_data)   <-columns[1:length(columns)-1]

#creating data-frame
test_data_df          <- tbl_df(test_data)

test_data_df$MGEMLEEF <- as.factor(test_data_df$MGEMLEEF)
test_data_df$PWAPART  <- as.factor(test_data_df$PWAPART)
test_data_df$MOSHOOFD <- as.factor(test_data_df$MOSHOOFD)
test_data_df$MOSTYPE  <- as.factor(test_data_df$MOSTYPE)



test_caravan          <- read.table("./Data/tictgts2000.txt" , sep="\t", header=FALSE)
names(test_caravan)   <-"CARAVAN"

#summary(data)
#cor(data[1:85],data[86], method="spearman") 



#Religious variables are excluded now as their correlation coefficient is very small
train_data_df_mR       <- select(train_data_df, -contains("MGOD"))
test_data_df_mR        <- select(test_data_df, -contains("MGOD"))


require(Hmisc)
#estimgating logistic regression
cust.logit <- glm(CARAVAN~., data=train_data_df_mR)
summary(cust.logit)


require(rpart)
require(caret)
#decision tree package is loaded to build recursive partitioning rpart
#Mostype and MOSHOOFD are excluded from recursive partitioning rausgenommen
train_data_rp  <- select(train_data_df_mR, -contains("MOSTYPE"), -contains("MOSHOOFD"))
test_data_rp   <- select(test_data_df_mR, -contains("MOSTYPE"), -contains("MOSHOOFD"))

#Estimating rp model
cust.rp <-rpart(CARAVAN~., data=train_data_rp)

#presents results of table 7.7
printcp(cust.rp)

#now creating rp model of relevant variables
cust.var.imp <- varImp(cust.rp, useModel=rpart)
x_label <-row.names(cust.var.imp)[cust.var.imp>0]
x_value <-  cust.var.imp[cust.var.imp>0,]
barplot(x_value, space=1, col=grey)
text(seq(1.5,23.5,2),0, x_label,srt=90, adj=0)


#creating bagging model
require(ipred)
cust.ip <-bagging(CARAVAN~., data=train_data_rp, coob=TRUE)
cust_ip.var.imp <- varImp(cust.ip)
x_label <-row.names(cust_ip.var.imp)[cust_ip.var.imp>0]
x_value <-  cust_ip.var.imp[cust_ip.var.imp>0,]
bp_bagging <- barplot(x_value, space=2)
text(bp_bagging,0, x_label,srt=90, adj=0, cex=0.7)


#creating svm model
require(e1071)
cust.svm <-svm(CARAVAN~.,data=train_data_rp, method ="C-classification", kernel="radial",cost=10, gamma=0.1, cross=0,
               fitted=TRUE,probability=TRUE)


#creating random forest model
require(randomForest)
cust.rf <- randomForest(CARAVAN~., data= train_data_rp, mtry=79, importance=TRUE )

#creating boosting model
require(gbm)
cust.boost <-gbm(CARAVAN~., data=train_data_rp, distribution = "gaussian", n.trees=5000, interaction.depth = 4)

#boosting extended different shrinkage
cust.boost.2 <-gbm(CARAVAN~., data=train_data_rp, distribution = "gaussian", n.trees=5000, interaction.depth = 8)

#ROC and AUC comparison
require(ROCR)
#RP-model
cust.rp.pred <- predict(cust.rp, type="matrix", newdata=test_data_rp)
cust.rp.prob.rocr <- prediction(cust.rp.pred,test_caravan)
cust.rp.perf <- performance(cust.rp.prob.rocr, "tpr", "fpr")
plot(cust.rp.perf, main="ROC Curve using Recursive Partitioning", colorize=T)

#Bagging
cust.ip.pred <- predict(cust.ip, type="probability", newdata=test_data_rp)
cust.ip.prob.rocr <- prediction(cust.ip.pred,test_caravan)
cust.ip.perf <- performance(cust.ip.prob.rocr, "tpr", "fpr")
plot(cust.ip.perf, main="ROC Curve using Bagging", colorize=T)

#SVM
cust.svm.pred <- predict(cust.svm, type="probability", newdata=test_data_rp, probability=TRUE)
cust.svm.prob.rocr <- prediction(cust.svm.pred,test_caravan)
cust.svm.perf <- performance(cust.svm.prob.rocr, "tpr", "fpr")
plot(cust.svm.perf, main="ROC Curve using SVM", colorize=T)

#Logistic
cust.logit.pred <- predict(cust.logit, newdata=test_data_df_mR, type="response")
cust.logit.prob.rocr <- prediction(cust.logit.pred,test_caravan)
cust.logit.perf <- performance(cust.logit.prob.rocr, "tpr", "fpr")
plot(cust.logit.perf, main="ROC Curve using Logistic regression", colorize=T)

#RandomForest
cust.rf.pred <- predict(cust.rf, newdata=test_data_rp)
cust.rf.prob.rocr <- prediction(cust.rf.pred,test_caravan)
cust.rf.perf <- performance(cust.rf.prob.rocr, "tpr", "fpr")
plot(cust.rf.perf, main="ROC Curve using Random Forest", colorize=T)

#Boosting
cust.boost.pred <- predict(cust.boost, newdata=test_data_rp, n.trees=5000)
cust.boost.prob.rocr <- prediction(cust.boost.pred,test_caravan)
cust.boost.perf <- performance(cust.boost.prob.rocr, "tpr", "fpr")
plot(cust.boost.perf, main="ROC Curve using Boosting", colorize=T)

#Boosting
cust.boost2.pred <- predict(cust.boost.2, newdata=test_data_rp, n.trees=5000)
cust.boost2.prob.rocr <- prediction(cust.boost2.pred,test_caravan)
cust.boost2.perf <- performance(cust.boost2.prob.rocr, "tpr", "fpr")
plot(cust.boost2.perf, main="ROC Curve using Boosting 2", colorize=T)





#comparison pdf
ppi <- 300
png(filename="ROC_Curve_withoutReligion.png", width=6*ppi, height=6*ppi, res=ppi)
plot(cust.rp.perf, col=2, main="ROC curve without religion variables")
legend(0.5,0.5,c('rpart','bagging','svm','logistic','randomF','boosting','boosting2'), 2:8)
plot(cust.ip.perf, col=3, add=TRUE)
plot(cust.svm.perf, col=4, add=TRUE)
plot(cust.logit.perf, col=5, add=TRUE)
plot(cust.rf.perf, col=6, add=TRUE)
plot(cust.boost.perf, col=7, add=TRUE)
plot(cust.boost2.perf, col=8, add=TRUE)
dev.off()















