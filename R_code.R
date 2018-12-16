
datas<-read.csv("credits.csv")
str(datas)

#Converting int to factors
datas$AGE[is.na(datas$AGE)]<-mean(datas$AGE,na.rm = T)
datas[, 5:10][is.na(datas[, 5:10])] <- '0'
datas$NEW_CAR<-as.factor(datas$NEW_CAR)
datas$USED_CAR<-as.factor(datas$USED_CAR)
datas$FURNITURE<-as.factor(datas$FURNITURE)
datas$RADIO.TV<-as.factor(datas$RADIO.TV)
datas$EDUCATION<-as.factor(datas$EDUCATION)
datas$RETRAINING<-as.factor(datas$RETRAINING)
str(datas)
datac<-datas[,1:32]
sum(is.na(datac))
plot(datac)
#The following can be done in other way like:dt[c(2,3,5:7,13,15:22)]<-lapply(dt[c(2,3,5:7,13,15:22)],factor)
datas$CHK_ACCT <- as.factor(datas$CHK_ACCT)
datas$HISTORY<-as.factor(datas$HISTORY)
datas$SAV_ACCT<-as.factor(datas$SAV_ACCT)
datas$EMPLOYMENT<-as.factor(datas$EMPLOYMENT)
datas$MALE_DIV<-as.factor(datas$MALE_DIV)
datas$MALE_SINGLE<-as.factor(datas$MALE_SINGLE)
datas$MALE_MAR_or_WID<-as.factor(datas$MALE_MAR_or_WID)
datas$CO.APPLICANT<-as.factor(datas$CO.APPLICANT)
datas$GUARANTOR<-as.factor(datas$GUARANTOR)
datas$PRESENT_RESIDENT<-as.factor(datas$PRESENT_RESIDENT)
datas$REAL_ESTATE<-as.factor(datas$REAL_ESTATE)
datas$PROP_UNKN_NONE<-as.factor(datas$PROP_UNKN_NONE)
datas$OTHER_INSTALL<-as.factor(datas$OTHER_INSTALL)
datas$RENT<-as.factor(datas$RENT)
datas$OWN_RES<-as.factor(datas$OWN_RES)
datas$JOB<-as.factor(datas$JOB)
datas$TELEPHONE<-as.factor(datas$TELEPHONE)
datas$FOREIGN<-as.factor(datas$FOREIGN)
datas$RESPONSE<-as.factor(datas$RESPONSE)
datas$NEW_CAR<-as.factor(datas$NEW_CAR)
datas$USED_CAR<-as.factor(datas$USED_CAR)
datas$FURNITURE<-as.factor(datas$FURNITURE)
datas$RADIO.TV<-as.factor(datas$RADIO.TV)
datas$EDUCATION<-as.factor(datas$EDUCATION)
datas$RETRAINING<-as.factor(datas$RETRAINING)

datas$X <- NULL

#packages
install.packages("C50")
install.packages("rpart.plot")
install.packages("ROCR")
install.packages("randomForest")
library('rpart')

#ANSWER PART 2
mdData<-datas

rpModel1=rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split = 'information'),control = rpart.control(minsplit = 5,maxdepth = 15,  minbucket =2))
predTrn=predict(rpModel1, mdData, type='class')
mean(predTrn==mdData$RESPONSE)
summary(rpModel1)
table(pred=predict(rpModel1,mdData, type="class"), true=mdData$RESPONSE) #Confusion matrixx

#LIFT-CHART
library("ROCR")
#score test data set
mdData$score<-predict(rpModel1,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

####################################NEXT PART OF SECOND QUESTION: DIVIDING THE DATA INTO 50-50##################################
mdData1<-datas

TRG_PCT=0.6
nr=nrow(mdData1)
set.seed(123)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
mdTrn=mdData1[trnIndex,]   
mdTst = mdData1[-trnIndex,]  
rpModel1 <- rpart(RESPONSE ~AMOUNT+CHK_ACCT+DURATION+OBS.+AGE, data=mdTrn, method="class",control = rpart.control(maxdepth = 15, minsplit = 5, minbucket = 2,xval = 81, cp = 0.05))


predTrn=predict(rpModel1, mdTrn, type='class')

mean(predTrn==mdTrn$RESPONSE)
predTest=predict(rpModel1, mdTst, type='class')


mean(predTest==mdTst$RESPONSE)
table(pred = predTrn, true=mdTrn$RESPONSE)
table(pred=predict(rpModel1, mdTst, type="class"), true=mdTst$RESPONSE) #Confusion matrixx
summary(rpModel1)

CTHRESH=0.3

predProbTrn=predict(rpModel1, mdTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=mdTrn$RESPONSE)
#Accuracy
mean(predTrn==mdTrn$RESPONSE)
predTest=predict(rpModel1, mdTst, type='prob')
predTests = ifelse(predTest[,'1'] >= CTHRESH, '1', '0')

mean(predTests==mdTst$RESPONSE)



#Obtain the model's predictions on the test data
#combining the two steps for ge
cm <- table(pred=predict(rpModel1,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
f1

library("ROCR")
#score test data set
mdData$score<-predict(rpModel1,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#C5 tree.
library(C50)
C5imp(c50Tree)

c50Tree = C5.0 (RESPONSE~CHK_ACCT+GUARANTOR+SAV_ACCT+OTHER_INSTALL+DURATION+FOREIGN, data = datas, rules = FALSE,control = C5.0Control (subset = TRUE,bands = 0, winnow=FALSE, CF=0.25, minCases=5, noGlobalPruning=TRUE, sample=0,fuzzyThreshold = FALSE,seed = sample.int(4096, size = 1) -
                                                                                 1L, earlyStopping = TRUE, label = "RESPONSE"))

predTrn=predict(c50Tree, datas, type='class')
mean(predTrn==mdData$RESPONSE)

table(pred=predict(rpModel1,mdData, type="class"), true=mdData$RESPONSE) #Confusion matrixx



####################################NEXT PART OF SECOND QUESTION: DIVIDING THE DATA INTO 50-50##################################
mdData1<-datas
TRG_PCT=0.8
nr=nrow(mdData1)
set.seed(123)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
mdTrn=mdData1[trnIndex,]   
mdTst = mdData1[-trnIndex,] 
c50Tree5 = C5.0 (RESPONSE~CHK_ACCT+GUARANTOR+SAV_ACCT+OTHER_INSTALL+DURATION+FOREIGN, data = mdTrn,
                rules = FALSE,control =
                  C5.0Control (subset =FALSE,
                               bands = 0,
                               winnow=FALSE, 
                               CF=0.25,
                               minCases=1,
                               noGlobalPruning=TRUE,
                               sample=0,
                               fuzzyThreshold = FALSE,
                               seed = sample.int(4096, size = 1) -1L, 
                               earlyStopping = TRUE, label = "RESPONSE"))


predTrn3=predict(c50Tree5, mdTrn, type='class')

mean(predTrn3==mdTrn$RESPONSE)
predTest4=predict(c50Tree5, mdTst, type='class')


mean(predTest4==mdTst$RESPONSE)



#Obtain the model's predictions on the test data
#combining the two steps for ge
cm4 <- table(pred=predict(c50Tree5,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm4) # number of instances
diag = diag(cm4) # number of correctly classified instances per class 
rowsums = apply(cm4, 2, sum) # number of instances per class
colsums = apply(cm4, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
f1
precision
recall
library("ROCR")
#score test data set
mdData1$score<-predict(c50Tree5,type='prob',mdData1)
pred<-prediction(mdData1$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)      

###########################################################################################################



library("ROCR")
#score test data set
mdData$score<-predict(rpModel1,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)        
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ques2-d)
mdData<-datas

rpModel1=rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split = 'information'),control = rpart.control(minsplit = 5,maxdepth = 15,  minbucket =2))
predTrn=predict(rpModel1, mdData, type='class')
mean(predTrn==mdData$RESPONSE)
summary(rpModel1)
table(pred=predict(rpModel1,mdData, type="class"), true=mdData$RESPONSE) #Confusion matrixx

#LIFT-CHART
library("ROCR")
#score test data set
mdData$score<-predict(rpModel1,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

####################################NEXT PART OF SECOND QUESTION: DIVIDING THE DATA INTO 50-50##################################
mdData1<-datas

TRG_PCT1=0.8
nr=nrow(mdData1)
set.seed(123)
trnIndex = sample(1:nr, size = round(TRG_PCT1*nr), replace=FALSE) 
mdTrn=mdData1[trnIndex,]   
mdTst = mdData1[-trnIndex,]  
rpModel7 <- rpart(RESPONSE ~AMOUNT+CHK_ACCT+DURATION+OBS.+AGE, data=mdTrn, method="class",control = rpart.control(maxdepth = 15, minsplit = 5, minbucket = 2,cp=0.03))


predTrn2=predict(rpModel7, mdTrn, type='class')

mean(predTrn2==mdTrn$RESPONSE)
predTest2=predict(rpModel7, mdTst, type='class')


mean(predTest2==mdTst$RESPONSE)
table(pred = predTrn, true=mdTrn$RESPONSE)
table(pred=predict(rpModel1, mdTst, type="class"), true=mdTst$RESPONSE) #Confusion matrixx




library(rpart.plot)
rpart.plot::prp(rpModel7, type=2, extra=1)

#Obtain the model's predictions on the test data
#combining the two steps for ge
cm1 <- table(pred=predict(rpModel7,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm1) # number of instances
diag = diag(cm1) # number of correctly classified instances per class 
rowsums = apply(cm1, 2, sum) # number of instances per class
colsums = apply(cm1, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
f1
precision

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#question 3-a
#WE git the highest precision for the 60-30 split

mdData2<-datas
TRG_PCT1=0.60
nr=nrow(mdData2)
set.seed(123)
trnIndex = sample(1:nr, size = round(TRG_PCT1*nr), replace=FALSE) 
mdTrn1=mdData2[trnIndex,]   
mdTst1 = mdData2[-trnIndex,] 
c50Tree5_6030 = C5.0 (RESPONSE~CHK_ACCT+GUARANTOR+SAV_ACCT+OTHER_INSTALL+DURATION+FOREIGN, data = mdTrn1,
                 rules = FALSE,control =
                   C5.0Control (subset =FALSE,
                                bands = 0,
                                winnow=FALSE, 
                                CF=0.25,
                                minCases=1,
                                noGlobalPruning=TRUE,
                                sample=0,
                                fuzzyThreshold = FALSE,
                                seed = sample.int(4096, size = 1) -1L, 
                                earlyStopping = TRUE, label = "RESPONSE"))


predTrn3=predict(c50Tree5_6030, mdTrn, type='class')

mean(predTrn3==mdTrn$RESPONSE)
predTest4=predict(c50Tree5_6030, mdTst, type='class')


mean(predTest4==mdTst$RESPONSE)



#Obtain the model's predictions on the test data
#combining the two steps for ge
cm4 <- table(pred=predict(c50Tree5_6030,mdTst, type="class"), true=mdTst$RESPONSE)
n = sum(cm4) # number of instances
diag = diag(cm4) # number of correctly classified instances per class 
rowsums = apply(cm4, 2, sum) # number of instances per class
colsums = apply(cm4, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
f1
precision
recall
library("ROCR")
#score test data set
mdData2$score<-predict(c50Tree5_6030,type='prob',mdData2)
pred<-prediction(mdData2$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf) 
library(C50.plot)

plot(c50Tree5_6030, subtree=3)

#calculating misclassification cost.
costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

rpTree = rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list( prior = c(.70,.30), loss = costMatrix, split = "information"))


th = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th
