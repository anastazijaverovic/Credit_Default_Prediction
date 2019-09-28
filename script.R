#save csv document into credit variable
credit=read.csv("/Users/anastazijaverovic/Downloads/german_cre
dit.csv",header=TRUE,sep=",")



#Creditability - variable showing if client is credit worthy

#view inner structure of R object
str(credit) 

summary(credit)

#look into the data
View(credit)

#save all categorical variables into F object
F=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20)
#convert those columns into factors (categorical)
for(i in F) credit[,i]=as.factor(credit[,i])
#1/3 of data makes test dataset
i_test=sample(1:nrow(credit),size=333)
#the rest is training data set
i_calibration=(1:nrow(credit))[-i_test]

#univariable analysis
plot(credit$Account.Balance)
hist(credit$Duration.of.Credit..month.)
plot(credit$Payment.Status.of.Previous.Credit)
plot(credit$Purpose)
hist(credit$Credit.Amount)
plot(credit$Value.Savings.Stocks)
plot(credit$Length.of.current.employment)
plot(credit$Instalment.per.cent)
plot(credit$Sex...Marital.Status)
plot(credit$Guarantors)
plot(credit$Duration.in.Current.address)
plot(credit$Most.valuable.available.asset)
hist(credit$Age..years.)
plot(credit$Type.of.apartment)
plot(credit$No.of.Credits.at.this.Bank)
plot(credit$Occupation)
plot(credit$No.of.dependents)
plot(credit$Telephone) hist(credit$Foreign.Worker,breaks=2)
#bivariable analysis
#categorical variables
ggplot(credit,aes(factor(Instalment.per.cent),..count..))+geom
_bar(aes(fill=Creditability),
position="dodge")+xlab("Instalment Rates”)

ggplot(credit, aes(factor(Duration.of.Credit..month),
..count..)) +
+ geom_bar(aes(fill = Creditability), position = "dodge") +
xlab("Account Balance")


ggplot(credit, aes(factor(Payment.Status.of.Previous.Credit),
..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Previous Credit Payment")


ggplot(credit, aes(factor(Purpose), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Credit Purpose")


ggplot(credit, aes(factor(Value.Savings.Stocks), ..count..)) +
+ geom_bar(aes(fill = Creditability), position = "dodge") +
xlab("Savings")


ggplot(credit, aes(factor(Length.of.current.employment),
..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Current Employment Length")


ggplot(credit, aes(factor(Sex...Marital.Status), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Personal Status")


ggplot(credit, aes(factor(Guarantors), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Guarantors")


ggplot(credit, aes(factor(Duration.in.Current.address),
..count..)) +
   + geom_bar(aes(fill = Creditability), position = "dodge") +
   xlab("Current Address Duration")


ggplot(credit, aes(factor(Most.valuable.available.asset),
..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Most Valuable Asset")


ggplot(credit, aes(factor(Type.of.apartment), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Housing")


ggplot(credit, aes(factor(No.of.Credits.at.this.Bank),
..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Number of previous credits at this bank")


ggplot(credit, aes(factor(Occupation), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Occupation")


ggplot(credit, aes(factor(No.of.dependents), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Number of people being liable to provide maintenance
  for")


ggplot(credit, aes(factor(Telephone), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Telephone")


ggplot(credit, aes(factor(Foreign.Worker), ..count..)) +
  + geom_bar(aes(fill = Creditability), position = "dodge") +
  xlab("Foreign Worker")


#continuous variables

#credit amount
amount<-credit$Credit.Amount
amount.mean <- mean(amount)
> amount.mean
[1] 3271.248 


ggplot(credit, aes(amount, fill=Creditability)) +
+ geom_density(alpha=.5) + geom_vline(data=credit,
aes(xintercept=amount.mean,
colour=Creditability),linetype="dashed", size=1)

#client’s age
age <- credit$Age..years.
age.mean <- mean(age)
ggplot(credit, aes(age, fill=Creditability)) +
+ geom_density(alpha=.5) + geom_vline(data=credit,
aes(xintercept=age.mean,
colour=Creditability),linetype="dashed", size=1)



#credit duration

duration <- credit$Duration.of.Credit..month.
duration.mean<-mean(duration)
ggplot(credit, aes(duration, fill=Creditability)) +
+ geom_density(alpha=.5) + geom_vline(data=credit,
aes(xintercept=duration.mean,
colour=Creditability),linetype="dashed", size=1)


#multivariable analysis

install.packages(“corrplot")

#save credit variable as data frame
> Columns <- as.data.frame(credit,stringAsFactors = FALSE) 
#take numeric variables (columns)
> res <- cor(Columns[sapply(Columns, is.numeric)])
#draw correlation matrix - circles
> corrplot(res, method = "circle", type=“upper”)

#draw correlation matrix - numbers
> corrplot(res, method = "number", type=“upper")

#prediction tree
#Creditability - variable showing if client is credit worthy

StabloModel <- rpart(Creditability ~ ., data = 
credit[i_calibration,])


install.packages("rpart.plot") library("rpart.plot")
#draw decision tree
prp(StabloModel, type=2,extra=1) 


#draw ROC curve of prediction tree model

> pred = prediction( fitStablo, credit$Creditability[i_test]) > perf <- performance(pred, "tpr", "fpr")
> plot(perf)


#AUC = surface under the ROC tree

> AUCStablo=performance(pred, measure = "auc")@y.values[[1]]
> cat("AUC: ",AUCStablo,"\n")
AUC:  0.7204525


#logistic regression

#logistic regression on chosen variables

+ > LogisticModel <- glm(Creditability ~ Account.Balance +
Payment.Status.of.Previous.Credit + Purpose +
Length.of.current.employment + Sex...Marital.Status,
family=binomial, data = credit[i_calibration,])



> fitLog <-
predict(LogisticModel,type="response",newdata=credit[i_test,])


> pred = prediction( fitLog, credit$Creditability[i_test])


> perf <- performance(pred, "tpr", "fpr")


> plot(perf)


> AUCLog1=performance(pred, measure = "auc")@y.values[[1]]


> cat("AUC: ",AUCLog1,"\n")

AUC:  0.7708557



#logistic regression on all variables

LogisticModel <- glm(Creditability ~ .,family=binomial, data = credit[i_calibration,]) 

> pred = prediction( fitLog, credit$Creditability[i_test])

> perf <- performance(pred, "tpr", "fpr")
> plot(perf)
> AUCLog2=performance(pred, measure = "auc")@y.values[[1]]
> cat("AUC: ",AUCLog2,"\n")
AUC:  0.7860663



#Naive Bayes

install.packages("e1071")
> library("e1071")
> model_naive<-naiveBayes(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status,data = credit[i_calibration,] ,laplace=1) 
> pred_naive<-predict(model_naive, newdata = credit[i_test,])
confusionMatrix(data=pred_naive, reference = credit[i_test,]$Creditability) 
pred_test_naive<-predict(model_naive, newdata = credit[i_test,], type="raw") 
>                 p_test_naive<-prediction(pred_test_naive[,2],
credit[i_test,]$Creditability)
> perf_naive<-performance(p_test_naive, "tpr", "fpr") > plot(perf_naive)

AUC:
performance(p_test_naive, "auc")@y.values
[1] 0.7779346



#Random forest

> RF <- randomForest(Creditability ~ ., data =
credit[i_calibration, ])
> fitForest <- predict(RF, newdata = credit[i_test, ], type =
'prob')[, 2]
> library(ROCR)
> pred4 <- prediction(fitForest, credit$Creditability[i_test])
> perf4 <- performance(pred4, 'tpr', 'fpr')
> plot(perf4)
> AUCRF <- performance(pred4, measure = 'auc')@y.values[[1]]
> AUCRF
[1] 0.7707167


