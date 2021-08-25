gfu<-read.csv("goodforu.csv", header = T, stringsAsFactors = T)  #update pathname in read.csv() where the file is located
dim(gfu)
str(gfu)
head(gfu)
names(gfu)
summary(gfu)

#Data Preparation and Exploration
library(dplyr)
gfu1<-select(gfu,X2,X9,X16,X23,X30)
summary(gfu1)
colnames(gfu1)<-c("fa.in","tran.fat","nat.oil","gud.bad","procsd")
gfu1%>%mutate(target=ifelse(gud.bad>5,1,0))->gfu2
gfu2%>%mutate(procsd1=ifelse(procsd<=5,"HeavilyProcessed","minimallyprocessed"))->gfu2
names(gfu2)
gfu2<-gfu2[,-c(4,5)]
summary(gfu2)

list<-names(gfu2)
list
list<-list[-4]
gfu2[,list]<-lapply(gfu2[,list],factor)
str(gfu2)
summary(gfu2)

#determining the overall brand perception in the customer's mind
#Q: Are my brands made with farm grown ingredients like potato, corn or wheat?

gfu2%>%group_by(fa.in)%>%summarise(Count=n(),Percent_Count=n()/24114)%>%ungroup()%>%data.frame()

#Answer: 80% customers believe that Brand A snacks are made of farm grown Ingredients.

#Q: Do my brands have zero grams trans-fat?

gfu2%>%group_by(tran.fat)%>%summarise(Count=n(),Percent_Count=n()/24114)%>%ungroup()%>%data.frame()

#Answer: 32% customers believe Brand A snacks have zero grams trans-fat.

#Q: Are my brands made with natural oils?

gfu2%>%group_by(nat.oil)%>%summarise(Count=n(),Percent_Count=n()/24114)%>%ungroup()%>%data.frame()

#Answer: 44% customers believe Brand A snacks are made with natural oils.



#Splitting into Test and Training Samples
set.seed(200)
index<-sample(nrow(gfu2),0.70*nrow(gfu2),replace=F)
train<-gfu2[index,]
test<-gfu2[-index,]

#Checking good Rate 
table(gfu2$target)/nrow(gfu2)
table(train$target)/nrow(train)
table(test$target)/nrow(test)

#All the 3 data sets have the same good/bad rate.


#Building Logistic Regression Model
mod1<-glm(data=gfu2,target~.,family="binomial")
summary(mod1)

#Equation For the model is log(p/1-p)=-1.08394 + (-0.37027)*X2 + (-0.41854)*X9 + (-0.41854)*X16 + 1.68645*X30
#The summary shows that all the viriables are highly significant with probablity values of much less
#than 0%. Also all the signs of the beta estimates are in line with the output, i.e. example for variable
#farm ingredient 2 meaning farm ingredients not present is inversely related to snack being good. 
#So we finalise this model.

#Model Diagnostics
#Predicted Values
pred<-predict(mod1,type="response",newdata=test)
head(pred)

#Checking the rate of 1, according to that the cut-off probablity value will be set.
table(gfu2$target)/nrow(gfu2)

#So the cut off is 0.2539189. 
#Applying cutoff value to predicted valued to obtain class labels for all predicted values
pred1<-ifelse(pred>=0.2539189,1,0)

#Kappa Matrix
library(irr)
kappa2(data.frame(test$target,pred1))
#In terms of classification performance, the kappa matrix is 0.3912.

#Confusion Matrix
library(caret)
confusionMatrix(pred1,test$target,positive = "1")

#The confusion MAtrix shows Accuracy : 0.761, with 1074 correct events and 4432 correct non-events,
# 946 incorrect events and 783 incorrect non-events


#The model can be further optimized by chossing different cutoffs. 
#The cutoff value with Maximum Kappa matrix should be chosen for maximum accuracy.
s<-seq(0.25,1,0.001)
n<-1 
a<-as.vector(length(s))
for (i in s ) {
  
  print(i)
  pred2<-ifelse(pred>i,1,0)
  a[n]<-confusionMatrix(pred2,test$target,positive = "1")$overall[2]
  print(n)
  n=n+1
}

#Now 'a' has all the different kappa matrix's for different cutoff stored in it.
#Extracting the cutoff with max kappa.
index<-which(a==max(a))

#So the cutoff value w.r.t the max kappa is 's[index]'
s[index]

#Proceeding further with the model with cutoff value of 0.39.

pred3<-ifelse(pred>0.337,1,0)
confusionMatrix(pred3,test$target,positive = "1")
#Now the Accuracy : 0.761 and Kappa : 0.3912 for the optimised model.


#ROCR Curve
library(ROCR)
pred4<-prediction(pred,test$target)
pref<-performance(pred4,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred4,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

#The auc is 0.7535689 which is more than 0.50. 
#So the auc value explains the model is very good.

#Gains Chart
library(gains)
str(test$target)
summary(test$target)
test$target<-as.numeric(test$target)
head(test$target)
gains(test$target,predict(mod1,type="response",newdata=test),groups = 5)

#the Gains Chart shows that the top 40% of the probabilities contain 70% customers
#who believe the Brand A snack is good for them.

test$prob<-predict(mod1,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 30% of the probability scores lie between 0.25276207  to 0.64623214 
#We can use this probablity to extract the data of customers who think the Brand A snacks are good.

#Q: Is there an impact due to Processing Level?

gfu2%>%group_by(procsd1)%>%summarise(Count=n(),Percent_Count=n()/24114)%>%ungroup()%>%data.frame()
gfu2%>%group_by(target)%>%summarise(Count=n(),Percent_Count=n()/24114)%>%ungroup()%>%data.frame()

with(gfu2,table(target,procsd1)/nrow(gfu2))

# Answer: 62% customers believe Brand A snacks are heavily processed and is bad for them. 
#  14% customers believe that the snack minimally processed and is good for them. 
#  12% customers believe Brand A snacks are minimally processed and are still bad for them.
#  and 11% customers believe Brand A snacks are heavily processed and are still good for them.


library(lm.beta)
?lm.beta
lm.beta(mod1)

#The output of standardized Coefficients show that with 1 Standard Deviation change in variable
#Processing Level -> minimally processed, there is atleast 3 times more impact on the customer's 
#opinion about the brand.
