library('class')
library('smotefamily')
library('caret')
library('randomForest')
library('dplyr')
library('ROSE')

library('VIM')     #for.KNN.imputation 

#reading csv file -------
data<-read.csv('Churn_Modelling.csv')

#converting from int to factor ----------
data$Exited<-as.factor(data$Exited)
data$IsActiveMember<-as.factor(data$IsActiveMember)
data$HasCrCard<-as.factor(data$HasCrCard)



#converting 0 balance to NA ---------

data$Balance[data$Balance == 0 ] = NA
# data.numeric <- select_if(data, is.numeric)


# randomly reorder the data-------

set.seed(1234)                                        # setting a seed for sample function
new.data<-data[sample(1:nrow(data)),]

#------- mean imputation
new.mean.data<-new.data
new.mean.data$Balance[which(is.na(new.mean.data$Balance))]=mean(new.mean.data$Balance,na.rm=TRUE)


#KNN imputation-----
new.knn.data<-new.data
new.knn.imputed<-kNN(new.knn.data , variable= c('Balance') , k=7)
new.knn.imputed<-subset(new.knn.imputed , select = RowNumber : Exited)     








#train test-------
set.seed(12345)
sample.size<-sample(2,nrow(new.mean.data),replace=TRUE , prob = c(0.7,0.3))
train<- new.mean.data[sample.size==1,]
test<- new.mean.data[sample.size==2,]

#checking class imbalance--
summary(train$Exited)



#correcting class imbalance ---------
over<-ovun.sample(Exited~.,data=train , method = 'over' , N=11230)$data
under<-ovun.sample(Exited~.,data=train , method = 'under' , N=2786)$data
both<-ovun.sample(Exited~.,data=train , method = 'both' , 
                  p=0.5,  seed=123,  N=5615)$data


#model----------
rf.train <-randomForest(Exited~.,data=train)
rf.over <-randomForest(Exited~.,data=over)
rf.under <-randomForest(Exited~.,data=under)
rf.both <-randomForest(Exited~.,data=both)



#evaluation------
confusionMatrix(predict(rf.train,test),test$Exited ) #Accuracy : 0.857  
confusionMatrix(predict(rf.over,test),test$Exited )  #Accuracy : 0.8473
confusionMatrix(predict(rf.under,test),test$Exited )  #Accuracy : 0.7834 
confusionMatrix(predict(rf.both,test),test$Exited )   #Accuracy : 0.8326