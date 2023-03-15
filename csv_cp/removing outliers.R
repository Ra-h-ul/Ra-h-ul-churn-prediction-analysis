#removing outliers --------
df3=subset(new.mean.data,new.mean.data$CreditScore>405 & Age<65)

set.seed(12345)
sample.size<-sample(2,nrow(df3),replace=TRUE , prob = c(0.7,0.3))
trainx<- df3[sample.size==1,]
testx<- df3[sample.size==2,]

overx<-ovun.sample(Exited~.,data=trainx , method = 'over' , N=10862)$data
underx<-ovun.sample(Exited~.,data=trainx , method = 'under' , N=2730)$data
bothx<-ovun.sample(Exited~.,data=trainx , method = 'both' , 
                   p=0.5,  seed=123,  N=5431)$data



rf.trainx <-randomForest(Exited~.,data=trainx)
rf.overx <-randomForest(Exited~.,data=overx)
rf.underx <-randomForest(Exited~.,data=underx)
rf.bothx <-randomForest(Exited~.,data=bothx)


confusionMatrix(predict(rf.trainx,testx),testx$Exited ) #Accuracy : 0.8575  
confusionMatrix(predict(rf.overx,testx),testx$Exited )  #Accuracy : 0.8462
confusionMatrix(predict(rf.underx,testx),testx$Exited )  #Accuracy : 0.7654 
confusionMatrix(predict(rf.bothx,testx),testx$Exited )   #Accuracy : 0.8224
