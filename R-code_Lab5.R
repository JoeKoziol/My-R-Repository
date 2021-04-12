

## Q1: Performing the linear classification using logistic regression 
setwd("/home/joseph_koziol/School/STAT_385")
hemodata <- read.table(file="Hemophilia-data.txt")
colnames(hemodata) <-c ("group", "AHF activity", "AHF-like antigen")
normalgroup <- hemodata[hemodata[,1]==1,]
carriergroup <- hemodata[hemodata[,1]==2,]

train <- sample(1:dim(hemodata)[1],60)
logitmod <- glm((group-1)~.,hemodata, subset=train, family="binomial")
summary(logitmod)
projections <- predict(logitmod, hemodata)

## Q2: Separation line with wrong labeled marked

classlabel <- -1*(hemodata$group==1)+(hemodata$group==2)
distance <- classlabel*projections

plot(normalgroup[,2],normalgroup[,3],xlim=c(-0.7,0.2),ylim=c(-0.4,0.4),xlab="log(AHF activity)", ylab="log(AHF-like antigen)",col=2)
points(carriergroup[,2],carriergroup[,3],pch=3, col=4)
abline(a=-coef(logitmod)[1]/coef(logitmod)[3],b=-coef(logitmod)[2]/coef(logitmod)[3])
legend("topleft", legend=c("normal","carrier"),col=c(2,4), pch=c(1,3))

wrongs<-which(distance<0)
text(hemodata[wrongs,3]~hemodata[wrongs,2],labels=rownames(hemodata[wrongs,]))
text(x=-0.6,y=0.2,labels="y=+1",col=4)
text(x=0,y=-0.3,labels="y=-1",col=2)

wrongset<-cbind(classlabel[wrongs],hemodata[wrongs,-1],projections[wrongs],distance[wrongs])
colnames(wrongset)<-c("classlabel","AHF activity","AHF-like antigen","signed distance","misclassified distance")

wrongsetDistance<-distance[wrongs]



## Q3: Remove the data points that are close to separation line and on the wrong side of separation line

## by calculating the signed y*(beta_1*x+beta_0). Remove the missclassified ones and those close to separation plane

removes<-which(distance<1)
modified.hemodata<-hemodata[-removes,]
modified.normal<-modified.hemodata[modified.hemodata==1,]
modified.carrier<-modified.hemodata[modified.hemodata==2,]

plot(modified.normal[,2],modified.normal[,3],xlim=c(-0.7,0.2),ylim=c(-0.4,0.4),xlab="log(AHF activity)", ylab="log(AHF-like antigen)",col=2)
points(modified.carrier[,2],modified.carrier[,3],pch=3, col=4)
legend("topleft", legend=c("normal","carrier"),col=c(2,4), pch=c(1,3))



## Q4: Use support vector machine package to find out the maximal margin, support vector and separation line

library("e1071")
hemodata2<-data.frame(group=as.factor(modified.hemodata$group),modified.hemodata[,2:3])
svmmodel<-svm(group~., data=hemodata2, kernel="linear",scale=F, method = "C-classification",cost=1000)
print(svmmodel)
plot(svmmodel,hemodata2)

## Draw the separation line and find the support vectors

beta<-drop(t(svmmodel$coefs)%*%as.matrix(hemodata2[svmmodel$index,c(2:3)]))
beta0<-svmmodel$rho
plot(modified.normal[,2],modified.normal[,3],xlim=c(-0.7,0.2),ylim=c(-0.4,0.4),xlab="log(AHF activity)", ylab="log(AHF-like antigen)",col=2)

points(modified.carrier[,2],modified.carrier[,3],pch=3, col=4)
abline(a=beta0/beta[2], b=-beta[1]/beta[2],col=3)

abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty=3,col=3)

abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty=3,col=3)

text(hemodata2[svmmodel$index,3]~hemodata2[svmmodel$index,2],labels=rownames(hemodata2[svmmodel$index,]))
legend("topleft", legend=c("normal","carrier"),col=c(2,4), pch=c(1,3))



## Find the maximal margin and check if support vectors on the margin lines

## 
MaxMargin<-abs(2/beta[2])/sqrt(1+(beta[1]/beta[2])^2)

MaxMargin<-2/sqrt(beta[1]^2+beta[2]^2)
SupportVec<-hemodata2[svmmodel$index,]

SupportVec[,3]

(beta0 - 1)/beta[2]-(beta[1]/beta[2])*SupportVec[,2]

(beta0 + 1)/beta[2]-(beta[1]/beta[2])*SupportVec[,2]



## Q5: Plot the separation line of logistic regression together with that from SVM


logitmod2<-glm((group-1)~.,modified.hemodata,family="binomial")
summary(logitmod2)

abline(a=-coef(logitmod2)[1]/coef(logitmod2)[3],b=-coef(logitmod2)[2]/coef(logitmod2)[3],lty=4, col=5)




## Q6: Compare the performance of logistic regression and svm for classification



logitErr<-matrix(0,100,2)

svmErr<-matrix(0,100,2)

for (rep in 1:100)
{
 
train<-sample(1:dim(hemodata)[1],60)
 
logitmod3<-glm((group-1)~.,hemodata,subset=train,family="binomial")
logitpredvals<-predict(logitmod3,hemodata[-train,])
 
logitpredgroups<-(sign(logitpredvals)+3)/2
truth<-hemodata[-train,1]
 
logitpredgroups==truth
 
logitresults<-table(truth,logitpredgroups)
 
logitErr[rep,]<-c(logitresults[1,2],logitresults[2,1])
 
Trainhemodata<-data.frame(hemodata[train,2:3],group=as.factor(hemodata[train,]$group))
 
Testhemodata<-data.frame(hemodata[-train,2:3],group=as.factor(hemodata[-train,]$group))
 
svmmodel3<-svm(group~., data=Trainhemodata, kernel="linear",scale=F, method="C-classification",cost=1000)
 
svmpredval<-predict(svmmodel3,Testhemodata)
 
svmresults<-table(truth,svmpredval)
 
svmErr[rep,]<-c(svmresults[1,2],svmresults[2,1])
}

cbind(logitErr,svmErr)

colMeans(logitErr)

colMeans(svmErr)



