## Set working directory
setwd("/home/joseph_koziol/School/STAT_385")

## Read data sets
features=scan(file="features.txt", what="character")

## Reform data sets
len_features=length(features)
nlen=len_features/2
featureframe=data.frame(feature=features[2*(1:nlen)-1],type=features[2*(1:nlen)])

## Extract the first feature
featureframe[1,1]
feature1vec=strsplit(as.character(featureframe[1,1]),split="_")
feature1=feature1vec[[1]][3]
feature1F=strsplit(feature1,split=":")[[1]][1]

## Extract the 55th feature
featureframe[55,1]
feature55vec=strsplit(as.character(featureframe[55,1]),split="_")
feature55=feature55vec[[1]][4]
feature55F=strsplit(feature55,split=":")[[1]][1]

## Extract all the features
allfeatures=matrix(0,nlen,1)
for (i in 1:(nlen-3))
{
 feature1vec=strsplit(as.character(featureframe[i,1]),split="_")
 feature1=feature1vec[[1]][3]
 feature1F=strsplit(feature1,split=":")[[1]][1]
 allfeatures[i]=feature1F
}
for (i in (nlen-2):nlen)
{
 feature55vec=strsplit(as.character(featureframe[i,1]),split="_")
 feature55=feature55vec[[1]][4]
 feature55F=strsplit(feature55,split=":")[[1]][1]
 allfeatures[i]=feature55F
}

## Spam data set: combining feature names with the relative frequencies; Organize them into an appropriate format

Spamdata=read.table(file="spam_data.txt")
dim(Spamdata)
columnames=c(allfeatures,"SpamOrNot")
colnames(Spamdata)=columnames

## Delete the 55th-57th feature variables and create a new data set for Lab 3

newSpamdata=Spamdata[,-c(55:57)]
nnlen=dim(newSpamdata)[2]

## Find the averages frequencies of words or characters in spam and good emails
Spamemails=newSpamdata[newSpamdata$SpamOrNot==1,]
NonSpams=newSpamdata[newSpamdata$SpamOrNot==0,]
SpamAves=colMeans(Spamemails)
NonSpamAves=colMeans(NonSpams)
AbsDifferences=abs(SpamAves-NonSpamAves)
SortbyAbsDifferences=sort(AbsDifferences[-(nnlen)],decreasing=TRUE)
indexafterorder=order(AbsDifferences[-(nnlen)],decreasing=TRUE)
summaryinfo=rbind((SpamAves[-(nnlen)])[indexafterorder],(NonSpamAves[-(nlen)])[indexafterorder],SortbyAbsDifferences)
rownames(summaryinfo)=c("spam","email","abs diff")

## Bar plot for the average frequencies of words or characters in spam or non-spam

barplotinfo1=summaryinfo[1:2,1:27]
barplotinfo2=summaryinfo[1:2,28:54]
par(mfrow=c(2,1))
barplot(barplotinfo1, main="Average frequencies of features by spam and non-spam",
  xlab="Feature words or characters", col=c("darkblue","red"),
  legend = rownames(barplotinfo1), beside=TRUE, ylim=c(0,2.3))
barplot(barplotinfo2, xlab="Feature words or characters", col=c("darkblue","red"),
  legend = rownames(barplotinfo2), beside=TRUE, ylim=c(0,2.3))

## Extract the top four features ranked by the average frequencies

georgehp=(newSpamdata$george + newSpamdata$hp)/2
youyour=(newSpamdata$you+newSpamdata$your)/2
newSpamdatasub=cbind(georgehp,youyour,newSpamdata$SpamOrNot)
par(mfrow=c(1,1))
y=newSpamdata$SpamOrNot
plot(newSpamdatasub[,1:2], col=y+1, pch=y+2, xlab="georgehp", ylab="youyour")

## Fit a linear SVM to data and find the seperation line

newSpamdatasub0<-data.frame(group=as.factor(newSpamdata$SpamOrNot),X1=newSpamdatasub[,1], X2=newSpamdatasub[,2])

library("e1071")
svmmodel=svm(group~., data=newSpamdatasub0, kernel="linear",scale=F,cost=100)
print(svmmodel)

beta=drop(t(svmmodel$coefs)%*%as.matrix(newSpamdatasub0[svmmodel$index,c(2:3)]))

beta0=svmmodel$rho

plot(newSpamdatasub0[,2:3], col=y+1, pch=y+1, xlab="georgehp", ylab="youyour")
abline(a=beta0/beta[2], b=-beta[1]/beta[2],col=3)

abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 3,col=3)

abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 3,col=3)
legend("topright", legend=c("Non-spam","Spam"),col=c(1,2), pch=c(1,2))



## Evaluating the performance using training and testing data sets

train=sample(1:dim(newSpamdatasub0)[1],4000)

svmmodeltrain=svm(group~., data=newSpamdatasub0, subset=train, kernel="linear",scale=F,cost=100)
predvals=predict(svmmodeltrain, newdata=newSpamdatasub0[-train,], decision.values=T) 

truth=newSpamdatasub0[-train,1]

table(truth,predvals)

## Using a nonlinear SVM with radial basis function

nonlinearsvmfit=svm(group~., data=newSpamdatasub0, scale = FALSE, subset=train, kernel = "radial", cost=100)
nonlinearpreds=predict(nonlinearsvmfit, newdata=newSpamdatasub0[-train,], decision.values=T) 

table(truth,nonlinearpreds)

## Draw the separation plane for the non-linear classification

x1range=range(newSpamdatasub0$X1)
x2range=range(newSpamdatasub0$X2)
px1=seq(x1range[1],x1range[2],length=50)
px2=seq(x2range[1],x2range[2],length=50)
xgrid=expand.grid(X1=px1, X2=px2)
newygrid=predict(nonlinearsvmfit, xgrid)
predfunc=predict(nonlinearsvmfit, newdata=xgrid, decision.values = T)
predfunc=attributes(predfunc)$decision
plot(xgrid, col = as.numeric(newygrid), pch = 20, cex = .2)
points(newSpamdatasub0[,2:3], col=y+1, pch=19, xlab="georgehp", ylab="youyour")
contour(px1, px2, matrix(predfunc, length(px1), length(px2)), level = 0, add = TRUE)

## Using a nonlinear SVM with polynomial kernel function

nonlinearsvmfit1=svm(group~., data=newSpamdatasub0, kernel = "polynomial", degree=5, cost=1000)
tobj<-tune.svm(group~., data=newSpamdatasub0, cost=c(1:2)*100, degree=c(2:3), coef0=c(-2:2), gamma=1/c(2:5))


nonlinearpreds1=predict(nonlinearsvmfit1, newdata=newSpamdatasub0[-train,], decision.values=T) 


table(truth,nonlinearpreds1)

## Draw the separation plane for the non-linear classification

x1range=range(newSpamdatasub0$X1)
x2range=range(newSpamdatasub0$X2)
px1=seq(x1range[1],x1range[2],length=50)
px2=seq(x2range[1],x2range[2],length=50)
xgrid=expand.grid(X1=px1, X2=px2)
newygrid=predict(nonlinearsvmfit1, xgrid)
predfunc=predict(nonlinearsvmfit1, newdata=xgrid, decision.values = T)
predfunc=attributes(predfunc)$decision
plot(xgrid, col = as.numeric(newygrid), pch = 20, cex = .2)
points(newSpamdatasub0[,2:3], col=y+1, pch=19, xlab="georgehp", ylab="youyour")
contour(px1, px2, matrix(predfunc, length(px1), length(px2)), level = 0, add = TRUE)











