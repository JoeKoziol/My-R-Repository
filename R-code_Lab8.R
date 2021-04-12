## Set working directory
setwd("/home/joseph_koziol/School/STAT_385")

## Define a function that can compute entropy gain for given feature variable, class label and threshold value

EntropyIG <- function(threshold, feature, classlabel, unit=exp(1))
{
nlen <- length(classlabel)
probs0 <- table(classlabel)/nlen
entropyparent <- sum(-probs0*log(probs0,base=unit))
featurecat <- (feature<threshold)*1+0
Classtable <- table(featurecat,classlabel)
props <- rowSums(Classtable)/sum(Classtable)
probs <- Classtable/rowSums(Classtable)
entropyele <- probs^(-probs)
entropyprod <- apply(entropyele,1,prod)
logentropy <- log(entropyprod,base=unit)
entropychild <- sum(props*logentropy)
IG <- entropyparent-entropychild
return(IG)
}

## Read the data set created in Lab 5

newSpamdatasub <- read.csv(file="spam_new_features.csv",header=TRUE)
newSpamdatasub0 <- newSpamdatasub[,-1]

## Q1: Compute information gain for both features "georegehp" and "youyour" at a sequence of threholds

th1seq <- matrix(seq(0,max(newSpamdatasub0$georgehp),length=500),500,1)
IGgeorgehp <- apply(th1seq,1,EntropyIG,feature=newSpamdatasub0$georgehp,classlabel=newSpamdatasub0$SpamOrNot)
th1IGs <- cbind(th1seq,IGgeorgehp)
plot(th1seq,IGgeorgehp,type="l",xlab="Threshold for feature georgehp",ylab="Entropy Information Gain")

th2seq <- matrix(seq(0,max(newSpamdatasub0$youyour),length=500),500,1)
IGyouyour <- apply(th2seq,1,EntropyIG,feature=newSpamdatasub0$youyour,classlabel=newSpamdatasub0$SpamOrNot)
th2IGs <- cbind(th2seq,IGgeorgehp)
plot(th2seq,IGyouyour,type="l",xlab="Threshold for feature youyour",ylab="Entropy Information Gain")

max(IGyouyour)
max(IGgeorgehp) 

## Q2: Apply the classification tree to the data set newSpamdatasub0
library(rpart)

library(rpart.plot)

treefit <- rpart(SpamOrNot~., data=newSpamdatasub0, method="class", parms=list(split = "information"),control = rpart.control(minsplit=10))
treefit$split
prp(treefit,type=2,extra=1)

## Q3: Partition the feature space according to the above classification tree

th1seq <- matrix(seq(0,2,length=100),100,1)
th2seq <- matrix(seq(0,4,length=100),100,1)
xgrid <- expand.grid(georgehp=th1seq,youyour=th2seq)
predtions <- predict(treefit,newdata=xgrid,type="prob")[,2]
ygrid <- (predtions>0.5)*1+0
plot(xgrid, col=as.numeric(ygrid)+2, pch = 20, cex = .2,xlim=c(0,2),ylim=c(0,4))
points(newSpamdatasub0$georgehp,newSpamdatasub0$youyour,pch=20, cex=1.2,col=newSpamdatasub0$SpamOrNot+2)
contour(th1seq,th2seq,matrix(predtions,100,100),levels = .5,add=TRUE)

## Q4: Fit the classification tree by present minsplit=10 and cost-complexity parameter=0.03

treefit1 <- rpart(as.factor(SpamOrNot)~., data=newSpamdatasub0, method="class", parms=list(split = "information"), control = rpart.control(minsplit=10, cp=0.03))
printcp(treefit1)
prp(treefit1,type=2,extra=1)
plotcp(treefit1)

## Q6: Post-pruning of the classification tree

treefit2 <- rpart(as.factor(SpamOrNot)~ ., data=newSpamdatasub0, control=rpart.control(minsplit = 2, minbucket = 1, cp=0))
bestcp <- treefit2$cptable[which.min(treefit2$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(treefit2, cp = bestcp)
prp(tree.pruned,type=2,extra=1)
predvals <- predict(treefit2,newdata=newSpamdatasub0, type="class")
truth <- newSpamdatasub0$SpamOrNot
table(truth,predvals)

## Q7: Adjust cost function in classification trees
lmat <- matrix(c(0,1,10,0), ncol = 2)
lmat

treefit2.cost <- rpart(as.factor(SpamOrNot)~ ., data=newSpamdatasub0, parms = list(loss = lmat),control=rpart.control(minsplit = 2, minbucket = 1, cp=0))
bestcp2 <- treefit2.cost$cptable[which.min(treefit2.cost$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(treefit2.cost, cp = bestcp2)

predvals2 <- predict(tree.pruned,newdata=newSpamdatasub0, type="class")
truth <- newSpamdatasub0$SpamOrNot
table(truth,predvals2)
prp(treefit2.cost,type=2,extra=1)









