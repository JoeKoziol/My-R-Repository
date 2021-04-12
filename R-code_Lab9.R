#############################################################################
#################### Read Data into R from local file #######################
setwd("/home/joseph_koziol/School/STAT_385")

chromoX<-read.table(file="ChromoXmicroarray.txt",header=TRUE)
BLClabels<-c(rep(1,27),rep(2,20))

############################################################################
#
# Perform t-test to select the most relevant genes for classification
# 
############################################################################

ttests<-function(x,group)
{
x<-as.numeric(x)
pvalue<-t.test(x ~ group)$p.value;
return(pvalue)
}
pvalues<-apply(chromoX[,c(3:49)],1,ttests,group=BLClabels)
smallpvals<-order(pvalues)[1:2]
features2<-chromoX[smallpvals,c(3:49)]

plot(t(features2),col=BLClabels,pch=BLClabels)
legend("topleft",c("non-BLC","BLC"),col=c(1,2),pch=c(1,2))

############################################################################
#
# Perform normalization on two feature variables
# 
############################################################################


normalization<-function(x)
{
norms<-(x-min(x))/(max(x)-min(x))
return(norms)
}
microdata2<-apply(features2,1,normalization)

microdata2<-data.frame(gene20475=microdata2[,1],gene20947=microdata2[,2])



#############################################################################
#
# Using nearest neighbor method to perform classification; Split the data

# into training and test data sets, and use 5-fold cross-validation method to 

# select the tuning parameter k (the number of nearest neighborhood)
# 
############################################################################



library(class)

nfold<-5

nsample<-ceiling(nrow(microdata2)/nfold)

FpFns<-matrix(0,10,nfold)

kvec<-c(1:10)

orderandom<-sample(1:nrow(microdata2), nrow(microdata2))

intfold<-nrow(microdata2)%/%nsample

remainder<-nrow(microdata2)%%nsample

for (j in 1:10)

{
 
confusmat<-matrix(0,2,2)
 
for (i in 1:intfold)
 
 {
 
 test<-orderandom[(i-1)*nsample+c(1:nsample)]
 
 train.set<-microdata2[-test,]
 
 test.set<-microdata2[test,]
 
 microdata2.pred<- knn(train=train.set, test=test.set, cl=BLClabels[-test], k=kvec[j])
 
 truth<-BLClabels[test]
 
 confusmatij<-table(truth, microdata2.pred)

 confusmat<-confusmat+confusmatij
 FpFns[j,i]<-(confusmat[1,2]+confusmat[2,1])/nfold
 }
 
 confusmat<-matrix(0,2,2)
 
 if (remainder!=0)
 {
 
 test<-orderandom[intfold*nsample+c(1:remainder)]
 
 train.set<-microdata2[-test,]
 
 test.set<-microdata2[test,]
 
 microdata2.pred<-knn(train=train.set, test=test.set, cl=BLClabels[-test], k=kvec[j])
 
 truth<-BLClabels[test]
 
 confusmatij<-table(truth, microdata2.pred)

 confusmat<-confusmat+confusmatij 
 FpFns[j,nfold]<-(confusmat[1,2]+confusmat[2,1])/nfold
 
 }

}

AveFpFns<-rowMeans(FpFns)

plot(kvec,AveFpFns,xlab="Value of K",ylab="Average of Total False Positives and False Negatives")


allmicrodata.pred<- knn(train=microdata2, test=microdata2, cl=BLClabels, k=3)

truth<-BLClabels
confusmat<-table(truth, allmicrodata.pred)



############################################################################
#
# Plot the separation plane
# 
############################################################################



px1<-seq(min(microdata2$gene20475),max(microdata2$gene20475),length=100)

px2<-seq(min(microdata2$gene20947),max(microdata2$gene20947),length=100)

par(mfrow=c(1,2))
xgrid<-expand.grid(gene20475=px1,gene20947=px2)

ygrid3<-knn(train=microdata2, test=xgrid, cl=BLClabels, k=3)

plot(xgrid, col = as.numeric(ygrid3)+1, pch = 20, cex = .2, main="k=3")

points(microdata2, col=as.numeric(BLClabels)+1, pch = 19)


ygrid8<-knn(train=microdata2, test=xgrid, cl=BLClabels, k=8)

plot(xgrid, col = as.numeric(ygrid8)+1, pch = 20, cex = .2, main="k=8")

points(microdata2, col=as.numeric(BLClabels)+1, pch = 19)



############################################################################
#
# Choosing K for K-means clustering
# 
############################################################################



par(mfrow=c(1,1))
kmeans.winss<-rep(0,10)
kval<-c(1:10)
for (k in kval)
{
kmeans.clusterk<-kmeans(microdata2,k)
kmeans.winss[k]<-kmeans.clusterk$tot.withinss
}
plot(kval,kmeans.winss,type="l")
points(kval,kmeans.winss)



############################################################################
#
# K-means clustering with k=2 and k=3
# 
############################################################################



par(mfrow=c(2,2))
kmeans.cluster2<-kmeans(microdata2,2)
plot(microdata2, col=kmeans.cluster2$cluster, main="K-means clustering")
points(kmeans.cluster2$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=BLClabels, main="true labels")


kmeans.cluster3<-kmeans(microdata2,3)
plot(microdata2, col=kmeans.cluster3$cluster, main="K-means clustering")
points(kmeans.cluster3$centers,cex=2,pch=20, col=c(1:3))
plot(microdata2, col=BLClabels, main="true labels")



############################################################################
#
# Hierarchical clustering using manhattan distance and centroid-linkage merging
# 
############################################################################



par(mfrow=c(1,1))

DistMat<-dist(microdata2,method="manhattan")

hclustCen<-hclust(DistMat,method="centroid")

plot(hclustCen)


cuttree2<-cutree(hclustCen,k=2)

plot(hclustCen)

rect.hclust(hclustCen,k=2,border="red")


cuttree3<-cutree(hclustCen,k=3)

plot(hclustCen)

rect.hclust(hclustCen,k=3,border="red")



############################################################################
#
# Hierarchical clustering versus K-means clustering
# 
############################################################################



par(mfrow=c(2,2))
plot(microdata2, col=kmeans.cluster2$cluster, main="K-means clustering (K=2)")
points(kmeans.cluster2$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=cuttree2, main="Hierarchical clustering (K=2)")

plot(microdata2, col=kmeans.cluster3$cluster, main="K-means clustering (K=3)")
points(kmeans.cluster3$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=cuttree3, main="Hierarchical clustering (K=3)")




