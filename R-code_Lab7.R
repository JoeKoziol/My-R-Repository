##################################################
# Mushroom Data Set
##################################################


setwd("/home/joseph_koziol/School/STAT_385")
MushroomData <-read.csv(unz(description="Mushroom.zip", filename="Mushroom/Mushroom Categorico.csv"))

##################################################
#
# Box plots for each value of each feature variable
#
##################################################


#create a matrix to place graphs in (5 rows, 2 columns)
par(mfrow=c(5,2),mar=c(2,2,2,2))

#create a table measuring the amount of poisonous vs nonpoisonous mushrooms by cap shape
boxplotinfo <- table(MushroomData$classes, MushroomData[,2])
#use the table to create a barplot (this should fill the matrix)
barplot(boxplotinfo, main=names(MushroomData)[2], col=c("darkblue","red"),
  legend = rownames(boxplotinfo), beside=TRUE, ylim=c(0,max(boxplotinfo)))


# These for loops will iterively create barplots for each feature variable (we use two separate matricies)
for (i in 3:11)
{
boxplotinfo <- table(MushroomData$classes, MushroomData[,i])
barplot(boxplotinfo, main=names(MushroomData)[i], col=c("darkblue","red"),
  beside=TRUE, ylim=c(0,max(boxplotinfo)))
}

par(mfrow=c(6,2),mar=c(2,2,2,2))
boxplotinfo <- table(MushroomData$classes, MushroomData[,12])
barplot(boxplotinfo, main=names(MushroomData)[12], col=c("darkblue","red"),
  legend = rownames(boxplotinfo), beside=TRUE, ylim=c(0,max(boxplotinfo)))
for (i in 13:23)
{
boxplotinfo<-table(MushroomData$classes, MushroomData[,i])
barplot(boxplotinfo, main=names(MushroomData)[i], col=c("darkblue","red"),
  beside=TRUE, ylim=c(0,max(boxplotinfo)))
}

##################################################
#
# Entropy used for information gain computation
#
##################################################


## Define function to compute entropy
Entropy4feature <- function(feature, classlabel, unit)
{
  Classtable <- table(feature,classlabel)
  props <- rowSums(Classtable)/sum(Classtable)
  probs <- Classtable/rowSums(Classtable)
  entropyele <- probs^(-probs)
  entropyprod <- apply(entropyele,1,prod)
  logentropy <- log(entropyprod,base=unit)
  entropy <- sum(props*logentropy)
  return(entropy)
}

#################################################################################################################################################

# Entropy4feature <- function(feature, classlabel, unit)
# {
# Classtable <- table(MushroomData$odor, MushroomData$classes)
# props <- rowSums(Classtable)/sum(Classtable)
# probs <- Classtable/rowSums(Classtable)
# entropyele <- probs^(-probs)
# entropyprod <- apply(entropyele,1,prod)
# logentropy <- log(entropyprod,base=exp(1))
# entropy <- sum(props*logentropy)
# return(entropy)
# }
# 
# classtable2 <- table(MushroomData$odor, MushroomData$classes)
# proportions <- rowSums(classtable2)/sum(classtable2)
# probabilities <- classtable2/rowSums(classtable2)

################################################################################################################################################

nlen <- dim(MushroomData)[1]
probs0 <- table(MushroomData$classes)/nlen
entropyparent <- sum(-probs0*log(probs0,base=exp(1)))

## Information gain by odor (calculated by formula, use log2 base)
odorEntropy <- Entropy4feature(MushroomData$odor,MushroomData$classes, exp(1))
odorIG <- entropyparent-odorEntropy

## Information gain by Gill-color (calculated by formula, use log2 base)
gillcolorEntropy <- Entropy4feature(MushroomData$gill_color,MushroomData$classes,2)
gillcolorIG <- entropyparent-gillcolorEntropy

## Information gain by Spore-print-color (calculated by formula, use log2 base)
SporeprintcolorEntropy <- Entropy4feature(MushroomData$spore_print_color,MushroomData$classes, 2)
SporeprintcolorIG <- entropyparent-SporeprintcolorEntropy

## Compute information gain for all the features
AllIG <- matrix(0,22,1)
for (i in 2:23)
{
ChildEntropy <- Entropy4feature(MushroomData[,i],MushroomData$classes,unit=2)
IG0 <- entropyparent-ChildEntropy
AllIG[i-1,1] <- IG0
}
rownames(AllIG) <- colnames(MushroomData)[2:23]
orderedIG <- AllIG[order(AllIG,decreasing=TRUE),]

## Entropy in natural log base

nlen <- dim(MushroomData)[1]
probs0 <- table(MushroomData$classes)/nlen
entropyparent <- sum(-probs0*log(probs0,base=exp(1)))
AllIG <- matrix(0,22,1)
for (i in 2:23)
{
ChildEntropy <- Entropy4feature(MushroomData[,i],MushroomData$classes,unit=exp(1))
IG0 <- 1-ChildEntropy/entropyparent
AllIG[i-1,1] <- IG0
}
rownames(AllIG) <- colnames(MushroomData)[2:23]


##################################################
#
# Gini coefficient for information gain 
#
##################################################

Gini4feature <- function(feature, classlabel)
{
Classtable <- table(feature,classlabel)
props <- rowSums(Classtable)/sum(Classtable)
probs <- Classtable/rowSums(Classtable)
giniele <- probs*(1-probs)
ginisum <- apply(giniele,1,sum)
gini <- sum(props*ginisum)
return(gini)
}

nlen <- dim(MushroomData)[1]
probs0 <- table(MushroomData$classes)/nlen
giniparent <- sum(probs0*(1-probs0))

GiniIG <- matrix(0,22,1)
for (i in 2:23)
{
ChildGini <- Gini4feature(MushroomData[,i],MushroomData$classes)
IG0 <- giniparent-ChildGini
GiniIG[i-1,1] <- IG0
}
rownames(GiniIG) <- colnames(MushroomData)[2:23]

orderedGiniIG <- GiniIG[order(GiniIG,decreasing=TRUE),]

#################################################
## Fit classification tree and Improve (Based on entropy and Gini) in rpart function in R

#################################################

library(rpart)

library(rpart.plot)

treefit <- rpart(classes~., data=MushroomData, parms=list(split = "information"),control = rpart.control(minsplit=0,maxcompete=14,maxsurrogate=15))
treefit$split
prp(treefit,type=2,extra=1)



## Try to reproduce the entropy improve by ODOR in R


newodor <- 1*(MushroomData$odor%in%c("almond","anise","none"))+0

nlen <- dim(MushroomData)[1]
probs0 <- table(MushroomData$classes)/nlen
entropyparent <- sum(-probs0*log(probs0,base=exp(1)))
newodorEntropy <- Entropy4feature(newodor,MushroomData$classes, exp(1))
newodorIG <- entropyparent-newodorEntropy

treefit2 <- rpart(classes~., data=MushroomData, control = rpart.control(minsplit=0,maxcompete=14,maxsurrogate=15))
treefit2$split
prp(treefit2,type=2,extra=1)


