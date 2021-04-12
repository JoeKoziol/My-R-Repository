## Set working directory, please set the working directory as the folder where you saved
## the ZipCode images data sets

setwd("/home/joseph_koziol/School/STAT_385")

## Q1: 
X<-rnorm(100, 0, 2) ## Generating a N(0,4) distributed random vector of length 100
meanX<-mean(X) ## Compute sample mean
varX<-var(X)  ## Compute sample variance
hist(X,freq=FALSE) ## Create a histogram for X using probability density
dnorm2<-function(x) ## Define the density function of N(0,4)
{return(dnorm(x,0,2))}
curve(dnorm2,add=TRUE,col=2) ## Add the density to the histogram
prob1<-mean(X>1) ## Compute the empirical probability, namely the percentage of elements in X that is 
                 ## greater than 1.

## Q2: Create a new vector that contains all positive numbers from the vector X 
positiveX<-X[X>0]
 
## Q3: Approximate the expectation of NB(r,p)
rvec<-c(10,20,30)
pvec<-c(0.2,0.5)
N<-1000
expX<-matrix(0,3,2) ## A matrix contains all approximations of expectations with different combinations of r and p
for (i in 1:3)
 for (j in 1:2)
  for (k in rvec[i]:N)
  {
   pk<-choose(k-1,rvec[i]-1)*(pvec[j]^(rvec[i]))*((1-pvec[j])^(k-rvec[i]))	
   expX[i,j]<-expX[i,j]+k*pk
  } 
expX

## Q4:Visualize all the probability mass functions in Q3

rvec<-c(10, 20, 30)
pvec<-c(0.2, 0.5)
N<-200
xvec<-c(rvec[1]:N)
pkvec<-choose(xvec-1,rvec[1]-1)*(pvec[1]^rvec[1])*((1-pvec[1])^(xvec-rvec[1]))
plot(xvec,pkvec,type="n",xlab="x",ylab="probability mass of NB(r,p)",ylim=c(0,0.1))
legendtext<-NULL
colvec<-NULL
ltyvec<-NULL
for (i in 1:3)
 for (j in 1:2)
 {
  xvec<-c(rvec[i]:N)
  pkvec<-choose(xvec-1,rvec[i]-1)*(pvec[j]^rvec[i])*((1-pvec[j])^(xvec-rvec[i]))
  lines(xvec,pkvec,lty=j, col=i)
  legendtext<-c(legendtext,paste("r=",rvec[i],",p=",pvec[j],sep=""))
  colvec<-c(colvec,i)
  ltyvec<-c(ltyvec,j)
 }
legend("topright",legendtext,col=colvec,lty=ltyvec)

## Q5: Rotate the matrix A clockwise 90 degree: Reverse the matrix column by column and do a transpose
## When image function plot A, it will rotate A counter-clockwise 90 degree.
## In gray function: 0 means black and 1 means white

A<-matrix(c(1,1,-1,-1,-1,1,1,1,-1),3,3)
image(A,col=gray((0:32)/32)) ## Wrong usage
imA<-t(apply(A,2,rev)) ## Turn the matrix clockwise 90 degree
image(-imA,col=gray((0:32)/32)) ## The minus sign is to match the definition of colors in gray function

## Q6-Q9: Hand-written digit recognition data
## Q6: Read data sets
zipTrain=read.csv("zip.train.gz", header = F, sep=" ")
dim(zipTrain)
head(zipTrain)
zipTrain=zipTrain[,1:257]
zipTest=read.csv("zip.test.gz", header = F, sep=" ")
dim(zipTest)

## Q7: Create a frequency table

table(zipTrain[,1])

## Q8: Classify training data sets according to the numbers

number0=zipTrain[zipTrain[,1]==0,]
number1=zipTrain[zipTrain[,1]==1,]
number2=zipTrain[zipTrain[,1]==2,]
number3=zipTrain[zipTrain[,1]==3,]
number4=zipTrain[zipTrain[,1]==4,]
number5=zipTrain[zipTrain[,1]==5,]
number6=zipTrain[zipTrain[,1]==6,]
number7=zipTrain[zipTrain[,1]==7,]
number8=zipTrain[zipTrain[,1]==8,]
number9=zipTrain[zipTrain[,1]==9,]

## Q9: Visualize the image of a handwritten number

par(mfrow=c(1,2))
im=matrix(as.numeric(zipTrain[4,2:257]), nrow = 16, ncol = 16)
image(t(apply(-im,1,rev)),col=gray((0:32)/32)) # plot column by column using values row by row
newim=matrix(as.numeric(zipTrain[4,2:257]), nrow = 16, ncol = 16, byrow=TRUE)
image(t(apply(-newim,2,rev)),col=gray((0:32)/32)) # This gives the same image as above

## Visualize some handwritten numbers

par(mfrow=c(5,10))
par(mar=rep(1,4))
for (i in 1:5)
{
 for (j in 0:9)
 {
 numberj=zipTrain[zipTrain[,1]==j,]
 im=matrix(as.numeric(numberj[i,2:257]), nrow = 16, ncol = 16)
 image(t(apply(-im,1,rev)),col=gray((0:32)/32),axes = FALSE)
 }
}