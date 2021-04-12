setwd("/home/joseph_koziol/School/STAT_385")

## Load the data

load(file="Alldata.Rdata")


## Find the average of the gene expressions of gene with IDENTIFIER="RFC2" as responses

subsetGenes<-sapply(Alldata$originaldata[Alldata$originaldata[,2]=="RFC2",3:110], as.numeric)
averageRFC2<-colMeans(subsetGenes)

## Create predictor variables: Beverage and Hours 

samIDs<-names(averageRFC2)
Beverages<-(samIDs%in%Alldata$trt1)*1+(samIDs%in%Alldata$trt2)*2+(samIDs%in%Alldata$trt3)*3+(samIDs%in%Alldata$trt4)*4
hours<-(samIDs%in%Alldata$time_h0)*0+(samIDs%in%Alldata$time_h1)*1+(samIDs%in%Alldata$time_h2)*2+(samIDs%in%Alldata$time_h4)*4+(samIDs%in%Alldata$time_h12)*12

## Fit a linear model with Beverage as the predictor

BeverFac<-as.factor(Beverages)
resp<-averageRFC2
lm2<-lm(resp~BeverFac)
summary(lm2)

## Find the different effect between alcohol and water, and if there is a significant difference

AlcoWater<-summary(lm2)$coefficients[4,1]
Zstat<-summary(lm2)$coefficients[4,3]
pval<-summary(lm2)$coefficients[4,4]

## Plot the curves: average gene expression versus hours by beverages

colvec<-NULL
pchvec<-NULL
ltyvec<-NULL
plot(hours,resp,xlab="Hours",ylab="gene expression average",type="n")
for (bever in 1:4)
{
 BeverHourAve<-tapply(resp[(Beverages==bever)],hours[(Beverages==bever)],mean)
 points(c(0,1,2,4,12),BeverHourAve,col=bever,pch=bever)
 lines(c(0,1,2,4,12),BeverHourAve,col=bever)
 colvec<-c(colvec,bever)
 pchvec<-c(pchvec,bever)
 ltyvec<-c(ltyvec,bever)
}
legendtext<-c("Alcohol","Grape juice","Red wine","Water")
legend(8,4.5,legendtext,col=colvec,pch=pchvec,lty=ltyvec)


## Linear models with cubic time effects

hours2<-(hours^2)
hours3<-(hours^3)
lm3<-lm(resp~BeverFac+hours+hours2+hours3)
summary(lm3)

## Define new data set for prediction

newdata<-as.data.frame(list(BeverFac="1",hours=6, hours2=36, hours3=216))
newprediction<-predict(lm3,newdata,interval="predict")










