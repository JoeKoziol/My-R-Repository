##################################################################################################################
#
# The R code is used to demonstrate the application of the logistic regression models to presidential election data set
#
##################################################################################################################

## read data into R
setwd("/home/joseph_koziol/School/STAT_385")
polls2008<-read.csv(file="2008-polls.csv",header=TRUE)
polls2012<-read.csv(file="2012-polls.csv",header=TRUE)
results2008<-read.csv(file="2008-results.csv",header=TRUE)

## Select data sampled by the pollers 

pollsters20085<-table(polls2008$Pollster)[table(polls2008$Pollster)>=5]
pollsters20125<-table(polls2012$Pollster)[table(polls2012$Pollster)>=5]
subset1<-names(pollsters20085)[names(pollsters20085)%in%names(pollsters20125)]
pollers<-names(pollsters20125)[names(pollsters20125)%in%subset1]
subsamplesID2008<-polls2008[,5]%in%pollers
polls2008sub<-polls2008[subsamplesID2008,]
subsamplesID2012<-polls2012[,5]%in%pollers
polls2012sub<-polls2012[subsamplesID2012,]

## reformatting the 2008 poll and true results data set as desired

winers2008<-(results2008[,2]-results2008[,3]>0)+0
StateID2008<-results2008[,1]
Allresponses<-NULL
for (sid in 1:51)
{
 polls2008substate<-polls2008sub[polls2008sub$State==StateID2008[sid],]
 pollwiners2008state<-(polls2008substate[,2]-polls2008substate[,3]>0)+0
 pollwinersIND<-(pollwiners2008state==winers2008[sid])+0
 Allresponses<-c(Allresponses,pollwinersIND)
}
margins<-abs(polls2008sub[,2]-polls2008sub[,3])
lagtime<-rep(0,dim(polls2008sub)[1])
electiondate2008<-c("Nov 04 2008")
for (i in 1:dim(polls2008sub)[1])
{
 lagtime[i]<-as.Date(electiondate2008, format="%b %d %Y")-as.Date(as.character(polls2008sub[i,4]), format="%b %d %Y")
}
dataset2008<-cbind(Allresponses,as.character(polls2008sub[,1]),margins,lagtime,as.character(polls2008sub[,5]))

## Focusing on the states with both successes and failures

stateslist<-unique(dataset2008[which(dataset2008[,1]=="0"),2])
subdataset2008<-dataset2008[dataset2008[,2]%in%stateslist,]
resp<-as.integer(subdataset2008[,1])
statesFAC<-as.factor(subdataset2008[,2])
margins<-as.double(subdataset2008[,3])
lagtime<-as.double(subdataset2008[,4])
pollersFAC<-as.factor(subdataset2008[,5])
logitreg<-glm(resp~statesFAC+margins+lagtime+pollersFAC,family="binomial")
summary(logitreg)

## Fit a simple logistic regression model without considering states as covariates

logitreg1<-glm(resp~margins+lagtime+pollersFAC,family="binomial")
summary(logitreg1)

## reformating the 2012 poll data for prediction purpose

pollwiners2012<-(polls2012sub[,2]-polls2012sub[,3]>0)+0
margins2012<-abs(polls2012sub[,2]-polls2012sub[,3])
lagtime2012<-rep(0,dim(polls2012sub)[1])
electiondate2012<-c("Nov 06 2012")
for (i in 1:dim(polls2012sub)[1])
{
 lagtime2012[i]<-as.Date(electiondate2012, format="%b %d %Y")-as.Date(as.character(polls2012sub[i,4]), format="%b %d %Y")
}
dataset2012<-cbind(pollwiners2012,as.character(polls2012sub[,1]),margins2012,lagtime2012,as.character(polls2012sub[,5]))

## Focusing on the states in the state list of 2008

subdataset2012<-dataset2012[dataset2012[,2]%in%stateslist,]

####################################################################################
## Do the prediction for the state "MI" using logistic regression models
####################################################################################

margins2012<-as.double(subdataset2012[,3])
lagtime2012<-as.double(subdataset2012[,4])
pollersFAC2012<-as.factor(subdataset2012[,5])

NOpolls<-sum(subdataset2012[,2]=="MI")
locations<-which(subdataset2012[,2]=="MI")
MIPredictresults<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 MIdatapoints<-data.frame(statesFAC="MI", margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 MIPredictresults[counts,2]<-predict(logitreg, MIdatapoints, type="response") 
}

MIprobDemwin<-MIPredictresults[,1]*MIPredictresults[,2]+(1-MIPredictresults[,1])*(1-MIPredictresults[,2])
MImeanProbDemwin<-mean(MIprobDemwin)
MIprobGopwin<-(1-MIPredictresults[,1])*MIPredictresults[,2]+MIPredictresults[,1]*(1-MIPredictresults[,2])
MImeanProbGopwin<-mean(MIprobGopwin)

## Do the prediction for the state "MI" using the simple logistic regression model

NOpolls<-sum(subdataset2012[,2]=="MI")
locations<-which(subdataset2012[,2]=="MI")
MIPredictresults1<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 MIdatapoints<-data.frame(margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 MIPredictresults1[counts,2]<-predict(logitreg1, MIdatapoints, type="response") 
}

MIprobDemwin1<-MIPredictresults1[,1]*MIPredictresults1[,2]+(1-MIPredictresults1[,1])*(1-MIPredictresults1[,2])
MImeanProbDemwin1<-mean(MIprobDemwin1)
MIprobGopwin1<-(1-MIPredictresults1[,1])*MIPredictresults1[,2]+MIPredictresults1[,1]*(1-MIPredictresults1[,2])
MImeanProbGopwin1<-mean(MIprobGopwin1)

####################################################################################
## Do the prediction for the state "FL" using the logistic regression models
####################################################################################

margins2012<-as.double(subdataset2012[,3])
lagtime2012<-as.double(subdataset2012[,4])
pollersFAC2012<-as.factor(subdataset2012[,5])

NOpolls<-sum(subdataset2012[,2]=="FL")
locations<-which(subdataset2012[,2]=="FL")
FLPredictresults<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 FLdatapoints<-data.frame(statesFAC="FL", margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 FLPredictresults[counts,2]<-predict(logitreg, FLdatapoints, type="response") 
}

FLprobDemwin<-FLPredictresults[,1]*FLPredictresults[,2]+(1-FLPredictresults[,1])*(1-FLPredictresults[,2])
FLmeanProbDemwin<-mean(FLprobDemwin)
FLprobGopwin<-(1-FLPredictresults[,1])*FLPredictresults[,2]+FLPredictresults[,1]*(1-FLPredictresults[,2])
FLmeanProbGopwin<-mean(FLprobGopwin)

## Do the prediction for the state "FL" using the simple logistic regression model

NOpolls<-sum(subdataset2012[,2]=="FL")
locations<-which(subdataset2012[,2]=="FL")
FLPredictresults1<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 FLdatapoints<-data.frame(margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 FLPredictresults1[counts,2]<-predict(logitreg1, FLdatapoints, type="response") 
}

FLprobDemwin1<-FLPredictresults1[,1]*FLPredictresults1[,2]+(1-FLPredictresults1[,1])*(1-FLPredictresults1[,2])
FLmeanProbDemwin1<-mean(FLprobDemwin1)
FLprobGopwin1<-(1-FLPredictresults1[,1])*FLPredictresults1[,2]+FLPredictresults1[,1]*(1-FLPredictresults1[,2])
FLmeanProbGopwin1<-mean(FLprobGopwin1)

####################################################################################
## Do the prediction for the state "MO" using the logistic regression models
####################################################################################

margins2012<-as.double(subdataset2012[,3])
lagtime2012<-as.double(subdataset2012[,4])
pollersFAC2012<-as.factor(subdataset2012[,5])

NOpolls<-sum(subdataset2012[,2]=="MO")
locations<-which(subdataset2012[,2]=="MO")
MOPredictresults<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 MOdatapoints<-data.frame(statesFAC="MO", margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 MOPredictresults[counts,2]<-predict(logitreg, MOdatapoints, type="response") 
}

MOprobDemwin<-MOPredictresults[,1]*MOPredictresults[,2]+(1-MOPredictresults[,1])*(1-MOPredictresults[,2])
MOmeanProbDemwin<-mean(MOprobDemwin)
MOprobGopwin<-(1-MOPredictresults[,1])*MOPredictresults[,2]+MOPredictresults[,1]*(1-MOPredictresults[,2])
MOmeanProbGopwin<-mean(MOprobGopwin)

## Do the prediction for the state "MO" using the simple logistic regression model

NOpolls<-sum(subdataset2012[,2]=="MO")
locations<-which(subdataset2012[,2]=="MO")
MOPredictresults1<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
 counts<-counts+1
 MOdatapoints<-data.frame(margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 MOPredictresults1[counts,2]<-predict(logitreg1, MOdatapoints, type="response") 
}

MOprobDemwin1<-MOPredictresults1[,1]*MOPredictresults1[,2]+(1-MOPredictresults1[,1])*(1-MOPredictresults1[,2])
MOmeanProbDemwin1<-mean(MOprobDemwin1)
MOprobGopwin1<-(1-MOPredictresults1[,1])*MOPredictresults1[,2]+MOPredictresults1[,1]*(1-MOPredictresults1[,2])
MOmeanProbGopwin1<-mean(MOprobGopwin1)

####################################################################################
## Do the prediction for the state "CO" using logistic regression models
####################################################################################

margins2012<-as.double(subdataset2012[,3])
lagtime2012<-as.double(subdataset2012[,4])
pollersFAC2012<-as.factor(subdataset2012[,5])

NOpolls<-sum(subdataset2012[,2]=="CO")
COPredictresults<-cbind(as.double(subdataset2012[1:NOpolls,1]),rep(0,NOpolls))
for (i in 1:NOpolls)
{
 COdatapoints<-data.frame(statesFAC="CO", margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 COPredictresults[i,2]<-predict(logitreg, COdatapoints, type="response") 
}

COprobDemwin<-COPredictresults[,1]*COPredictresults[,2]+(1-COPredictresults[,1])*(1-COPredictresults[,2])
COmeanProbDemwin<-mean(COprobDemwin)
COprobGopwin<-(1-COPredictresults[,1])*COPredictresults[,2]+COPredictresults[,1]*(1-COPredictresults[,2])
COmeanProbGopwin<-mean(COprobGopwin)

## Do the prediction for the state "CO" using the simple logistic regression model

NOpolls<-sum(subdataset2012[,2]=="CO")
COPredictresults1<-cbind(as.double(subdataset2012[1:NOpolls,1]),rep(0,NOpolls))
for (i in 1:NOpolls)
{
 COdatapoints<-data.frame(margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
 COPredictresults1[i,2]<-predict(logitreg1, COdatapoints, type="response") 
}

COprobDemwin1<-COPredictresults1[,1]*COPredictresults1[,2]+(1-COPredictresults1[,1])*(1-COPredictresults1[,2])
COmeanProbDemwin1<-mean(COprobDemwin1)
COprobGopwin1<-(1-COPredictresults1[,1])*COPredictresults1[,2]+COPredictresults1[,1]*(1-COPredictresults1[,2])
COmeanProbGopwin1<-mean(COprobGopwin1)


#####################################################
#
# Construct prediction intervals using logistic regression model
#
#####################################################

Deritive<-function(x,beta)
{
 deri0<-exp(x%*%beta)
 deri1<-deri0/((1+deri0)^2)
 return(deri1)
}

## Michigan

locations<-which(subdataset2012[,2]=="MI")
sub.MI2012<-subdataset2012[locations,]
loc2008<-which(subdataset2008[,2]=="MI")
SApart<-model.matrix(logitreg)[loc2008[1],c(1:23)]
ModMatQ4<-NULL
for (i in 1:dim(sub.MI2012)[1])
{
 pollerloc2008<-which(subdataset2008[,5]==sub.MI2012[i,5])
 PollersIND<-model.matrix(logitreg)[pollerloc2008[1],c(26:38)]
 ModMatQ4<-rbind(ModMatQ4,c(SApart,as.numeric(sub.MI2012[i,3:4]),PollersIND))
}

Ghat1<-apply(ModMatQ4, 1, Deritive, beta=coef(logitreg))
Ghat2<-ModMatQ4*Ghat1
Ghat3<-Ghat2*((-1)^(1+as.numeric(sub.MI2012[,1])))
Ghat<-colMeans(Ghat3)
Varphat<-t(Ghat)%*%vcov(logitreg)%*%Ghat
MIPredIntQ4Dem<-c(MImeanProbDemwin-qnorm(0.975)*sqrt(Varphat),MImeanProbDemwin+qnorm(0.975)*sqrt(Varphat))
Ghat3rep<-Ghat2*((-1)^(as.numeric(sub.MI2012[,1])))
Ghatrep<-colMeans(Ghat3rep)
Varphatrep<-t(Ghatrep)%*%vcov(logitreg)%*%Ghatrep
MIPredIntQ4Rep<-c(MImeanProbGopwin-qnorm(0.975)*sqrt(Varphatrep),MImeanProbGopwin+qnorm(0.975)*sqrt(Varphatrep))

## Florida

locations<-which(subdataset2012[,2]=="FL")
sub.FL2012<-subdataset2012[locations,]
loc2008<-which(subdataset2008[,2]=="FL")
SApart<-model.matrix(logitreg)[loc2008[1],c(1:23)]
ModMatQ4<-NULL
for (i in 1:dim(sub.FL2012)[1])
{
 pollerloc2008<-which(subdataset2008[,5]==sub.FL2012[i,5])
 PollersIND<-model.matrix(logitreg)[pollerloc2008[1],c(26:38)]
 ModMatQ4<-rbind(ModMatQ4,c(SApart,as.numeric(sub.FL2012[i,3:4]),PollersIND))
}

Ghat1<-apply(ModMatQ4, 1, Deritive, beta=coef(logitreg))
Ghat2<-ModMatQ4*Ghat1
Ghat3<-Ghat2*((-1)^(1+as.numeric(sub.FL2012[,1])))
Ghat<-colMeans(Ghat3)
Varphat<-t(Ghat)%*%vcov(logitreg)%*%Ghat
FLPredIntQ4Dem<-c(FLmeanProbDemwin-qnorm(0.975)*sqrt(Varphat),FLmeanProbDemwin+qnorm(0.975)*sqrt(Varphat))
Ghat3rep<-Ghat2*((-1)^(as.numeric(sub.FL2012[,1])))
Ghatrep<-colMeans(Ghat3rep)
Varphatrep<-t(Ghatrep)%*%vcov(logitreg)%*%Ghatrep
FLPredIntQ4Rep<-c(FLmeanProbGopwin-qnorm(0.975)*sqrt(Varphatrep),FLmeanProbGopwin+qnorm(0.975)*sqrt(Varphatrep))

## MO

locations<-which(subdataset2012[,2]=="MO")
sub.MO2012<-subdataset2012[locations,]
loc2008<-which(subdataset2008[,2]=="MO")
SApart<-model.matrix(logitreg)[loc2008[1],c(1:23)]
ModMatQ4<-NULL
for (i in 1:dim(sub.MO2012)[1])
{
 pollerloc2008<-which(subdataset2008[,5]==sub.MO2012[i,5])
 PollersIND<-model.matrix(logitreg)[pollerloc2008[1],c(26:38)]
 ModMatQ4<-rbind(ModMatQ4,c(SApart,as.numeric(sub.MO2012[i,3:4]),PollersIND))
}

Ghat1<-apply(ModMatQ4, 1, Deritive, beta=coef(logitreg))
Ghat2<-ModMatQ4*Ghat1
Ghat3<-Ghat2*((-1)^(1+as.numeric(sub.MO2012[,1])))
Ghat<-colMeans(Ghat3)
Varphat<-t(Ghat)%*%vcov(logitreg)%*%Ghat
MOPredIntQ4Dem<-c(MOmeanProbDemwin-qnorm(0.975)*sqrt(Varphat),MOmeanProbDemwin+qnorm(0.975)*sqrt(Varphat))
Ghat3rep<-Ghat2*((-1)^(as.numeric(sub.MO2012[,1])))
Ghatrep<-colMeans(Ghat3rep)
Varphatrep<-t(Ghatrep)%*%vcov(logitreg)%*%Ghatrep
MOPredIntQ4Rep<-c(MOmeanProbGopwin-qnorm(0.975)*sqrt(Varphatrep),MOmeanProbGopwin+qnorm(0.975)*sqrt(Varphatrep))

## Colorado

locations<-which(subdataset2012[,2]=="CO")
sub.CO2012<-subdataset2012[locations,]
loc2008<-which(subdataset2008[,2]=="CO")
SApart<-model.matrix(logitreg)[loc2008[1],c(1:23)]
ModMatQ4<-NULL
for (i in 1:dim(sub.CO2012)[1])
{
 pollerloc2008<-which(subdataset2008[,5]==sub.CO2012[i,5])
 PollersIND<-model.matrix(logitreg)[pollerloc2008[1],c(26:38)]
 ModMatQ4<-rbind(ModMatQ4,c(SApart,as.numeric(sub.CO2012[i,3:4]),PollersIND))
}

Ghat1<-apply(ModMatQ4, 1, Deritive, beta=coef(logitreg))
Ghat2<-ModMatQ4*Ghat1
Ghat3<-Ghat2*((-1)^(1+as.numeric(sub.CO2012[,1])))
Ghat<-colMeans(Ghat3)
Varphat<-t(Ghat)%*%vcov(logitreg)%*%Ghat
COPredIntQ4Dem<-c(COmeanProbDemwin-qnorm(0.975)*sqrt(Varphat),COmeanProbDemwin+qnorm(0.975)*sqrt(Varphat))
Ghat3rep<-Ghat2*((-1)^(as.numeric(sub.CO2012[,1])))
Ghatrep<-colMeans(Ghat3rep)
Varphatrep<-t(Ghatrep)%*%vcov(logitreg)%*%Ghatrep
COPredIntQ4Rep<-c(COmeanProbGopwin-qnorm(0.975)*sqrt(Varphatrep),COmeanProbGopwin+qnorm(0.975)*sqrt(Varphatrep))





