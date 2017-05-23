############################################################################################################
## R script for Faulkenberry & Wood, "Dynamics of the operator preview effect in mental arithmetic"
########################################################################################################################

library(ggplot2)
library(BayesFactor)
setwd("~/github/mousetrackingOperatorPreviewEffect/results/")
source("helperFunctions.R")

rawData <- read.csv("processed.csv")

# note: 16 trials manually removed before processing..total of 51*256=13056 trials completed
# also manually removed 81 non-RT trials from processed.csv; this makes RTs import correctly
# clean up data
dataStep3<-subset(rawData,subset=accuracy==1) # remove errors

meanRT<-mean(dataStep3$rt)
sdRT<-sd(dataStep3$rt)
data<-subset(dataStep3,subset=rt<meanRT+3*rt & rt>200) # remove 3 SD outliers
data$size <- factor(data$size, levels=c("small","large"))
# get true trials 

dataTrue <- subset(data,subset=truth=="True")

# Time measures
# RT
agg=aggregate(rt~subject_nr+SOAcondition+operation+size,data=dataTrue,FUN="mean") # RT performance data aggregated by subject
agg$subject_nr <- as.factor(agg$subject_nr)
RT.aov=aov(rt~SOAcondition*operation*size+Error(subject_nr/(SOAcondition*operation*size)),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)


bf1 <- anovaBF(rt~SOAcondition*operation*size+subject_nr,data=agg,whichRandom="subject_nr")
bf1
bf1[14]/bf1[15]  # favors model with no SOA x operation interaction by factor of 7.79



# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="rt",withinvars=c("SOAcondition","operation","size"),idvar="subject_nr")
RTtrue <- ggplot(summary,aes(x=SOAcondition,y=rt,shape=operation))+geom_line(aes(group=operation,linetype=operation))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=rt-ci,ymax=rt+ci))+labs(x="SOA condition",y="Mean RT (ms)")+facet_grid(~size)+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))

RTtrue
# get false trials 

dataFalse <- subset(data,subset=truth=="False")

# Time measures
# RT
aggFalse=aggregate(rt~subject_nr+SOAcondition+operation+size,data=dataFalse,FUN="mean") # RT performance data aggregated by subject
aggFalse$subject_nr <- as.factor(aggFalse$subject_nr)
RT.aov=aov(rt~SOAcondition*operation*size+Error(subject_nr/(SOAcondition*operation*size)),data=aggFalse)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)

bf2 <- anovaBF(rt~SOAcondition*operation*size+subject_nr,data=aggFalse,whichRandom="subject_nr")
bf2
bf2[14]/bf2[15]  # favors model with no SOA x operation interaction by factor of 4.86


# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="rt",withinvars=c("SOAcondition","operation","size"),idvar="subject_nr")
RTfalse <- ggplot(summary,aes(x=SOAcondition,y=rt,shape=operation))+geom_line(aes(group=operation,linetype=operation))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=rt-ci,ymax=rt+ci))+labs(x="SOA condition",y="Mean RT (ms)")+facet_grid(~size)+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))
RTfalse



# plot mouse tracks for addition problems

dataLeftNeutral<-subset(data,response==1 & SOAcondition=="neutral" & operation=="add")
dataLeftNegative<-subset(data,response==1 & SOAcondition=="negative" & operation=="add")
dataRightNeutral<-subset(data,response==2 & SOAcondition=="neutral" & operation=="add")
dataRightNegative<-subset(data,response==2 & SOAcondition=="negative" & operation=="add")

xCoords=rep(0,404)
yCoords=rep(0,404)
condition=rep(0,404)
side=rep(0,404)

for (i in 1:101){
  xCoords[i]=-mean(dataLeftNeutral[,i+15])
  yCoords[i]=mean(dataLeftNeutral[,i+116])
  condition[i]="neutral SOA"
  side[i]="leftward"
  
  xCoords[i+101]=mean(dataRightNeutral[,i+15])
  yCoords[i+101]=mean(dataRightNeutral[,i+116])
  condition[i+101]="neutral SOA"
  side[i+101]="rightward"
  
  xCoords[i+202]=-mean(dataLeftNegative[,i+15])
  yCoords[i+202]=mean(dataLeftNegative[,i+116])
  condition[i+202]="negative SOA"
  side[i+202]="leftward"
  
  xCoords[i+303]=mean(dataRightNegative[,i+15])
  yCoords[i+303]=mean(dataRightNegative[,i+116])
  condition[i+303]="negative SOA"
  side[i+303]="rightward"
}


trajectoryData=data.frame(xCoords,yCoords,condition,side)
plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=condition))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=condition),size=0.8)
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(~side)
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axisFormat=theme(axis.title=element_text(size=rel(1.4)))
legend=labs(linetype="Condition")+theme(legend.position=c(0.5,1))+theme(legend.background=element_rect(fill="white",colour="black"))
classic=theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))

plot+paths+labels+faceting+axisFormat+legend+legendFormat+classic+theme(legend.position=c(0.5,0.4))



# plot mouse tracks for multiplication problems

dataLeftNeutral<-subset(data,response==1 & SOAcondition=="neutral" & operation=="mult")
dataLeftNegative<-subset(data,response==1 & SOAcondition=="negative" & operation=="mult")
dataRightNeutral<-subset(data,response==2 & SOAcondition=="neutral" & operation=="mult")
dataRightNegative<-subset(data,response==2 & SOAcondition=="negative" & operation=="mult")

xCoords=rep(0,404)
yCoords=rep(0,404)
condition=rep(0,404)
side=rep(0,404)

for (i in 1:101){
  xCoords[i]=-mean(dataLeftNeutral[,i+15])
  yCoords[i]=mean(dataLeftNeutral[,i+116])
  condition[i]="neutral SOA"
  side[i]="leftward"
  
  xCoords[i+101]=mean(dataRightNeutral[,i+15])
  yCoords[i+101]=mean(dataRightNeutral[,i+116])
  condition[i+101]="neutral SOA"
  side[i+101]="rightward"
  
  xCoords[i+202]=-mean(dataLeftNegative[,i+15])
  yCoords[i+202]=mean(dataLeftNegative[,i+116])
  condition[i+202]="negative SOA"
  side[i+202]="leftward"
  
  xCoords[i+303]=mean(dataRightNegative[,i+15])
  yCoords[i+303]=mean(dataRightNegative[,i+116])
  condition[i+303]="negative SOA"
  side[i+303]="rightward"
}


trajectoryData=data.frame(xCoords,yCoords,condition,side)
plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=condition))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=condition),size=0.8)
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(~side)
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axisFormat=theme(axis.title=element_text(size=rel(1.4)))
legend=labs(linetype="Condition")+theme(legend.position=c(0.5,1))+theme(legend.background=element_rect(fill="white",colour="black"))
classic=theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))

plot+paths+labels+faceting+axisFormat+legend+legendFormat+classic+theme(legend.position=c(0.5,0.4))



# Trajectory measures

# AUC
agg=aggregate(auc~subject_nr+SOAcondition+operation+response,data=data,FUN="mean") # RT performance data aggregated by subject
agg$subject_nr <- as.factor(agg$subject_nr)
agg$side <- factor(agg$response, labels=c("left","right"))
auc.aov=aov(auc~SOAcondition*operation*side+Error(subject_nr/(SOAcondition*operation*side)),data=agg)
summary(auc.aov)
print(model.tables(auc.aov,"means"),digits=6)

bf3 <- anovaBF(auc~SOAcondition*operation*side+subject_nr,data=agg,whichRandom="subject_nr")
sort(bf3)
bf3


# MD
agg=aggregate(md~subject_nr+SOAcondition+operation+response,data=data,FUN="mean") # RT performance data aggregated by subject
agg$subject_nr <- as.factor(agg$subject_nr)
agg$response <- as.factor(agg$response)
md.aov=aov(md~SOAcondition*operation*response+Error(subject_nr/(SOAcondition*operation*response)),data=agg)
summary(md.aov)
print(model.tables(auc.aov,"means"),digits=6)

bf3 <- anovaBF(md~SOAcondition*operation*response+subject_nr,data=agg,whichRandom="subject_nr")
bf3
