########################################################################################################################
## R script for Faulkenberry & Wood, "Dynamics of the operator preview effect in mental arithmetic"
##
## note: there are helper functions at the bottom of the script that need to be excuted before plotting
########################################################################################################################

library(ggplot2)
setwd("~/github/mousetrackingOperatorPreviewEffect/results/")
source("helperFunctions.R")

summaryMeasures <- read.csv("processed2.csv")
summaryMeasures2<-read.csv("processed.csv")
xNorm <- read.csv("nx.csv")
yNorm <- read.csv("ny.csv")

rawData <- summaryMeasures

# note: 16 trials manually removed before processing..total of 51*256=13056 trials completed
# also manually removed error trials from processed.csv; this makes RTs import correctly
# clean up data
#dataStep3<-subset(rawData,subset=accuracy==1) # remove errors

meanInit<-mean(dataStep3$init_time)
sdInit<-sd(dataStep3$init_time)
data<-subset(dataStep3,subset=init_time<meanInit+3*sdInit & rt>200) # remove 3 SD outliers
attach(data)

# Time measures

# RT
agg=aggregate(rt~subject_nr+SOAcondition+operation+size+truth,data=data,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(rt~SOAcondition*operation*size*as.factor(truth)+Error(as.factor(subject_nr)/(SOAcondition*operation*size*as.factor(truth))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="rt",withinvars=c("SOAcondition","operation","size","truth"),idvar="subject_nr")
ggplot(summary,aes(x=SOAcondition,y=rt,shape=operation))+geom_line(aes(group=operation,linetype=operation))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=rt-ci,ymax=rt+ci))+labs(x="SOA condition",y="Mean RT (ms)")+facet_grid(truth~size)+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))


# Time measures

# init
agg=aggregate(init_time~subject_nr+SOAcondition+operation+size+truth,data=data,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init_time~SOAcondition*operation*size*as.factor(truth)+Error(as.factor(subject_nr)/(SOAcondition*operation*size*as.factor(truth))),data=agg)
summary(init.aov)
print(model.tables(RT.aov,"means"),digits=6)

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="init_time",withinvars=c("SOAcondition","operation","size","truth"),idvar="subject_nr")
ggplot(summary,aes(x=SOAcondition,y=init_time,shape=operation))+geom_line(aes(group=operation,linetype=operation))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=init_time-ci,ymax=init_time+ci))+labs(x="SOA condition",y="Mean Initiation Time (ms)")+facet_grid(truth~size)+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))


# Time measures

# MT
agg=aggregate(rt-init_time~subject_nr+SOAcondition+operation+size+truth,data=data,FUN="mean") # RT performance data aggregated by subject
names(agg)[6]<-c("MT")
MT.aov=aov(MT~SOAcondition*operation*size*as.factor(truth)+Error(as.factor(subject_nr)/(SOAcondition*operation*size*as.factor(truth))),data=agg)
summary(MT.aov)
print(model.tables(RT.aov,"means"),digits=6)

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="MT",withinvars=c("SOAcondition","operation","size","truth"),idvar="subject_nr")
ggplot(summary,aes(x=SOAcondition,y=MT,shape=operation))+geom_line(aes(group=operation,linetype=operation))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=MT-ci,ymax=MT+ci))+labs(x="SOA condition",y="Mean MT (ms)")+facet_grid(truth~size)+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))+theme_classic(20)+theme(axis.line.x=element_line(color="black",size=0.5,linetype="solid"),axis.line.y=element_line(color="black",size=0.5,linetype="solid"))



